"""Pure, testable adapter from a Document AI-style response to an app schema.

The boundary deliberately accepts a JSON-like mapping rather than a live Google
client object. Obtain the mapping from the raw Document AI response, store it,
and pass it here. This keeps Google SDK calls, business rules, and persistence
separate.
"""
from __future__ import annotations

from dataclasses import dataclass
from decimal import Decimal, InvalidOperation
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple

MONEY_TOLERANCE = Decimal("0.01")


@dataclass(frozen=True)
class FieldRule:
    """One field in the application schema and its expected entity types."""

    entity_types: Tuple[str, ...]
    kind: str  # "text", "date", or "money"
    required: bool = False


@dataclass(frozen=True)
class Evidence:
    entity_type: str
    raw_text: Optional[str]
    confidence: Optional[float]
    page: Optional[int]
    text_segments: Tuple[Tuple[int, int], ...]


@dataclass(frozen=True)
class InvoiceRecord:
    invoice_number: Optional[str]
    supplier_name: Optional[str]
    invoice_date: Optional[str]  # ISO 8601 YYYY-MM-DD
    net_amount: Optional[Decimal]
    vat_amount: Optional[Decimal]
    total_amount: Optional[Decimal]
    currency: Optional[str]
    evidence: Mapping[str, Evidence]


@dataclass(frozen=True)
class AdaptationResult:
    record: InvoiceRecord
    errors: Tuple[str, ...]
    needs_review: bool


def adapt_invoice(
    document: Mapping[str, Any], rules: Mapping[str, FieldRule]
) -> AdaptationResult:
    """Map Document AI entities to a stable invoice schema.

    `document` must contain an `entities` list from the raw Document AI
    response. `rules` is application-owned configuration: inspect a saved
    sample response and set entity type names deliberately.
    """
    entities_by_type = _index_entities(document.get("entities", []))
    values: Dict[str, Any] = {}
    evidence: Dict[str, Evidence] = {}
    errors: List[str] = []
    currencies: List[str] = []

    for field_name, rule in rules.items():
        candidates = _candidates(entities_by_type, rule.entity_types)
        entity = _choose_candidate(field_name, candidates, errors)
        if entity is None:
            values[field_name] = None
            if rule.required:
                errors.append("missing required field: {0}".format(field_name))
            continue

        evidence[field_name] = _evidence(entity)
        value, currency, parse_error = _parse_entity(entity, rule.kind)
        values[field_name] = value
        if currency:
            currencies.append(currency)
        if parse_error:
            errors.append("{0}: {1}".format(field_name, parse_error))

    currency = _single_currency(currencies, errors)
    _validate_totals(values, errors)

    record = InvoiceRecord(
        invoice_number=values.get("invoice_number"),
        supplier_name=values.get("supplier_name"),
        invoice_date=values.get("invoice_date"),
        net_amount=values.get("net_amount"),
        vat_amount=values.get("vat_amount"),
        total_amount=values.get("total_amount"),
        currency=currency,
        evidence=evidence,
    )
    return AdaptationResult(
        record=record,
        errors=tuple(errors),
        needs_review=bool(errors),
    )


def _index_entities(entities: Sequence[Mapping[str, Any]]) -> Dict[str, List[Mapping[str, Any]]]:
    indexed: Dict[str, List[Mapping[str, Any]]] = {}
    for entity in entities:
        entity_type = entity.get("type") or entity.get("type_")
        if entity_type:
            indexed.setdefault(str(entity_type), []).append(entity)
    return indexed


def _candidates(
    indexed: Mapping[str, List[Mapping[str, Any]]], entity_types: Sequence[str]
) -> List[Mapping[str, Any]]:
    found: List[Mapping[str, Any]] = []
    for entity_type in entity_types:
        found.extend(indexed.get(entity_type, []))
    return found


def _choose_candidate(
    field_name: str, candidates: Sequence[Mapping[str, Any]], errors: List[str]
) -> Optional[Mapping[str, Any]]:
    if not candidates:
        return None
    if len(candidates) > 1:
        errors.append("ambiguous field: {0} has {1} candidates".format(field_name, len(candidates)))
    return max(candidates, key=lambda item: float(item.get("confidence") or 0.0))


def _parse_entity(entity: Mapping[str, Any], kind: str) -> Tuple[Any, Optional[str], Optional[str]]:
    normalized = entity.get("normalizedValue") or entity.get("normalized_value") or {}
    if kind == "text":
        value = normalized.get("text") or entity.get("mentionText") or entity.get("mention_text")
        return (str(value).strip() if value is not None else None), None, None
    if kind == "date":
        date_value = normalized.get("dateValue") or normalized.get("date_value")
        if not date_value:
            return None, None, "missing normalized dateValue"
        try:
            return "{0:04d}-{1:02d}-{2:02d}".format(
                int(date_value["year"]), int(date_value["month"]), int(date_value["day"])
            ), None, None
        except (KeyError, TypeError, ValueError):
            return None, None, "invalid normalized dateValue"
    if kind == "money":
        money = normalized.get("moneyValue") or normalized.get("money_value")
        if not money:
            return None, None, "missing normalized moneyValue"
        try:
            units = Decimal(str(money.get("units", "0")))
            nanos = Decimal(str(money.get("nanos", 0))) / Decimal("1000000000")
            return units + nanos, money.get("currencyCode") or money.get("currency_code"), None
        except (InvalidOperation, TypeError, ValueError):
            return None, None, "invalid normalized moneyValue"
    return None, None, "unsupported field kind: {0}".format(kind)


def _evidence(entity: Mapping[str, Any]) -> Evidence:
    anchor = entity.get("textAnchor") or entity.get("text_anchor") or {}
    segments = anchor.get("textSegments") or anchor.get("text_segments") or []
    normalized_segments: List[Tuple[int, int]] = []
    for segment in segments:
        try:
            normalized_segments.append((int(segment.get("startIndex", segment.get("start_index", 0))), int(segment["endIndex"] if "endIndex" in segment else segment["end_index"])))
        except (KeyError, TypeError, ValueError):
            continue

    page = None
    page_anchor = entity.get("pageAnchor") or entity.get("page_anchor") or {}
    page_refs = page_anchor.get("pageRefs") or page_anchor.get("page_refs") or []
    if page_refs:
        raw_page = page_refs[0].get("page")
        try:
            page = int(raw_page) + 1 if raw_page is not None else None  # present humans with 1-based pages
        except (TypeError, ValueError):
            pass

    confidence = entity.get("confidence")
    try:
        confidence = float(confidence) if confidence is not None else None
    except (TypeError, ValueError):
        confidence = None
    return Evidence(
        entity_type=str(entity.get("type") or entity.get("type_") or ""),
        raw_text=entity.get("mentionText") or entity.get("mention_text"),
        confidence=confidence,
        page=page,
        text_segments=tuple(normalized_segments),
    )


def _single_currency(currencies: Sequence[str], errors: List[str]) -> Optional[str]:
    distinct = sorted(set(currency for currency in currencies if currency))
    if len(distinct) > 1:
        errors.append("conflicting currencies: {0}".format(", ".join(distinct)))
    return distinct[0] if len(distinct) == 1 else None


def _validate_totals(values: Mapping[str, Any], errors: List[str]) -> None:
    net, vat, total = values.get("net_amount"), values.get("vat_amount"), values.get("total_amount")
    if net is None or vat is None or total is None:
        return
    if abs((net + vat) - total) > MONEY_TOLERANCE:
        errors.append("total mismatch: net_amount + vat_amount != total_amount")
