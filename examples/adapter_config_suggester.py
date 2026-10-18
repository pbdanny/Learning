"""Suggest draft adapter profiles from human-reviewed extraction observations.

This is intentionally a proposal generator, not a production self-modifier.
Only reviewed observations are accepted. Generated profiles remain in DRAFT
until held-out evaluation and explicit approval.
"""
from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass
from typing import Dict, Iterable, List, Mapping, Optional, Sequence, Tuple


@dataclass(frozen=True)
class ReviewedObservation:
    document_id: str
    supplier_tax_id: Optional[str]
    document_class: str
    language: str
    canonical_field: str
    observed_alias: str
    mapping_correct: bool
    reviewed: bool = True


@dataclass(frozen=True)
class AliasSuggestion:
    alias: str
    support_documents: int
    observations: int
    precision: float


@dataclass(frozen=True)
class ProfileSuggestion:
    status: str
    profile_id: str
    supplier_tax_id: Optional[str]
    document_class: str
    language: str
    training_documents: int
    fields: Mapping[str, Tuple[AliasSuggestion, ...]]
    review_reasons: Tuple[str, ...]


def suggest_profiles(
    observations: Iterable[ReviewedObservation],
    *,
    minimum_documents: int = 3,
    minimum_alias_documents: int = 2,
    minimum_precision: float = 0.95,
) -> Tuple[ProfileSuggestion, ...]:
    """Create evidence-backed DRAFT profile suggestions.

    An alias is suggested only when it occurs in enough distinct reviewed
    documents and its reviewed mapping precision clears the configured gate.
    An alias observed against multiple canonical fields is excluded as a
    semantic conflict.
    """
    reviewed = tuple(item for item in observations if item.reviewed)
    groups: Dict[Tuple[Optional[str], str, str], List[ReviewedObservation]] = defaultdict(list)
    for item in reviewed:
        key = (_normalise_id(item.supplier_tax_id), item.document_class.lower(), item.language.lower())
        groups[key].append(item)

    suggestions: List[ProfileSuggestion] = []
    for (tax_id, document_class, language), items in sorted(groups.items(), key=lambda pair: str(pair[0])):
        document_ids = {item.document_id for item in items}
        if len(document_ids) < minimum_documents:
            continue

        aliases_to_fields: Dict[str, set] = defaultdict(set)
        for item in items:
            aliases_to_fields[_normalise_alias(item.observed_alias)].add(item.canonical_field)
        conflicting_aliases = {
            alias for alias, fields in aliases_to_fields.items() if len(fields) > 1
        }

        by_field_alias: Dict[Tuple[str, str], List[ReviewedObservation]] = defaultdict(list)
        for item in items:
            alias = _normalise_alias(item.observed_alias)
            by_field_alias[(item.canonical_field, alias)].append(item)

        fields: Dict[str, List[AliasSuggestion]] = defaultdict(list)
        review_reasons: List[str] = []
        for alias in sorted(conflicting_aliases):
            review_reasons.append("alias maps to multiple fields: {0}".format(alias))

        for (field_name, alias), alias_items in sorted(by_field_alias.items()):
            if alias in conflicting_aliases:
                continue
            support_documents = len({item.document_id for item in alias_items})
            if support_documents < minimum_alias_documents:
                continue
            precision = sum(item.mapping_correct for item in alias_items) / len(alias_items)
            if precision < minimum_precision:
                review_reasons.append(
                    "alias below precision gate: {0}.{1}={2:.3f}".format(
                        field_name, alias, precision
                    )
                )
                continue
            fields[field_name].append(
                AliasSuggestion(
                    alias=alias,
                    support_documents=support_documents,
                    observations=len(alias_items),
                    precision=precision,
                )
            )

        profile_id = "{0}-{1}-{2}-draft".format(
            tax_id or "generic", document_class, language
        )
        suggestions.append(
            ProfileSuggestion(
                status="DRAFT",
                profile_id=profile_id,
                supplier_tax_id=tax_id,
                document_class=document_class,
                language=language,
                training_documents=len(document_ids),
                fields={name: tuple(values) for name, values in sorted(fields.items())},
                review_reasons=tuple(sorted(set(review_reasons))),
            )
        )
    return tuple(suggestions)


def _normalise_id(value: Optional[str]) -> Optional[str]:
    if not value:
        return None
    return "".join(character for character in value.upper() if character.isalnum())


def _normalise_alias(value: str) -> str:
    return " ".join(value.casefold().split())
