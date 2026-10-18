"""Deterministic routing for invoice-adapter profiles.

This module routes from verified routing facts to an adapter profile. It does not
perform OCR or guess a vendor from free text. A tie is deliberately a REVIEW
result rather than an arbitrary choice.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Optional, Sequence, Tuple


@dataclass(frozen=True)
class RoutingFacts:
    supplier_tax_id: Optional[str]
    document_class: Optional[str]  # e.g. invoice, credit_note, receipt
    languages: Tuple[str, ...]


@dataclass(frozen=True)
class Profile:
    id: str
    priority: int = 0
    supplier_tax_ids: Tuple[str, ...] = ()
    document_classes: Tuple[str, ...] = ()
    languages: Tuple[str, ...] = ()
    is_default: bool = False


@dataclass(frozen=True)
class RouteDecision:
    status: str  # ROUTED or REVIEW
    profile_id: Optional[str]
    reason: str
    candidates: Tuple[str, ...] = ()


def route_document(facts: RoutingFacts, profiles: Iterable[Profile]) -> RouteDecision:
    """Choose one profile, a default, or a review route.

    Matching is intentionally strict: every constraint configured in a profile
    must match a trustworthy fact. The rank uses stable business weights, but
    equal winners are never broken arbitrarily.
    """
    profiles = tuple(profiles)
    defaults = [profile for profile in profiles if profile.is_default]
    if len(defaults) != 1:
        raise ValueError("exactly one default profile is required")

    matches = []
    for profile in profiles:
        if profile.is_default:
            continue
        score = _match_score(facts, profile)
        if score is not None:
            matches.append((score, profile.priority, profile))

    if not matches:
        return RouteDecision(
            status="ROUTED",
            profile_id=defaults[0].id,
            reason="no specific profile matched; selected default",
        )

    matches.sort(key=lambda item: (-item[0], -item[1], item[2].id))
    best_score, best_priority, best_profile = matches[0]
    tied = [
        profile.id
        for score, priority, profile in matches
        if score == best_score and priority == best_priority
    ]
    if len(tied) > 1:
        return RouteDecision(
            status="REVIEW",
            profile_id=None,
            reason="ambiguous profile match; no arbitrary tie-break",
            candidates=tuple(sorted(tied)),
        )

    return RouteDecision(
        status="ROUTED",
        profile_id=best_profile.id,
        reason="matched deterministic routing facts (score={0})".format(best_score),
    )


def _match_score(facts: RoutingFacts, profile: Profile) -> Optional[int]:
    score = 0
    if profile.supplier_tax_ids:
        if not facts.supplier_tax_id or _normalise_id(facts.supplier_tax_id) not in {
            _normalise_id(value) for value in profile.supplier_tax_ids
        }:
            return None
        score += 1000

    if profile.document_classes:
        if not facts.document_class or facts.document_class.lower() not in {
            value.lower() for value in profile.document_classes
        }:
            return None
        score += 100

    if profile.languages:
        fact_languages = {value.lower() for value in facts.languages}
        allowed_languages = {value.lower() for value in profile.languages}
        if not fact_languages.intersection(allowed_languages):
            return None
        score += 10

    if score == 0:
        return None  # a non-default profile needs at least one real condition
    return score


def _normalise_id(value: str) -> str:
    return "".join(character for character in value.upper() if character.isalnum())
