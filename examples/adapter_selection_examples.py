"""Runnable examples for selecting invoice/form schema adapters.

Run from the repository root:
    python3 examples/adapter_selection_examples.py
"""
from profile_router import Profile, RoutingFacts, route_document


PROFILES = (
    # The sole default is deliberately conservative. It can save raw output and
    # open a review task, but should not silently post financial data.
    Profile(id="manual-review-default", is_default=True),
    Profile(
        id="vendor-a-invoice-adapter",
        priority=100,
        supplier_tax_ids=("0105559999999",),
        document_classes=("invoice",),
        languages=("th", "en"),
    ),
    Profile(
        id="vendor-a-credit-note-adapter",
        priority=100,
        supplier_tax_ids=("0105559999999",),
        document_classes=("credit_note",),
        languages=("th", "en"),
    ),
    Profile(
        id="generic-thai-invoice-adapter",
        priority=10,
        document_classes=("invoice",),
        languages=("th",),
    ),
    Profile(
        id="generic-english-invoice-adapter",
        priority=10,
        document_classes=("invoice",),
        languages=("en",),
    ),
    Profile(
        id="generic-receipt-adapter",
        priority=10,
        document_classes=("receipt",),
    ),
    Profile(
        id="generic-thai-form-adapter",
        priority=10,
        document_classes=("form",),
        languages=("th",),
    ),
)


CASES = (
    (
        "Known Vendor A Thai invoice",
        RoutingFacts("0105559999999", "invoice", ("th",)),
        "vendor-a-invoice-adapter",
    ),
    (
        "Same supplier, but credit note",
        RoutingFacts("0105559999999", "credit_note", ("th",)),
        "vendor-a-credit-note-adapter",
    ),
    (
        "Unknown supplier, Thai invoice",
        RoutingFacts("0999999999999", "invoice", ("th",)),
        "generic-thai-invoice-adapter",
    ),
    (
        "Unknown supplier, English invoice",
        RoutingFacts(None, "invoice", ("en",)),
        "generic-english-invoice-adapter",
    ),
    (
        "Vendor A invoice but tax ID was not reliably extracted",
        RoutingFacts(None, "invoice", ("th",)),
        "generic-thai-invoice-adapter",
    ),
    (
        "Receipt from any supplier",
        RoutingFacts("0105559999999", "receipt", ("th",)),
        "generic-receipt-adapter",
    ),
    (
        "Thai non-invoice form",
        RoutingFacts(None, "form", ("th",)),
        "generic-thai-form-adapter",
    ),
    (
        "Unknown document class",
        RoutingFacts(None, "unknown", ("th",)),
        "manual-review-default",
    ),
)


def main() -> None:
    for label, facts, expected_profile in CASES:
        decision = route_document(facts, PROFILES)
        assert decision.status == "ROUTED", (label, decision)
        assert decision.profile_id == expected_profile, (label, decision)
        print("{0:<56} -> {1}".format(label, decision.profile_id))

    # Misconfiguration example: two equally specific profiles match. The
    # deterministic result is REVIEW, not whichever profile appeared first.
    ambiguous_profiles = PROFILES + (
        Profile(
            id="duplicate-generic-thai-invoice-adapter",
            priority=10,
            document_classes=("invoice",),
            languages=("th",),
        ),
    )
    ambiguous = route_document(
        RoutingFacts(None, "invoice", ("th",)), ambiguous_profiles
    )
    assert ambiguous.status == "REVIEW"
    print("{0:<56} -> REVIEW {1}".format(
        "Duplicate equally specific profiles", ambiguous.candidates
    ))


if __name__ == "__main__":
    main()
