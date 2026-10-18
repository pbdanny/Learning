import unittest

from profile_router import Profile, RoutingFacts, route_document


DEFAULT = Profile(id="default-invoice", is_default=True)
VENDOR_A = Profile(
    id="vendor-a-thai-invoice",
    priority=100,
    supplier_tax_ids=("0105559999999",),
    document_classes=("invoice",),
    languages=("th", "en"),
)
VENDOR_B = Profile(
    id="vendor-b-invoice",
    priority=100,
    supplier_tax_ids=("0999999999999",),
    document_classes=("invoice",),
)


class ProfileRouterTests(unittest.TestCase):
    def test_routes_exact_vendor_tax_id_with_formatting_variation(self):
        decision = route_document(
            RoutingFacts("0105-5599-9999-9", "invoice", ("th",)),
            (DEFAULT, VENDOR_A, VENDOR_B),
        )
        self.assertEqual(decision.status, "ROUTED")
        self.assertEqual(decision.profile_id, "vendor-a-thai-invoice")

    def test_uses_default_when_no_specific_profile_matches(self):
        decision = route_document(
            RoutingFacts("0123456789012", "invoice", ("th",)),
            (DEFAULT, VENDOR_A, VENDOR_B),
        )
        self.assertEqual(decision.status, "ROUTED")
        self.assertEqual(decision.profile_id, "default-invoice")

    def test_never_arbitrarily_breaks_a_tie(self):
        duplicate_a = Profile(id="a", priority=10, supplier_tax_ids=("0105559999999",))
        duplicate_b = Profile(id="b", priority=10, supplier_tax_ids=("0105559999999",))
        decision = route_document(
            RoutingFacts("0105559999999", "invoice", ("th",)),
            (DEFAULT, duplicate_a, duplicate_b),
        )
        self.assertEqual(decision.status, "REVIEW")
        self.assertEqual(decision.candidates, ("a", "b"))

    def test_does_not_match_a_profile_when_document_class_conflicts(self):
        decision = route_document(
            RoutingFacts("0105559999999", "credit_note", ("th",)),
            (DEFAULT, VENDOR_A),
        )
        self.assertEqual(decision.profile_id, "default-invoice")


if __name__ == "__main__":
    unittest.main(verbosity=2)
