import unittest

from adapter_config_suggester import ReviewedObservation, suggest_profiles


class AdapterConfigSuggesterTests(unittest.TestCase):
    def test_proposes_repeated_high_precision_aliases(self):
        observations = []
        for document_id in ("a", "b", "c", "d"):
            observations.extend(
                [
                    ReviewedObservation(document_id, "0105559999999", "invoice", "th", "invoice_number", "เลขที่ใบกำกับภาษี", True),
                    ReviewedObservation(document_id, "0105559999999", "invoice", "th", "total_amount", "ยอดรวมสุทธิ", True),
                ]
            )
        suggestions = suggest_profiles(observations)
        self.assertEqual(len(suggestions), 1)
        suggestion = suggestions[0]
        self.assertEqual(suggestion.status, "DRAFT")
        self.assertEqual(suggestion.training_documents, 4)
        self.assertEqual(suggestion.fields["total_amount"][0].alias, "ยอดรวมสุทธิ")
        self.assertEqual(suggestion.fields["total_amount"][0].precision, 1.0)

    def test_excludes_weak_and_conflicting_aliases(self):
        observations = [
            ReviewedObservation("a", "0105559999999", "invoice", "en", "total_amount", "Total", True),
            ReviewedObservation("b", "0105559999999", "invoice", "en", "total_amount", "Total", False),
            ReviewedObservation("c", "0105559999999", "invoice", "en", "net_amount", "Total", True),
            ReviewedObservation("d", "0105559999999", "invoice", "en", "total_amount", "Grand Total", True),
            ReviewedObservation("e", "0105559999999", "invoice", "en", "total_amount", "Grand Total", True),
        ]
        suggestion = suggest_profiles(observations, minimum_documents=3)
        self.assertEqual(len(suggestion), 1)
        profile = suggestion[0]
        aliases = [item.alias for item in profile.fields["total_amount"]]
        self.assertEqual(aliases, ["grand total"])
        self.assertTrue(any("multiple fields" in reason for reason in profile.review_reasons))

    def test_ignores_unreviewed_and_under_supported_clusters(self):
        observations = [
            ReviewedObservation("a", None, "invoice", "th", "total_amount", "ยอดรวม", True),
            ReviewedObservation("b", None, "invoice", "th", "total_amount", "ยอดรวม", True, reviewed=False),
        ]
        self.assertEqual(suggest_profiles(observations, minimum_documents=2), ())


if __name__ == "__main__":
    unittest.main(verbosity=2)
