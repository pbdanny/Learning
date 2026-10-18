import unittest
from decimal import Decimal

from document_ai_schema_adapter import FieldRule, adapt_invoice


RULES = {
    "invoice_number": FieldRule(("invoice_id",), "text", required=True),
    "supplier_name": FieldRule(("supplier_name",), "text", required=True),
    "invoice_date": FieldRule(("invoice_date",), "date", required=True),
    "net_amount": FieldRule(("net_amount",), "money"),
    "vat_amount": FieldRule(("vat_amount",), "money"),
    "total_amount": FieldRule(("total_amount",), "money", required=True),
}


def money(units, nanos=0, currency="THB"):
    return {"normalizedValue": {"moneyValue": {"units": str(units), "nanos": nanos, "currencyCode": currency}}}


class AdapterTests(unittest.TestCase):
    def test_maps_values_preserves_evidence_and_validates_totals(self):
        document = {
            "entities": [
                {"type": "invoice_id", "mentionText": "INV-42", "confidence": 0.99},
                {"type": "supplier_name", "mentionText": "Thai Supply Co.", "confidence": 0.98},
                {"type": "invoice_date", "normalizedValue": {"dateValue": {"year": 2026, "month": 10, "day": 18}}},
                dict({"type": "net_amount", "mentionText": "10,000.00"}, **money(10000)),
                dict({"type": "vat_amount", "mentionText": "700.00"}, **money(700)),
                dict({"type": "total_amount", "mentionText": "10,700.00", "textAnchor": {"textSegments": [{"startIndex": "52", "endIndex": "61"}]}, "pageAnchor": {"pageRefs": [{"page": "0"}]}}, **money(10700)),
            ]
        }
        result = adapt_invoice(document, RULES)

        self.assertFalse(result.needs_review, result.errors)
        self.assertEqual(result.record.invoice_number, "INV-42")
        self.assertEqual(result.record.invoice_date, "2026-10-18")
        self.assertEqual(result.record.total_amount, Decimal("10700"))
        self.assertEqual(result.record.currency, "THB")
        self.assertEqual(result.record.evidence["total_amount"].page, 1)
        self.assertEqual(result.record.evidence["total_amount"].text_segments, ((52, 61),))

    def test_total_mismatch_requires_review(self):
        document = {
            "entities": [
                {"type": "invoice_id", "mentionText": "INV-43"},
                {"type": "supplier_name", "mentionText": "Thai Supply Co."},
                {"type": "invoice_date", "normalizedValue": {"dateValue": {"year": 2026, "month": 10, "day": 18}}},
                dict({"type": "net_amount"}, **money(10000)),
                dict({"type": "vat_amount"}, **money(700)),
                dict({"type": "total_amount"}, **money(10900)),
            ]
        }
        result = adapt_invoice(document, RULES)
        self.assertTrue(result.needs_review)
        self.assertIn("total mismatch: net_amount + vat_amount != total_amount", result.errors)


if __name__ == "__main__":
    unittest.main(verbosity=2)
