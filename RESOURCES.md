# Google Cloud Document AI Resources

## Knowledge

- [Document AI setup](https://cloud.google.com/document-ai/docs/setup)
  Official prerequisites: Google Cloud project, API enablement, authentication, and supported client libraries. Use for: first local or service deployment.
- [Processor list](https://cloud.google.com/document-ai/docs/processors-list)
  Canonical list of processor types, regions, release stages, language support, and request-page limits. Use for: choosing a processor and checking current constraints.
- [Enterprise Document OCR](https://cloud.google.com/document-ai/docs/enterprise-document-ocr)
  Explains document-oriented OCR, layout extraction, quality analysis, language hints, and OCR add-ons. Use for: text/layout-first use cases and difficult scans.
- [Form Parser](https://cloud.google.com/document-ai/docs/form-parser)
  Describes key-value, table, checkbox, generic-entity, and text extraction for forms. Use for: variable forms with labeled fields.
- [Process a document with a specialized processor](https://cloud.google.com/document-ai/docs/samples/documentai-process-specialized-document)
  Official client-library sample for invoking a processor and receiving a `Document` response. Use for: a first Python implementation.
- [Document AI API: `processors.process`](https://cloud.google.com/document-ai/docs/reference/rest/v1/projects.locations.processors/process)
  REST contract for synchronous processing. Use for: API design, authorization, request limits, and error handling.
- [Sample processor output](https://cloud.google.com/document-ai/docs/output)
  Real inputs and `Document` JSON responses across processor types. Use for: learning the response object before writing parsers.

## Wisdom (Communities)

- [Google Cloud Community](https://www.googlecloudcommunity.com/)
  Practitioner discussions and product-specific troubleshooting. Use for: non-sensitive integration questions after reproducing the issue with a minimal example.

## Gaps

- Benchmark results on the target Thai and mixed-language invoice/form corpus.
- Current per-page pricing and regional availability for the final chosen processor; check the official pricing page immediately before a paid rollout.
