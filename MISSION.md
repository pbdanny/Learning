# Mission: Production-grade Document AI extraction

## Why
Learn how to use Google Cloud Document AI to turn invoices and forms into auditable structured data for an OCR/extraction workflow. The goal is to make a defensible processor choice, integrate it safely, and measure field-level quality before treating output as business data.

## Success looks like
- Choose the appropriate Document AI processor for a representative invoice or form corpus and explain why.
- Process one PDF and map the response into a stable application schema while retaining source evidence.
- Build a small evaluation set that exposes field-level errors, language limitations, and document-quality failures.

## Constraints
- Start with supported Google-managed processors before considering training a custom model.
- Keep raw source documents and raw Document AI responses for auditability.
- Treat Thai or mixed-language invoices as an evaluation requirement, not an assumed supported case.
- Prefer Python tooling managed with `uv`; integrate with workflow tooling only after the extraction contract is stable.

## Out of scope
- Fine-tuning a custom extractor in the first lesson.
- Building a full production deployment before processor selection and baseline evaluation.
- Treating OCR output as ground truth without validation.
