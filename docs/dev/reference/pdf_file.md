# Create a PDF Object

Stores a reference to a local PDF file along with a pdftools text
extraction. Binary encoding is deferred to format time; providers that
support binary PDF encode on demand, others use the stored text
fallback.

## Usage

``` r
pdf_file(.path, pages = NULL, .text_extract = FALSE, engine = NULL)
```

## Arguments

- .path:

  The path to the PDF file on disk.

- pages:

  An integer vector of page numbers to include (e.g. `1:3`). NULL uses
  all pages.

- .text_extract:

  Logical; if TRUE always use pdftools text extraction regardless of
  provider (equivalent to the old `.pdf` parameter behaviour). Default
  FALSE uses binary-first.

- engine:

  Optional engine name (e.g. `"mistral-ocr"`); a warning is issued if
  the provider does not support binary PDF and the engine choice cannot
  be honoured.

## Value

A `tidyllm_pdf` object.
