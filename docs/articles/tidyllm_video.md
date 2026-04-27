# Working with Files and Media

**tidyllm** makes it straightforward to send images, audio, video, and
documents to any supported model. There are two ways to attach non-text
content to a message: the `.media` argument for content you want to send
directly, and the `.files` argument for content you have previously
uploaded to a provider’s servers. This article covers both.

## Inline Images

The simplest way to send an image is with
[`img()`](https://edubruell.github.io/tidyllm/reference/img.md). Pass
the path to a local image file, attach it to a message with `.media`,
and tidyllm takes care of the rest:

``` r

library(tidyllm)

llm_message("What food is in this image?",
            .media = img("hotdog.jpg")) |>
  chat(claude())
```

Images work with all major providers that support vision: Claude,
Gemini, OpenAI, Mistral, Groq, Ollama (with a vision-capable model), and
OpenRouter.

To send multiple images in a single message, pass them as a list:

``` r

llm_message("Describe the difference between these two images.",
            .media = list(img("photo_a.jpg"), img("photo_b.jpg"))) |>
  chat(openai())
```

### Mixing media types

The `.media` list can contain any combination of types. Gemini handles
mixed content most flexibly — you can send images, PDFs, audio, and
video together in a single message. Here we ask a model to
cross-reference a figure against the paper it came from:

``` r

llm_message("The figure is from this paper. Does it accurately represent the results in Table 2?",
            .media = list(
              img("figure_3.png"),
              pdf_file("paper.pdf", pages = 1:8)
            )) |>
  chat(gemini())
```

Or combine a chart with an audio commentary on it:

``` r

llm_message("The audio is my verbal annotation of this chart. Summarise both together.",
            .media = list(
              img("quarterly_results.png"),
              audio_file("my_notes.mp3")
            )) |>
  chat(gemini())
```

Each item in the list is processed in order and all items end up in the
same message. The provider sees them as one multi-part input alongside
the text prompt.

## PDFs

Use
[`pdf_file()`](https://edubruell.github.io/tidyllm/reference/pdf_file.md)
to attach a PDF. On providers that support native PDF rendering (Claude
and Gemini), the file is sent as-is, so the model can see tables,
figures, and formatting just as a human reader would. On other
providers, tidyllm automatically extracts the text and passes it to the
model instead. Either way, you write the same code:

``` r

llm_message("Summarise the key findings from this report.",
            .media = pdf_file("annual_report.pdf")) |>
  chat(claude())
```

To always use the text extraction path regardless of the provider, pass
`.text_extract = TRUE`. This is useful when you want consistent
behaviour across providers or when a document is text-heavy and layout
does not matter:

``` r

llm_message("Summarise the key findings.",
            .media = pdf_file("annual_report.pdf", .text_extract = TRUE)) |>
  chat(openai())
```

Limit the pages sent to the model with the `pages` argument:

``` r

llm_message("What does the introduction argue?",
            .media = pdf_file("paper.pdf", pages = 1:5)) |>
  chat(claude())
```

### Scanned documents

A particularly compelling use case is working with scanned PDFs that
contain no machine-readable text at all. Text extraction would return
nothing useful, but sending the binary file to Claude or Gemini lets the
model read the actual page images. This makes it possible to process old
journal scans, archival documents, or any PDF that was created from
photographs rather than typeset text.

Here we use a scanned copy of a 1995 economics paper and ask Claude to
extract all references in APA format:

``` r

ref_schema <- tidyllm_schema(
  apa_references = field_chr("All references in APA format, one entry",
                             .vector = TRUE)
)

result <- llm_message("Extract all the references from this paper.",
                      .media = pdf_file("1995_Neal_Industry_Specific.pdf")) |>
  chat(claude(), .json_schema = ref_schema)

get_reply_data(result)$apa_references[1:3]
```

    ## [1] "Addison, J., & Portugal, P. (1989). Job displacement, relative wage changes, and duration of unemployment. Journal of Labor Economics, 7(3), 281-302."
    ## [2] "Altonji, J., & Shakotko, R. (1987). Do wages rise with seniority? Review of Economic Studies, 54(3), 437-459."                                        
    ## [3] "Becker, G. (1975). Human capital. Columbia University Press."

The same approach works for handwritten notes, printed forms, historical
records, or any document where the visual layout carries meaning that
text extraction would lose.

## Audio and Video

For audio and video, use
[`audio_file()`](https://edubruell.github.io/tidyllm/reference/audio_file.md)
and
[`video_file()`](https://edubruell.github.io/tidyllm/reference/video_file.md).
Gemini has the broadest native support and handles most common formats
(MP3, WAV, MP4, MOV, and more). OpenRouter routes audio and video to
underlying models that accept them; you can filter by audio or video
modality at [openrouter.ai/models](https://openrouter.ai/models). Local
models via llama.cpp support audio with compatible model files
(Ultravox, Qwen2.5-Omni, Qwen3-Omni, and Gemma 4 variants).

The examples in this section use two pieces of archival material: an MP3
recording of an interview with Robert Bosch describing the early history
of his company, and a 1960s TV documentary segment on artificial
intelligence called **The Thinking Machine**, featuring Claude Shannon
and other early AI researchers.

# Ein Fehler ist aufgetreten.

JavaScript kann nicht ausgeführt werden.

``` r

# Describe or transcribe an audio recording
llm_message("Summarise what is discussed in this recording.",
            .media = audio_file("bosch_interview.mp3")) |>
  chat(gemini())
```

    ## Message History:
    ## system:
    ## You are a helpful assistant
    ## --------------------------------------------------------------
    ## user:
    ## Summarise what is discussed in this recording.
    ## --------------------------------------------------------------
    ## assistant:
    ## This is an audio recording of an interview with Robert
    ## Bosch. He discusses his early life, his path from apprentice
    ## to founder of his renowned company, and his philosophy on
    ## business. Bosch recounts his decision to become a precision
    ## mechanic, his travels to America and England, and the
    ## development of his famous magneto ignition system. He
    ## emphasises principles of high-quality work, fair treatment
    ## of employees, and the importance of trust and reliability in
    ## business dealings.
    ## --------------------------------------------------------------

``` r

# Route to a Gemini model via OpenRouter
llm_message("Summarise this recording.",
            .media = audio_file("bosch_interview.mp3")) |>
  chat(openrouter(.model = "google/gemini-2.5-flash"))
```

Video works the same way:

``` r

llm_message("Give a two-sentence summary of what happens in this video.",
            .media = video_file("documentary.mp4")) |>
  chat(gemini())
```

### Structured output from video

You can combine video with a JSON schema to extract specific fields from
a clip, just as you would with text. This is useful for cataloguing
video archives or extracting metadata at scale. Here we define a schema
for a short documentary clip and ask Gemini to populate it:

``` r

video_schema <- tidyllm_schema(
  title       = field_chr("The title or subject of the clip, as best you can tell"),
  era         = field_chr("The approximate decade or period depicted"),
  format      = field_fct("What type of content is this?",
                           .levels = c("Documentary", "Interview", "Lecture", "News", "Other")),
  key_people  = field_chr("Names of people who appear or are mentioned, semicolon-separated"),
  main_topics = field_chr("The two or three main topics covered, semicolon-separated")
)

result <- llm_message("Analyse this video clip and fill in the schema.",
                      .media = video_file("the_thinking_machine.mp4")) |>
  chat(gemini(), .json_schema = video_schema)

get_reply_data(result)
```

    ## $title
    ## [1] "The Thinking Machine"
    ## 
    ## $era
    ## [1] "1960s"
    ## 
    ## $format
    ## [1] "Documentary"
    ## 
    ## $key_people
    ## [1] "Claude Shannon; unnamed narrator"
    ## 
    ## $main_topics
    ## [1] "Artificial intelligence; machine learning; early computers"

[`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)
returns a named list matching your schema. Wrap it in
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
to get a single-row data frame, or use
[`purrr::map_dfr()`](https://purrr.tidyverse.org/reference/map_dfr.html)
to process a whole folder of clips into a tidy table in one go.

For files up to around 20 MB for audio and 100 MB for video, sending
inline is fine. For larger files, use the Files API described in the
next section.

## Provider Files API

When you want to send the same document to a model several times, or
when a file is large enough that repeatedly attaching it inline would be
slow, you can upload it once to the provider’s servers and keep a
reference to it. That reference can then be included in any number of
messages without re-sending the file.

``` r

# Upload a file; returns a handle you can reuse
report <- upload_file(gemini(), .path = "quarterly_report.pdf")
report
```

    ## <tidyllm_file [gemini] id=files/abc123 mime=application/pdf file=quarterly_report.pdf>

Attach the handle to messages with the `.files` argument:

``` r

llm_message("What were the key financial results this quarter?",
            .files = report) |>
  chat(gemini())

llm_message("List the top three risks mentioned in the document.",
            .files = report) |>
  chat(gemini())
```

Providers also expose file management functions.
[`list_files()`](https://edubruell.github.io/tidyllm/reference/list_files.md)
returns a tibble of all uploaded files,
[`file_info()`](https://edubruell.github.io/tidyllm/reference/file_info.md)
retrieves details for a single file, and
[`delete_file()`](https://edubruell.github.io/tidyllm/reference/delete_file.md)
removes it:

``` r

list_files(gemini())
file_info(gemini(), report)
delete_file(gemini(), report)
```

    ## # A tibble: 1 × 7
    ##   file_id  filename size_bytes mime_type created_at          expires_at         
    ##   <chr>    <chr>         <int> <chr>     <dttm>              <dttm>             
    ## 1 files/a… quarter…    1243500 applicat… 2026-04-20 10:15:00 2026-04-22 10:15:00
    ## # ℹ 1 more variable: uri <chr>

The same pattern works with Claude and OpenAI:

``` r

# Claude: well suited for dense academic or legal text
doc <- upload_file(claude(), .path = "paper.pdf")
llm_message("What is the main contribution of this paper?",
            .files = doc) |>
  chat(claude())
delete_file(claude(), doc)

# OpenAI: accepts a wide range of formats beyond PDFs
slides <- upload_file(openai(), .path = "presentation.pptx")
llm_message("Summarise these slides in five bullet points.",
            .files = slides) |>
  chat(openai())
delete_file(openai(), slides)
```

### What each provider supports

Provider file APIs differ in which file types they accept and how long
files are kept:

- **Gemini**: video, audio, images, PDFs, and a range of document
  formats; uploaded files expire after 48 hours
- **Claude**: PDFs and images; no automatic expiry; works well for long,
  text-heavy documents where precision matters
- **OpenAI**: 80+ formats including Office documents (DOCX, PPTX, XLSX),
  source code, ZIP archives, images, and PDFs; note that images uploaded
  via the Files API cannot be used for vision tasks in chat, so use
  inline [`img()`](https://edubruell.github.io/tidyllm/reference/img.md)
  for that instead

### Files are provider-specific

A file uploaded to Claude cannot be used in a message sent to Gemini or
OpenAI. Each uploaded file belongs to the provider it was uploaded to,
and tidyllm will catch any mismatch before sending the request.

## Supported formats

**Inline media** (`.media` argument):

| Type | Constructor | Providers | Common formats |
|----|----|----|----|
| Images | [`img()`](https://edubruell.github.io/tidyllm/reference/img.md) | Claude, Gemini, OpenAI, Mistral, Groq, Ollama, OpenRouter | PNG, JPEG, WEBP, HEIC, GIF |
| PDFs | [`pdf_file()`](https://edubruell.github.io/tidyllm/reference/pdf_file.md) | Claude, Gemini (binary); all others (text extraction) | PDF |
| Audio | [`audio_file()`](https://edubruell.github.io/tidyllm/reference/audio_file.md) | Gemini, OpenRouter, llama.cpp, Mistral (Voxtral) | MP3, WAV, AIFF, AAC, OGG, FLAC |
| Video | [`video_file()`](https://edubruell.github.io/tidyllm/reference/video_file.md) | Gemini, OpenRouter | MP4, MPEG, MOV, AVI, WEBM, WMV |

**Provider Files API**
([`upload_file()`](https://edubruell.github.io/tidyllm/reference/upload_file.md)):

- **Gemini**: PDFs, images, audio, video, plain text, HTML, CSV, XML,
  Markdown, code files; files expire after 48 hours
- **Claude**: PDFs and images; no automatic expiry
- **OpenAI**: 80+ formats including PDFs, images, Office documents
  (DOCX, PPTX, XLSX), source code, ZIP archives, and more

## What this makes possible

Working with audio, video, and scanned documents opens up research
workflows that are simply not available with text-only models. Oral
history recordings can be transcribed and analysed without manual
transcription. Video interviews and documentary footage become queryable
data sources. Scanned archival documents, handwritten records, and
image-heavy reports that would have required specialist OCR software can
now be read and structured with a single API call. Survey recordings,
lecture recordings, and qualitative interview data can be processed at
scale using the same batch infrastructure that tidyllm provides for
text. The common thread across all of these is that you write the same
tidy pipeline you would for any other LLM task; the only change is what
you attach to the message.
