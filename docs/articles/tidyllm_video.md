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

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyllm`](https://edubruell.github.io/tidyllm/)`)`\
\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"What food is in this image?"``,`\
`            .media ``=`` `[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"hotdog.jpg"``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`claude`](https://edubruell.github.io/tidyllm/reference/claude.md)`(``)``)`

Images work with all major providers that support vision: Claude,
Gemini, OpenAI, Mistral, Groq, Ollama (with a vision-capable model), and
OpenRouter.

To send multiple images in a single message, pass them as a list:

\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Describe the difference between these two images."``,`\
`            .media ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"photo_a.jpg"``)``, `[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"photo_b.jpg"``)``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`openai`](https://edubruell.github.io/tidyllm/reference/openai.md)`(``)``)`

### Mixing media types

The `.media` list can contain any combination of types. Gemini handles
mixed content most flexibly: you can send images, PDFs, audio, and video
together in a single message. Here we ask a model to cross-reference a
figure against the paper it came from:

\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"The figure is from this paper. Does it accurately represent the results in Table 2?"``,`\
`            .media ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`              `[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"figure_3.png"``)``,`\
`              `[`pdf_file`](https://edubruell.github.io/tidyllm/reference/pdf_file.md)`(``"paper.pdf"``, pages ``=`` ``1``:``8``)`\
`            ``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``)`

Or combine a chart with an audio commentary on it:

\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"The audio is my verbal annotation of this chart. Summarise both together."``,`\
`            .media ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`              `[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"quarterly_results.png"``)``,`\
`              `[`audio_file`](https://edubruell.github.io/tidyllm/reference/audio_file.md)`(``"my_notes.mp3"``)`\
`            ``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``)`

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

\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Summarise the key findings from this report."``,`\
`            .media ``=`` `[`pdf_file`](https://edubruell.github.io/tidyllm/reference/pdf_file.md)`(``"annual_report.pdf"``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`claude`](https://edubruell.github.io/tidyllm/reference/claude.md)`(``)``)`

To always use the text extraction path regardless of the provider, pass
`.text_extract = TRUE`. This is useful when you want consistent
behaviour across providers or when a document is text-heavy and layout
does not matter:

\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Summarise the key findings."``,`\
`            .media ``=`` `[`pdf_file`](https://edubruell.github.io/tidyllm/reference/pdf_file.md)`(``"annual_report.pdf"``, .text_extract ``=`` ``TRUE``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`openai`](https://edubruell.github.io/tidyllm/reference/openai.md)`(``)``)`

Limit the pages sent to the model with the `pages` argument:

\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"What does the introduction argue?"``,`\
`            .media ``=`` `[`pdf_file`](https://edubruell.github.io/tidyllm/reference/pdf_file.md)`(``"paper.pdf"``, pages ``=`` ``1``:``5``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`claude`](https://edubruell.github.io/tidyllm/reference/claude.md)`(``)``)`

### Scanned documents

A particularly compelling use case is working with scanned PDFs that
contain no machine-readable text at all. Text extraction would return
nothing useful, but sending the binary file to Claude or Gemini lets the
model read the actual page images. This makes it possible to process old
journal scans, archival documents, or any PDF that was created from
photographs rather than typeset text.

Here we use a scanned copy of a 1995 economics paper and ask Claude to
extract all references in APA format:

\
`ref_schema`` ``<-`` `[`tidyllm_schema`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)`(`\
`  apa_references ``=`` `[`field_chr`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"All references in APA format, one entry"``,`\
`                             .vector ``=`` ``TRUE``)`\
`)`\
\
`result`` ``<-`` `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Extract all the references from this paper."``,`\
`                      .media ``=`` `[`pdf_file`](https://edubruell.github.io/tidyllm/reference/pdf_file.md)`(``"1995_Neal_Industry_Specific.pdf"``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`claude`](https://edubruell.github.io/tidyllm/reference/claude.md)`(``)``, .json_schema ``=`` ``ref_schema``)`\
\
[`get_reply_data`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)`(``result``)``$``apa_references``[``1``:``3``]`

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

\
`# Describe or transcribe an audio recording`\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Summarise what is discussed in this recording."``,`\
`            .media ``=`` `[`audio_file`](https://edubruell.github.io/tidyllm/reference/audio_file.md)`(``"bosch_interview.mp3"``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``)`

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

\
`# Route to a Gemini model via OpenRouter`\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Summarise this recording."``,`\
`            .media ``=`` `[`audio_file`](https://edubruell.github.io/tidyllm/reference/audio_file.md)`(``"bosch_interview.mp3"``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`openrouter`](https://edubruell.github.io/tidyllm/reference/openrouter.md)`(``.model ``=`` ``"google/gemini-3.8-flash"``)``)`

Video works the same way:

\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Give a two-sentence summary of what happens in this video."``,`\
`            .media ``=`` `[`video_file`](https://edubruell.github.io/tidyllm/reference/video_file.md)`(``"documentary.mp4"``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``)`

### Structured output from video

You can combine video with a JSON schema to extract specific fields from
a clip, just as you would with text. This is useful for cataloguing
video archives or extracting metadata at scale. Here we define a schema
for a short documentary clip and ask Gemini to populate it:

\
`video_schema`` ``<-`` `[`tidyllm_schema`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)`(`\
`  title       ``=`` `[`field_chr`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"The title or subject of the clip, as best you can tell"``)``,`\
`  era         ``=`` `[`field_chr`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"The approximate decade or period depicted"``)``,`\
`  format      ``=`` `[`field_fct`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"What type of content is this?"``,`\
`                           .levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Documentary"``, ``"Interview"``, ``"Lecture"``, ``"News"``, ``"Other"``)``)``,`\
`  key_people  ``=`` `[`field_chr`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"Names of people who appear or are mentioned, semicolon-separated"``)``,`\
`  main_topics ``=`` `[`field_chr`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"The two or three main topics covered, semicolon-separated"``)`\
`)`\
\
`result`` ``<-`` `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Analyse this video clip and fill in the schema."``,`\
`                      .media ``=`` `[`video_file`](https://edubruell.github.io/tidyllm/reference/video_file.md)`(``"the_thinking_machine.mp4"``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``, .json_schema ``=`` ``video_schema``)`\
\
[`get_reply_data`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)`(``result``)`

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
to get a single-row data frame. To process a whole folder of clips,
collect the replies in a list and pass it to
[`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md),
which returns one row per clip.

For files up to around 20 MB for audio and 100 MB for video, sending
inline is fine. For larger files, use the Files API described in the
next section.

## Provider Files API

When you want to send the same document to a model several times, or
when a file is large enough that repeatedly attaching it inline would be
slow, you can upload it once to the provider’s servers and keep a
reference to it. That reference can then be included in any number of
messages without re-sending the file.

\
`# Upload a file; returns a handle you can reuse`\
`report`` ``<-`` `[`upload_file`](https://edubruell.github.io/tidyllm/reference/upload_file.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``, .path ``=`` ``"quarterly_report.pdf"``)`\
`report`

    ## <tidyllm_file [gemini] id=files/abc123 mime=application/pdf file=quarterly_report.pdf>

Attach the handle to messages with the `.files` argument:

\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"What were the key financial results this quarter?"``,`\
`            .files ``=`` ``report``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``)`\
\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"List the top three risks mentioned in the document."``,`\
`            .files ``=`` ``report``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``)`

Providers also expose file management functions.
[`list_files()`](https://edubruell.github.io/tidyllm/reference/list_files.md)
returns a tibble of all uploaded files,
[`file_info()`](https://edubruell.github.io/tidyllm/reference/file_info.md)
retrieves details for a single file, and
[`delete_file()`](https://edubruell.github.io/tidyllm/reference/delete_file.md)
removes it:

\
[`list_files`](https://edubruell.github.io/tidyllm/reference/list_files.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``)`\
[`file_info`](https://edubruell.github.io/tidyllm/reference/file_info.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``, ``report``)`\
[`delete_file`](https://edubruell.github.io/tidyllm/reference/delete_file.md)`(`[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``)``, ``report``)`

    ## # A tibble: 1 × 7
    ##   file_id  filename size_bytes mime_type created_at          expires_at         
    ##   <chr>    <chr>         <int> <chr>     <dttm>              <dttm>             
    ## 1 files/a… quarter…    1243500 applicat… 2026-04-20 10:15:00 2026-04-22 10:15:00
    ## # ℹ 1 more variable: uri <chr>

The same pattern works with Claude and OpenAI:

\
`# Claude: well suited for dense academic or legal text`\
`doc`` ``<-`` `[`upload_file`](https://edubruell.github.io/tidyllm/reference/upload_file.md)`(`[`claude`](https://edubruell.github.io/tidyllm/reference/claude.md)`(``)``, .path ``=`` ``"paper.pdf"``)`\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"What is the main contribution of this paper?"``,`\
`            .files ``=`` ``doc``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`claude`](https://edubruell.github.io/tidyllm/reference/claude.md)`(``)``)`\
[`delete_file`](https://edubruell.github.io/tidyllm/reference/delete_file.md)`(`[`claude`](https://edubruell.github.io/tidyllm/reference/claude.md)`(``)``, ``doc``)`\
\
`# OpenAI: accepts a wide range of formats beyond PDFs`\
`slides`` ``<-`` `[`upload_file`](https://edubruell.github.io/tidyllm/reference/upload_file.md)`(`[`openai`](https://edubruell.github.io/tidyllm/reference/openai.md)`(``)``, .path ``=`` ``"presentation.pptx"``)`\
[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Summarise these slides in five bullet points."``,`\
`            .files ``=`` ``slides``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`openai`](https://edubruell.github.io/tidyllm/reference/openai.md)`(``)``)`\
[`delete_file`](https://edubruell.github.io/tidyllm/reference/delete_file.md)`(`[`openai`](https://edubruell.github.io/tidyllm/reference/openai.md)`(``)``, ``slides``)`

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

The command line providers
[`claude_cli()`](https://edubruell.github.io/tidyllm/reference/claude_cli.md),
[`codex_cli()`](https://edubruell.github.io/tidyllm/reference/codex_cli.md)
and
[`vibe_cli()`](https://edubruell.github.io/tidyllm/reference/vibe_cli.md)
take text only: a PDF reaches them as extracted text, and images, audio,
video and uploaded files are rejected.

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
