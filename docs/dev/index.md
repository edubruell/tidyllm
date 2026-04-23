# tidyllm

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN
Status](https://www.r-pkg.org/badges/version/tidyllm)](https://cran.r-project.org/package=tidyllm)

**tidyllm** is an R package for working with large language model APIs
in data analysis workflows. It supports **Anthropic Claude**,
**OpenAI**, **Google Gemini**, **Mistral**, **Groq**, **Perplexity**,
**DeepSeek**, **OpenRouter**, local models via **Ollama** and
**llama.cpp**, and more — all through a single consistent interface.

## Features

- **Multiple providers**: Switch between cloud and local models using
  the same verb + provider pattern.
- **Unified media system**: Send images, audio, video, and PDFs to any
  provider that supports them via `.media`. Upload files to provider
  servers for reuse via `.files` and
  [`upload_file()`](https://edubruell.github.io/tidyllm/dev/reference/upload_file.md).
- **Interactive message history**: Manage multi-turn conversations with
  structured history automatically formatted for each API.
- **Batch processing**: Handle large workloads with Anthropic, OpenAI,
  Mistral, Groq, and Gemini batch APIs, reducing costs by up to 50%.
- **Tidy workflow**: Pipeline-oriented, side-effect-free design that
  integrates naturally with tidyverse data workflows.

## Installation

To install **tidyllm** from CRAN, use:

``` r

install.packages("tidyllm")
```

Or for the development version from GitHub:

``` r

devtools::install_github("edubruell/tidyllm")
```

## Basic Example

``` r

library(tidyllm)

# Describe an image with Claude, continue with a local model
conversation <- llm_message("Describe this image.",
                             .media = img("photo.jpg")) |>
  chat(claude())

conversation |>
  llm_message("Based on that description, what research topic could this figure relate to?") |>
  chat(ollama(.model = "qwen3.5:4b"))
```

For more examples and advanced usage, see the [Get Started
vignette](https://edubruell.github.io/tidyllm/articles/tidyllm.html).

Please note: To use **tidyllm** you need either a local Ollama or
llama.cpp installation, or an active API key for one of the supported
cloud providers. See the [Get Started
vignette](https://edubruell.github.io/tidyllm/articles/tidyllm.html) for
setup instructions.

## What’s new in 0.5.0

**Unified media and files system.** All non-text content now attaches to
messages, not to chat() call arguments:

``` r

# Inline binary — works with any provider that supports the type
llm_message("Transcribe this audio.", .media = audio_file("recording.mp3")) |>
  chat(gemini())

llm_message("Summarize this paper.", .media = pdf_file("paper.pdf")) |>
  chat(claude())

# Multiple images in one message
llm_message("Compare these two charts.",
            .media = list(img("chart_a.png"), img("chart_b.png"))) |>
  chat(openai())

# Provider Files API — upload once, reuse across requests
report <- upload_file(claude(), .path = "annual_report.pdf")
llm_message("What are the key risks?", .files = report) |>
  chat(claude())
```

Audio and video are supported by Gemini, OpenRouter (Gemini, Gemma 4,
Qwen 3.6, and other models), Mistral (Voxtral), and llama.cpp (Ultravox,
Gemma 4, Qwen2.5-Omni). The `.imagefile` and old provider-specific
upload functions still work but now emit deprecation warnings.

OpenAI now uses the Responses API (`POST /v1/responses`) with reasoning
effort control, built-in web search, and background deep research:

``` r

llm_message("What are the latest developments in fusion energy?") |>
  chat(openai(), .tools = openai_websearch())

llm_message("Write a detailed report on EU AI regulation.") |>
  deep_research(openai())
```

Read the [Changelog](https://edubruell.github.io/tidyllm/news/) for the
full list of changes.

## Learn More

- [Get Started with
  tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm.html)
- [Changelog](https://edubruell.github.io/tidyllm/news/)
- [Documentation](https://edubruell.github.io/tidyllm/)
- Use-case oriented articles:
  - [Classifying Texts with
    tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_classifiers.html)
  - [Structured Question Answering from
    PDFs](https://edubruell.github.io/tidyllm/articles/tidyllm-pdfquestions.html)
  - [Embedding Models in
    tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_embed.html)
  - [Working with Files and
    Media](https://edubruell.github.io/tidyllm/articles/tidyllm_video.html)
  - [Local Models with
    tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_local_models.html)

## Similar packages

- [ellmer](https://ellmer.tidyverse.org/) is especially well-suited for
  asynchronous chats, chatbots in Shiny, and advanced tool-calling
  workflows. Its design philosophy differs from tidyllm: ellmer targets
  interactive agents while tidyllm targets data pipelines and batch
  operations.
- [rollama](https://jbgruber.github.io/rollama/) is purpose-built for
  the Ollama API with specialized model management features not
  currently in tidyllm.

## Contributing

We welcome contributions! Feel free to open issues or submit pull
requests on [GitHub](https://github.com/edubruell/tidyllm).

## License

This project is licensed under the MIT License — see the
[LICENSE](https://opensource.org/licenses/MIT) file for details.
