# tidyllm

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN
Status](https://www.r-pkg.org/badges/version/tidyllm)](https://cran.r-project.org/package=tidyllm)

**tidyllm** is an R package for working with large language model APIs
in data analysis workflows. It supports **Anthropic Claude**,
**OpenAI**, **Google Gemini**, **Mistral**, **Groq**, **DeepSeek**,
**OpenRouter**, **TypeSafe** (decision models), local models via
**Ollama** and **llama.cpp**, and more, all through a single consistent
interface.

## Features

- **Multiple providers**: Switch between cloud and local models using
  the same verb + provider pattern.
- **Unified media system**: Send images, audio, video, and PDFs to any
  provider that supports them via `.media`. Upload files to provider
  servers for reuse via `.files` and
  [`upload_file()`](https://edubruell.github.io/tidyllm/reference/upload_file.md).
- **Closed questions with probabilities**:
  [`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
  puts yes/no, choice and score questions to a text or an image and
  returns a tibble with one row per question and the probability of each
  answer. It works with
  [`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md),
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  and
  [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md).
- **Interactive message history**: Manage multi-turn conversations with
  structured history automatically formatted for each API.
- **Batch processing**: Handle large workloads with Anthropic, OpenAI,
  Mistral, Groq, and Gemini batch APIs, reducing costs by up to 50%.
- **Tidy workflow**: Pipeline-oriented, side-effect-free design that
  integrates naturally with tidyverse data workflows.

## Installation

To install **tidyllm** from CRAN, use:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"tidyllm"``)`

Or for the development version from GitHub:

\
`devtools``::`[`install_github`](https://devtools.r-lib.org/reference/install-deprecated.html)`(``"edubruell/tidyllm"``)`

## Basic Example

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyllm`](https://edubruell.github.io/tidyllm/)`)`\
\
`# Describe an image with Claude, continue with a local model`\
`conversation`` ``<-`` `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Describe this image."``,`\
`                             .media ``=`` `[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"photo.jpg"``)``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`claude`](https://edubruell.github.io/tidyllm/reference/claude.md)`(``)``)`\
\
`conversation`` ``|>`\
`  `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Based on that description, what research topic could this figure relate to?"``)`` ``|>`\
`  `[`chat`](https://edubruell.github.io/tidyllm/reference/chat.md)`(`[`ollama`](https://edubruell.github.io/tidyllm/reference/ollama.md)`(``.model ``=`` ``"qwen3.5:4b"``)``)`

For more examples and advanced usage, see the [Get Started
vignette](https://edubruell.github.io/tidyllm/articles/tidyllm.html).

Please note: To use **tidyllm** you need either a local Ollama or
llama.cpp installation, or an active API key for one of the supported
cloud providers. See the [Get Started
vignette](https://edubruell.github.io/tidyllm/articles/tidyllm.html) for
setup instructions.

## What’s new in 0.8.0

**Closed questions with probabilities.**
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
puts yes/no, choice and score questions to a text, an image, a recording
or a PDF. It returns a tibble with one row per question, the answer and
its probability. You write the questions once as a
[`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)
and switch between
[`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md),
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
and
[`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md).

\
`questions`` ``<-`` `[`tidyllm_schema`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)`(`\
`  late ``=`` `[`field_lgl`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"Was the payment late?"``)``,`\
`  tone ``=`` `[`field_fct`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"Tone of the message"``, .levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"polite"``, ``"angry"``)``)`\
`)`\
\
[`decide`](https://edubruell.github.io/tidyllm/reference/decide.md)`(``"Your invoice is two weeks overdue and I am furious."``, ``questions``, `[`jev`](https://edubruell.github.io/tidyllm/reference/jev.md)`(``)``)`

Pass a character vector to code many texts in parallel.
[`field_box()`](https://edubruell.github.io/tidyllm/reference/field_box.md)
asks where something is in a picture (experimental,
[`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) only).
The [decision models
article](https://edubruell.github.io/tidyllm/articles/tidyllm_decide.html)
walks through it.

**Read many replies at once.**
[`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md),
[`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)
and
[`get_metadata()`](https://edubruell.github.io/tidyllm/reference/get_metadata.md)
accept the list that
[`parallel_chat()`](https://edubruell.github.io/tidyllm/reference/parallel_chat.md)
and
[`fetch_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_batch.md)
return.

**Current provider defaults.**
[`mistral()`](https://edubruell.github.io/tidyllm/reference/mistral.md)
defaults to `mistral-large-4`, which always reasons (`.reasoning_effort`
of `"low"`, `"medium"` or `"high"`).
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
accepts every reasoning effort the API names, and
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
accepts `.thinking_level = "xhigh"`.
[`llamacpp()`](https://edubruell.github.io/tidyllm/reference/llamacpp.md)
looks for the server on port 9931.

**Perplexity removed.**
[`perplexity()`](https://edubruell.github.io/tidyllm/reference/perplexity.md)
is gone. Use Perplexity models through
[`openrouter()`](https://edubruell.github.io/tidyllm/reference/openrouter.md)
and web search through
[`websearch_tool()`](https://edubruell.github.io/tidyllm/reference/websearch_tool.md).

Read the [Changelog](https://edubruell.github.io/tidyllm/news/) for the
full list of changes.

## Learn More

[Get Started with
tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm.html)
covers setup, your first chat and the basic verbs. Read it first, then
pick the article that matches your task. The
[Changelog](https://edubruell.github.io/tidyllm/news/) lists every
change, and the [documentation](https://edubruell.github.io/tidyllm/)
holds the function reference.

**Start here: the common research tasks, simplest first**

- [Working with Files and
  Media](https://edubruell.github.io/tidyllm/articles/tidyllm_video.html):
  Ask a model about an image, a recording, a video or a PDF page.
- [Classifying Texts with
  tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_classifiers.html):
  Label thousands of free-text answers with a codebook, and check the
  labels against a sample coded by people.
- [Decision Models with
  tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_decide.html):
  Not a chat model: a cheap model that answers fixed yes/no, choice or
  score questions about a text or image and returns a probability for
  each answer. Use it to code many texts quickly.
- [Embedding Models in
  tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_embed.html):
  Models that translate texts into numbers, which you can use to find
  similar texts, group them or search them.
- [Structured Question Answering from
  PDFs](https://edubruell.github.io/tidyllm/articles/tidyllm-pdfquestions.html):
  Ask the same questions of a folder of papers and get a table with one
  row per paper.

**Go deeper: setup choices and larger builds**

- [Local Models with
  tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_local_models.html):
  Run models on your own computer, for confidential data.
- [Command Line
  Providers](https://edubruell.github.io/tidyllm/articles/tidyllm_cli.html):
  Chat through a coding agent you already pay for, such as Claude Code,
  Codex or Vibe, without an API key.
- [Using Tools with
  tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_tools.html):
  Let a model call your own R functions or search the web while it
  answers.
- [Using tidyllm in
  Shiny](https://edubruell.github.io/tidyllm/articles/tidyllm_shiny.html):
  Build an app in which many users call models without freezing it.

## Similar packages

- [ellmer](https://ellmer.tidyverse.org/) keeps conversation state in
  objects and concentrates on chat providers, which suits interactive
  agents, chatbots in Shiny and tool-calling workflows. tidyllm keeps
  its verbs as stateless as possible, so they fit data pipelines and
  batch work, and it covers API features beyond chat, such as batch
  jobs, embeddings, file uploads and web search. The two packages work
  together: `chat(ellmer(.ellmer_chat = ...))` runs a tidyllm message
  through any ellmer chat object.
- [rollama](https://jbgruber.github.io/rollama/) is purpose-built for
  the Ollama API with specialized model management features not
  currently in tidyllm.

## Contributing

Contributions are welcome. Open an issue or a pull request on
[GitHub](https://github.com/edubruell/tidyllm).

## License

This project is licensed under the MIT License; see the
[LICENSE](https://opensource.org/licenses/MIT) file for details.
