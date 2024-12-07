# tidyllm <a href="https://edubruell.github.io/tidyllm/"><img src="man/figures/logo.png" align="right" height="139" alt="tidyllm website" /></a>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN Status](https://www.r-pkg.org/badges/version/tidyllm)](https://cran.r-project.org/package=tidyllm)


**tidyllm** is an R package designed to access various large language model APIs, including **Anthropic Claude**, **OpenAI**,**Google Gemini**, **Perplexity**,**Groq**, **Mistral**, and local models via **Ollama** or OpenAI-compatible APIs. Built for simplicity and functionality, it helps you generate text, analyze media, and integrate model feedback into your data workflows with ease.

## Features

- **Multiple Model Support**: Seamlessly switch between various model providers using the best of what each has to offer.
- **Media Handling**: Extract and process text from PDFs and capture console outputs for messaging. Upload imagefiles or the last plotpane to multimodal models. For the Gemini API even video and audio inputs are supported.
- **Interactive Messaging History**: Manage an ongoing conversation with models, maintaining a structured history of messages and media interactions, which are automatically formatted for each API
- **Batch processing:** Efficiently handle large workloads with Anthropic, OpenAI or Mistral batch processing APIs, reducing costs by up to 50%.
- **Tidy Workflow**: Use R's functional programming features for a side-effect-free, pipeline-oriented operation style.


## Installation

To install **tidyllm** from CRAN, use:

```r
install.packages("tidyllm")
```

Or for the development version from GitHub:
```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("edubruell/tidyllm")
```

## Basic Example

Here’s a quick  example using tidyllm to describe an image using the Claude model 
to and follow up with local open-source models:

```R
library("tidyllm")

# Describe an image with  claude
conversation <- llm_message("Describe this image", 
                              .imagefile = here("image.png")) |>
  chat(claude())

# Use the description to query further with groq
conversation |>
  llm_message("Based on the previous description,
  what could the research in the figure be about?") |>
  chat(ollama(.model = "gemma2"))
```

For more examples and advanced usage, check the [Get Started vignette](https://edubruell.github.io/tidyllm/articles/tidyllm.html).

Please note: To use **tidyllm**, you need either an installation of **ollama** or an active API key for one of the supported providers (e.g., Claude, ChatGPT). See the [Get Started vignette](https://edubruell.github.io/tidyllm/articles/tidyllm.html) for setup instructions.

## Interface-change in 0.2.3.

The development version 0.2.3. of **tidyllm**,  introduces a major interface change to provide a more intuitive user experience. Previously, provider-specific functions like `claude()`, `openai()`, and others were directly used for chat-based workflows. They specified both an API-provider and performed a chat-interaction. Now, these functions primarily serve as provider configuration for more general verbs like `chat()`,`embed()` or `send_batch()`. A combination of a general verb and a provider will always route requests to a provider-specific function like `openai_chat()`. Read the [Changelog](https://edubruell.github.io/tidyllm/news/) or the [package vignette](https://edubruell.github.io/tidyllm/articles/tidyllm.html) for more information. 

For backward compatibility, the old use of functions like `openai()` or `claude()` directly for chat requests still works but now but issues deprecation warnings. It is recommended to either use the verb-based interface:
```r
llm_message("Hallo") |> chat(openai(.model="gpt-4o"))
```
or to use the more verbose provider-specific functions directly:
```r
llm_message("Hallo") |> openai_chat(.model="gpt-4o")
```

## Learn More

For detailed instructions and advanced features, see:

- [Get Started with tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm.html)
- [Changelog](https://edubruell.github.io/tidyllm/news/)
- [Documentation](https://edubruell.github.io/tidyllm/)
- Use-case oriented articles: 
  - [Classifying Texts with tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_classifiers.html)
  - [Structured Question Answering from PDFs](https://edubruell.github.io/tidyllm/articles/tidyllm-pdfquestions.html)
  - [Using Embedding Models for Semantic Search](https://edubruell.github.io/tidyllm/articles/tidyllm_embed.html)
  - [Video and Audio Data with the Gemini API](https://edubruell.github.io/tidyllm/articles/tidyllm_video.html)
  
## Similar packages
The are some similar R packages for working with LLMs:

  - [elmer](https://elmer.tidyverse.org/) is espiacially great for asynchronous workflows, chatbots in Shiny and advanced tool-calling capabilities. Its schema functions offer robust support for complex structured data extraction, making it a great choice for applications that require highly interactive or structured LLM interactions. While **elmer**’s feature set overlaps with **tidyllm** in some areas, its interface and design philosophy are very different.
  - [rollama](https://jbgruber.github.io/rollama/) is specifically designed to support the Ollama API, enabling seamless interaction with local LLM models. A key strength of **rollama** lies in its specialized Ollama API functionalities, such as `copy`, `create`, and `delete`, which are not currently available in **tidyllm**. These features make **rollama** particularly suited for workflows requiring model management or deployment within the Ollama ecosystem.
  
## Contributing

We welcome contributions! Feel free to open issues or submit pull requests on [GitHub](https://github.com/edubruell/tidyllm).

## License

This project is licensed under the MIT License - see the [LICENSE](https://opensource.org/licenses/MIT) file for details.

