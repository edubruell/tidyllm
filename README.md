# TidyLLM: Tidy Large Language Model Integration for R

**TidyLLM** is an R package designed to access various large language model APIs, including **Claude**, **ChatGPT**, and open-source models on **Groq** using their unique dedicated hardware accelerators. This package is built with simplicity and functionality in mind. Whether you're looking to generate text, analyze media, or integrate rich model feedback into your applications, **TidyLLM** provides a unified interface to get the job done.

## Features

- **Multiple Model Support**: Seamlessly switch between various model providers like Claude, ChatGPT, and Groq, using the best of what each has to offer.
- **Media Handling**: Extract and process text from PDF files and capture console outputs for messaging. Upload imagefiles or the last plotpane to multimodal models.
- **Interactive Messaging History**: Manage an ongoing conversation with models, maintaining a structured history of messages and media interactions, which are automatically formatted for each API
- **Tidy Workflow**: Making use of R's functional programming features for a side-effect-free, pipeline-oriented operation style that feels natural to R users.

## Getting Started

Install **TidyLLM** directly from GitHub using devtools:

```R
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install TidyLLM from GitHub
devtools::install_github("edubruell/tidyllm")
```
## Basic Usage Example

Here’s a quick run-through using **TidyLLM** to send image descriptions through the Claude model and then follow-up with task-specific queries on Groq's open-source models:

```R
library(tidyllm)

# Describe an image with Claude
conversation <- llm_message("Describe this image",
                      .imagefile = "path/to/image.png") |>
            claude()

# Use the description to query further with Groq
conversation |>
  llm_message("Based on the previous description, what could the research here be about?") |>
  chatgpt()
```

## Functions Overview

- **`llm_message()`**: Create or update a message object, adding prompts or media content.
- **`claude()`**: Send and receive messages  Anthropic's Claude models
- **`chatgpt()`**: Send and receive messages from OpenAI's ChatGPT models.
- **`groq()`**: Interact with Groq's fast open-source models, taking advantage of their dedicated hardware accelerators for efficient processing.
- **`last_reply()`**: Fetch the most recent assistant's response from a message history.

## Future Work

- **Useful utility functions:** Utility functions like `count_tokens()` to estimate the cost of a message history and better functions to extract agent feedback
- **Tool use and json-mode:** Add support for models ability to use tools and structured json-output to directly create R lists and objects
- **Local models**: If possible support for local models via llama.cpp

