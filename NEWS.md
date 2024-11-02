# tidyllm Development Version Overview

The development version of `tidyllm` reflects the ongoing updates in the GitHub repository. Milestone versions are incremented when significant new features, improvements, or breaking changes are introduced.

## Versioning Policy
- **0.1.x**: Minor updates and feature additions.
- **0.x.0**: Releases (will be sent to CRAN)
- **0.1.0**: First CRAN release and the beginning of versioning.

---

# Version 0.1.11 (Current Development Version)

## Major Features

- Support for both the Anthropic and the OpenAI batch request API added
- New `.compatible`-arguement in `openai()` to allow working with compatible third party APIs

## Improvements

- **Complete refactor of `to_api_format()`**: API format generation now has much less code duplication and is more maintainable.


# Version 0.1.10

## Breaking Changes

- `get_reply()` was split into two type-stable functions: `get_reply()` for text and `get_reply_data()` for structured outputs.

## Improvements

- **Rate limiting updated to use `httr2::req_retry()`**: Rate limiting now uses the right 429 headers where they come. 

# Version 0.1.9 

## Major Features

- **Enhanced Input Validation**: All API functions now have improved input validation, ensuring better alignment with API documentation

- **Improved error handling**  More human-readable error messages for failed requests from the API
  
- **Advanced JSON Mode in `openai()`**: The `openai()` function now supports advanced `.json_schemas`, allowing structured output in JSON mode for more precise responses.

- **Reasoning Models Support**: Support for O1 reasoning models has been added, with better handling of system prompts in the `openai()` function.

- **Streaming callback functions refactored:** Given that the streaming callback format for Open AI, Mistral and Groq is nearly identical the three now rely on the same callback function. 

## Breaking Changes

- **`chatgpt()` Deprecated**: The `chatgpt()` function has been deprecated in favor of `openai()`. Users should migrate to `openai()` to take advantage of the new features and enhancements.

## Improvements

- **Better Error Handling**: The `openai()`, `ollama()`, and `claude()` functions now return more informative error messages when API calls fail, helping with debugging and troubleshooting.

---

# Version 0.1.8

## Major Features

- **Embedding Models Support:** Embedding model support for three APIs:
  - Embedding functions process message histories and combine text from message content and media attachments for embedding models.
  - `ollama_embedding()` to generate embeddings using the Ollama API.
  - `openai_embedding()` to generate embeddings using the OpenAI API.
  - `mistral_embedding()` to generate embeddings using the Mistral API.

## Improvements

- **PDF Page Support in `llm_message()`:** The `llm_message()` function now supports specifying a range of pages in a PDF by passing a list with `filename`, `start_page`, and `end_page`. This allows users to extract and process specific pages of a PDF.

---

# Version 0.1.7

## Major Features

- **PDF Page Batch Processing**: Introduced the `pdf_page_batch()` function, which processes PDF files page by page, extracting text and converting each page into an image, allowing for a general prompt or page-specific prompts. The function generates a list of `LLMMessage` objects that can  be sent to an API and work with the batch-API functions in **tidyllm**.

---

# Version 0.1.6

## Major Features

- **Support for the Mistral API**: New `mistral()` function to use Mistral Models on Le Platforme on servers hosted in the EU, with rate-limiting and streaming support.

---

# Version 0.1.5

## Major Features

- **Message Retrieval Functions**: Added functions to retrieve single messages from conversations:
  - `last_user_message()` pulls the last message the user sent.
  - `get_reply()` gets the assistant reply at a given index of assistant messages.
  - `get_user_message()` gets the user message at a given index of user messages.

## Improvements

- **Easier Troubleshooting in API-function**: All API functions now support the `.dry_run` argument, allowing users to generate an `httr2`-request for easier debugging and inspection.
- **API Function Tests:** Implemented `httptest2`-based tests with mock responses for all API functions, covering both basic functionality and rate-limiting.

---

# Version 0.1.4

## Major Features

- **New Ollama functions**:
  + **Model Download:** Introduced the `ollama_download_model()` function to download models from the Ollama API. It supports a streaming mode that provides live progress bar updates on the download progress.
  
## Improvements

- Refactoring of `llm_message()`

---

# Version 0.1.3

## Major Features

- The `groq()` function now supports images.
- More complete streaming support across API-functions.

## Breaking Changes

- **Groq Models**: System prompts are no longer sent for Groq models, since many models on Groq do not support them and all multimodal models on Groq disallow them.

---

# Version 0.1.2

## Improvements

- **New unit tests for `llm_message()`**.
- Improvements in streaming functions.

---

# Version 0.1.1

## Major Features

- **JSON Mode**: JSON mode is now more widely supported across all API functions, allowing for structured outputs when APIs support them. The `.json` argument is now passed only to API functions, specifying how the API should respond, and it is not needed anymore in `last_reply()`.
  
- **Improved `last_reply()` Behavior**: The behavior of the `last_reply()` function has changed. It now automatically handles JSON replies by parsing them into structured data and falling back to raw text in case of errors. You can still force raw text replies even for JSON output using the `.raw` argument.

## Breaking Changes

- **`last_reply()`**: The `.json` argument is no longer used, and JSON replies are automatically parsed. Use `.raw` to force raw text replies.

