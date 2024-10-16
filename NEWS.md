## Changelog for Development Version  0.1.7

Changes since the last CRAN Release 0.1.0

### Breaking Changes (Compared to CRAN Release 0.1.0)

- **`last_reply()` Changes**: The `.json` argument is no longer used, and JSON replies are automatically parsed. Use `.raw` for raw text.
- **Groq Models**: System prompts are no longer sent for Groq  models, since many models on groq do not support them and all multimodal models on groq do not allow for them.

### New Features

- **Message Retrieval Functions**: Added functions to retrieve single messages from conversations:
  - `last_user_message()`, `get_reply(index)`, `get_user_message(index)`

- **Updated `last_reply()`**: Now a wrapper around `get_reply()` for more consistent behavior.

- **New Ollama functions**:

  + **Model Download:** Introduced the `ollama_download_model()` function to download models from the Ollama API. It supports a streaming mode that provides live progress bar updates on the download progress.
  
  + **Embedding Generation:** Added `ollama_embedding()` to generate embeddings using the Ollama API. It processes message histories and combines  text from message content and media attachements for embeddings.

- **PDF Page Batch Processing**: Introduced the `pdf_page_batch()` function, which processes PDF files page by page, extracting text and converting each page into an image and allows for a general prompt or page specific prompts. The function generates a list of `LLMMessage` objects that can each be sent to an API 

- **Support for the Mistral API**: New `mistral()` function to use Mistral Models on Le Platforme on servers hosted in the EU. With rate-limiting and streaming-support.

### Improvements

### Groq support for vision

The `groq()` function now supports images. Since more modern models on groq, especially the ones with
multimodal abilities do not support system prompts, the system role is deleted from groq api calls.

### JSON Mode Improvements 

Since version 0.1.1, JSON mode is now more widely supported across all API functions, allowing for structured outputs when APIs support them. The `.json` argument is now passed only to API functions, specifying how the API should respond, it is not needed anymore in `last_reply()`.

Additionally, the behavior of the reply functions has changed. They now automatically handle JSON replies by parsing them into structured data and falling back to raw text in case of errors. You can still force raw text replies even for JSON output using the `.raw` argument.

### New tests for API functions
- **Easier Troubleshooting in API-function**: All API functions now support the `.dry_run` argument, allowing users to generate an `httr2`-request for easier debugging and inspection.
- **API Function Tests:** Implemented `httptest2`-based tests with mock responses for all API functions, covering both basic functionality and rate-limiting.

