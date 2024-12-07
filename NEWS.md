# Version 0.3.0 

Version 0.3.0 is currently being prepared for CRAN. This release represents a major milestone for **tidyllm**

The largest changes compared to **0.2.0** are:

## New Verb-Based Interface  

- **New Verb-Based Interface**: Users can now use verbs like `chat()`, `embed()`, `send_batch()`, `check_batch()`, and `fetch_batch()` to interact with APIs. These functions always work with a combination of verbs and providers:
  - **Verbs** (e.g., `chat()`, `embed()`, `send_batch()`) define the type of action you want to perform.
  - **Providers** (e.g., `openai()`, `claude()`, `ollama()`) are an arguement of verbs and specify the API to handle the action with and take provider-specific arguments

Each verb and provider combination routes the interaction to provider-specific functions like `openai_chat()` or `claude_chat()` that do the work in the background. These functions can  also be called directly as an alternative more verbose and  provider-specific interface. 

### Old Usage:  
```r
llm_message("Hello World") |>
  openai(.model = "gpt-4o")
```

### New Usage:
```r
# Recommended Verb-Based Approach
llm_message("Hello World") |>
  chat(openai(.model = "gpt-4o"))
  
# Or even configuring a provider outside
my_ollama <- ollama(.model = "llama3.2-vision:90B",
       .ollama_server = "https://ollama.example-server.de",
       .temperature = 0)

llm_message("Hello World") |>
  chat(my_ollama)

# Alternative Approach is to use more verbose specific functions:
llm_message("Hello World") |>
  openai_chat(.model = "gpt-4o")
```

### Backward Compatibility:

  - The old functions (`openai()`, `claude()`, etc.) still work if you directly supply an `LLMMessage` as arguement, but issue deprecation warnings when used directly for chat.
  - Users are encouraged to transition to the new interface for future-proof workflows.

## Breaking Changes:

- The output format of embedding APIs was changed from a matrix to a tibble with an input column and a list column containing one embedding vector and one input per row.
- `R6`-based saved `LLMMessage` objects are no longer compatible with the new version. Saved objects from earlier versions need to be re-created

## Other Major Features:

- `gemini()` and `perplexity()` as new supported API providers. `gemini()` brings interesting Video and Audio features as well as search grounding to **tidyllm**.  `perplexity()` also offers well cited search grounded assitant replies
- Batch-Processing for `mistral()`
- New Metadata-Extraction function `get_reply_metadata()` to get information on token usage, or on other relevant metadata (like sources used for grounding)

## Improvements:
- Refactored Package Internals: 
  - Transitioned from `R6` to `S7` for the main `LLMMessage` class, improving maintainability, interoperability, and future-proofing.
  - Consolidated all API-specific functionality into dedicated files
  
# Version 0.2.7

## Major Features

- Batch API functions for the Mistral API
- Search Grounding with the `.grounding_threshold` argument added of  the `gemini_chat()` function allowing you to use Google searches to ground model responses to a search result Gemini models. For example, asking about the maintainer of an obscure R package works with grounding but does only lead to a hallucination without: 
```r
llm_message("What is tidyllm and who maintains this package?") |>
  gemini_chat(.grounding_threshold = 0.3)
```

- Perplexity as additional API provider available through `perplexity_chat()`. The neat feature of perplexity is the up-to-date web search it does with detailed citations. Cited sources are available in the `api_specific`-list column of `get_metadata()`
- `.json_schema` support for `ollama()` available with Ollama 0.5.0

## Improvements

- Metadata extraction is now handled by api-specific methods. `get_metadata()` returns a list column with API-specific metadata

# Version 0.2.6

## Large Refactor of package internals

- Switch from `R6` to `S7` for the main `LLMMessage` class
- Several bug-fixes for `df_llm_message()`
- API formatting methods are now in the code files for API providers
- Rate-limit header extraction for tracking and streaming callback generation are now methods for `APIProvider` classes 
- All api-specific code is now in the `api_openai.R`,`api_gemini.R`,etc. files
- Support for `as_tibble()` S3 Generic for `LLMMessage`
- Rate limit tracking and output for verbose mode in API-functions moved to a single function `track_rate_limit()`
- Unnecessary `.onattach()` removed
- Bugfix in callback method of Gemini streaming responses (still not ideal, but works)
- Embedding functions refactored to reduce repeated code
- Small test code to look at potential interoperability with [elmer](https://github.com/tidyverse/elmer) for using elmer-type schemata. 
- API-key check moved into API-object method
- Slight refactoring for batch functions (there is still quite a bit of potential to reduce duplication)

## Breaking Changes

- Old `R6`-based `LLMMessage`-objects are not compatible with the new version anymore! This also applies to saved objects, like lists of batch files. 

## Minor Features

- Google Gemini now supports working with multiple files in one message for the file upload functionality

```r
here::here("local_wip","example.mp3") |> gemini_upload_file()
here::here("local_wip","legrille.mp4") |> gemini_upload_file()

file_tibble <- gemini_list_files()

llm_message("What are these two files about?") |>
  gemini_chat(.fileid=file_tibble$name)
```



# Version 0.2.5

## Major Features

Better embedding functions with improved output and error handling and new documentation. New article on using embeddings with **tidyllm**. Support for embedding models on azure with `azure_openai_embedding()`

## Breaking Changes

- The output format of `embed()` and the related API-specific functions was changed from a matrix to a tibble with an input column and a list column containing one embedding vector and one input per row.

# Version 0.2.4

## Refinements of the new interface

One disadvantage of the first iteration of the new interface was that all arguements that needed to be passed to provider-specific functions, were going through the provider function. This feels, unintuitive, because users expect common arguments (e.g., .model, .temperature) to be set directly in main verbs like `chat()` or `send_batch()`.Moreover,  provider functions don't expose arguments for autocomplete, making it harder for users to explore options. Therefore, the main API verbs now directly accept common arguements, and check them against the available arguements for each API.

## Bug-fixes

- New error message for not setting a provider in main verbs
- Missing export of main verbs fixed
- Wrong documentation fixed

# Version 0.2.3

## Major Interface Overhaul

`tidyllm` has introduced a verb-based interface overhaul to provide a more intuitive and flexible user experience. Previously, provider-specific functions like `claude()`, `openai()`, and others were directly used for chat-based workflows. Now, these functions primarily serve as provider configuration for some general verbs like `chat()`.

### Key Changes:
- **New Verb-Based Interface**: Users can now use verbs like `chat()`, `embed()`, `send_batch()`, `check_batch()`, and `fetch_batch()` to interact with APIs. These functions always work with a combination of verbs and providers:
  - **Verbs** (e.g., `chat()`, `embed()`, `send_batch()`) define the type of action you want to perform.
  - **Providers** (e.g., `openai()`, `claude()`, `ollama()`) are an arguement of verbs and specify the API to handle the action with and take provider-specific arguments

Each verb and provider combination routes the interaction to provider-specific functions like `openai_chat()` or `claude_chat()` that do the work in the background. These functions can  also be called directly as an alternative more verbose and  provider-specific interface. 
  
### Old Usage:
```r
llm_message("Hello World") |>
  openai(.model = "gpt-4o")
```

### New Usage:
```r
# Recommended Verb-Based Approach
llm_message("Hello World") |>
  chat(openai(.model = "gpt-4o"))
  
# Or even configuring a provider outside
my_ollama <- ollama(.model = "llama3.2-vision:90B",
       .ollama_server = "https://ollama.example-server.de",
       .temperature = 0)

llm_message("Hello World") |>
  chat(my_ollama)

# Alternative Approach is to use more verbose specific functions:
llm_message("Hello World") |>
  openai_chat(.model = "gpt-4o")
```

- **Backward Compatibility**:
  - The old functions (`openai()`, `claude()`, etc.) still work if you directly supply an `LLMMessage` as arguement, but issue deprecation warnings when used directly for chat.
  - Users are encouraged to transition to the new interface for future-proof workflows.
  
# Version 0.2.2

## Major Features

- Added functions to work with the Google Gemini API, with the new  `gemini()` main API-function
- Support for the file upload workflows for Gemini:
```r
#Upload a file for use with gemini
upload_info <- gemini_upload_file("example.mp3")

#Make the file available during a Gemini API call
llm_message("Summarize this speech") |>
  gemini(.fileid = upload_info$name)
  
#Delte the file from the Google servers
gemini_delete_file(upload_info$name)
```

- Brings video and audio support to tidyllm
- Google Gemini is the second API to fully support `tidyllm_schema()`
- `gemini()`-requests allow for a wide range of  file types  that can be used for context in messages
- Supported document formats for `gemini()` file workflows:
  - **PDF**: `application/pdf`
  - **TXT**: `text/plain`
  - **HTML**: `text/html`
  - **CSS**: `text/css`
  - **Markdown**: `text/md`
  - **CSV**: `text/csv`
  - **XML**: `text/xml`
  - **RTF**: `text/rtf`
- Supported code formats for `gemini()` file workflows:
  - **JavaScript**: `application/x-javascript`, `text/javascript`
  - **Python**: `application/x-python`, `text/x-python`
- Supported image formats for `gemini()` file workflows:
  - **PNG**: `image/png`
  - **JPEG**: `image/jpeg`
  - **WEBP**: `image/webp`
  - **HEIC**: `image/heic`
  - **HEIF**: `image/heif`
- Supported video formats for `gemini()` file workflows:
  - **MP4**: `video/mp4`
  - **MPEG**: `video/mpeg`
  - **MOV**: `video/mov`
  - **AVI**: `video/avi`
  - **FLV**: `video/x-flv`
  - **MPG**: `video/mpg`
  - **WEBM**: `video/webm`
  - **WMV**: `video/wmv`
  - **3GPP**: `video/3gpp`
- Supported audio formats for `gemini()` file workflows:
  - **WAV**: `audio/wav`
  - **MP3**: `audio/mp3`
  - **AIFF**: `audio/aiff`
  - **AAC**: `audio/aac`
  - **OGG Vorbis**: `audio/ogg`
  - **FLAC**: `audio/flac`
  
# Version 0.2.1

## Major Features:

- Added `get_metadata()` function to retrieve and format metadata from `LLMMessage` objects.
- Enhanced the `print` method for `LLMMessage` to support printing metadata, controlled via the new `tidyllm_print_metadata` option or a new `.meta`-arguement for the print method.


```r
conversation <- llm_message("Write a short poem about software development") |>
  claude()
  
#Get metdata on token usage and model as tibble  
get_metadata(conversation)

#or print it with the message
print(conversation,.meta=TRUE)

#Or allways print it
options(tidyllm_print_metadata=TRUE)
```

## Bug-fixes:

- Fixed a bug in `send_openai_batch()` caused by  a missing `.json`-arguement not being passed for messages without schema

# Version 0.2.0

New CRAN release. Largest changes compared to **0.1.0**:

**Major Features:**

- Batch Request Support: Added support for batch requests with both Anthropic and OpenAI APIs, enabling large-scale request handling.
- Schema Support: Improved structured outputs in JSON mode with advanced `.json_schema` handling in `openai()`, enhancing support for well-defined JSON responses.
- Azure OpenAI Integration: Introduced `azure_openai()` function for accessing the Azure OpenAI service, with full support for rate-limiting and batch operations tailored to Azure’s API structure.
- Embedding Model Support: Added embedding generation functions for the OpenAI, Ollama, and Mistral APIs, supporting message content and media embedding.
- Mistral API Integration: New `mistral()` function provides full support for Mistral models hosted in the EU, including rate-limiting and streaming capabilities.
- PDF Batch Processing: Introduced the `pdf_page_batch()` function, which processes PDFs page by page, allowing users to define page-specific prompts for detailed analysis.
- Support for OpenAI-compatible APIs: Introduced a `.compatible` argument (and flexible url and path) in `openai()` to allow compatibility with third-party OpenAI-compatible APIs.

**Improvements:**

- API Format Refactoring: Complete refactor of `to_api_format()` to reduce code duplication, simplify API format generation, and improve maintainability.
- Improved Error Handling: Enhanced input validation and error messaging for all API-functions functions, making troubleshooting easier.
- Rate-Limiting Enhancements: Updated rate limiting to use `httr2::req_retry()` in addition to the rate-limit tracking functions in tidyllm, using 429 headers to wait for rate limit resets.
- Expanded Testing: Added comprehensive tests for API functions using `httptest2`

**Breaking Changes:**

- Redesigned Reply Functions: `get_reply()` was split into `get_reply()` for text outputs and `get_reply_data()` for structured outputs, improving type stability compared to an earlier function that had different outputs based on a `.json`-arguement.
- Deprecation of `chatgpt()`: The `chatgpt()` function has been deprecated in favor of `openai()` for feature alignment and improved consistency.

**Minor Updates and Bug Fixes:**

- Expanded PDF Support in `llm_message()`: Allows extraction of specific page ranges from PDFs, improving flexibility in document handling.
- New `ollama_download_model()` function to download models from the Ollama API
- All sequential chat API functions now support streaming

# Version 0.1.11 

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

