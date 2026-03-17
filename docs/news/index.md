# Changelog

## tidyllm 0.4.0

### New Providers

#### OpenRouter (`openrouter()`)

Access to 300+ models from a single API key via OpenRouter. Supports
chat, embeddings, model listing, and fallback routing across providers:

``` r

# Chat with any model on OpenRouter
llm_message("What is the capital of France?") |>
  chat(openrouter(.model = "anthropic/claude-3.5-sonnet"))

# List available models
list_models(openrouter())

# Check account credits
openrouter_credits()

# Retrieve generation metadata (tokens, cost) for a completed request
openrouter_generation(generation_id)
```

OpenRouter also supports **provider fallback routing** — specify a list
of fallback providers to use if the primary model is unavailable.

#### llama.cpp (`llamacpp()`)

Full support for local
[llama.cpp](https://github.com/ggerganov/llama.cpp) servers, including
chat, embeddings, reranking, and model management:

``` r

# Chat with a local llama.cpp server
llm_message("Explain R to a Python developer") |>
  chat(llamacpp())

# Generate embeddings
c("text one", "text two") |> embed(llamacpp())

# Rerank documents by relevance
llamacpp_rerank("best R package for LLMs", c("tidyllm", "ellmer", "httr2"))

# Model management
llamacpp_list_local_models()          # list models in the model directory
list_hf_gguf_files("Qwen/Qwen2.5-7B-Instruct-GGUF")  # browse HuggingFace GGUF files
llamacpp_download_model("Qwen/Qwen2.5-7B-Instruct-GGUF", "qwen2.5-7b-instruct-q4_k_m.gguf")
llamacpp_delete_model("path/to/model.gguf")
llamacpp_health()                     # check server status
```

### New Verbs

#### `deep_research()`, `check_job()`, `fetch_job()`

A new
[`deep_research()`](https://edubruell.github.io/tidyllm/reference/deep_research.md)
verb for running long-horizon research tasks. Currently supported by
[`perplexity()`](https://edubruell.github.io/tidyllm/reference/perplexity.md)
via the `sonar-deep-research` model:

``` r

# Blocking — waits for completion and returns an LLMMessage
result <- llm_message("Compare Rust and Go for systems programming") |>
  deep_research(perplexity())

get_reply(result)
get_metadata(result)$api_specific[[1]]$citations

# Background — returns immediately, poll with check_job() / fetch_job()
job <- llm_message("Summarize the latest EU AI Act developments") |>
  deep_research(perplexity(), .background = TRUE)

check_job(job)   # poll status
result <- fetch_job(job)  # retrieve when complete
```

[`check_job()`](https://edubruell.github.io/tidyllm/reference/check_job.md)
and
[`fetch_job()`](https://edubruell.github.io/tidyllm/reference/fetch_job.md)
are type-dispatching aliases — they delegate to
[`check_batch()`](https://edubruell.github.io/tidyllm/reference/check_batch.md)/[`fetch_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_batch.md)
for batch objects, or to
[`perplexity_check_research()`](https://edubruell.github.io/tidyllm/reference/perplexity_check_research.md)/[`perplexity_fetch_research()`](https://edubruell.github.io/tidyllm/reference/perplexity_fetch_research.md)
for research jobs.

### Provider Enhancements

#### Perplexity

- New
  [`perplexity_deep_research()`](https://edubruell.github.io/tidyllm/reference/perplexity_deep_research.md),
  [`perplexity_check_research()`](https://edubruell.github.io/tidyllm/reference/perplexity_check_research.md),
  [`perplexity_fetch_research()`](https://edubruell.github.io/tidyllm/reference/perplexity_fetch_research.md)
  functions for async deep research via the `sonar-deep-research` model
- `.json_schema` structured output support for both
  [`perplexity_chat()`](https://edubruell.github.io/tidyllm/reference/perplexity_chat.md)
  and
  [`perplexity_deep_research()`](https://edubruell.github.io/tidyllm/reference/perplexity_deep_research.md)
- New `.search_domain_filter` parameter to restrict or exclude domains
  (up to 10, prefix `-` to exclude)
- New `.reasoning_effort` parameter for
  [`perplexity_deep_research()`](https://edubruell.github.io/tidyllm/reference/perplexity_deep_research.md)
  (`"low"`, `"medium"`, `"high"`)

#### Thinking modes

Extended thinking is now available for two additional providers:

- **Gemini**: `.thinking_budget` parameter in
  [`gemini_chat()`](https://edubruell.github.io/tidyllm/reference/gemini_chat.md)
  sets the token budget for internal reasoning (works with
  `gemini-2.5-flash` and `gemini-2.5-pro`). Thinking output is stored in
  `get_metadata()$api_specific[[1]]$thinking`.
- **DeepSeek**: `.thinking = TRUE` in
  [`deepseek_chat()`](https://edubruell.github.io/tidyllm/reference/deepseek_chat.md)
  switches to the `deepseek-reasoner` model and captures the reasoning
  trace in `get_metadata()$api_specific[[1]]$thinking`.

#### Tool use improvements

- **Unified multi-turn tool loop** across all providers — the same logic
  now handles iterative tool calls for OpenAI, Claude, Gemini, Mistral,
  Groq, Ollama, and OpenRouter
- **Enum and vector support in tool schemas** —
  [`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
  (enum) and vector fields are now correctly serialised in tool
  parameter schemas
- Multi-turn and parallel tool use verified across all supported
  providers

#### Ellmer compatibility

- [`ellmer_tool()`](https://edubruell.github.io/tidyllm/reference/ellmer_tool.md)
  converts ellmer `ToolDef` objects to tidyllm `TOOL` objects, enabling
  tools defined in ellmer (and packages like `btw`) to be used directly
  with tidyllm:

  ``` r

  btw_tool <- ellmer_tool(btw::btw_tool_files_list_files)
  llm_message("List files in the R/ folder") |>
    chat(claude(), .tools = btw_tool)
  ```

- [`ellmer_tool()`](https://edubruell.github.io/tidyllm/reference/ellmer_tool.md)
  also supports provider-native **builtin tools** such as
  [`ellmer::claude_tool_web_search()`](https://ellmer.tidyverse.org/reference/claude_tool_web_search.html):

  ``` r

  web_search <- ellmer_tool(ellmer::claude_tool_web_search())
  llm_message("Latest AI safety news?") |>
    chat(claude(), .tools = web_search)
  ```

- [`chat_ellmer()`](https://edubruell.github.io/tidyllm/reference/chat_ellmer.md)
  lets you use any ellmer `Chat` object as a tidyllm provider, bridging
  the two ecosystems

#### Groq

- `.json_schema` structured output support for
  [`groq_chat()`](https://edubruell.github.io/tidyllm/reference/groq_chat.md)
  and Groq batch requests
- Dropped legacy `json_object` mode in favour of proper JSON schema
  responses
- Fixed JSON attribute propagation from
  [`send_groq_batch()`](https://edubruell.github.io/tidyllm/reference/send_groq_batch.md)
  to
  [`fetch_groq_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_groq_batch.md)

#### Voyage AI

- [`voyage_rerank()`](https://edubruell.github.io/tidyllm/reference/voyage_rerank.md)
  — new reranking function using the `rerank-2` model; returns a tibble
  sorted by relevance score
- `.output_dimension` parameter for
  [`voyage_embedding()`](https://edubruell.github.io/tidyllm/reference/voyage_embedding.md)
  — control output vector size (256, 512, 1024, 2048) for Voyage-4
  models

#### OpenRouter

- [`openrouter_embedding()`](https://edubruell.github.io/tidyllm/reference/openrouter_embedding.md)
  — generate embeddings via OpenRouter
- [`openrouter_credits()`](https://edubruell.github.io/tidyllm/reference/openrouter_credits.md)
  — check account balance and credit usage
- [`openrouter_generation()`](https://edubruell.github.io/tidyllm/reference/openrouter_generation.md)
  — retrieve token and cost metadata for a completed generation

#### Azure OpenAI

- Fixed
  [`check_azure_openai_batch()`](https://edubruell.github.io/tidyllm/reference/check_azure_openai_batch.md)
  and
  [`fetch_azure_openai_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_azure_openai_batch.md)
  to handle `null` values for `created_at` and `expires_at` fields
  returned by some deployments

### Small Changes / Housekeeping

- Default chat model for
  [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md)
  changed to `qwen3.5:4b` (faster, better instruction following for
  local use)
- Default embedding model for
  [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md)
  changed to `qwen3-embedding:0.6b`
- Verb dispatch refactored into a shared `dispatch_to_provider()`
  helper, reducing duplication across all verbs

### Bug Fixes

- Fixed JSON attribute not being propagated from
  [`send_mistral_batch()`](https://edubruell.github.io/tidyllm/reference/send_mistral_batch.md)
  and
  [`send_groq_batch()`](https://edubruell.github.io/tidyllm/reference/send_groq_batch.md)
  to their respective fetch functions, causing structured-output batch
  results to be returned as raw text
- Fixed
  [`perplexity_deep_research()`](https://edubruell.github.io/tidyllm/reference/perplexity_deep_research.md)
  API request format for the async endpoint

## Version 0.3.5

CRAN release: 2025-08-22

### Key Improvements

- Rudimentary support for file uploads in
  [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md).
  At the moment only implemented for the
  [`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) verb

``` r

example_file <- here::here("vignettes","die_verwandlung.pdf") |> 
  claude_upload_file()

llm_message("Summarize the document in 100 words") |>
  chat(claude(.file_ids = example_file$file_id)) 
  
#> Message History:
#> system:
#> You are a helpful assistant
#> --------------------------------------------------------------
#> user:
#> Summarize the document in 100 words
#> --------------------------------------------------------------
#> assistant:
#> This document is the German text of Franz Kafka's novella
#> "Die Verwandlung" (The Metamorphosis), published through
#> Project Gutenberg. The story follows Gregor Samsa, a
#> traveling salesman who wakes up one morning transformed into
#> a monstrous insect-like creature. Unable to work and support
#> his family, Gregor becomes isolated in his room while his
#> family struggles with the burden of his transformation.
#> His sister Grete initially cares for him, bringing food
#> and cleaning his room, but over time the family's situation
#> deteriorates financially and emotionally. The story explores
#> themes of alienation, family duty, and dehumanization as
#> Gregor gradually loses his human identity and connection to
#> his family. Eventually, Gregor dies, and his family, though
#> initially grief-stricken, ultimately feels relieved and
#> optimistic about their future without the burden of caring
#> for him. The text includes the complete three-part novella
#> along with Project Gutenberg licensing information.
#> --------------------------------------------------------------  
```

- **Expanded Perplexity Support:** The
  [`perplexity()`](https://edubruell.github.io/tidyllm/reference/perplexity.md)
  provider now supports more Perplexity API parameters, allowing you to
  set reasoning and search effort.
- **Gemini Batches:** Batch support for
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  with most functionality of
  [`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md)
  requests.

## Version 0.3.4

CRAN release: 2025-03-27

This release marks a **major internal refactor** accompanied by a suite
of subtle yet impactful improvements. While many changes occur under the
hood, they collectively deliver a more robust, flexible, and
maintainable framework.

### Key Improvements

- **Robust Streaming:**

  - **New Streaming Backend:** Streaming is now handled via
    [`httr2::req_perform_connection()`](https://httr2.r-lib.org/reference/req_perform_connection.html)
    (httr2 ≥ 1.1.1), resulting in a more stable and reliable experience.
  - **Metadata for Streaming:** Streaming requests now also support
    metadata extraction, logprobs, and other features, making them even
    more informative.

- **Optimized Internal Processing:**

  - **S7 Methods Integration:** Improved handling of streams and chat
    parsing using more proper S7 methods instead of clunky old function
    generation.
  - **OpenAI Request Construction:** Both OpenAI and Azure OpenAI (along
    with their batch functions) now use a common request construction
    function to reduce code duplication and simplify maintenance.

- **Schema support:**

- New
  [`field_object()`](https://edubruell.github.io/tidyllm/reference/field_object.md)
  function to allow for nested schemata

- **Expanded API Features:**

  - **JSON Schema Support:**
    - [`mistral()`](https://edubruell.github.io/tidyllm/reference/mistral.md)
      now accepts the `.json_schema` argument.
    - [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md)
      incorporates `.json_schema` via a JSON-extractor tool, in line
      with Anthropic’s guidelines.
  - **Batch API for groq():** A new batch processing interface has been
    implemented for groq().

### Bug Fixes

- **claude() Batch Requests:** Fixed an issue where system prompts were
  not transmitted correctly in batch mode.
- **gemini() Prompt Handling:** Resolved a bug causing system prompts to
  be omitted from API calls in older versions.

## Dev-Version 0.3.3

### Thinking support in Claude

Claude now supports reasoning:

``` r
conversation <- llm_message("Are there an infinite number of prime numbers such that n mod 4 == 3?") |>
   chat(claude(.thinking=TRUE)) |>
  print()
   
#> Message History:
#> system:
#> You are a helpful assistant
#> --------------------------------------------------------------
#> user:
#> Are there an infinite number of prime numbers such that n
#> mod 4 == 3?
#> --------------------------------------------------------------
#> assistant:
#> # Infinitude of Primes Congruent to 3 mod 4
#> 
#> Yes, there are infinitely many prime numbers $p$ such
#> that $p \equiv 3 \pmod{4}$ (when $p$ divided by 4 leaves
#> remainder 3).
#> 
#> ## Proof by Contradiction
#> 
#> I'll use a proof technique similar to Euclid's classic proof
#> of the infinitude of primes:
#> 
#> 1) Assume there are only finitely many primes $p$ such that
#> $p \equiv 3 \pmod{4}$. Let's call them $p_1, p_2, ..., p_k$.
#> 
#> 2) Consider the number $N = 4p_1p_2...p_k - 1$
#> 
#> 3) Note that $N \equiv 3 \pmod{4}$ since $4p_1p_2...p_k
#> \equiv 0 \pmod{4}$ and $4p_1p_2...p_k - 1 \equiv -1 \equiv 3
#> \pmod{4}$
#> 
#> 4) $N$ must have at least one prime factor $q$
#> 
#> 5) For any $i$ between 1 and $k$, we have $N \equiv -1
#> \pmod{p_i}$, so $N$ is not divisible by any of the primes
#> $p_1, p_2, ..., p_k$
#> 
#> 6) Therefore, $q$ is a prime not in our original list
#> 
#> 7) Furthermore, $q$ must be congruent to 3 modulo 4:
#> - $q$ cannot be 2 because $N$ is odd
#> - If $q \equiv 1 \pmod{4}$, then $\frac{N}{q} \equiv 3
#> \pmod{4}$ would need another prime factor congruent to 3
#> modulo 4
#> - So $q \equiv 3 \pmod{4}$
#> 
#> 8) This contradicts our assumption that we listed all primes
#> of the form $p \equiv 3 \pmod{4}$
#> 
#> Therefore, there must be infinitely many primes of the form
#> $p \equiv 3 \pmod{4}$.
#> --------------------------------------------------------------

#Thinking process is stored in API-specific metadata
conversation |> 
   get_metadata() |>
   dplyr::pull(api_specific) |>
   purrr::map_chr("thinking") |>
   cat()
   
#> The question is asking if there are infinitely many prime numbers $p$ such that $p \equiv 3 \pmod{4}$, i.e., when divided by 4, the remainder is 3.
#> 
#> I know that there are infinitely many prime numbers overall. The classic proof is Euclid's proof by contradiction: if there were only finitely many primes, we could multiply them all together, add 1, and get a new number not divisible by any of the existing primes, which gives us a contradiction.
#> 
#> For primes of the form $p \equiv 3 \pmod{4}$, we can use a similar proof strategy. 
#> 
#> Let's assume there are only finitely many primes $p_1, p_2, \ldots, p_k$ such that $p_i \equiv 3 \pmod{4}$ for all $i$. 
#> 
#> Now, consider the number $N = 4 \cdot p_1 \cdot p_2 \cdot \ldots \cdot p_k - 1$. 
#> 
#> Note that $N \equiv -1 \equiv 3 \pmod{4}$. 
#> 
#> Now, let's consider the prime factorization of $N$. If $N$ is itself prime, then we have found a new prime $N$ such that $N \equiv 3 \pmod{4}$, which contradicts our assumption that we enumerated all such primes.
#> 
> ...
```

### Bugfixes

- Bugfix for
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md):
  Sytem prompts were not sent to the API in older versions

## Version 0.3.2

CRAN release: 2025-03-07

### Tool usage introduced to tidyllm

A first tool usage system inspired by a similar system in `ellmer` has
been introduced to tidyllm. At the moment tool use is available for
[`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
[`mistral()`](https://edubruell.github.io/tidyllm/reference/mistral.md),
[`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md),
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
and [`groq()`](https://edubruell.github.io/tidyllm/reference/groq.md):

``` r

get_current_time <- function(tz, format = "%Y-%m-%d %H:%M:%S") {
  format(Sys.time(), tz = tz, format = format, usetz = TRUE)
}

time_tool <- tidyllm_tool(
  .f = get_current_time,
  .description = "Returns the current time in a specified timezone. Use this to determine the current time in any location.",
  tz = field_chr("The time zone identifier (e.g., 'Europe/Berlin', 'America/New_York', 'Asia/Tokyo', 'UTC'). Required."),
  format = field_chr("Format string for the time output. Default is '%Y-%m-%d %H:%M:%S'.")
)


llm_message("What's the exact time in Stuttgart?") |>
  chat(openai,.tools=time_tool)
  
#> Message History:
#> system:
#> You are a helpful assistant
#> --------------------------------------------------------------
#> user:
#> What's the exact time in Stuttgart?
#> --------------------------------------------------------------
#> assistant:
#> The current time in Stuttgart (Europe/Berlin timezone) is
#> 2025-03-03 09:51:22 CET.
#> --------------------------------------------------------------  
```

You can use the
[`tidyllm_tool()`](https://edubruell.github.io/tidyllm/reference/tidyllm_tool.md)
function to define tools available to a large language model. Once a
tool or a list of tools is passed to a model, it can request to run
these these functions in your current session and use their output for
further generation context.

### Support for DeepSeek added

tidyllm now supports the deepseek API as provider via
[`deepseek_chat()`](https://edubruell.github.io/tidyllm/reference/deepseek_chat.md)
or the
[`deepseek()`](https://edubruell.github.io/tidyllm/reference/deepseek.md)
provider function. Deepseek supports logprobs just like
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
which you can get via
[`get_logprobs()`](https://edubruell.github.io/tidyllm/reference/get_logprobs.md).
At the moment tool usage for deepseek is very inconsistent.

### Support for Voyage.ai and Multimodal Embeddings Added

Voyage.ai introduces a **unique multimodal embeddings feature**,
allowing you to generate embeddings not only for text but also for
images. The new
[`voyage_embedding()`](https://edubruell.github.io/tidyllm/reference/voyage_embedding.md)
function in **tidyllm** enables this functionality by seamlessly
handling different input types, working with both the new feature as
well as the same inputs as for other embedding functions.

The new [`img()`](https://edubruell.github.io/tidyllm/reference/img.md)
function lets you create image objects for embedding. You can mix text
and [`img()`](https://edubruell.github.io/tidyllm/reference/img.md)
objects in a list and send them to Voyage AI for multimodal embeddings:

``` r

list("tidyllm", img(here::here("docs", "logo.png"))) |>
  embed(voyage)
#> # A tibble: 2 × 2
#>   input          embeddings   
#>   <chr>          <list>       
#> 1 tidyllm        <dbl [1,024]>
#> 2 [IMG] logo.png <dbl [1,024]>
```

In this example, both text (`"tidyllm"`) and an image (`logo.png`) are
embedded together. The function returns a tibble where the `input`
column contains the text and labeled image names, and the `embeddings`
column contains the corresponding embedding vectors.

### New Tests and Bugfixes

- Several Bugfixes in
  [`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)
  and
  [`tidyllm_tool()`](https://edubruell.github.io/tidyllm/reference/tidyllm_tool.md)
- New Tests for less covered APIs.

## Version 0.3.1

CRAN release: 2025-02-24

⚠️ There is a bad bug in the latest CRAN release in the
[`fetch_openai_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_openai_batch.md)
function that is only fixed in version 0.3.2. For the release 0.3.1. the
[`fetch_openai_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_openai_batch.md)
function throws errors if the logprobs are turned off.

### Changes compared to last release

- **New schema field functions inspired by ellmer and more schema
  compatibility with ellmer:** If you have ellmer installed you can now
  directly use ellmer type objects in the `.json_schema` option of
  api-functions. Moreover,
  [`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)
  now accepts ellmer types as field definitions. In addition four
  ellmer-inspired type-definition
  functions[`field_chr()`](https://edubruell.github.io/tidyllm/reference/field_chr.md),
  [`field_dbl()`](https://edubruell.github.io/tidyllm/reference/field_chr.md),
  [`field_lgl()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
  and
  [`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
  were added that allow you to set description fields in schemata

``` r
 ellmer_adress <-ellmer::type_object(
    street = ellmer::type_string("A famous street"),
    houseNumber = ellmer::type_number("a 3 digit number"),
    postcode = ellmer::type_string(),
    city = ellmer::type_string("A large city"),
    region = ellmer::type_string(),
    country = ellmer::type_enum(values = c("Germany", "France"))
  ) 

person_schema <-  tidyllm_schema(
                person_name = "string",
                age = field_dbl("An age between 25 and 40"),
                is_employed = field_lgl("Employment Status in the last year")
                occupation = field_fct(.levels=c("Lawyer","Butcher")),
                address = ellmer_adress
                )

address_message <- llm_message("imagine an address") |>
  chat(openai,.json_schema = ellmer_adress)
  
person_message  <- llm_message("imagine a person profile") |>
  chat(openai,.json_schema = person_schema)
```

- Support for logprobs in
  [`openai_chat()`](https://edubruell.github.io/tidyllm/reference/openai_chat.md)
  and
  [`send_openai_batch()`](https://edubruell.github.io/tidyllm/reference/send_openai_batch.md)
  and new
  [`get_logprobs()`](https://edubruell.github.io/tidyllm/reference/get_logprobs.md)
  function:

``` r

badger_poem <- llm_message("Write a haiku about badgers") |>
    chat(openai(.logprobs=TRUE,.top_logprobs=5))

 badger_poem |> get_logprobs()
#> # A tibble: 19 × 5
#>   reply_index token          logprob bytes     top_logprobs
#>          <int> <chr>            <dbl> <list>    <list>      
#>  1           1 "In"       -0.491      <int [2]> <list [5]>  
#>  2           1 " moon"    -1.12       <int [5]> <list [5]>  
#>  3           1 "lit"      -0.00489    <int [3]> <list [5]>  
#>  4           1 " forest"  -1.18       <int [7]> <list [5]>  
#>  5           1 ","        -0.00532    <int [1]> <list [5]>  
```

- Bugfix in OpenAI metadata extraction
- New
  [`ollama_delete_model()`](https://edubruell.github.io/tidyllm/reference/ollama_delete_model.md)
  function
- [`list_models()`](https://edubruell.github.io/tidyllm/reference/list_models.md)
  is now a verb supporting most providers.

``` r

list_models(openai)
#> # A tibble: 52 × 3
#>    id                                   created             owned_by
#>    <chr>                                <chr>               <chr>   
#>  1 gpt-4o-mini-audio-preview-2024-12-17 2024-12-13 18:52:00 system  
#>  2 gpt-4-turbo-2024-04-09               2024-04-08 18:41:17 system  
#>  3 dall-e-3                             2023-10-31 20:46:29 system  
#>  4 dall-e-2                             2023-11-01 00:22:57 system  
```

- New synchronous
  [`send_ollama_batch()`](https://edubruell.github.io/tidyllm/reference/send_ollama_batch.md)
  function to make use of the fast parallel request features of Ollama.
- New batch functions for Azure Openai (thanks [Jia
  Zhang](https://github.com/JiaZhang42))
- New parameters for
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
  reasoning models supported
- Default models updated for
  [`perplexity()`](https://edubruell.github.io/tidyllm/reference/perplexity.md)
  and
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
- Fixed bug in the print method of `LLMMessage`

## Version 0.3.0

CRAN release: 2024-12-08

**tidyllm 0.3.0** represents a major milestone for **tidyllm**

The largest changes compared to **0.2.0** are:

### New Verb-Based Interface

- **New Verb-Based Interface**: Users can now use verbs like
  [`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md),
  [`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md),
  [`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md),
  [`check_batch()`](https://edubruell.github.io/tidyllm/reference/check_batch.md),
  and
  [`fetch_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_batch.md)
  to interact with APIs. These functions always work with a combination
  of verbs and providers:
  - **Verbs** (e.g.,
    [`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md),
    [`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md),
    [`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md))
    define the type of action you want to perform.
  - **Providers** (e.g.,
    [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
    [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
    [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md))
    are an arguement of verbs and specify the API to handle the action
    with and take provider-specific arguments

Each verb and provider combination routes the interaction to
provider-specific functions like
[`openai_chat()`](https://edubruell.github.io/tidyllm/reference/openai_chat.md)
or
[`claude_chat()`](https://edubruell.github.io/tidyllm/reference/claude_chat.md)
that do the work in the background. These functions can also be called
directly as an alternative more verbose and provider-specific interface.

#### Old Usage:

``` r

llm_message("Hello World") |>
  openai(.model = "gpt-4o")
```

#### New Usage:

``` r

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

#### Backward Compatibility:

- The old functions
  ([`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
  [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
  etc.) still work if you directly supply an `LLMMessage` as arguement,
  but issue deprecation warnings when used directly for chat.
- Users are encouraged to transition to the new interface for
  future-proof workflows.

### Breaking Changes:

- The output format of embedding APIs was changed from a matrix to a
  tibble with an input column and a list column containing one embedding
  vector and one input per row.
- `R6`-based saved `LLMMessage` objects are no longer compatible with
  the new version. Saved objects from earlier versions need to be
  re-created

### Other Major Features:

- [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  and
  [`perplexity()`](https://edubruell.github.io/tidyllm/reference/perplexity.md)
  as new supported API providers.
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  brings interesting Video and Audio features as well as search
  grounding to **tidyllm**.
  [`perplexity()`](https://edubruell.github.io/tidyllm/reference/perplexity.md)
  also offers well cited search grounded assitant replies
- Batch-Processing for
  [`mistral()`](https://edubruell.github.io/tidyllm/reference/mistral.md)
- New Metadata-Extraction function `get_reply_metadata()` to get
  information on token usage, or on other relevant metadata (like
  sources used for grounding)

### Improvements:

- Refactored Package Internals:
  - Transitioned from `R6` to `S7` for the main `LLMMessage` class,
    improving maintainability, interoperability, and future-proofing.
  - Consolidated all API-specific functionality into dedicated files

## Version 0.2.7

### Major Features

- Batch API functions for the Mistral API
- Search Grounding with the `.grounding_threshold` argument added of the
  [`gemini_chat()`](https://edubruell.github.io/tidyllm/reference/gemini_chat.md)
  function allowing you to use Google searches to ground model responses
  to a search result Gemini models. For example, asking about the
  maintainer of an obscure R package works with grounding but does only
  lead to a hallucination without:

``` r

llm_message("What is tidyllm and who maintains this package?") |>
  gemini_chat(.grounding_threshold = 0.3)
```

- Perplexity as additional API provider available through
  [`perplexity_chat()`](https://edubruell.github.io/tidyllm/reference/perplexity_chat.md).
  The neat feature of perplexity is the up-to-date web search it does
  with detailed citations. Cited sources are available in the
  `api_specific`-list column of
  [`get_metadata()`](https://edubruell.github.io/tidyllm/reference/get_metadata.md)
- `.json_schema` support for
  [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md)
  available with Ollama 0.5.0

### Improvements

- Metadata extraction is now handled by api-specific methods.
  [`get_metadata()`](https://edubruell.github.io/tidyllm/reference/get_metadata.md)
  returns a list column with API-specific metadata

## Version 0.2.6

### Large Refactor of package internals

- Switch from `R6` to `S7` for the main `LLMMessage` class
- Several bug-fixes for
  [`df_llm_message()`](https://edubruell.github.io/tidyllm/reference/df_llm_message.md)
- API formatting methods are now in the code files for API providers
- Rate-limit header extraction for tracking and streaming callback
  generation are now methods for `APIProvider` classes
- All api-specific code is now in the `api_openai.R`,`api_gemini.R`,etc.
  files
- Support for
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  S3 Generic for `LLMMessage`
- Rate limit tracking and output for verbose mode in API-functions moved
  to a single function `track_rate_limit()`
- Unnecessary `.onattach()` removed
- Bugfix in callback method of Gemini streaming responses (still not
  ideal, but works)
- Embedding functions refactored to reduce repeated code
- API-key check moved into API-object method
- Slight refactoring for batch functions (there is still quite a bit of
  potential to reduce duplication)

### Breaking Changes

- Old `R6`-based `LLMMessage`-objects are not compatible with the new
  version anymore! This also applies to saved objects, like lists of
  batch files.

### Minor Features

- Google Gemini now supports working with multiple files in one message
  for the file upload functionality

``` r

here::here("local_wip","example.mp3") |> gemini_upload_file()
here::here("local_wip","legrille.mp4") |> gemini_upload_file()

file_tibble <- gemini_list_files()

llm_message("What are these two files about?") |>
  gemini_chat(.fileid=file_tibble$name)
```

## Version 0.2.5

### Major Features

Better embedding functions with improved output and error handling and
new documentation. New article on using embeddings with **tidyllm**.
Support for embedding models on azure with
[`azure_openai_embedding()`](https://edubruell.github.io/tidyllm/reference/azure_openai_embedding.md)

### Breaking Changes

- The output format of
  [`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md)
  and the related API-specific functions was changed from a matrix to a
  tibble with an input column and a list column containing one embedding
  vector and one input per row.

## Version 0.2.4

### Refinements of the new interface

One disadvantage of the first iteration of the new interface was that
all arguements that needed to be passed to provider-specific functions,
were going through the provider function. This feels, unintuitive,
because users expect common arguments (e.g., .model, .temperature) to be
set directly in main verbs like
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) or
[`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md).Moreover,
provider functions don’t expose arguments for autocomplete, making it
harder for users to explore options. Therefore, the main API verbs now
directly accept common arguements, and check them against the available
arguements for each API.

### Bug-fixes

- New error message for not setting a provider in main verbs
- Missing export of main verbs fixed
- Wrong documentation fixed

## Version 0.2.3

### Major Interface Overhaul

`tidyllm` has introduced a verb-based interface overhaul to provide a
more intuitive and flexible user experience. Previously,
provider-specific functions like
[`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
and others were directly used for chat-based workflows. Now, these
functions primarily serve as provider configuration for some general
verbs like
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md).

#### Key Changes:

- **New Verb-Based Interface**: Users can now use verbs like
  [`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md),
  [`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md),
  [`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md),
  [`check_batch()`](https://edubruell.github.io/tidyllm/reference/check_batch.md),
  and
  [`fetch_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_batch.md)
  to interact with APIs. These functions always work with a combination
  of verbs and providers:
  - **Verbs** (e.g.,
    [`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md),
    [`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md),
    [`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md))
    define the type of action you want to perform.
  - **Providers** (e.g.,
    [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
    [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
    [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md))
    are an arguement of verbs and specify the API to handle the action
    with and take provider-specific arguments

Each verb and provider combination routes the interaction to
provider-specific functions like
[`openai_chat()`](https://edubruell.github.io/tidyllm/reference/openai_chat.md)
or
[`claude_chat()`](https://edubruell.github.io/tidyllm/reference/claude_chat.md)
that do the work in the background. These functions can also be called
directly as an alternative more verbose and provider-specific interface.

#### Old Usage:

``` r

llm_message("Hello World") |>
  openai(.model = "gpt-4o")
```

#### New Usage:

``` r

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
  - The old functions
    ([`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
    [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
    etc.) still work if you directly supply an `LLMMessage` as
    arguement, but issue deprecation warnings when used directly for
    chat.
  - Users are encouraged to transition to the new interface for
    future-proof workflows.

## Version 0.2.2

### Major Features

- Added functions to work with the Google Gemini API, with the new
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  main API-function
- Support for the file upload workflows for Gemini:

``` r

#Upload a file for use with gemini
upload_info <- gemini_upload_file("example.mp3")

#Make the file available during a Gemini API call
llm_message("Summarize this speech") |>
  gemini(.fileid = upload_info$name)
  
#Delte the file from the Google servers
gemini_delete_file(upload_info$name)
```

- Brings video and audio support to tidyllm
- Google Gemini is the second API to fully support
  [`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)
- [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)-requests
  allow for a wide range of file types that can be used for context in
  messages
- Supported document formats for
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  file workflows:
  - **PDF**: `application/pdf`
  - **TXT**: `text/plain`
  - **HTML**: `text/html`
  - **CSS**: `text/css`
  - **Markdown**: `text/md`
  - **CSV**: `text/csv`
  - **XML**: `text/xml`
  - **RTF**: `text/rtf`
- Supported code formats for
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  file workflows:
  - **JavaScript**: `application/x-javascript`, `text/javascript`
  - **Python**: `application/x-python`, `text/x-python`
- Supported image formats for
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  file workflows:
  - **PNG**: `image/png`
  - **JPEG**: `image/jpeg`
  - **WEBP**: `image/webp`
  - **HEIC**: `image/heic`
  - **HEIF**: `image/heif`
- Supported video formats for
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  file workflows:
  - **MP4**: `video/mp4`
  - **MPEG**: `video/mpeg`
  - **MOV**: `video/mov`
  - **AVI**: `video/avi`
  - **FLV**: `video/x-flv`
  - **MPG**: `video/mpg`
  - **WEBM**: `video/webm`
  - **WMV**: `video/wmv`
  - **3GPP**: `video/3gpp`
- Supported audio formats for
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
  file workflows:
  - **WAV**: `audio/wav`
  - **MP3**: `audio/mp3`
  - **AIFF**: `audio/aiff`
  - **AAC**: `audio/aac`
  - **OGG Vorbis**: `audio/ogg`
  - **FLAC**: `audio/flac`

## Version 0.2.1

### Major Features:

- Added
  [`get_metadata()`](https://edubruell.github.io/tidyllm/reference/get_metadata.md)
  function to retrieve and format metadata from `LLMMessage` objects.
- Enhanced the `print` method for `LLMMessage` to support printing
  metadata, controlled via the new `tidyllm_print_metadata` option or a
  new `.meta`-arguement for the print method.

``` r

conversation <- llm_message("Write a short poem about software development") |>
  claude()
  
#Get metdata on token usage and model as tibble  
get_metadata(conversation)

#or print it with the message
print(conversation,.meta=TRUE)

#Or allways print it
options(tidyllm_print_metadata=TRUE)
```

### Bug-fixes:

- Fixed a bug in
  [`send_openai_batch()`](https://edubruell.github.io/tidyllm/reference/send_openai_batch.md)
  caused by a missing `.json`-arguement not being passed for messages
  without schema

## Version 0.2.0

CRAN release: 2024-11-07

New CRAN release. Largest changes compared to **0.1.0**:

**Major Features:**

- Batch Request Support: Added support for batch requests with both
  Anthropic and OpenAI APIs, enabling large-scale request handling.
- Schema Support: Improved structured outputs in JSON mode with advanced
  `.json_schema` handling in
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
  enhancing support for well-defined JSON responses.
- Azure OpenAI Integration: Introduced
  [`azure_openai()`](https://edubruell.github.io/tidyllm/reference/azure_openai.md)
  function for accessing the Azure OpenAI service, with full support for
  rate-limiting and batch operations tailored to Azure’s API structure.
- Embedding Model Support: Added embedding generation functions for the
  OpenAI, Ollama, and Mistral APIs, supporting message content and media
  embedding.
- Mistral API Integration: New
  [`mistral()`](https://edubruell.github.io/tidyllm/reference/mistral.md)
  function provides full support for Mistral models hosted in the EU,
  including rate-limiting and streaming capabilities.
- PDF Batch Processing: Introduced the
  [`pdf_page_batch()`](https://edubruell.github.io/tidyllm/reference/pdf_page_batch.md)
  function, which processes PDFs page by page, allowing users to define
  page-specific prompts for detailed analysis.
- Support for OpenAI-compatible APIs: Introduced a `.compatible`
  argument (and flexible url and path) in
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
  to allow compatibility with third-party OpenAI-compatible APIs.

**Improvements:**

- API Format Refactoring: Complete refactor of `to_api_format()` to
  reduce code duplication, simplify API format generation, and improve
  maintainability.
- Improved Error Handling: Enhanced input validation and error messaging
  for all API-functions functions, making troubleshooting easier.
- Rate-Limiting Enhancements: Updated rate limiting to use
  [`httr2::req_retry()`](https://httr2.r-lib.org/reference/req_retry.html)
  in addition to the rate-limit tracking functions in tidyllm, using 429
  headers to wait for rate limit resets.
- Expanded Testing: Added comprehensive tests for API functions using
  `httptest2`

**Breaking Changes:**

- Redesigned Reply Functions:
  [`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)
  was split into
  [`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)
  for text outputs and
  [`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)
  for structured outputs, improving type stability compared to an
  earlier function that had different outputs based on a
  `.json`-arguement.
- Deprecation of
  [`chatgpt()`](https://edubruell.github.io/tidyllm/reference/chatgpt.md):
  The
  [`chatgpt()`](https://edubruell.github.io/tidyllm/reference/chatgpt.md)
  function has been deprecated in favor of
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
  for feature alignment and improved consistency.

**Minor Updates and Bug Fixes:**

- Expanded PDF Support in
  [`llm_message()`](https://edubruell.github.io/tidyllm/reference/llm_message.md):
  Allows extraction of specific page ranges from PDFs, improving
  flexibility in document handling.
- New
  [`ollama_download_model()`](https://edubruell.github.io/tidyllm/reference/ollama_download_model.md)
  function to download models from the Ollama API
- All sequential chat API functions now support streaming

## Version 0.1.11

### Major Features

- Support for both the Anthropic and the OpenAI batch request API added
- New `.compatible`-arguement in
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
  to allow working with compatible third party APIs

### Improvements

- **Complete refactor of `to_api_format()`**: API format generation now
  has much less code duplication and is more maintainable.

## Version 0.1.10

### Breaking Changes

- [`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)
  was split into two type-stable functions:
  [`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)
  for text and
  [`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)
  for structured outputs.

### Improvements

- **Rate limiting updated to use
  [`httr2::req_retry()`](https://httr2.r-lib.org/reference/req_retry.html)**:
  Rate limiting now uses the right 429 headers where they come.

## Version 0.1.9

### Major Features

- **Enhanced Input Validation**: All API functions now have improved
  input validation, ensuring better alignment with API documentation

- **Improved error handling** More human-readable error messages for
  failed requests from the API

- **Advanced JSON Mode in
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)**:
  The
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
  function now supports advanced `.json_schemas`, allowing structured
  output in JSON mode for more precise responses.

- **Reasoning Models Support**: Support for O1 reasoning models has been
  added, with better handling of system prompts in the
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
  function.

- **Streaming callback functions refactored:** Given that the streaming
  callback format for Open AI, Mistral and Groq is nearly identical the
  three now rely on the same callback function.

### Breaking Changes

- **[`chatgpt()`](https://edubruell.github.io/tidyllm/reference/chatgpt.md)
  Deprecated**: The
  [`chatgpt()`](https://edubruell.github.io/tidyllm/reference/chatgpt.md)
  function has been deprecated in favor of
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md).
  Users should migrate to
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
  to take advantage of the new features and enhancements.

### Improvements

- **Better Error Handling**: The
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
  [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md),
  and
  [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md)
  functions now return more informative error messages when API calls
  fail, helping with debugging and troubleshooting.

------------------------------------------------------------------------

## Version 0.1.8

### Major Features

- **Embedding Models Support:** Embedding model support for three APIs:
  - Embedding functions process message histories and combine text from
    message content and media attachments for embedding models.
  - [`ollama_embedding()`](https://edubruell.github.io/tidyllm/reference/ollama_embedding.md)
    to generate embeddings using the Ollama API.
  - [`openai_embedding()`](https://edubruell.github.io/tidyllm/reference/openai_embedding.md)
    to generate embeddings using the OpenAI API.
  - [`mistral_embedding()`](https://edubruell.github.io/tidyllm/reference/mistral_embedding.md)
    to generate embeddings using the Mistral API.

### Improvements

- **PDF Page Support in
  [`llm_message()`](https://edubruell.github.io/tidyllm/reference/llm_message.md):**
  The
  [`llm_message()`](https://edubruell.github.io/tidyllm/reference/llm_message.md)
  function now supports specifying a range of pages in a PDF by passing
  a list with `filename`, `start_page`, and `end_page`. This allows
  users to extract and process specific pages of a PDF.

------------------------------------------------------------------------

## Version 0.1.7

### Major Features

- **PDF Page Batch Processing**: Introduced the
  [`pdf_page_batch()`](https://edubruell.github.io/tidyllm/reference/pdf_page_batch.md)
  function, which processes PDF files page by page, extracting text and
  converting each page into an image, allowing for a general prompt or
  page-specific prompts. The function generates a list of `LLMMessage`
  objects that can be sent to an API and work with the batch-API
  functions in **tidyllm**.

------------------------------------------------------------------------

## Version 0.1.6

### Major Features

- **Support for the Mistral API**: New
  [`mistral()`](https://edubruell.github.io/tidyllm/reference/mistral.md)
  function to use Mistral Models on Le Platforme on servers hosted in
  the EU, with rate-limiting and streaming support.

------------------------------------------------------------------------

## Version 0.1.5

### Major Features

- **Message Retrieval Functions**: Added functions to retrieve single
  messages from conversations:
  - [`last_user_message()`](https://edubruell.github.io/tidyllm/reference/get_user_message.md)
    pulls the last message the user sent.
  - [`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)
    gets the assistant reply at a given index of assistant messages.
  - [`get_user_message()`](https://edubruell.github.io/tidyllm/reference/get_user_message.md)
    gets the user message at a given index of user messages.

### Improvements

- **Easier Troubleshooting in API-function**: All API functions now
  support the `.dry_run` argument, allowing users to generate an
  `httr2`-request for easier debugging and inspection.
- **API Function Tests:** Implemented `httptest2`-based tests with mock
  responses for all API functions, covering both basic functionality and
  rate-limiting.

------------------------------------------------------------------------

## Version 0.1.4

### Major Features

- **New Ollama functions**:
  - **Model Download:** Introduced the
    [`ollama_download_model()`](https://edubruell.github.io/tidyllm/reference/ollama_download_model.md)
    function to download models from the Ollama API. It supports a
    streaming mode that provides live progress bar updates on the
    download progress.

### Improvements

- Refactoring of
  [`llm_message()`](https://edubruell.github.io/tidyllm/reference/llm_message.md)

------------------------------------------------------------------------

## Version 0.1.3

### Major Features

- The [`groq()`](https://edubruell.github.io/tidyllm/reference/groq.md)
  function now supports images.
- More complete streaming support across API-functions.

### Breaking Changes

- **Groq Models**: System prompts are no longer sent for Groq models,
  since many models on Groq do not support them and all multimodal
  models on Groq disallow them.

------------------------------------------------------------------------

## Version 0.1.2

### Improvements

- **New unit tests for
  [`llm_message()`](https://edubruell.github.io/tidyllm/reference/llm_message.md)**.
- Improvements in streaming functions.

------------------------------------------------------------------------

## Version 0.1.1

### Major Features

- **JSON Mode**: JSON mode is now more widely supported across all API
  functions, allowing for structured outputs when APIs support them. The
  `.json` argument is now passed only to API functions, specifying how
  the API should respond, and it is not needed anymore in
  [`last_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md).

- **Improved
  [`last_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)
  Behavior**: The behavior of the
  [`last_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)
  function has changed. It now automatically handles JSON replies by
  parsing them into structured data and falling back to raw text in case
  of errors. You can still force raw text replies even for JSON output
  using the `.raw` argument.

### Breaking Changes

- **[`last_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)**:
  The `.json` argument is no longer used, and JSON replies are
  automatically parsed. Use `.raw` to force raw text replies.
