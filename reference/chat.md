# Chat with a Language Model

The `chat()` function sends a message to a language model via a
specified provider and returns the response. It routes the provided
`LLMMessage` object to the appropriate provider-specific chat function,
while allowing for the specification of common arguments applicable
across different providers.

## Usage

``` r
chat(
  .llm,
  .provider = getOption("tidyllm_chat_default"),
  .dry_run = NULL,
  .stream = NULL,
  .temperature = NULL,
  .timeout = NULL,
  .top_p = NULL,
  .max_tries = NULL,
  .model = NULL,
  .verbose = NULL,
  .json_schema = NULL,
  .tools = NULL,
  .max_tool_rounds = NULL,
  .seed = NULL,
  .stop = NULL,
  .frequency_penalty = NULL,
  .presence_penalty = NULL
)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the message or conversation history
  to send to the language model.

- .provider:

  A function or function call specifying the language model provider and
  any additional parameters. This should be a call to a provider
  function like
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
  [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
  etc. You can also set a default provider function via the
  `tidyllm_chat_default` option.

- .dry_run:

  Logical; if `TRUE`, simulates the request without sending it to the
  provider. Useful for testing.

- .stream:

  Logical; if `TRUE`, streams the response from the provider in
  real-time.

- .temperature:

  Numeric; controls the randomness of the model's output (0 =
  deterministic).

- .timeout:

  Numeric; the maximum time (in seconds) to wait for a response.

- .top_p:

  Numeric; nucleus sampling parameter, which limits the sampling to the
  top cumulative probability `p`.

- .max_tries:

  Integer; the maximum number of retries for failed requests.

- .model:

  Character; the model identifier to use (e.g., `"gpt-4"`).

- .verbose:

  Logical; if `TRUE`, prints additional information about the request
  and response.

- .json_schema:

  List; A JSON schema object as R list to enforce the output structure

- .tools:

  Either a single TOOL object or a list of TOOL objects representing the
  available functions for tool calls.

- .max_tool_rounds:

  Integer; the maximum number of tool use iterations for multi-turn tool
  calling (default varies by provider).

- .seed:

  Integer; sets a random seed for reproducibility.

- .stop:

  Character vector; specifies sequences where the model should stop
  generating further tokens.

- .frequency_penalty:

  Numeric; adjusts the likelihood of repeating tokens (positive values
  decrease repetition).

- .presence_penalty:

  Numeric; adjusts the likelihood of introducing new tokens (positive
  values encourage novelty).

## Value

An updated `LLMMessage` object containing the response from the language
model.

## Details

The `chat()` function provides a unified interface for interacting with
different language model providers. Common arguments such as
`.temperature`, `.model`, and `.stream` are supported by most providers
and can be passed directly to `chat()`. If a provider does not support a
particular argument, an error will be raised.

Advanced provider-specific configurations can be accessed via the
provider functions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with OpenAI provider
llm_message("Hello World") |>
   chat(ollama(.ollama_server = "https://my-ollama-server.de"),.model="mixtral")
   
   chat(mistral,.model="mixtral")

# Use streaming with Claude provider
llm_message("Tell me a story") |>
   chat(claude(),.stream=TRUE)
} # }
```
