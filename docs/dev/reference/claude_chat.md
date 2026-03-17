# Interact with Claude AI models via the Anthropic API

Interact with Claude AI models via the Anthropic API

## Usage

``` r
claude_chat(
  .llm,
  .model = "claude-sonnet-4-6",
  .max_tokens = 2048,
  .temperature = NULL,
  .top_k = NULL,
  .top_p = NULL,
  .metadata = NULL,
  .stop_sequences = NULL,
  .tools = NULL,
  .json_schema = NULL,
  .file_ids = NULL,
  .api_url = "https://api.anthropic.com/",
  .verbose = FALSE,
  .max_tries = 3,
  .timeout = 60,
  .stream = FALSE,
  .dry_run = FALSE,
  .thinking = FALSE,
  .thinking_budget = 1024,
  .max_tool_rounds = 10
)
```

## Arguments

- .llm:

  An LLMMessage object containing the conversation history and system
  prompt.

- .model:

  Character string specifying the Claude model version (default:
  "claude-sonnet-4-6").

- .max_tokens:

  Integer specifying the maximum number of tokens in the response
  (default: 1024).

- .temperature:

  Numeric between 0 and 1 controlling response randomness.

- .top_k:

  Integer controlling diversity by limiting the top K tokens.

- .top_p:

  Numeric between 0 and 1 for nucleus sampling.

- .metadata:

  List of additional metadata to include with the request.

- .stop_sequences:

  Character vector of sequences that will halt response generation.

- .tools:

  List of additional tools or functions the model can use.

- .json_schema:

  A schema to enforce an output structure

- .file_ids:

  Character; A vector of file IDs for files that were uploaded to
  Anthropics Servers

- .api_url:

  Base URL for the Anthropic API (default:
  "https://api.anthropic.com/").

- .verbose:

  Logical; if TRUE, displays additional information about the API call
  (default: FALSE).

- .max_tries:

  Maximum retries to peform request

- .timeout:

  Integer specifying the request timeout in seconds (default: 60).

- .stream:

  Logical; if TRUE, streams the response piece by piece (default:
  FALSE).

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it (default: FALSE).

- .thinking:

  Logical; if TRUE, enables Claude's thinking mode for complex reasoning
  tasks (default: FALSE).

- .thinking_budget:

  Integer specifying the maximum tokens Claude can spend on thinking
  (default: 1024). Must be at least 1024.

- .max_tool_rounds:

  Integer specifying the maximum number of tool use iterations (default:
  10). Set to 1 for single-round tool use, or higher for multi-turn
  agentic loops.

## Value

A new LLMMessage object containing the original messages plus Claude's
response.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
msg <- llm_message("What is R programming?")
result <- claude_chat(msg)

# With custom parameters
result2 <- claude_chat(msg, 
                 .temperature = 0.7, 
                 .max_tokens = 1000)
} # }
```
