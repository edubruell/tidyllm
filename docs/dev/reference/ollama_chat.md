# Interact with local AI models via the Ollama API

Interact with local AI models via the Ollama API

## Usage

``` r
ollama_chat(
  .llm,
  .model = "qwen3.5:4b",
  .stream = FALSE,
  .seed = NULL,
  .json_schema = NULL,
  .temperature = NULL,
  .num_ctx = 2048,
  .num_predict = NULL,
  .top_k = NULL,
  .top_p = NULL,
  .min_p = NULL,
  .mirostat = NULL,
  .mirostat_eta = NULL,
  .mirostat_tau = NULL,
  .repeat_last_n = NULL,
  .repeat_penalty = NULL,
  .tools = NULL,
  .max_tool_rounds = 10,
  .tfs_z = NULL,
  .stop = NULL,
  .think = NULL,
  .ollama_server = "http://localhost:11434",
  .timeout = 120,
  .keep_alive = NULL,
  .dry_run = FALSE
)
```

## Arguments

- .llm:

  An LLMMessage object containing the conversation history and system
  prompt.

- .model:

  Character string specifying the Ollama model to use (default:
  "qwen3-vl")

- .stream:

  Logical; whether to stream the response (default: FALSE)

- .seed:

  Integer; seed for reproducible generation (default: NULL)

- .json_schema:

  A JSON schema object as R list to enforce the output structure
  (default: NULL)

- .temperature:

  Float between 0-2; controls randomness in responses (default: NULL)

- .num_ctx:

  Integer; sets the context window size (default: 2048)

- .num_predict:

  Integer; maximum number of tokens to predict (default: NULL)

- .top_k:

  Integer; controls diversity by limiting top tokens considered
  (default: NULL)

- .top_p:

  Float between 0-1; nucleus sampling threshold (default: NULL)

- .min_p:

  Float between 0-1; minimum probability threshold (default: NULL)

- .mirostat:

  Integer (0,1,2); enables Mirostat sampling algorithm (default: NULL)

- .mirostat_eta:

  Float; Mirostat learning rate (default: NULL)

- .mirostat_tau:

  Float; Mirostat target entropy (default: NULL)

- .repeat_last_n:

  Integer; tokens to look back for repetition (default: NULL)

- .repeat_penalty:

  Float; penalty for repeated tokens (default: NULL)

- .tools:

  Either a single TOOL object or a list of TOOL objects representing the
  available functions for tool calls.

- .max_tool_rounds:

  Integer; maximum number of tool use iterations for multi-turn tool
  calling (default: 10). Set to 1 for single-round tool use, or higher
  for multi-turn agentic loops.

- .tfs_z:

  Float; tail free sampling parameter (default: NULL)

- .stop:

  Character; custom stop sequence(s) (default: NULL)

- .think:

  Logical or character; controls thinking mode for supported models like
  Qwen3. Use FALSE to disable, TRUE to enable, or "high"/"medium"/"low"
  to set effort level (default: NULL - model default)

- .ollama_server:

  String; Ollama API endpoint (default: "http://localhost:11434")

- .timeout:

  Integer; API request timeout in seconds (default: 120)

- .keep_alive:

  Character; How long should the ollama model be kept in memory after
  request (default: NULL - 5 Minutes)

- .dry_run:

  Logical; if TRUE, returns request object without execution (default:
  FALSE)

## Value

A new LLMMessage object containing the original messages plus the
model's response

## Details

The function provides extensive control over the generation process
through various parameters:

- Temperature (0-2): Higher values increase creativity, lower values
  make responses more focused

- Top-k/Top-p: Control diversity of generated text

- Mirostat: Advanced sampling algorithm for maintaining consistent
  complexity

- Repeat penalties: Prevent repetitive text

- Context window: Control how much previous conversation is considered

## Examples

``` r
if (FALSE) { # \dontrun{
llm_message("user", "Hello, how are you?")
response <- ollama_chat(llm, .model = "gemma2", .temperature = 0.7)

# With custom parameters
response <- ollama_chat(
  llm,
  .model = "llama2",
  .temperature = 0.8,
  .top_p = 0.9,
  .num_ctx = 4096
)
} # }
```
