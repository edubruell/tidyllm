# Send a Batch of Messages to Ollama API

This function creates and submits a batch of messages to the Ollama API
Contrary to other batch functions, this functions waits for the batch to
finish and receives requests. The advantage compared to sending single
messages via
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md) is
that Ollama handles large parallel requests quicker than many individual
chat requests.

## Usage

``` r
send_ollama_batch(
  .llms,
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
  .tfs_z = NULL,
  .stop = NULL,
  .ollama_server = "http://localhost:11434",
  .timeout = 120,
  .keep_alive = NULL,
  .dry_run = FALSE
)
```

## Arguments

- .llms:

  A list of LLMMessage objects containing conversation histories.

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

- .tfs_z:

  Float; tail free sampling parameter (default: NULL)

- .stop:

  Character; custom stop sequence(s) (default: NULL)

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

A list of updated `LLMMessage` objects, each with the assistant's
response added if successful.

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
