devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("llamacpp")

# Requires: llama-server running on http://localhost:8080
# Start with: llama-server -m ~/models/Qwen3-0.6B-Q8_0.gguf -c 8192 --port 8080
#
# NOTE: Qwen3 models use extended "thinking" by default, consuming tokens before
# outputting a response. Tests pass .thinking = FALSE and use >=256 tokens
# to reliably get non-empty replies from the 0.6B model.

# ── Health ────────────────────────────────────────────────────────────────────

llt_test("llamacpp_health returns ok", {
  h <- llamacpp_health()
  llt_expect_true("status" %in% names(h), "health response should have 'status'")
  llt_expect_true(h$status == "ok", paste0("expected ok, got: ", h$status))
})

# ── list_models ───────────────────────────────────────────────────────────────

llt_test("llamacpp_list_models returns tibble with loaded model", {
  models <- llamacpp_list_models()
  llt_expect_true(tibble::is_tibble(models), "list_models() should return a tibble")
  llt_expect_true(nrow(models) >= 1, "should have at least one model loaded")
  llt_expect_true("id" %in% names(models), "should have 'id' column")
})

llt_test("list_models via verb returns tibble", {
  models <- list_models(llamacpp())
  llt_expect_true(tibble::is_tibble(models), "list_models() should return a tibble")
})

# ── Basic chat ────────────────────────────────────────────────────────────────

llt_test("basic chat returns LLMMessage", {
  result <- llm_message("Say hello in one word.") |>
    chat(llamacpp(.max_tokens = 512, .thinking = FALSE))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("get_metadata returns token counts", {
  result <- llm_message("Hello") |>
    chat(llamacpp(.max_tokens = 256, .thinking = FALSE))
  llt_expect_metadata(result, c("prompt_tokens", "completion_tokens"))
})

llt_test("streaming returns non-empty reply", {
  result <- llm_message("Count to 3.") |>
    chat(llamacpp(.stream = TRUE, .max_tokens = 512, .thinking = FALSE))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Multi-turn ────────────────────────────────────────────────────────────────

llt_test("multi-turn conversation works", {
  result <- llm_message("My favourite colour is blue. Just say 'got it'.") |>
    chat(llamacpp(.max_tokens = 256, .thinking = FALSE)) |>
    llm_message("What is my favourite colour?") |>
    chat(llamacpp(.max_tokens = 256, .thinking = FALSE))
  llt_expect_reply(result)
  reply <- get_reply(result)
  llt_expect_true(grepl("blue", reply, ignore.case = TRUE), "Should remember favourite colour")
})

# ── Structured output via JSON schema ─────────────────────────────────────────

llt_test("json_schema structured output returns parseable JSON", {
  schema <- tidyllm_schema(
    person_name = field_chr("Person's first name"),
    age         = field_dbl("Person's age as a number")
  )
  result <- llm_message("Extract: John is 30 years old.") |>
    chat(llamacpp(.json_schema = schema, .max_tokens = 512, .thinking = FALSE))
  llt_expect_reply(result)
  data <- get_reply_data(result)
  llt_expect_true(is.list(data), "get_reply_data should return a list")
  llt_expect_true("person_name" %in% names(data), "should have 'person_name' field")
})

# ── BNF grammar constraint ─────────────────────────────────────────────────────
# NOTE: Grammar-constrained output works best with non-thinking models or larger
# models (>=7B). With Qwen3-0.6B, the model spends all tokens on reasoning before
# producing constrained visible output. The dry_run test verifies the grammar
# parameter is correctly transmitted to the server.

llt_test(".grammar is transmitted in request body (dry_run)", {
  req <- llm_message("Capital of France?") |>
    chat(llamacpp(.grammar = "root ::= [A-Za-z]+", .max_tokens = 64, .dry_run = TRUE))
  body_json <- jsonlite::toJSON(req$body$data, auto_unbox = TRUE)
  llt_expect_true(grepl("grammar", body_json), "request body should contain 'grammar' field")
  llt_expect_true(grepl("A-Za-z", body_json, fixed = TRUE), "grammar value should be preserved in body")
})

# ── Logprobs ──────────────────────────────────────────────────────────────────

llt_test("logprobs are returned and parseable via get_logprobs()", {
  result <- llm_message("Say yes.") |>
    chat(llamacpp(.logprobs = TRUE, .top_logprobs = 3, .max_tokens = 256, .thinking = FALSE))
  llt_expect_reply(result)
  lp <- get_logprobs(result)
  llt_expect_true(nrow(lp) > 0, "get_logprobs() should return at least one row")
  llt_expect_true("token" %in% names(lp), "logprobs tibble should have 'token' column")
  llt_expect_true("logprob" %in% names(lp), "logprobs tibble should have 'logprob' column")
})

# ── Tool use ──────────────────────────────────────────────────────────────────

llt_test("single tool call returns reply", {
  get_weather <- function(location) paste0("Weather in ", location, ": Sunny, 22C")
  weather_tool <- tidyllm_tool(get_weather, "Get weather for a location",
                               location = field_chr("City name"))
  # Qwen3 thinking mode is incompatible with tool-call prefill on this server version;
  # pass .thinking = FALSE explicitly. On servers/models that support it, omit this.
  result <- llm_message("What's the weather in Paris?") |>
    chat(llamacpp(.max_tokens = 512, .thinking = FALSE), .tools = weather_tool)
  llt_expect_reply(result)
})

# ── Model management (no server needed) ───────────────────────────────────────

llt_test("llamacpp_list_local_models finds downloaded model", {
  models <- llamacpp_list_local_models("~/models")
  llt_expect_true(tibble::is_tibble(models), "should return a tibble")
  llt_expect_true(nrow(models) >= 1, "should find at least one .gguf file")
  llt_expect_true("filename" %in% names(models), "should have 'filename' column")
  llt_expect_true("size_gb" %in% names(models), "should have 'size_gb' column")
})

llt_test("list_hf_gguf_files returns tibble with gguf files", {
  files <- list_hf_gguf_files("Qwen/Qwen3-0.6B-GGUF")
  llt_expect_true(tibble::is_tibble(files), "should return a tibble")
  llt_expect_true(nrow(files) >= 1, "should find at least one GGUF file")
  llt_expect_true(all(c("filename", "size_gb", "url") %in% names(files)),
                  "should have filename, size_gb, url columns")
  llt_expect_true(all(grepl("\\.gguf$", files$filename, ignore.case = TRUE)),
                  "all files should have .gguf extension")
})

# ── dry_run ───────────────────────────────────────────────────────────────────

llt_test("dry_run returns httr2 request with grammar in body", {
  req <- llm_message("test") |>
    chat(llamacpp(.grammar = "root ::= [0-9]+", .dry_run = TRUE))
  llt_expect_true(inherits(req, "httr2_request"), "dry_run should return httr2_request")
  body <- req$body$data |> jsonlite::toJSON(auto_unbox = TRUE) |> as.character() |>
    (\(x) if (startsWith(x, '"')) jsonlite::fromJSON(x) else jsonlite::fromJSON(x))()
  llt_expect_true(!is.null(body$grammar), "request body should contain 'grammar' field")
})

# ── Embeddings ────────────────────────────────────────────────────────────────
# Requires: separate embedding server on port 8081
# Start with: llama-server -m ~/models/Qwen3-Embedding-0.6B-Q8_0.gguf -c 512 --port 8081 --embeddings

llt_test("embed returns tibble with 1024-dim embeddings", {
  result <- embed(c("Hello world", "Goodbye world"),
                  llamacpp(.server = "http://localhost:8081"))
  llt_expect_true(tibble::is_tibble(result), "embed() should return a tibble")
  llt_expect_true(nrow(result) == 2, "Should have 2 rows")
  llt_expect_true("embeddings" %in% names(result), "Should have embeddings column")
  llt_expect_true(length(result$embeddings[[1]]) == 1024,
                  "Qwen3-Embedding-0.6B produces 1024-dim vectors")
})

llt_test("embed similarity: similar texts score higher than dissimilar", {
  result <- embed(c("The cat sat on the mat", "A feline rested on a rug", "Quantum physics"),
                  llamacpp(.server = "http://localhost:8081"))
  e1 <- result$embeddings[[1]]
  e2 <- result$embeddings[[2]]
  e3 <- result$embeddings[[3]]
  cosine <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
  sim_related   <- cosine(e1, e2)
  sim_unrelated <- cosine(e1, e3)
  llt_expect_true(sim_related > sim_unrelated,
                  paste0("Similar texts (", round(sim_related, 3),
                         ") should score higher than dissimilar (", round(sim_unrelated, 3), ")"))
})

llt_report()
