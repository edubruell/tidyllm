devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("codex_cli")

# codex_cli() runs the user's own installed Codex CLI. This suite pays with the
# OPENAI_API_KEY (`.use_api_key = TRUE`), because the dev machine has no ChatGPT
# login. Codex adds about 12,000 input tokens of its own instructions to every
# call, so prompts stay tiny and reasoning stays low.
#
# The whole suite skips on a machine without the CLI or the key.

CX <- function(...) codex_cli(.use_api_key = TRUE, .reasoning_effort = "low", ...)

if (inherits(tryCatch(codex_cli_binary(), error = function(e) e), "condition")) {
  cat("  [skip] codex_cli - the `codex` command was not found\n")
} else if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  cat("  [skip] codex_cli - OPENAI_API_KEY is not set\n")
} else if (!requireNamespace("processx", quietly = TRUE)) {
  cat("  [skip] codex_cli - processx is not installed\n")
} else {

llt_test("codex_cli basic chat", {
  result <- llm_message("Reply with exactly the word: pineapple") |> chat(CX())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
  llt_expect_true(grepl("pineapple", get_reply(result), ignore.case = TRUE),
                  "Codex did not return the requested word")
})

llt_test("codex_cli reports token usage and the session", {
  result <- llm_message("Say OK.") |> chat(CX(.model = "gpt-5.5"))
  meta <- get_metadata(result)
  llt_expect_true(identical(meta$model, "gpt-5.5"), paste("model was", meta$model))
  llt_expect_true(is.numeric(meta$prompt_tokens) && meta$prompt_tokens > 1000,
                  "prompt_tokens should include Codex's own instructions")
  llt_expect_true(is.numeric(meta$completion_tokens) && meta$completion_tokens > 0,
                  "completion_tokens missing")
  specific <- meta$api_specific[[1]]
  llt_expect_true(!is.na(specific$session_id) && nzchar(specific$session_id),
                  "no session id was recorded")
})

llt_test("codex_cli passes the system prompt as developer instructions", {
  result <- llm_message("What is two plus two? One word.",
                        .system_prompt = "Always answer in German.") |>
    chat(CX())
  llt_expect_true(grepl("vier", get_reply(result), ignore.case = TRUE),
                  paste("the system prompt was not followed:", get_reply(result)))
})

llt_test("codex_cli structured output", {
  schema <- tidyllm_schema(capital = "character", population_millions = "numeric")
  result <- llm_message("Give the capital of France and its population in millions.") |>
    chat(CX(.json_schema = schema))
  data <- get_reply_data(result)
  llt_expect_true(identical(data$capital, "Paris"),
                  paste("expected Paris, got:", data$capital))
  llt_expect_true(is.numeric(data$population_millions),
                  "population_millions did not come back as a number")
})

llt_test("codex_cli carries history when stateless", {
  first <- llm_message("Remember the word: marzipan. Reply with just OK.") |> chat(CX())
  second <- first |>
    llm_message("What word did I ask you to remember? One word only.") |>
    chat(CX())
  llt_expect_true(grepl("marzipan", get_reply(second), ignore.case = TRUE),
                  "the flattened history did not reach the model")
})

llt_test("codex_cli resumes its own session when stateful", {
  first <- llm_message("Remember the word: zeppelin. Reply with just OK.") |>
    chat(CX(.stateful = TRUE))
  first_id <- get_metadata(first)$api_specific[[1]]$session_id

  second <- first |>
    llm_message("What word did I ask you to remember? One word only.") |>
    chat(CX(.stateful = TRUE))
  second_id <- get_metadata(second)$api_specific[[1]]$session_id

  llt_expect_true(identical(first_id, second_id),
                  "the second call did not resume the first call's session")
  llt_expect_true(grepl("zeppelin", get_reply(second), ignore.case = TRUE),
                  "the resumed session did not remember the word")
})

llt_test("codex_cli streams every message it later reports as the reply", {
  chunks <- character(0)
  result <- llm_message("Write two short sentences about R.") |>
    chat(CX(), .stream = TRUE)
  llt_expect_reply(result)
  llt_expect_true(isTRUE(get_metadata(result)$stream),
                  "the stream flag was not set on a streamed reply")

  job <- llm_message("Write two short sentences about R.") |>
    send_chat(CX(), .stream = TRUE,
              .on_chunk = function(.text) chunks <<- c(chunks, .text))
  result <- fetch_job(job)
  llt_expect_true(length(chunks) > 0, "the sink was never called")
  llt_expect_true(identical(trimws(chunks[[length(chunks)]]), trimws(get_reply(result))),
                  "the last streamed message is not the final reply")
})

llt_test("codex_cli send_chat does not block, non-streaming", {
  started <- Sys.time()
  job <- llm_message("Reply with exactly: async-ok") |> send_chat(CX())
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  llt_expect_true(elapsed < 2, paste("send_chat() blocked for", round(elapsed, 2), "seconds"))
  result <- fetch_job(job)
  llt_expect_true(grepl("async-ok", get_reply(result), fixed = TRUE),
                  "the background job returned the wrong answer")
})

llt_test("codex_cli cannot read files with tools off", {
  dir <- tempfile("codex_tools_")
  dir.create(dir)
  writeLines("secretvalue42", file.path(dir, "note.txt"))
  old <- setwd(dir)
  result <- tryCatch(
    llm_message("Read note.txt in the current directory and print its content.") |> chat(CX()),
    finally = setwd(old)
  )
  llt_expect_true(!grepl("secretvalue42", get_reply(result), fixed = TRUE),
                  "Codex read a file although its tools were off")
})

llt_test("codex_cli surfaces Codex's error message", {
  msg <- tryCatch({
    llm_message("hi") |> chat(CX(.model = "no-such-model-xyz"))
    NULL
  }, error = function(e) conditionMessage(e))
  llt_expect_true(!is.null(msg) && grepl("no-such-model-xyz", msg, fixed = TRUE),
                  paste("unhelpful error for a bad model:", msg))
})

llt_test("codex_cli dry run hides the key and disables tools", {
  command <- llm_message("Say hello.") |> chat(CX(), .dry_run = TRUE)
  printed <- paste(capture.output(print(command)), collapse = "\n")
  llt_expect_true(!grepl(Sys.getenv("OPENAI_API_KEY"), printed, fixed = TRUE),
                  "the printed command shows the API key")
  llt_expect_true(!grepl(Sys.getenv("OPENAI_API_KEY"), paste(deparse(command), collapse = ""), fixed = TRUE),
                  "the command object holds the API key")
  llt_expect_true("features.shell_tool=false" %in% command$args,
                  "the shell tool was not turned off")
  llt_expect_true("--ephemeral" %in% command$args, "a stateless call should not save a session")

  open <- llm_message("Say hello.") |> chat(CX(.cli_tools = TRUE), .dry_run = TRUE)
  llt_expect_true(!any(grepl("^features\\.", open$args)),
                  ".cli_tools = TRUE still turned features off")
})

llt_report()

}
