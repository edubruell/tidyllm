devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("vibe_cli")

# vibe_cli() runs the user's own installed Mistral Vibe CLI with MISTRAL_API_KEY.
# Vibe reports no token counts, so the metadata checks cover only what it does
# report. The whole suite skips on a machine without the CLI or the key.

if (inherits(tryCatch(vibe_cli_binary(), error = function(e) e), "condition")) {
  cat("  [skip] vibe_cli - the `vibe` command was not found\n")
} else if (!nzchar(Sys.getenv("MISTRAL_API_KEY"))) {
  cat("  [skip] vibe_cli - MISTRAL_API_KEY is not set\n")
} else if (!requireNamespace("processx", quietly = TRUE)) {
  cat("  [skip] vibe_cli - processx is not installed\n")
} else {

llt_test("vibe_cli basic chat", {
  result <- llm_message("Reply with exactly the word: pineapple") |> chat(vibe_cli())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
  llt_expect_true(grepl("pineapple", get_reply(result), ignore.case = TRUE),
                  "Vibe did not return the requested word")
  specific <- get_metadata(result)$api_specific[[1]]
  llt_expect_true(!is.na(specific$session_id) && nzchar(specific$session_id),
                  "no session id was recorded")
})

llt_test("vibe_cli uses the requested model", {
  result <- llm_message("Say OK.") |> chat(vibe_cli(.model = "mistral-large-4"))
  llt_expect_reply(result)
  llt_expect_true(identical(get_metadata(result)$model, "mistral-large-4"),
                  "the metadata does not name the requested model")

  msg <- tryCatch({
    llm_message("Say OK.") |> chat(vibe_cli(.model = "no-such-model-xyz"))
    NULL
  }, error = function(e) conditionMessage(e))
  llt_expect_true(!is.null(msg),
                  "a model id that does not exist did not fail, so .model never reached Vibe")
})

llt_test("vibe_cli puts the system prompt in front of the prompt", {
  result <- llm_message("What is two plus two? One word.",
                        .system_prompt = "Always answer in German.") |>
    chat(vibe_cli())
  llt_expect_true(grepl("vier", get_reply(result), ignore.case = TRUE),
                  paste("the system prompt was not followed:", get_reply(result)))
})

llt_test("vibe_cli carries history when stateless", {
  first <- llm_message("Remember the word: marzipan. Reply with just OK.") |> chat(vibe_cli())
  second <- first |>
    llm_message("What word did I ask you to remember? One word only.") |>
    chat(vibe_cli())
  llt_expect_true(grepl("marzipan", get_reply(second), ignore.case = TRUE),
                  "the flattened history did not reach the model")
})

llt_test("vibe_cli resumes its own session when stateful", {
  first <- llm_message("Remember the word: zeppelin. Reply with just OK.") |>
    chat(vibe_cli(.stateful = TRUE))
  first_id <- get_metadata(first)$api_specific[[1]]$session_id

  second <- first |>
    llm_message("What word did I ask you to remember? One word only.") |>
    chat(vibe_cli(.stateful = TRUE))
  second_id <- get_metadata(second)$api_specific[[1]]$session_id

  llt_expect_true(identical(first_id, second_id),
                  "the second call did not resume the first call's session")
  llt_expect_true(grepl("zeppelin", get_reply(second), ignore.case = TRUE),
                  "the resumed session did not remember the word")
})

llt_test("vibe_cli send_chat does not block", {
  started <- Sys.time()
  job <- llm_message("Reply with exactly: async-ok") |> send_chat(vibe_cli())
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  llt_expect_true(elapsed < 2, paste("send_chat() blocked for", round(elapsed, 2), "seconds"))
  result <- fetch_job(job)
  llt_expect_true(grepl("async-ok", get_reply(result), fixed = TRUE),
                  "the background job returned the wrong answer")
})

llt_test("vibe_cli cannot read files with tools off", {
  dir <- tempfile("vibe_tools_")
  dir.create(dir)
  writeLines("secretvalue42", file.path(dir, "note.txt"))
  old <- setwd(dir)
  result <- tryCatch(
    llm_message("Read note.txt in the current directory and print its content.") |>
      chat(vibe_cli(.max_turns = 4)),
    error = function(e) NULL,
    finally = setwd(old)
  )
  reply <- if (is.null(result)) "" else get_reply(result)
  llt_expect_true(!grepl("secretvalue42", reply, fixed = TRUE),
                  "Vibe read a file although its tools were off")
})

llt_test("vibe_cli shows Vibe's error for a bad key", {
  old <- Sys.getenv("MISTRAL_API_KEY")
  Sys.setenv(MISTRAL_API_KEY = "not-a-key")
  msg <- tryCatch({
    llm_message("hi") |> chat(vibe_cli())
    NULL
  }, error = function(e) conditionMessage(e), finally = Sys.setenv(MISTRAL_API_KEY = old))
  llt_expect_true(!is.null(msg) && grepl("Invalid API key", msg, fixed = TRUE),
                  paste("unhelpful error for a bad key:", msg))
})

llt_test("vibe_cli answers a streaming request in one piece and shows the command on a dry run", {
  result <- llm_message("Reply with exactly the word: tangerine") |>
    chat(vibe_cli(), .stream = TRUE)
  llt_expect_true(grepl("tangerine", get_reply(result), ignore.case = TRUE),
                  "a .stream = TRUE call did not return the reply")

  command <- llm_message("Say hello.") |> chat(vibe_cli(), .dry_run = TRUE)
  llt_expect_true(all(c("--agent", "ask") %in% command$args),
                  "the default call does not use the ask-first agent")
  llt_expect_true(identical(command$stdin, "Say hello."), "the prompt did not reach stdin")
})

llt_report()

}
