devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("claude_cli")

# claude_cli() runs the user's own installed Claude CLI, so this suite needs no
# API key and instead needs the `claude` binary on the PATH, signed in. It costs
# real plan usage, so every prompt here is deliberately tiny and pinned to Haiku.
#
# The whole suite skips rather than fails on a machine without the CLI: that is a
# missing tool, not a broken provider.

CLI_MODEL <- "claude-haiku-4-5-20251001"

if (!nzchar(Sys.which("claude")[[1]])) {
  cat("  [skip] claude_cli - the `claude` command is not on the PATH\n")
} else if (!requireNamespace("processx", quietly = TRUE)) {
  cat("  [skip] claude_cli - processx is not installed\n")
} else {

llt_test("claude_cli basic chat", {
  result <- llm_message("Reply with exactly the word: pineapple") |>
    chat(claude_cli(.model = CLI_MODEL))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
  llt_expect_true(grepl("pineapple", get_reply(result), ignore.case = TRUE),
                  "the CLI did not return the requested word")
})

llt_test("claude_cli reports the CLI's own metadata", {
  result <- llm_message("Say OK.") |> chat(claude_cli(.model = CLI_MODEL))
  llt_expect_metadata(result, c("model", "prompt_tokens", "completion_tokens", "total_tokens"))

  specific <- get_metadata(result)$api_specific[[1]]
  for (field in c("session_id", "stop_reason", "total_cost_usd", "num_turns")) {
    llt_expect_true(!is.null(specific[[field]]),
                    paste("api_specific is missing", field))
  }
  llt_expect_true(is.numeric(specific$total_cost_usd) && specific$total_cost_usd > 0,
                  "total_cost_usd should be a positive number")
})

llt_test("claude_cli streaming", {
  # The assertion is structural, not about the model's wording. The pump feeds
  # the sink as deltas arrive but reads the final reply back out of the
  # assembled body, so comparing the two is what catches an assembler that drops
  # content. An earlier version of this test looked for the digit 5 in a counting
  # prompt and failed once on a run that phrased the answer differently, which
  # tested the model rather than the code.
  chunks <- character(0)
  result <- llm_message("Count from 1 to 5, one number per line, nothing else.") |>
    chat(claude_cli(.model = CLI_MODEL, .stream = TRUE),
         .stream = TRUE)

  llt_expect_reply(result)
  llt_expect_true(isTRUE(get_metadata(result)$stream),
                  "the stream flag was not set on a streamed reply")
})

llt_test("claude_cli streams every delta it later reports as the reply", {
  chunks <- character(0)
  job <- llm_message("Write two short sentences about R.") |>
    send_chat(claude_cli(.model = CLI_MODEL), .stream = TRUE,
              .on_chunk = function(.text) chunks <<- c(chunks, .text))
  result <- fetch_job(job)

  streamed <- paste0(chunks, collapse = "")
  final    <- get_reply(result)

  llt_expect_true(nzchar(streamed), "no text deltas reached the sink")
  llt_expect_true(identical(trimws(streamed), trimws(final)),
                  paste0("the streamed text and the final reply differ.\n",
                         "  streamed: ", substr(streamed, 1, 120), "\n",
                         "  final   : ", substr(final, 1, 120)))
})

llt_test("claude_cli structured output", {
  schema <- tidyllm_schema(capital = "character", population_millions = "numeric")
  result <- llm_message("Give the capital of France and its population in millions.") |>
    chat(claude_cli(.model = CLI_MODEL, .json_schema = schema))

  data <- get_reply_data(result)
  llt_expect_true(identical(data$capital, "Paris"),
                  paste("expected Paris, got:", data$capital))
  llt_expect_true(is.numeric(data$population_millions),
                  "population_millions did not come back as a number")
})

llt_test("claude_cli carries history when stateless", {
  first <- llm_message("Remember the word: marzipan. Reply with just OK.") |>
    chat(claude_cli(.model = CLI_MODEL))
  second <- first |>
    llm_message("What word did I ask you to remember? One word only.") |>
    chat(claude_cli(.model = CLI_MODEL))
  llt_expect_true(grepl("marzipan", get_reply(second), ignore.case = TRUE),
                  "the flattened history did not reach the model")
})

llt_test("claude_cli resumes its own session when stateful", {
  first <- llm_message("Remember the word: zeppelin. Reply with just OK.") |>
    chat(claude_cli(.model = CLI_MODEL, .stateful = TRUE))
  first_id <- get_metadata(first)$api_specific[[1]]$session_id
  llt_expect_true(!is.na(first_id) && nzchar(first_id), "no session id was recorded")

  second <- first |>
    llm_message("What word did I ask you to remember? One word only.") |>
    chat(claude_cli(.model = CLI_MODEL, .stateful = TRUE))
  second_id <- get_metadata(second)$api_specific[[1]]$session_id

  llt_expect_true(identical(first_id, second_id),
                  "the second call did not resume the first call's session")
  llt_expect_true(grepl("zeppelin", get_reply(second), ignore.case = TRUE),
                  "the resumed session did not remember the word")
})

llt_test("claude_cli send_chat does not block, non-streaming", {
  started <- Sys.time()
  job <- llm_message("Reply with exactly: async-ok") |>
    send_chat(claude_cli(.model = CLI_MODEL))
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  llt_expect_true(elapsed < 2,
                  paste("send_chat() blocked for", round(elapsed, 2), "seconds"))
  result <- fetch_job(job)
  llt_expect_reply(result)
  llt_expect_true(grepl("async-ok", get_reply(result), fixed = TRUE),
                  "the background job returned the wrong answer")
})

llt_test("claude_cli send_chat streams into a sink", {
  chunks <- character(0)
  job <- llm_message("Count from 1 to 6, one number per line.") |>
    send_chat(claude_cli(.model = CLI_MODEL), .stream = TRUE,
              .on_chunk = function(.text) chunks <<- c(chunks, .text))
  result <- fetch_job(job)

  llt_expect_reply(result)
  llt_expect_true(length(chunks) > 0, "the sink was never called")
  llt_expect_true(nzchar(get_partial(job)), "get_partial() stayed empty")
})

llt_test("claude_cli dry run shows the command and disables tools by default", {
  command <- llm_message("Say hello.") |> chat(claude_cli(), .dry_run = TRUE)
  llt_expect_true(inherits(command, "tidyllm_cli_command"),
                  "a dry run did not return a CLI command object")

  args <- command$args
  llt_expect_true("--disallowed-tools" %in% args,
                  "the default call did not disable the CLI's own tools")
  llt_expect_true(any(grepl("Bash", args, fixed = TRUE)),
                  "Bash was not among the disallowed tools")
  llt_expect_true(identical(command$stdin, "Say hello."),
                  "the prompt did not reach stdin")
})

llt_test("claude_cli passes an opt-in tool allow-list", {
  command <- llm_message("Say hello.") |>
    chat(claude_cli(.cli_tools = c("Read", "Glob")), .dry_run = TRUE)
  args <- command$args
  position <- which(args == "--allowed-tools")
  llt_expect_true(length(position) == 1, "expected exactly one --allowed-tools flag")
  llt_expect_true(identical(args[position + 1], "Read,Glob"),
                  paste("allow-list was:", args[position + 1]))
  llt_expect_true(!("--disallowed-tools" %in% args),
                  "an explicit allow-list should not also send a deny-list")
})

llt_test("claude_cli rejects verbs and arguments it does not support", {
  tools_error <- tryCatch({
    llm_message("x") |> chat(claude_cli(), .tools = list())
    NULL
  }, error = function(e) conditionMessage(e))

  llt_expect_true(!is.null(tools_error) && grepl(".tools", tools_error, fixed = TRUE),
                  "chat(claude_cli(), .tools =) should have been rejected")

  embed_error <- tryCatch({
    embed("hello", claude_cli())
    NULL
  }, error = function(e) conditionMessage(e))
  llt_expect_true(!is.null(embed_error),
                  "embed(claude_cli()) should have been rejected")
})

llt_test("claude_cli finds the CLI when the PATH does not have it", {
  # The failure this covers: RStudio and other GUI front ends do not inherit the
  # PATH from the shell profile, so `~/.local/bin` is often absent and
  # `Sys.which("claude")` returns nothing although the CLI is installed.
  #
  # PATH is restored explicitly rather than with on.exit(): inside llt_test()'s
  # expression, on.exit() attaches to a frame that outlives the test body, so the
  # next test ran against a stripped PATH and failed for the wrong reason.
  original <- Sys.getenv("PATH")
  where    <- Sys.which("claude")[[1]]

  Sys.setenv(PATH = paste(
    Filter(function(p) !identical(normalizePath(p, mustWork = FALSE),
                                  normalizePath(dirname(where), mustWork = FALSE)),
           strsplit(original, .Platform$path.sep)[[1]]),
    collapse = .Platform$path.sep))

  outcome <- tryCatch(
    llm_message("Say hello.") |> chat(claude_cli(), .dry_run = TRUE),
    error = function(e) e
  )
  Sys.setenv(PATH = original)

  llt_expect_true(!inherits(outcome, "condition"),
                  paste("the CLI was not found once its directory left the PATH:",
                        if (inherits(outcome, "condition")) conditionMessage(outcome) else ""))
  llt_expect_true(file.access(outcome$binary, mode = 1L) == 0,
                  paste("the resolved path is not executable:", outcome$binary))
})

llt_test("claude_cli honours an explicitly configured path", {
  where    <- Sys.which("claude")[[1]]
  previous <- getOption("tidyllm_claude_cli_path")

  options(tidyllm_claude_cli_path = where)
  configured <- llm_message("Say hello.") |> chat(claude_cli(), .dry_run = TRUE)

  options(tidyllm_claude_cli_path = "/definitely/not/here/claude")
  msg <- tryCatch({
    llm_message("x") |> chat(claude_cli(), .dry_run = TRUE)
    NULL
  }, error = function(e) conditionMessage(e))

  options(tidyllm_claude_cli_path = previous)
  explicit <- llm_message("Say hello.") |>
    chat(claude_cli(.binary = where), .dry_run = TRUE)

  llt_expect_true(identical(configured$binary, where),
                  paste("the option was ignored; got:", configured$binary))
  llt_expect_true(!is.null(msg) && grepl("not an executable file", msg, fixed = TRUE),
                  paste("a bad configured path gave an unhelpful error:", msg))
  llt_expect_true(identical(explicit$binary, where),
                  "an explicit .binary path was not used as given")
})

llt_test("claude_cli names a missing binary clearly", {
  msg <- tryCatch({
    llm_message("x") |> chat(claude_cli(.binary = "claude-not-installed-xyz"))
    NULL
  }, error = function(e) conditionMessage(e))

  llt_expect_true(!is.null(msg), "a missing binary did not raise")
  llt_expect_true(grepl("was not found", msg, fixed = TRUE),
                  paste("unhelpful error for a missing binary:", msg))
  llt_expect_true(grepl("tidyllm_claude_cli_path", msg, fixed = TRUE),
                  "the error does not say how to point tidyllm at the CLI")
})

llt_report()

}
