# Structural tests for send_chat() and the job handle.
#
# The driver itself needs a real connection and lives in
# local_tests/features/async_chat_replay.R (offline, webfakes) and
# async_chat_live.R. What is checked here is everything that holds without one:
# argument validation, dispatch, and the fact that every provider with a builder
# is reachable.

test_that("send_chat validates its inputs before touching the network", {
  skip_if_not_installed("later")

  expect_error(send_chat("not a message", claude()), "LLMMessage")
  expect_error(send_chat(llm_message("hi"), NULL), "provider")
  expect_error(send_chat(llm_message("hi"), claude(), .on_chunk = "not a function"),
               "function of one argument")
  # `.dry_run` would have to return a request rather than a job, so it is
  # refused with a pointer at the thing that does return one.
  expect_error(send_chat(llm_message("hi"), claude(), .dry_run = TRUE), "chat\\(.dry_run")
})

test_that("every provider with a builder registers it as a build action", {
  # `send_chat()` reaches the builder through the provider's own registration,
  # so a provider that gains a builder but not the `build =` entry would be
  # silently unavailable to it, with an error that blames the provider.
  providers <- c("claude", "openai", "gemini", "ollama", "groq", "mistral",
                 "deepseek", "openrouter", "llamacpp", "perplexity",
                 "chat_completions", "azure_openai")

  for (p in providers) {
    meta <- do.call(get(p), list(.called_from = "metadata"))
    expect_true("build" %in% names(meta$supported_args), label = paste0(p, " build action"))
    # The builder and the chat function take the same arguments, so anything
    # send_chat() forwards is accepted by exactly the providers chat() accepts
    # it from.
    expect_identical(meta$supported_args$build, meta$supported_args$chat,
                     label = paste0(p, " build/chat arguments"))
  }
})

test_that("the job accessors reject anything that is not a chat job", {
  for (f in list(get_partial, cancel_job)) {
    expect_error(f(list(1, 2)), "tidyllm_chat_job")
  }
})

test_that("check_job and fetch_job dispatch on the job's class", {
  # Three kinds of job with three different meanings of "check", now reached by
  # one mechanism. Batch objects carry an attribute rather than a class, which
  # is why the default method still looks for it.
  expect_true(is.function(utils::getS3method("check_job", "tidyllm_chat_job")))
  expect_true(is.function(utils::getS3method("check_job", "tidyllm_research_job")))
  expect_true(is.function(utils::getS3method("fetch_job", "tidyllm_chat_job")))
  expect_true(is.function(utils::getS3method("fetch_job", "tidyllm_research_job")))

  # A batch object reaches check_batch(), which then asks for the provider it
  # needs. Getting that far is the assertion: the default method recognised it.
  batch <- structure(list(), batch_id = "batch_1")
  expect_error(check_job(batch), "check_batch")

  expect_error(check_job(42), "send_chat")
  expect_error(fetch_job(42), "send_chat")
})

test_that("parallel_chat validates its inputs and refuses what it cannot do", {
  msgs <- list(llm_message("a"), llm_message("b"))

  expect_error(parallel_chat(list(), claude()), "non-empty list")
  expect_error(parallel_chat(list("a"), claude()), "LLMMessage")
  expect_error(parallel_chat(msgs, NULL), "provider")

  # Refused by name rather than left out of the signature, so the message says
  # why and points at the function that does support them.
  expect_error(parallel_chat(msgs, claude(), .stream = TRUE), "send_chat")
  expect_error(parallel_chat(msgs, claude(), .tools = list()), "send_chat")
})
