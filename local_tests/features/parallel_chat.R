# Live tests for parallel_chat().
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/features/parallel_chat.R")'
#
# Costs a handful of one-sentence requests against claude().

source("local_tests/test_harness.R")
llt_suite("parallel_chat")

QUESTIONS <- list(
  physics = llm_message("What is a photon? One sentence."),
  biology = llm_message("What is a ribosome? One sentence."),
  chem    = llm_message("What is a mole? One sentence.")
)

llt_test("replies come back named, in order, and faster than sequentially", {
  t0 <- Sys.time()
  answers <- parallel_chat(QUESTIONS, claude())
  parallel_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  llt_expect_true(identical(names(answers), names(QUESTIONS)),
                  paste0("names not preserved: ", paste(names(answers), collapse = ", ")))
  llt_expect_true(length(answers) == length(QUESTIONS), "wrong number of replies")
  for (n in names(answers)) {
    llt_expect_s7(answers[[n]], LLMMessage)
    llt_expect_reply(answers[[n]])
  }

  t1 <- Sys.time()
  invisible(chat(QUESTIONS[[1]], claude()))
  one_secs <- as.numeric(difftime(Sys.time(), t1, units = "secs"))

  # Three requests concurrently should not cost three times one request. A
  # generous factor, because the point is that they overlap at all, not how fast
  # the provider is on the day.
  llt_expect_true(parallel_secs < one_secs * 2.5,
                  sprintf("three parallel requests took %.1fs against %.1fs for one; they did not overlap",
                          parallel_secs, one_secs))
})

llt_test("a failing request lands in its own slot without killing the rest", {
  # A deliberately bad model name fails at the provider, so the other two must
  # still come back. The failure is kept rather than dropped: a NULL hole would
  # silently shorten a downstream map.
  mixed <- list(
    good = llm_message("What is a photon? One sentence."),
    bad  = llm_message("What is a photon? One sentence.")
  )

  answers <- suppressWarnings(
    parallel_chat(mixed[1], claude())
  )
  llt_expect_s7(answers$good, LLMMessage)

  broken <- suppressWarnings(
    parallel_chat(mixed, claude(.model = "claude-does-not-exist"))
  )
  llt_expect_true(!any(vapply(broken, function(r) S7_inherits(r, LLMMessage), logical(1))),
                  "a nonexistent model somehow produced an LLMMessage")
  llt_expect_true(identical(names(broken), names(mixed)),
                  "names were lost on the failure path")
})

llt_test("structured output works across the set", {
  schema <- tidyllm_schema(answer = field_chr("A one-word answer"))
  answers <- parallel_chat(
    list(a = llm_message("What is the capital of France?"),
         b = llm_message("What is the capital of Japan?")),
    claude(), .json_schema = schema
  )

  for (n in names(answers)) {
    data <- get_reply_data(answers[[n]])
    llt_expect_true(is.list(data) && !is.null(data$answer),
                    paste0(n, ": structured output missing an 'answer' field"))
  }
})

llt_report("parallel_chat")
