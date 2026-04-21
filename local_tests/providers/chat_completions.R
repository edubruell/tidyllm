devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("chat_completions")

# Tests the generic chat_completions() provider against the OpenAI endpoint
# (any OAI-compatible endpoint works here)

llt_test("chat_completions basic chat via OpenAI endpoint", {
  result <- llm_message("Say hello in one word.") |>
    chat(chat_completions(
      .api_url        = "https://api.openai.com/v1/chat/completions",
      .api_key_env_var = "OPENAI_API_KEY",
      .model          = "gpt-4o-mini"
    ))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("chat_completions streaming via OpenAI endpoint", {
  result <- llm_message("Count to 3.") |>
    chat(chat_completions(
      .api_url        = "https://api.openai.com/v1/chat/completions",
      .api_key_env_var = "OPENAI_API_KEY",
      .model          = "gpt-4o-mini",
      .stream         = TRUE
    ))
  llt_expect_reply(result)
})

llt_report()
