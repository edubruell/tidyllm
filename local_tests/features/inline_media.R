# local_tests/features/inline_media.R
# Cross-provider inline media tests for the new .media= interface.
#
# Tests:
#   - Multi-image (two images in one message) with Claude and OpenAI
#   - pdf_file(.text_extract=TRUE) text path with Claude, OpenAI, Gemini
#   - pdf_file() binary path with Gemini (native PDF support)
#   - pdf_file() binary-first with text fallback on Claude
#   - audio_file() with Gemini (native audio)
#   - audio_file() via OpenRouter (routed to Gemini-backed model)
#   - img() single inline image with Claude, OpenAI, Gemini

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("inline_media")

# ── Inline single image (img()) ───────────────────────────────────────────────

llt_test("img() inline image with Claude: identifies hotdog", {
  result <- llm_message("What food is in this image? One word.",
                        .media = img("local_tests/media/hotdog.jpg")) |>
    chat(claude())
  reply <- tolower(get_reply(result))
  llt_expect_true(
    grepl("hotdog|hot dog|sausage|frankfurter|bun", reply),
    paste("Claude img(): expected hotdog-related word, got:", reply)
  )
})

llt_test("img() inline image with OpenAI Responses API: identifies hotdog", {
  result <- llm_message("What food is in this image? One word.",
                        .media = img("local_tests/media/hotdog.jpg")) |>
    chat(openai())
  reply <- tolower(get_reply(result))
  llt_expect_true(
    grepl("hotdog|hot dog|sausage|frankfurter|bun", reply),
    paste("OpenAI img(): expected hotdog-related word, got:", reply)
  )
})

llt_test("img() inline image with Gemini: identifies hotdog", {
  result <- llm_message("What food is in this image? One word.",
                        .media = img("local_tests/media/hotdog.jpg")) |>
    chat(gemini())
  reply <- tolower(get_reply(result))
  llt_expect_true(
    grepl("hotdog|hot dog|sausage|frankfurter|bun", reply),
    paste("Gemini img(): expected hotdog-related word, got:", reply)
  )
})

# ── Multi-image (two images in one message) ───────────────────────────────────

llt_test("multi-image with Claude: two images, identifies both", {
  result <- llm_message(
    "There are two images. Describe each in one word, comma separated.",
    .media = list(
      img("local_tests/media/hotdog.jpg"),
      img("local_tests/media/dog.jpg")
    )
  ) |> chat(claude())
  reply <- tolower(get_reply(result))
  llt_expect_true(nzchar(reply), "Should get non-empty reply for two images")
})

llt_test("multi-image with OpenAI Responses API: two images", {
  result <- llm_message(
    "There are two images. Describe each in one word, comma separated.",
    .media = list(
      img("local_tests/media/hotdog.jpg"),
      img("local_tests/media/dog.jpg")
    )
  ) |> chat(openai())
  reply <- tolower(get_reply(result))
  llt_expect_true(nzchar(reply), "Should get non-empty reply for two images")
})

# ── pdf_file() text-extract path ─────────────────────────────────────────────

llt_test("pdf_file(.text_extract=TRUE) with Claude: model can answer about content", {
  pdf <- pdf_file("local_tests/media/ssrn-4983498.pdf", .text_extract = TRUE)
  result <- llm_message("What is the title of this document? Give the title only.",
                        .media = pdf) |>
    chat(claude())
  llt_expect_reply(result)
})

llt_test("pdf_file(.text_extract=TRUE) with OpenAI: model can answer about content", {
  pdf <- pdf_file("local_tests/media/ssrn-4983498.pdf", .text_extract = TRUE)
  result <- llm_message("What is the title of this document? Give the title only.",
                        .media = pdf) |>
    chat(openai())
  llt_expect_reply(result)
})

llt_test("pdf_file(.text_extract=TRUE) with Gemini: model can answer about content", {
  pdf <- pdf_file("local_tests/media/ssrn-4983498.pdf", .text_extract = TRUE)
  result <- llm_message("What is the title of this document? Give the title only.",
                        .media = pdf) |>
    chat(gemini())
  llt_expect_reply(result)
})

# ── pdf_file() binary path (provider-native) ─────────────────────────────────

llt_test("pdf_file() binary path with Gemini: native PDF support", {
  pdf <- pdf_file("local_tests/media/ssrn-4983498.pdf")
  result <- llm_message("What is the title of this document? Give the title only.",
                        .media = pdf) |>
    chat(gemini())
  llt_expect_reply(result)
})

llt_test("pdf_file() binary-first falls back to text on Claude", {
  pdf <- pdf_file("local_tests/media/ssrn-4983498.pdf")
  result <- llm_message("What is the title of this document? Give the title only.",
                        .media = pdf) |>
    chat(claude())
  llt_expect_reply(result)
})

# ── audio_file() ─────────────────────────────────────────────────────────────

llt_test("audio_file() with Gemini: model can describe audio content", {
  audio <- audio_file("local_tests/media/example.mp3")
  result <- llm_message("Describe this audio clip in one sentence.", .media = audio) |>
    chat(gemini())
  llt_expect_reply(result)
})

llt_test("audio_file() via OpenRouter routed to Gemini: model can describe audio", {
  audio <- audio_file("local_tests/media/example.mp3")
  result <- llm_message("Describe this audio clip in one sentence.", .media = audio) |>
    chat(openrouter(.model = "google/gemini-2.5-flash"))
  llt_expect_reply(result)
})

# ── video_file() ──────────────────────────────────────────────────────────────

llt_test("video_file() with Gemini: model can describe video content", {
  video <- video_file("local_tests/media/the_thinking_machine_1960s.mp4")
  result <- llm_message("Describe what this video is about in one sentence.", .media = video) |>
    chat(gemini())
  llt_expect_reply(result)
})

llt_test("video_file() via OpenRouter routed to Gemini: model can describe video", {
  video <- video_file("local_tests/media/the_thinking_machine_1960s.mp4")
  result <- llm_message("Describe what this video is about in one sentence.", .media = video) |>
    chat(openrouter(.model = "google/gemini-2.5-flash"))
  llt_expect_reply(result)
})

# ── cross-provider chain: audio leg (Gemini) → text continuation (Claude) ────

llt_test("cross-provider chain: Gemini audio reply continues cleanly with Claude", {
  audio <- audio_file("local_tests/media/example.mp3")
  step1 <- llm_message("Describe this audio clip in one sentence.", .media = audio) |>
    chat(gemini())
  llt_expect_reply(step1)

  step2 <- step1 |>
    llm_message("Summarize what you just told me in three words.") |>
    chat(claude())
  llt_expect_reply(step2)
})

llt_report()
