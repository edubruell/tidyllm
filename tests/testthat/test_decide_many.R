# Offline tests for vectorised decide(): state parsing, request building and result assembly.
#' Write a header-only PNG; shares the helper idea of test_decide_media.R.
#' @noRd
many_png <- function(.dir, .name, .w = 400, .h = 400) {
  be32 <- \(n) as.raw(c(n %/% 16777216, (n %/% 65536) %% 256, (n %/% 256) %% 256, n %% 256))
  path <- file.path(.dir, .name)
  writeBin(c(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)), be32(13L), charToRaw("IHDR"),
             be32(.w), be32(.h), as.raw(c(8, 2, 0, 0, 0))), path)
  path
}

#' Dry-run requests of decide() with fake keys.
#' @noRd
many_dry <- function(.provider, .state, .schema = tidyllm_schema(q = field_lgl("Is it late?"))) {
  withr::with_envvar(
    c(TYPESAFE_API_KEY = "x", OPENAI_API_KEY = "x", GOOGLE_API_KEY = "x"),
    decide(.state, .schema, .provider, .dry_run = TRUE)
  )
}

test_that("decide_states reads every element of .state as one item", {
  dir <- withr::local_tempdir()
  a <- img(many_png(dir, "a.png"))
  b <- img(many_png(dir, "b.png"))
  msg <- llm_message("Is it a cat?", .media = a)
  cases <- list(
    list(name = "one string", state = "x", n = 1),
    list(name = "one-element vector", state = c("x"), n = 1),
    list(name = "vector", state = c("x", "y", "x"), n = 3),
    list(name = "one image", state = a, n = 1),
    list(name = "one message", state = msg, n = 1),
    list(name = "list of media is two items", state = list(a, b), n = 2),
    list(name = "bare list of strings and media is several items", state = list("note", a), n = 2),
    list(name = "named list of media is two items", state = list(photo_a = a, photo_b = b), n = 2),
    list(name = "part list in a list is one item", state = list(list("Caption", a)), n = 1),
    list(name = "part lists", state = list(list(a), list(b), list("t", a)), n = 3),
    list(name = "mixed items", state = list("t", list(a, "u"), b, msg), n = 4),
    list(name = "one-element list", state = list(a), n = 1)
  )
  for (case in cases) expect_length(decide_states(case$state), case$n)
  labels <- vapply(decide_states(c("x", "y", "x")), \(s) s$label, character(1))
  expect_equal(labels, c("x", "y", "x"))
  named <- decide_states(list(first = a, second = list(p = "t", q = b), "text"))
  expect_equal(unname(vapply(named, \(s) s$label, character(1))), c("first", "second", "text"))
  expect_equal(named[[2]]$names, c("p", "q"))
})

test_that("decide_states stops on a malformed item and names its position", {
  a <- img(many_png(withr::local_tempdir(), "a.png"))
  file <- tidyllm_file(id = "x", provider = "openai")
  cases <- list(
    list(name = "NA in vector", state = c("x", NA), msg = "Item 2 of 2"),
    list(name = "empty string in vector", state = c("x", "", "z"), msg = "Item 2 of 3"),
    list(name = "number in list", state = list("x", list(a), 1), msg = "Item 3 of 3"),
    list(name = "empty part list", state = list(list(a), list()), msg = "Item 2 of 2"),
    list(name = "empty text part", state = list("a", list("b", "")), msg = "Item 2 of 2"),
    list(name = "NA in part list", state = list(list("a", NA_character_)), msg = "must be"),
    list(name = "nested twice", state = list("x", list(a, list("x"))), msg = "Item 2 of 2.*no nesting"),
    list(name = "tidyllm_file", state = list(a, file), msg = "Item 2 of 2.*tidyllm_file"),
    list(name = "tidyllm_file alone", state = file, msg = "tidyllm_file"),
    list(name = "mixed malformed", state = list("x", a, c("p", "q")), msg = "Item 3 of 3"),
    list(name = "duplicate part names", state = list(list(p = a, p = a), "x"), msg = "Item 1 of 2.*unique"),
    list(name = "empty list", state = list(), msg = "at least one item"),
    list(name = "empty vector", state = character(0), msg = "at least one item"),
    list(name = "NULL", state = NULL, msg = "must be")
  )
  for (case in cases) expect_error(decide_states(case$state), case$msg, info = case$name)
})

test_that("a named outer list labels the rows and a single item has no state columns", {
  dir <- withr::local_tempdir()
  a <- img(many_png(dir, "a.png"))
  b <- img(many_png(dir, "b.png"))
  expect_s3_class(many_dry(jev(), list(only = a)), "httr2_request")
  expect_s3_class(many_dry(jev(), list(list("Caption", a))), "httr2_request")
  expect_length(many_dry(jev(), list(a, b)), 2)
})

test_that("a single state keeps today's wrapper-free call and many states get one request each", {
  one <- many_dry(jev(), "late")
  expect_s3_class(one, "httr2_request")
  expect_equal(one$body$data$state, "late")
  for (provider in list(jev(), openai(), gemini(), ollama())) {
    reqs <- many_dry(provider, c("a", "b", "c"))
    expect_length(reqs, 3)
    expect_true(all(vapply(reqs, inherits, logical(1), "httr2_request")))
  }
  bodies <- lapply(many_dry(jev(), c("a", "b", "a")), \(r) r$body$data$state)
  expect_equal(unlist(bodies), c("a", "b", "a"))
  gem <- lapply(many_dry(gemini(), c("a", "b")), \(r) r$body$data$input)
  expect_equal(unlist(gem), c("a", "b"))
})

test_that("many image states build one request each with their own assets", {
  dir <- withr::local_tempdir()
  states <- list(list(img(many_png(dir, "a.png"))), list(img(many_png(dir, "b.png", 500, 500))))
  reqs <- many_dry(jev(), states)
  expect_length(reqs, 2)
  expect_true(all(vapply(reqs, \(r) "TypeSafe-Danger-Zone-Images" %in% names(r$headers), logical(1))))
  assets <- lapply(reqs, \(r) r$body$data$assets)
  expect_length(assets[[1]], 1)
  expect_false(identical(assets[[1]], assets[[2]]))
})

test_that("many states are checked against the provider before any request", {
  dir <- withr::local_tempdir()
  ok <- list(img(many_png(dir, "ok.png")))
  small <- list(img(many_png(dir, "small.png", 64, 64)))
  expect_error(many_dry(jev(), list(ok, small)), "Item 2 of 2.*64 x 64")
  wav <- file.path(dir, "a.wav")
  writeBin(as.raw(1:32), wav)
  expect_error(many_dry(openai(), list("t", list(audio_file(wav)))), "Item 2 of 2")
})

test_that("decide() validates the parallel arguments", {
  schema <- tidyllm_schema(q = field_lgl("Is it late?"))
  cases <- list(zero = 0, negative = -1, text = "5", two = c(1, 2), na = NA_real_,
                twoandahalf = 2.5)
  for (nm in names(cases)) {
    expect_error(decide(c("a", "b"), schema, jev(), .dry_run = TRUE, .max_active = cases[[nm]]),
                 ".max_active", info = nm)
  }
  expect_error(decide(c("a", "b"), schema, jev(), .dry_run = TRUE, .progress = NA), ".progress")
})

test_that("results stack behind state and state_id and keep every state", {
  qs <- decide_questions(tidyllm_schema(
    late = field_lgl("Late?"),
    topic = field_fct("Topic?", .levels = c("billing", "other")),
    anger = field_fct("Anger?", .levels = c("calm", "furious"), .ordered = TRUE)
  ))
  api <- api_jev(short_name = "jev", long_name = "Jev", api_key_env_var = "TYPESAFE_API_KEY",
                 server = "https://api.typesafe.ai", model = "jev-latest")
  reply <- list(
    model = "jev-1.14.0",
    answers = list(
      late = list(noul = 0.98),
      topic = list(choice = "billing", confidence = 0.85, probabilities = list(billing = 0.85, other = 0.15)),
      anger = list(score = 0.86, confidence = 0.6, probabilities = list("0" = 0.4, "1" = 0.6))
    ),
    usage = list(input_tokens = 403, output_tokens = 69)
  )
  ok <- decide_state_result(api, httr2::response(
    status_code = 200, headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(reply, auto_unbox = TRUE))), qs)
  bad <- decide_state_result(api, httr2::response(
    status_code = 400, headers = list(`Content-Type` = "application/json"),
    body = charToRaw("{\"detail\":\"nope\"}")), qs)
  down <- decide_state_result(api, rlang::error_cnd(message = "Could not resolve host"), qs)
  expect_null(ok$error)
  expect_match(bad$error, "HTTP 400: nope")
  expect_match(down$error, "Could not resolve host")

  states <- list(list(label = "first"), list(label = "second"), list(label = "third"))
  out <- decide_many_rows(list(ok, bad, down), states)
  expect_equal(names(out)[1:3], c("state", "state_id", "question"))
  expect_equal(out$state, rep(c("first", "second", "third"), each = 3))
  expect_equal(out$state_id, rep(1:3, each = 3))
  expect_type(out$state_id, "integer")
  expect_equal(out$answer, c("yes", "billing", "furious", rep(NA_character_, 6)))
  expect_equal(nrow(out), 9)
})

test_that("failed and refused states each give one warning", {
  failed <- list(rows = tibble::tibble(answer = NA_character_), error = "boom")
  refused <- list(rows = tibble::tibble(answer = c(NA, "yes")), error = NULL)
  fine <- list(rows = tibble::tibble(answer = "yes"), error = NULL)
  expect_warning(warn_many(list(fine, failed, failed)), "2 of 3 states failed")
  expect_warning(warn_many(list(fine, refused)), "no answer for 1 question")
  expect_no_warning(warn_many(list(fine, fine)))
})

test_that("usage sums over the states that succeeded", {
  ok <- list(error = NULL, usage = list(input_tokens = 10L, output_tokens = 2L, cache_write_tokens = NA_integer_,
                                        tokens_by_modality = c(text = 5, image = 100)))
  ok2 <- list(error = NULL, usage = list(input_tokens = 20L, output_tokens = NA_integer_, cache_write_tokens = NA_integer_,
                                         tokens_by_modality = c(text = 7)))
  bad <- list(error = "x", usage = list(input_tokens = NA_integer_, output_tokens = NA_integer_))
  total <- decide_usage_total(list(ok, bad, ok2))
  expect_equal(total$input_tokens, 30L)
  expect_equal(total$output_tokens, 2L)
  expect_true(is.na(total$cache_write_tokens))
  expect_equal(total$tokens_by_modality, c(image = 100, text = 12))
  expect_true(is.na(decide_usage_total(list(bad))$input_tokens %||% NA))
})
