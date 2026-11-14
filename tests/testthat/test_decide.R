jev_provider_obj <- function() api_jev(short_name = "jev", long_name = "Jev", api_key_env_var = "TYPESAFE_API_KEY",
                                       server = "https://api.typesafe.ai", model = "jev-latest")

decide_schema <- function() {
  tidyllm_schema(
    late = field_lgl("Was the delivery late?"),
    topic = field_fct("What is the topic?", .levels = c("billing", "delivery", "other"),
                      .level_descriptions = c("Invoices", "Shipping", "Anything else")),
    anger = field_fct("How angry is the writer?", .levels = c("calm", "annoyed", "furious"),
                      .ordered = TRUE,
                      .level_descriptions = c("No anger", "Some anger", "Strong anger"))
  )
}

decide_body <- function(.provider, .schema = decide_schema()) {
  withr::with_envvar(
    c(TYPESAFE_API_KEY = "x", OPENAI_API_KEY = "x", GOOGLE_API_KEY = "x"),
    decide("The invoice arrived late.", .schema, .provider, .dry_run = TRUE)
  )$body$data
}

test_that("decide_questions normalises each question type", {
  cases <- list(
    list(name = "yes_no", schema = tidyllm_schema(q = field_lgl("Is it late?")),
         type = "yes_no", options = character(0)),
    list(name = "choice", schema = tidyllm_schema(q = field_fct("Topic?", .levels = c("a", "b"))),
         type = "choice", options = c("a", "b")),
    list(name = "score", schema = tidyllm_schema(q = field_fct("Anger?", .levels = c("lo", "hi"), .ordered = TRUE)),
         type = "score", options = c("lo", "hi"))
  )
  for (case in cases) {
    q <- decide_questions(case$schema)$q
    expect_equal(q$type, case$type, info = case$name)
    expect_equal(q$options, case$options, info = case$name)
    expect_equal(q$name, "q", info = case$name)
  }
})

test_that("level descriptions are used when given and fall back to the levels", {
  with_desc <- decide_questions(decide_schema())
  expect_equal(with_desc$topic$descriptions, c("Invoices", "Shipping", "Anything else"))
  expect_equal(with_desc$anger$descriptions, c("No anger", "Some anger", "Strong anger"))
  without <- decide_questions(tidyllm_schema(q = field_fct("T?", .levels = c("a", "b"))))
  expect_equal(without$q$descriptions, c("a", "b"))
  expect_equal(without$q$instructions, "T?")
})

test_that("decide_questions rejects questions it cannot ask", {
  cases <- list(
    text_field = tidyllm_schema(q = field_chr("Free text")),
    number_field = tidyllm_schema(q = field_dbl("A number")),
    vector_field = tidyllm_schema(q = field_lgl("Many", .vector = TRUE)),
    one_level_choice = tidyllm_schema(q = field_fct("One", .levels = "a")),
    one_level_score = tidyllm_schema(q = field_fct("One", .levels = "a", .ordered = TRUE)),
    empty_levels = tidyllm_schema(q = field_fct("None", .levels = character(0)))
  )
  for (nm in names(cases)) {
    expect_error(decide_questions(cases[[nm]]), regexp = "Question|field", info = nm)
  }
  expect_error(decide_questions("not a schema"), "tidyllm_schema")
  expect_error(decide_questions(NULL), "tidyllm_schema")
})

test_that("field_fct validates its new arguments", {
  expect_error(field_fct("x", .levels = c("a", "b"), .level_descriptions = "only one"), "one entry per level")
  expect_error(field_fct("x", .levels = c("a", "b"), .ordered = NA), "TRUE or FALSE")
  expect_error(field_fct("x", .levels = c("a", "b"), .level_descriptions = 1:2), "character vector")
})

test_that("decide_state is validated", {
  bad <- list(empty = "", na = NA_character_, two = c("a", "b"), number = 1, null = NULL)
  for (nm in names(bad)) {
    expect_error(decide_parts(bad[[nm]]), "non-empty|must be", info = nm)
  }
  expect_silent(decide_parts("fine"))
})

test_that("new field_fct arguments leave ordinary schemas unchanged", {
  plain <- tidyllm_schema(a = field_fct("A", .levels = c("x", "y")), b = field_lgl("B"))
  expect_null(attr(plain, "decide_meta"))
  json <- jsonlite::toJSON(unclass(plain), auto_unbox = TRUE)
  expect_false(grepl("ordered|level_descriptions|decide_meta", json))
  with_args <- tidyllm_schema(a = field_fct("A", .levels = c("x", "y"), .ordered = TRUE,
                                            .level_descriptions = c("d1", "d2")))
  expect_false(grepl("ordered|level_descriptions|d1", jsonlite::toJSON(unclass(with_args), auto_unbox = TRUE)))
})

test_that("Jev and Ollama send the System One body", {
  for (provider in list(jev(), ollama())) {
    body <- decide_body(provider)
    expect_equal(body$state, "The invoice arrived late.")
    expect_equal(body$questions$late$type, "noul")
    expect_equal(body$questions$topic$type, "choice")
    expect_equal(body$questions$topic$criteria$billing, "Invoices")
    expect_equal(body$questions$anger$type, "score")
    expect_equal(unlist(body$questions$anger$criteria), c("No anger", "Some anger", "Strong anger"))
    expect_null(names(body$questions$anger$criteria))
  }
  expect_equal(decide_body(jev())$model, "jev-latest")
  expect_equal(decide_body(ollama())$model, "clef-flash")
})

test_that("Jev and Ollama requests go to the right place with the right auth", {
  jev_req <- withr::with_envvar(c(TYPESAFE_API_KEY = "x"),
    decide("s", decide_schema(), jev(), .dry_run = TRUE))
  expect_equal(jev_req$url, "https://api.typesafe.ai/v1/systemone")
  expect_true("Authorization" %in% names(jev_req$headers))
  ollama_req <- decide("s", decide_schema(), ollama(), .dry_run = TRUE)
  expect_match(ollama_req$url, "/v1/systemone$")
  expect_false("Authorization" %in% names(ollama_req$headers))
})

test_that("Gemini sends input instead of state and the key in a header", {
  req <- withr::with_envvar(c(GOOGLE_API_KEY = "x"),
    decide("s", decide_schema(), gemini(), .dry_run = TRUE))
  expect_match(req$url, "/v1beta/decisions$")
  expect_true("x-goog-api-key" %in% names(req$headers))
  body <- req$body$data
  expect_equal(body$input, "s")
  expect_null(body$state)
  expect_equal(body$model, "gemini-decision-1")
  expect_equal(body$questions$late$type, "noul")
})

test_that("OpenAI sends an unnamed question array", {
  body <- decide_body(openai())
  expect_equal(body$model, "gpt-6-luna")
  expect_equal(body$input, "The invoice arrived late.")
  types <- purrr::map_chr(body$questions, "type")
  expect_equal(types, c("predicate", "choice", "score"))
  expect_equal(purrr::map_chr(body$questions, "name"), c("late", "topic", "anger"))
  expect_equal(body$questions[[2]]$choices[[1]], list(value = "billing", description = "Invoices"))
  expect_equal(body$questions[[3]]$levels[[3]], list(label = "furious", description = "Strong anger"))
})

test_that("a missing API key fails on a real request but not on a dry run", {
  withr::local_envvar(TYPESAFE_API_KEY = "")
  expect_error(decide("s", decide_schema(), jev()))
  expect_s3_class(decide("s", decide_schema(), jev(), .dry_run = TRUE), "httr2_request")
})

test_that("decide() stops on malformed input before any request", {
  expect_error(decide("", decide_schema(), jev(), .dry_run = TRUE), "non-empty")
  expect_error(decide("s", tidyllm_schema(q = field_chr("x")), jev(), .dry_run = TRUE), "Question")
  expect_error(decide("s", decide_schema(), .dry_run = TRUE), "provider")
})

jev_reply <- function() {
  list(
    model = "jev-1.14.0",
    answers = list(
      late = list(noul = 0.98),
      topic = list(choice = "billing", confidence = 0.85,
                   probabilities = list(billing = 0.85, delivery = 0.1, other = 0.05)),
      anger = list(score = 0.86, confidence = 0.6,
                   probabilities = list("0" = 0.1, "1" = 0.6, "2" = 0.3))
    ),
    usage = list(input_tokens = 403, output_tokens = 69)
  )
}

test_that("a score answer keeps its row without a score field for both backends", {
  schema <- tidyllm_schema(
    anger = field_fct("Anger?", .levels = c("calm", "annoyed", "furious"), .ordered = TRUE),
    late = field_lgl("Late?")
  )
  qs <- decide_questions(schema)
  jev_body <- list(answers = list(
    anger = list(probabilities = list("0" = 0.1, "1" = 0.6, "2" = 0.3)),
    late = list(noul = 0.9)
  ))
  out <- parse_decide_response(jev_provider_obj(), jev_body, qs)$rows
  expect_equal(nrow(out), 2)
  expect_equal(out$answer, c("annoyed", "yes"))
  expect_false("score" %in% names(out))
  by_label <- list(answers = list(
    anger = list(score = 0.2, probabilities = list(calm = 0.1, annoyed = 0.2, furious = 0.7)),
    late = list(noul = 0.9)
  ))
  label_out <- parse_decide_response(jev_provider_obj(), by_label, qs)$rows
  expect_equal(label_out$answer, c("furious", "yes"))
  expect_equal(label_out$probability[[1]], 0.7)
  oa <- api_openai(short_name = "openai", long_name = "OpenAI", api_key_env_var = "OPENAI_API_KEY")
  oa_body <- list(answers = list(
    list(type = "score", name = "anger",
         probabilities = list(list(value = 0, label = "calm", probability = 0.1),
                              list(value = 1, label = "annoyed", probability = 0.9),
                              list(value = 2, label = "furious", probability = 0))),
    list(type = "predicate", name = "late", probability = 0.9)
  ))
  oa_out <- parse_decide_response(oa, oa_body, qs)$rows
  expect_equal(nrow(oa_out), 2)
})

test_that("Jev replies parse into the long tibble", {
  qs <- decide_questions(decide_schema())
  out <- parse_decide_response(jev_provider_obj(), jev_reply(), qs)
  expect_equal(out$rows$question, c("late", "topic", "anger"))
  expect_equal(out$rows$type, c("yes_no", "choice", "score"))
  expect_equal(out$rows$answer, c("yes", "billing", "annoyed"))
  expect_equal(out$rows$probability, c(0.98, 0.85, 0.6))
  expect_false("score" %in% names(out$rows))
  expect_equal(out$usage, list(input_tokens = 403, output_tokens = 69))
  expect_equal(out$model, "jev-1.14.0")
})

test_that("a low noul probability reads as no with the complementary probability", {
  qs <- decide_questions(tidyllm_schema(q = field_lgl("Q?")))
  out <- parse_system_one(list(answers = list(q = list(noul = 0.2))), qs)
  expect_equal(out$rows$answer, "no")
  expect_equal(out$rows$probability, 0.8)
  expect_true(is.na(out$model))
})

test_that("replies with a missing answer fail clearly", {
  qs <- decide_questions(decide_schema())
  broken <- jev_reply()
  broken$answers$topic <- NULL
  expect_error(parse_system_one(broken, qs), "no answer for: topic")
  expect_error(parse_system_one(list(), qs), "no `answers`")
  expect_error(parse_system_one(list(answers = list()), qs), "no answer for")
})

test_that("Gemini replies map its usage names and tolerate a missing model", {
  qs <- decide_questions(decide_schema())
  reply <- jev_reply()
  reply$model <- NULL
  reply$usage <- list(total_tokens = 195, total_input_tokens = 195, total_output_tokens = 0)
  gem <- api_gemini(short_name = "gemini", long_name = "Google Gemini", api_key_env_var = "GOOGLE_API_KEY")
  out <- parse_decide_response(gem, reply, qs)
  expect_equal(out$usage$input_tokens, 195)
  expect_equal(out$usage$output_tokens, 0)
  expect_true(is.na(out$model))
  expect_equal(out$rows$answer, c("yes", "billing", "annoyed"))
})

openai_reply <- function() {
  list(
    model = "gpt-6-luna",
    answers = list(
      list(type = "predicate", name = "late", probability = 0.94),
      list(type = "choice", name = "topic", choice = "billing", confidence = 0.85,
           probabilities = list(list(value = "billing", probability = 0.85),
                                list(value = "delivery", probability = 0.1),
                                list(value = "other", probability = 0.05))),
      list(type = "score", name = "anger", score = 1, confidence = 1,
           probabilities = list(list(value = 0, label = "calm", probability = 0),
                                list(value = 1, label = "annoyed", probability = 1),
                                list(value = 2, label = "furious", probability = 0)))
    ),
    usage = list(input_tokens = 398, output_tokens = 0,
                 input_tokens_details = list(cached_tokens = 0))
  )
}

test_that("OpenAI replies parse by answer name", {
  qs <- decide_questions(decide_schema())
  oa <- api_openai(short_name = "openai", long_name = "OpenAI", api_key_env_var = "OPENAI_API_KEY")
  out <- parse_decide_response(oa, openai_reply(), qs)
  expect_equal(out$rows$answer, c("yes", "billing", "annoyed"))
  expect_equal(out$rows$probability, c(0.94, 0.85, 1))
  expect_equal(out$usage$input_tokens, 398)
  expect_equal(out$usage$cached_tokens, 0)
  expect_equal(out$model, "gpt-6-luna")
  short <- openai_reply()
  short$answers <- short$answers[1:2]
  expect_error(parse_decide_response(oa, short, qs), "no answer for: anger")
})

test_that("OpenAI answers are matched by name, not position, and bad names fail", {
  qs <- decide_questions(decide_schema())
  oa <- api_openai(short_name = "openai", long_name = "OpenAI", api_key_env_var = "OPENAI_API_KEY")
  shuffled <- openai_reply()
  shuffled$answers <- rev(shuffled$answers)
  expect_equal(parse_decide_response(oa, shuffled, qs)$rows$question, c("late", "topic", "anger"))
  expect_equal(parse_decide_response(oa, shuffled, qs)$rows$answer, c("yes", "billing", "annoyed"))
  unnamed <- openai_reply(); unnamed$answers[[2]]$name <- NULL
  unknown <- openai_reply(); unknown$answers[[2]]$name <- "other_question"
  cases <- list(
    list(body = unnamed, msg = "no `name`"),
    list(body = unknown, msg = "unknown questions: other_question"),
    list(body = list(model = "m"), msg = "no `answers`"),
    list(body = "text", msg = "not a JSON object")
  )
  for (case in cases) expect_error(parse_decide_response(oa, case$body, qs), case$msg)
})

test_that("a null body or answers is a clear error for System One replies", {
  qs <- decide_questions(decide_schema())
  cases <- list(
    list(body = "text", msg = "not a JSON object"),
    list(body = NULL, msg = "not a JSON object"),
    list(body = list(answers = NULL), msg = "no `answers`"),
    list(body = list(answers = "x"), msg = "no `answers`")
  )
  for (case in cases) expect_error(parse_system_one(case$body, qs), case$msg)
})

test_that("choice rows look the probability up safely", {
  qs <- decide_questions(tidyllm_schema(t = field_fct("T?", .levels = c("a", "b"))))
  parse <- function(answer) parse_system_one(list(answers = list(t = answer)), qs)$rows
  expect_equal(parse(list(choice = "a", probabilities = list(a = 0.7, b = 0.3)))$probability, 0.7)
  expect_equal(parse(list(choice = "c", confidence = 0.4, probabilities = list(a = 0.7)))$probability, 0.4)
  expect_equal(parse(list(choice = "a", confidence = 0.55))$probability, 0.55)
  expect_error(parse(list(choice = "c", probabilities = list(a = 0.7))), "Question `t`")
  expect_error(parse(list(choice = "a")), "Question `t`")
})

test_that("a score answer with empty probabilities is a refused row on every backend", {
  qs <- decide_questions(tidyllm_schema(
    anger = field_fct("Anger?", .levels = c("calm", "annoyed"), .ordered = TRUE),
    late = field_lgl("Late?")
  ))
  gem <- api_gemini(short_name = "gemini", long_name = "Google Gemini", api_key_env_var = "GOOGLE_API_KEY")
  oa <- api_openai(short_name = "openai", long_name = "OpenAI", api_key_env_var = "OPENAI_API_KEY")
  system_one <- \(empty) list(answers = list(anger = list(probabilities = empty, score = 1), late = list(noul = 0.9)),
                              usage = list())
  openai <- list(answers = list(list(type = "score", name = "anger", probabilities = list(), score = 1),
                                list(type = "predicate", name = "late", probability = 0.9)))
  cases <- list(
    jev_empty_object = list(api = jev_provider_obj(), body = system_one(structure(list(), names = character(0)))),
    jev_empty_list = list(api = jev_provider_obj(), body = system_one(list())),
    gemini_empty_object = list(api = gem, body = system_one(structure(list(), names = character(0)))),
    openai_empty_array = list(api = oa, body = openai)
  )
  for (nm in names(cases)) {
    expect_warning(out <- parse_decide_response(cases[[nm]]$api, cases[[nm]]$body, qs)$rows,
                   "1 question", info = nm)
    expect_equal(out$answer, c(NA, "yes"), info = nm)
  }
})

test_that("null, error and refusal answers give NA rows and one warning", {
  qs <- decide_questions(decide_schema())
  na_row <- function(rows, q) rows[rows$question == q, ]
  cases <- list(
    null_answers = list(late = NULL, topic = NULL, anger = NULL),
    error_field = list(late = list(error = "boom"), topic = list(choice = "billing", error = "x"),
                       anger = list(error = list(message = "m"))),
    empty_objects = list(late = list(), topic = list(), anger = list())
  )
  for (nm in names(cases)) {
    body <- list(answers = cases[[nm]])
    names(body$answers) <- names(cases[[nm]])
    body$answers <- stats::setNames(lapply(names(cases[[nm]]), \(n) cases[[nm]][[n]]), names(cases[[nm]]))
    expect_warning(out <- parse_system_one(body, qs), "late.*topic.*anger", info = nm)
    expect_equal(nrow(out$rows), 3, info = nm)
    expect_true(all(is.na(out$rows$answer) & is.na(out$rows$probability)), info = nm)
    expect_equal(out$rows$type, c("yes_no", "choice", "score"), info = nm)
  }
  mixed <- jev_reply()
  mixed["topic"] <- list(NULL)
  mixed$answers["topic"] <- list(NULL)
  expect_warning(out <- parse_system_one(mixed, qs), "1 question")
  expect_equal(out$rows$answer, c("yes", NA, "annoyed"))
  expect_equal(na_row(out$rows, "topic")$probability, NA_real_)
  expect_no_warning(parse_system_one(jev_reply(), qs))
})

test_that("OpenAI refusals give NA rows without aborting the other questions", {
  qs <- decide_questions(decide_schema())
  oa <- api_openai(short_name = "openai", long_name = "OpenAI", api_key_env_var = "OPENAI_API_KEY")
  cases <- list(
    refusal_type = list(type = "refusal", name = "topic", refusal = "I cannot help"),
    error_field = list(type = "choice", name = "topic", error = "failed"),
    no_choice = list(type = "choice", name = "topic")
  )
  for (nm in names(cases)) {
    reply <- openai_reply()
    reply$answers[[2]] <- cases[[nm]]
    expect_warning(out <- parse_decide_response(oa, reply, qs), "topic", info = nm)
    expect_equal(out$rows$answer, c("yes", NA, "annoyed"), info = nm)
    expect_true(is.na(out$rows$probability[2]), info = nm)
  }
  both <- openai_reply()
  both$answers[[1]] <- list(type = "refusal", name = "late")
  both$answers[[3]] <- list(type = "refusal", name = "anger")
  expect_warning(out <- parse_decide_response(oa, both, qs), "late.*anger")
  expect_equal(out$rows$answer, c(NA, "billing", NA))
})

test_that("non-JSON, empty and error bodies report the HTTP status", {
  api <- jev_provider_obj()
  make <- function(status, body, type = "text/html") {
    httr2::response(status_code = status, headers = list(`Content-Type` = type), body = charToRaw(body))
  }
  cases <- list(
    list(resp = make(502, "<html>Bad Gateway</html>"), msg = "Jev returned HTTP 502"),
    list(resp = make(500, "", "application/json"), msg = "Jev returned HTTP 500"),
    list(resp = make(401, "{\"error\":{\"message\":\"bad key\"}}", "application/json"), msg = "HTTP 401: bad key"),
    list(resp = make(422, "{\"detail\":\"nope\"}", "application/json"), msg = "HTTP 422: nope"),
    list(resp = make(200, "<html>ok</html>"), msg = "HTTP 200 with a body that is not JSON"),
    list(resp = make(200, "", "application/json"), msg = "not JSON")
  )
  for (case in cases) expect_error(decide_reply_content(api, case$resp), case$msg)
  ok <- make(200, "{\"answers\":{}}", "application/json")
  expect_equal(decide_reply_content(api, ok), list(answers = setNames(list(), character(0))))
})

test_that("error fields of every shape become one readable message", {
  api <- jev_provider_obj()
  make <- function(status, body, type) {
    httr2::response(status_code = status, headers = list(`Content-Type` = type), body = charToRaw(body))
  }
  cases <- list(
    jev_object = list(resp = make(401, "{\"detail\":{\"error_type\":\"authentication_error\",\"message\":\"Check your key\"}}", "application/json"), msg = "HTTP 401: Check your key"),
    plain_text = list(resp = make(401, "Identity Edge route authentication policy requires an accepted credential", "text/plain"), msg = "HTTP 401: Identity Edge route"),
    object_without_message = list(resp = make(400, "{\"detail\":{\"code\":7}}", "application/json"), msg = "HTTP 400: \\{\"code\":7\\}"),
    empty_text = list(resp = make(503, "", "text/plain"), msg = "HTTP 503: no detail"),
    string_error = list(resp = make(400, "{\"error\":\"too big\"}", "application/json"), msg = "HTTP 400: too big")
  )
  for (nm in names(cases)) expect_error(decide_reply_content(api, cases[[nm]]$resp), cases[[nm]]$msg, info = nm)
})

test_that("a free-text field is named as such, without wrapper noise", {
  q <- tidyllm_schema(note = field_chr("A free note"))
  err <- expect_error(decide_questions(q), "free text")
  expect_match(conditionMessage(err), "note")
  expect_false(grepl("In index|Caused by", conditionMessage(err)))
})

test_that("OpenAI and Gemini carry a configurable model", {
  expect_equal(decide_body(openai(.model = "gpt-test"))$model, "gpt-test")
  expect_equal(decide_body(gemini(.model = "gem-test"))$model, "gem-test")
  expect_equal(decide_body(jev(.model = "jev-test"))$model, "jev-test")
  expect_equal(decide_body(openai())$model, "gpt-6-luna")
  expect_equal(decide_body(gemini())$model, "gemini-decision-1")
  caps <- provider_capabilities()
  expect_true(all(c("openai", "gemini") %in% caps$provider[caps$verb == "decide" & grepl("model", caps$argument)]))
})

test_that("HTTP 422 details become one readable message", {
  api <- jev_provider_obj()
  body <- list(detail = list(list(loc = list("body", "questions", "x"), msg = "Field required")))
  expect_match(decide_error_message(api, body, 422), "HTTP 422: body.questions.x Field required")
  expect_match(decide_error_message(api, list(error = list(message = "bad key")), 401), "bad key")
  expect_match(decide_error_message(api, list(), 500), "no detail")
})

test_that("provider_capabilities lists decide for the four backends", {
  caps <- provider_capabilities()
  for (p in c("jev", "openai", "gemini", "ollama")) {
    rows <- caps[caps$provider == p & caps$verb == "decide", ]
    expect_gt(nrow(rows), 0)
  }
})

test_that("rows carry confidence and per-option probabilities on every backend", {
  qs <- decide_questions(decide_schema())
  oa <- api_openai(short_name = "openai", long_name = "OpenAI", api_key_env_var = "OPENAI_API_KEY")
  gem <- api_gemini(short_name = "gemini", long_name = "Google Gemini", api_key_env_var = "GOOGLE_API_KEY")
  outs <- list(
    jev = parse_decide_response(jev_provider_obj(), jev_reply(), qs)$rows,
    gemini = parse_decide_response(gem, jev_reply(), qs)$rows,
    openai = parse_decide_response(oa, openai_reply(), qs)$rows
  )
  for (nm in names(outs)) {
    rows <- outs[[nm]]
    expect_named(rows, c("question", "type", "answer", "probability", "confidence", "probabilities"), info = nm)
    expect_equal(names(rows$probabilities[[1]]), c("yes", "no"), info = nm)
    expect_equal(names(rows$probabilities[[2]]), c("billing", "delivery", "other"), info = nm)
    expect_equal(names(rows$probabilities[[3]]), c("calm", "annoyed", "furious"), info = nm)
  }
  expect_equal(outs$jev$confidence, c(0.98, 0.85, 0.6))
  expect_equal(unname(outs$jev$probabilities[[3]]), c(0.1, 0.6, 0.3))
  expect_equal(unname(outs$openai$probabilities[[2]]), c(0.85, 0.1, 0.05))
  expect_equal(outs$openai$confidence, c(0.94, 0.85, 1))
})

test_that("confidence and probabilities cope with missing or odd reply fields", {
  qs <- decide_questions(decide_schema())
  reply <- jev_reply()
  reply$answers$topic$confidence <- NULL
  reply$answers$anger$probabilities <- list("2" = 0.3, "0" = 0.1, "1" = 0.6)
  reply$answers$late <- list(noul = 0.03, confidence = NULL)
  rows <- parse_decide_response(jev_provider_obj(), reply, qs)$rows
  expect_equal(rows$confidence[[1]], 0.97)
  expect_true(is.na(rows$confidence[[2]]))
  expect_equal(names(rows$probabilities[[3]]), c("calm", "annoyed", "furious"))
  expect_equal(unname(rows$probabilities[[3]]), c(0.1, 0.6, 0.3))
  expect_equal(rows$answer[[1]], "no")

  odd <- jev_reply()
  odd$answers$anger$probabilities <- list("x" = 0.5, "y" = 0.5)
  odd$answers$topic$confidence <- NULL
  odd$answers$topic$probabilities <- list(billing = 0.9)
  odd$answers$late$confidence <- "not a number"
  rows2 <- suppressWarnings(parse_decide_response(jev_provider_obj(), odd, qs)$rows)
  expect_equal(names(rows2$probabilities[[3]]), c("x", "y"))
  expect_equal(rows2$confidence[[1]], 0.98)

  empty <- jev_reply()
  empty$answers$topic$probabilities <- list()
  empty$answers$topic$confidence <- 0.7
  rows3 <- parse_decide_response(jev_provider_obj(), empty, qs)$rows
  expect_length(rows3$probabilities[[2]], 0)
  expect_equal(rows3$probability[[2]], 0.7)
})

test_that("refused answers give NA confidence and empty probabilities", {
  q <- decide_questions(tidyllm_schema(q = field_lgl("x")))$q
  row <- refused_row(q)
  expect_true(is.na(row$confidence))
  expect_length(row$probabilities[[1]], 0)
})

test_that("usage carries Gemini modality tokens and OpenAI cache writes", {
  qs <- decide_questions(decide_schema())
  gem <- api_gemini(short_name = "gemini", long_name = "Google Gemini", api_key_env_var = "GOOGLE_API_KEY")
  reply <- jev_reply()
  reply$usage <- list(total_input_tokens = 900, total_output_tokens = 0,
                      input_tokens_by_modality = list(text = 100, image = 800))
  usage <- parse_decide_response(gem, reply, qs)$usage
  expect_equal(usage$tokens_by_modality, c(text = 100, image = 800))
  expect_null(parse_decide_response(gem, jev_reply(), qs)$usage$tokens_by_modality)
  entries <- list(list(modality = "text", tokens = 100), list(modality = "image", tokens = 800))
  expect_equal(gemini_modality_tokens(entries), c(text = 100, image = 800))
  expect_equal(gemini_modality_tokens(list(list(modality = "text", tokens = 86))), c(text = 86))
  expect_null(gemini_modality_tokens(list()))
  expect_null(gemini_modality_tokens(NULL))
  oa <- api_openai(short_name = "openai", long_name = "OpenAI", api_key_env_var = "OPENAI_API_KEY")
  reply <- openai_reply()
  reply$usage$input_tokens_details$cache_write_tokens <- 1381
  expect_equal(parse_decide_response(oa, reply, qs)$usage$cache_write_tokens, 1381)
})

test_that("perplexity() is a stub that stops with the replacement providers, whatever it is given", {
  for (args in list(list(), list(.model = "sonar"), list("a", b = 2))) {
    expect_error(do.call(perplexity, args), "removed in tidyllm 0.8.0.*openai.*claude.*gemini.*openrouter.*deep_research")
  }
  expect_true("perplexity" %in% getNamespaceExports("tidyllm"))
})
