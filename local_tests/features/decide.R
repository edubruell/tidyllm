devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("decide")

STATE <- "The invoice arrived three weeks late and nobody answers my mails."

yes_no_q <- function() tidyllm_schema(late = field_lgl("Is the writer complaining about a delay?"))

choice_q <- function() {
  tidyllm_schema(topic = field_fct("What is the main topic?", .levels = c("billing", "delivery", "other"),
                                   .level_descriptions = c("Invoices and payments", "Shipping and delays", "Anything else")))
}

score_q <- function() {
  tidyllm_schema(anger = field_fct("How angry is the writer?", .levels = c("calm", "annoyed", "furious"),
                                   .ordered = TRUE,
                                   .level_descriptions = c("No anger", "Some irritation", "Strong anger")))
}

multi_q <- function() {
  tidyllm_schema(
    late = field_lgl("Is the writer complaining about a delay?"),
    topic = field_fct("What is the main topic?", .levels = c("billing", "delivery", "other")),
    anger = field_fct("How angry is the writer?", .levels = c("calm", "annoyed", "furious"), .ordered = TRUE)
  )
}

http_up <- function(.url) {
  tryCatch(
    httr2::request(.url) |> httr2::req_timeout(3) |> httr2::req_perform() |> httr2::resp_status() < 500,
    error = function(e) FALSE
  )
}

ollama_has_model <- function(.model) {
  tryCatch({
    tags <- httr2::request("http://localhost:11434/api/tags") |>
      httr2::req_timeout(3) |> httr2::req_perform() |> httr2::resp_body_json()
    any(purrr::map_lgl(tags$models, \(m) startsWith(m$name, .model)))
  }, error = function(e) FALSE)
}

expect_decide_shape <- function(.out, .questions) {
  llt_expect_tibble(.out, length(.questions))
  llt_expect_true(identical(names(.out), c("question", "type", "answer", "probability", "confidence", "probabilities")),
                  paste("columns were", paste(names(.out), collapse = ",")))
  llt_expect_true(identical(.out$question, .questions), paste("questions were", paste(.out$question, collapse = ",")))
  llt_expect_true(all(.out$probability >= 0 & .out$probability <= 1), "probabilities should lie in [0, 1]")
  llt_expect_true(is.numeric(attr(.out, "latency")) && attr(.out, "latency") > 0, "latency attribute missing")
  llt_expect_true(is.list(attr(.out, "usage")) && all(c("input_tokens", "output_tokens") %in% names(attr(.out, "usage"))),
                  "usage attribute should hold input_tokens and output_tokens")
  invisible(.out)
}

run_provider <- function(.name, .make) {
  llt_test(paste(.name, "yes/no question"), {
    out <- decide(STATE, yes_no_q(), .make())
    expect_decide_shape(out, "late")
    llt_expect_true(out$type == "yes_no" && out$answer == "yes", paste("answer was", out$answer))
    llt_expect_true(!"score" %in% names(out), "there is no score column")
  })
  llt_test(paste(.name, "choice question"), {
    out <- decide(STATE, choice_q(), .make())
    expect_decide_shape(out, "topic")
    llt_expect_true(out$type == "choice" && out$answer %in% c("billing", "delivery", "other"), paste("answer was", out$answer))
  })
  llt_test(paste(.name, "score question"), {
    out <- decide(STATE, score_q(), .make())
    expect_decide_shape(out, "anger")
    llt_expect_true(out$type == "score" && out$answer %in% c("calm", "annoyed", "furious"), paste("answer was", out$answer))
    llt_expect_true(is.finite(out$probability) && length(out$probabilities[[1]]) == 3, "score row should carry three level probabilities")
  })
  llt_test(paste(.name, "multi-question call keeps order"), {
    out <- decide(STATE, multi_q(), .make())
    expect_decide_shape(out, c("late", "topic", "anger"))
    llt_expect_true(identical(out$type, c("yes_no", "choice", "score")), paste(out$type, collapse = ","))
  })
  llt_test(paste(.name, "text field is rejected before any request"), {
    err <- tryCatch(decide(STATE, tidyllm_schema(q = field_chr("Free text")), .make()),
                    error = function(e) conditionMessage(e))
    llt_expect_true(is.character(err) && grepl("Question", err), "expected a clear question error")
  })
  llt_test(paste(.name, "empty state is rejected"), {
    err <- tryCatch(decide("", yes_no_q(), .make()), error = function(e) conditionMessage(e))
    llt_expect_true(is.character(err) && grepl("non-empty", err), "expected a state error")
  })
}

run_if_key <- function(.name, .var, .make) {
  if (!nzchar(Sys.getenv(.var))) {
    cat(sprintf("  [skip] %s - %s is not set\n", .name, .var))
  } else {
    run_provider(.name, .make)
  }
}

run_if_key("jev", "TYPESAFE_API_KEY", \() jev())
run_if_key("openai", "OPENAI_API_KEY", \() openai())
run_if_key("gemini", "GOOGLE_API_KEY", \() gemini())

MEDIA_DIR <- "local_context/test_data/multimodal"
#' Path of a sample media file.
#' @noRd
media_path <- function(.name) file.path(MEDIA_DIR, .name)

#' Two yes/no questions about the labelled photos.
#' @noRd
pet_q <- function() {
  tidyllm_schema(
    a_dog = field_lgl("Is the animal in photo_a a dog?"),
    b_dog = field_lgl("Is the animal in photo_b a dog?")
  )
}

#' One yes/no question about a cat picture.
#' @noRd
cat_q <- function() tidyllm_schema(cat = field_lgl("Is the animal in the picture a cat?"))

#' One item of two labelled parts: a cat photo and a dog photo.
#' @noRd
two_photos <- function() list(list(photo_a = img(media_path("cat.jpg")), photo_b = img(media_path("dog.jpg"))))

#' Input tokens reported in the usage attribute.
#' @noRd
input_tokens <- function(.out) attr(.out, "usage")$input_tokens

#' Assert that the labelled photos were told apart.
#' @noRd
expect_two_photos <- function(.out) {
  llt_expect_true(identical(.out$question, c("a_dog", "b_dog")), "questions out of order")
  llt_expect_true(identical(.out$answer, c("no", "yes")), paste("answers were", paste(.out$answer, collapse = ",")))
  llt_expect_true(all(c("confidence", "probabilities") %in% names(.out)), "confidence and probabilities columns missing")
  llt_expect_true(all(.out$confidence >= 0 & .out$confidence <= 1), "confidence should lie in [0, 1]")
  llt_expect_true(all(lengths(.out$probabilities) == 2), "yes/no rows should carry two probabilities")
}

#' Media cases shared by the backends that read images.
#' @noRd
run_media_common <- function(.name, .make, .make_low) {
  llt_test(paste(.name, "image: yes/no on a cat photo"), {
    out <- decide(img(media_path("cat.jpg")), cat_q(), .make())
    llt_expect_true(out$answer == "yes", paste("answer was", out$answer))
    llt_expect_true(input_tokens(out) > 200, paste("input tokens", input_tokens(out)))
  })
  llt_test(paste(.name, "two labelled images keep their names"), {
    expect_two_photos(decide(two_photos(), pet_q(), .make()))
  })
  llt_test(paste(.name, "low detail costs fewer tokens than the default"), {
    full <- input_tokens(decide(img(media_path("cat.jpg")), cat_q(), .make()))
    low <- input_tokens(decide(img(media_path("cat.jpg")), cat_q(), .make_low()))
    llt_expect_true(low < full, paste("low", low, "default", full))
  })
}

#' Run the shared media cases when the key and sample files exist.
#' @noRd
run_media_if_key <- function(.name, .var, .make, .make_low) {
  if (!nzchar(Sys.getenv(.var)) || !dir.exists(MEDIA_DIR)) {
    cat(sprintf("  [skip] %s media - %s or %s missing\n", .name, .var, MEDIA_DIR))
  } else {
    run_media_common(.name, .make, .make_low)
  }
}

run_media_if_key("openai", "OPENAI_API_KEY", \() openai(), \() openai(.detail = "low"))
run_media_if_key("gemini", "GOOGLE_API_KEY", \() gemini(), \() gemini(.resolution = "low"))

if (nzchar(Sys.getenv("TYPESAFE_API_KEY")) && dir.exists(MEDIA_DIR)) {
  llt_test("jev image: yes/no on a cat photo", {
    out <- decide(img(media_path("cat.jpg")), cat_q(), jev())
    llt_expect_true(out$answer == "yes", paste("answer was", out$answer))
    llt_expect_true(input_tokens(out) > 1000, paste("input tokens", input_tokens(out)))
  })
  llt_test("jev two labelled images keep their names", {
    expect_two_photos(decide(two_photos(), pet_q(), jev()))
  })
  llt_test("jev box: a presence question plus a box on the cat photo", {
    q <- tidyllm_schema(has_cat = field_lgl("Is there a cat in `photo`?"), cat = field_box("The cat in `photo`"))
    out <- decide(list(list(photo = img(media_path("cat.jpg")))), q, jev())
    box <- out[out$question == "cat", ]
    llt_expect_true(identical(out$type, c("yes_no", "box")), paste(out$type, collapse = ","))
    llt_expect_true(out$answer[1] == "yes", paste("presence answer was", out$answer[1]))
    llt_expect_true(is.na(box$answer) && is.na(box$probability), "a box row has no answer or probability")
    llt_expect_true(all(c("top", "bottom", "left", "right") %in% names(out)), "coordinate columns missing")
    edges <- unlist(box[c("top", "bottom", "left", "right")])
    llt_expect_true(!anyNA(edges) && all(edges >= 0 & edges <= 1), paste("edges were", paste(edges, collapse = ",")))
    llt_expect_true(box$top < box$bottom && box$left < box$right, "the box should have an area")
    llt_expect_true(all(is.na(out[1, c("top", "bottom", "left", "right")])), "non-box rows should have NA coordinates")
  })
  llt_test("jev box: a named second image can be targeted in the description", {
    q <- tidyllm_schema(dog = field_box("The dog in `b`"))
    out <- decide(list(list(a = img(media_path("cat.jpg")), b = img(media_path("dog.jpg")))), q, jev())
    llt_expect_true(out$type == "box" && !anyNA(out[c("top", "bottom", "left", "right")]), "no box returned")
  })
  llt_test("a box question stops before any request for providers that cannot answer it", {
    q <- tidyllm_schema(cat = field_box("The cat in `photo`"))
    err <- tryCatch(decide(img(media_path("cat.jpg")), q, openai(), .dry_run = TRUE), error = \(e) conditionMessage(e))
    err <- gsub("\\s+", " ", err)
    llt_expect_true(grepl("cannot answer", err) && grepl("jev", err), err)
  })
  llt_test("jev rejects a 64 x 64 image locally, before any request", {
    tiny <- tempfile(fileext = ".png")
    png::writePNG(matrix(0.5, 64, 64), tiny)
    err <- tryCatch(decide(img(tiny), cat_q(), jev()), error = \(e) conditionMessage(e))
    llt_expect_true(is.character(err) && grepl("64 x 64 pixels", err), "expected the local size error")
  })
}

if (nzchar(Sys.getenv("GOOGLE_API_KEY")) && dir.exists(MEDIA_DIR)) {
  llt_test("gemini audio: shape and confidence", {
    out <- decide(audio_file(media_path("cat.wav")),
                  tidyllm_schema(meow = field_lgl("Is the sound a cat meowing?")), gemini())
    expect_decide_shape(out, "meow")
    llt_expect_true(input_tokens(out) > 100, paste("input tokens", input_tokens(out)))
  })
  llt_test("gemini video with low resolution", {
    out <- decide(video_file(media_path("cat.mp4")), cat_q(), gemini(.resolution = "low"))
    expect_decide_shape(out, "cat")
    llt_expect_true(out$answer == "yes", paste("answer was", out$answer))
  })
  llt_test("gemini PDF is read natively", {
    pdf_path <- tempfile(fileext = ".pdf")
    grDevices::pdf(pdf_path)
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Invoice 4711: payment is three weeks overdue.")
    grDevices::dev.off()
    out <- decide(pdf_file(pdf_path),
                  tidyllm_schema(late = field_lgl("Does the document say a payment is overdue?")), gemini())
    llt_expect_true(out$answer == "yes", paste("answer was", out$answer))
    llt_expect_true(!is.null(attr(out, "usage")$tokens_by_modality), "modality tokens missing from usage")
  })
}

MANY_TEXTS <- c("The invoice arrived three weeks late and nobody answers my mails.",
                "Thank you, the parcel came on time.",
                "The invoice arrived three weeks late and nobody answers my mails.")

#' Cases for a vector of states: layout, order, usage and the no-new-columns rule.
#' @noRd
run_many_common <- function(.name, .make) {
  llt_test(paste(.name, "vectorised texts: state columns, order and usage"), {
    out <- decide(MANY_TEXTS, yes_no_q(), .make(), .max_active = 2)
    llt_expect_true(identical(names(out)[1:3], c("state", "state_id", "question")), paste(names(out), collapse = ","))
    llt_expect_true(identical(out$state_id, 1:3), paste("state_id was", paste(out$state_id, collapse = ",")))
    llt_expect_true(identical(out$answer, c("yes", "no", "yes")), paste("answers were", paste(out$answer, collapse = ",")))
    one <- decide(MANY_TEXTS[2], yes_no_q(), .make())
    llt_expect_true(!"state" %in% names(one), "a single state should not get a state column")
    llt_expect_true(input_tokens(out) > 2 * input_tokens(one), paste("summed tokens", input_tokens(out), "single", input_tokens(one)))
    llt_expect_true(is.numeric(attr(out, "latency")) && is.character(attr(out, "model")), "latency or model missing")
  })
}

#' Many image items: a named list of img() objects, one item per element.
#' @noRd
run_many_images <- function(.name, .make) {
  llt_test(paste(.name, "vectorised images answer per state"), {
    files <- stats::setNames(media_path(c("cat.jpg", "dog.jpg")), c("cat", "dog"))
    out <- decide(purrr::map(files, img), cat_q(), .make())
    llt_expect_true(identical(unique(out$state), c("cat", "dog")), "the outer names should label the state column")
    llt_expect_true(identical(out$answer, c("yes", "no")), paste("answers were", paste(out$answer, collapse = ",")))
    llt_expect_true(identical(out$state_id, 1:2), "state_id out of order")
  })
}

run_many_if_key <- function(.name, .var, .make) {
  if (!nzchar(Sys.getenv(.var))) {
    cat(sprintf("  [skip] %s vectorised - %s is not set\n", .name, .var))
  } else {
    run_many_common(.name, .make)
    if (dir.exists(MEDIA_DIR)) run_many_images(.name, .make)
  }
}

run_many_if_key("openai", "OPENAI_API_KEY", \() openai())
run_many_if_key("gemini", "GOOGLE_API_KEY", \() gemini())
run_many_if_key("jev", "TYPESAFE_API_KEY", \() jev())

if (nzchar(Sys.getenv("TYPESAFE_API_KEY"))) {
  llt_test("jev vectorised: an oversized state gives NA rows and one warning, the others survive", {
    huge <- paste(rep("late invoice", 300000), collapse = " ")
    warnings <- character()
    out <- withCallingHandlers(
      decide(c(MANY_TEXTS[1], huge, MANY_TEXTS[2]), yes_no_q(), jev()),
      warning = \(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    llt_expect_true(identical(out$state_id, 1:3), "all three states should be kept")
    llt_expect_true(identical(out$answer[c(1, 3)], c("yes", "no")), paste("answers were", paste(out$answer, collapse = ",")))
    llt_expect_true(is.na(out$answer[2]), "the oversized state should give an NA row")
    llt_expect_true(length(warnings) == 1 && grepl("1 of 3 state", warnings), paste("warnings:", paste(warnings, collapse = " | ")))
  })
}

if (!ollama_has_model("clef-flash")) {
  cat("  [skip] ollama and ollama media - clef-flash is not in /api/tags (never loaded by this suite)\n")
} else {
  run_provider("ollama", \() ollama())
}

llt_report("decide")
