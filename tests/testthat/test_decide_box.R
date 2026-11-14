# Offline tests for field_box(): schema, request body, reply parsing, checks and box_to_pixels().
#' Questions: a presence question and a box
#' @noRd
box_schema <- function() {
  tidyllm_schema(has = field_lgl("Is there a cat in `p`?"), cat = field_box("The cat in `p`"))
}

#' A Jev reply holding a presence answer and a box answer
#' @noRd
box_reply <- function(.box, .confidence = 0.79) {
  list(model = "jev-1.14.0", usage = list(input_tokens = 10, output_tokens = 2),
       answers = list(has = list(type = "noul", noul = 0.9, confidence = 0.9),
                      cat = list(type = "box", box = .box, confidence = .confidence)))
}

#' Parse a Jev reply for the box schema into result rows
#' @noRd
parse_box <- function(.body) {
  parse_system_one(.body, decide_questions(box_schema()))$rows
}

test_that("field_box is a box question and rejects malformed descriptions", {
  q <- decide_questions(box_schema())
  expect_equal(q$cat$type, "box")
  expect_equal(q$cat$instructions, "The cat in `p`")
  expect_equal(q$has$type, "yes_no")
  for (bad in list(character(0), NA_character_, "", 1, c("a", "b"))) {
    expect_error(field_box(bad), "one non-empty string")
  }
})

test_that("the box question body has no criteria and the image header is sent", {
  img_path <- fake_png(300, 300)
  req <- withr::with_envvar(c(TYPESAFE_API_KEY = "x"),
    decide(list(list(p = img(img_path))), box_schema(), jev(), .dry_run = TRUE))
  body <- req$body$data
  expect_equal(body$questions$cat, list(type = "box", instructions = "The cat in `p`"))
  expect_equal(body$questions$has$type, "noul")
  expect_equal(req$headers[["TypeSafe-Danger-Zone-Images"]], "true")
})

test_that("a box reply gives one box row and the coordinate columns", {
  out <- parse_box(box_reply(list(top = 0.19, bottom = 0.67, left = 0, right = 0.79)))
  box <- out[out$question == "cat", ]
  expect_equal(box$type, "box")
  expect_true(is.na(box$answer) && is.na(box$probability))
  expect_equal(box$confidence, 0.79)
  expect_length(box$probabilities[[1]], 0)
  expect_equal(unlist(box[c("top", "bottom", "left", "right")]), c(top = 0.19, bottom = 0.67, left = 0, right = 0.79))
  expect_true(all(is.na(out[out$question == "has", c("top", "bottom", "left", "right")])))
  expect_no_warning(parse_box(box_reply(list(top = 0.19, bottom = 0.67, left = 0, right = 0.79))))
})

test_that("coordinate columns appear only when the schema has a box question", {
  plain <- parse_system_one(list(answers = list(q = list(noul = 0.9))), decide_questions(tidyllm_schema(q = field_lgl("Q?"))))$rows
  expect_false(any(c("top", "bottom", "left", "right") %in% names(plain)))
})

test_that("null confidence is NA", {
  out <- parse_box(box_reply(list(top = 0.1, bottom = 0.5, left = 0.1, right = 0.5), .confidence = NULL))
  expect_true(is.na(out$confidence[out$question == "cat"]))
})

test_that("malformed box replies stop, warn or become refusals", {
  good <- list(top = 0.1, bottom = 0.5, left = 0.1, right = 0.5)
  stops <- list(
    missing_edge = good[c("top", "bottom", "left")],
    null_edge = replace(good, "right", list(NULL)),
    text_edge = replace(good, "left", "far")
  )
  for (nm in names(stops)) {
    expect_error(parse_box(box_reply(stops[[nm]])), "lacks a numeric", info = nm)
  }
  warns <- list(
    above_one = replace(good, "bottom", 1.2),
    negative = replace(good, "top", -0.1),
    inverted = list(top = 0.6, bottom = 0.2, left = 0.1, right = 0.5),
    no_width = list(top = 0.1, bottom = 0.5, left = 0.3, right = 0.3)
  )
  for (nm in names(warns)) {
    expect_warning(out <- parse_box(box_reply(warns[[nm]])), "not inside the image", info = nm)
    expect_false(is.na(out$top[out$question == "cat"]), info = nm)
  }
  expect_warning(refused <- parse_box(box_reply(list())), "no answer for 1 question")
  expect_true(is.na(refused$top[refused$question == "cat"]))
  expect_true("top" %in% names(refused))
})

test_that("box questions stop before any request for providers that cannot answer them", {
  path <- fake_png(300, 300)
  for (p in list(openai(), gemini(), ollama())) {
    expect_error(
      withr::with_envvar(c(OPENAI_API_KEY = "x", GOOGLE_API_KEY = "x"),
                         decide(img(path), box_schema(), p, .dry_run = TRUE)),
      "cannot answer.*Providers that can: .*jev"
    )
  }
})

test_that("a box question needs an image in the state, and many-item calls name the item", {
  withr::local_envvar(c(TYPESAFE_API_KEY = "x"))
  expect_error(decide("A cat sat here.", box_schema(), jev(), .dry_run = TRUE), "needs an image")
  expect_error(decide(list(list(a = "text", b = "more text")), box_schema(), jev(), .dry_run = TRUE), "needs an image")
  path <- fake_png(300, 300)
  expect_error(decide(list(img(path), "no image here"), box_schema(), jev(), .dry_run = TRUE), "Item 2 of 2.*needs an image")
  expect_length(decide(list(img(path), img(path)), box_schema(), jev(), .dry_run = TRUE), 2)
})

test_that("a refused box state in a many-item run keeps the coordinate columns", {
  q <- decide_questions(box_schema())
  failed <- decide_state_result(jev_provider_obj(), simpleError("boom"), q)
  expect_true(all(c("top", "bottom", "left", "right") %in% names(failed$rows)))
  expect_true(all(is.na(failed$rows$top)))
})

test_that("box_to_pixels turns fractions into pixels and keeps other rows", {
  res <- parse_box(box_reply(list(top = 0.2, bottom = 0.7, left = 0, right = 0.8)))
  px <- box_to_pixels(res, .width = 1000, .height = 600)
  expect_equal(nrow(px), nrow(res))
  box <- px[px$question == "cat", ]
  expect_equal(unlist(box[c("x_min", "x_max", "y_min", "y_max")]), c(x_min = 0, x_max = 800, y_min = 120, y_max = 420))
  expect_true(all(is.na(px[px$question == "has", c("x_min", "x_max", "y_min", "y_max")])))
  from_file <- box_to_pixels(res, .image = fake_png(500, 300))
  expect_equal(from_file$x_max[from_file$question == "cat"], 400)
  expect_equal(from_file$y_max[from_file$question == "cat"], 210)
  overshoot <- suppressWarnings(parse_box(box_reply(list(top = -0.1, bottom = 1.2, left = 0, right = 1))))
  clamped <- box_to_pixels(overshoot, 100, 100)
  expect_equal(unlist(clamped[clamped$question == "cat", c("y_min", "y_max")]), c(y_min = 0, y_max = 100))
})

test_that("box_to_pixels rejects malformed input", {
  res <- parse_box(box_reply(list(top = 0.2, bottom = 0.7, left = 0, right = 0.8)))
  bad <- list(
    no_columns = list(tibble::tibble(question = "a", type = "yes_no"), 100, 100, NULL, "holds a field_box"),
    not_frame = list("text", 100, 100, NULL, "holds a field_box"),
    zero_width = list(res, 0, 100, NULL, "positive numbers"),
    negative_height = list(res, 100, -5, NULL, "positive numbers"),
    missing_size = list(res, NULL, NULL, NULL, "positive numbers"),
    vector_size = list(res, c(100, 200), 100, NULL, "positive numbers"),
    text_size = list(res, "100", 100, NULL, "positive numbers"),
    na_size = list(res, NA_real_, 100, NULL, "positive numbers"),
    missing_file = list(res, NULL, NULL, "/no/such/file.png", "existing PNG or JPEG"),
    not_image = list(res, NULL, NULL, fake_file("t.png", charToRaw("text")), "existing PNG or JPEG")
  )
  for (nm in names(bad)) {
    a <- bad[[nm]]
    expect_error(box_to_pixels(a[[1]], a[[2]], a[[3]], a[[4]]), a[[5]], info = nm)
  }
})

test_that("box_to_pixels handles NA types, repeated calls, inverted boxes and fractional sizes", {
  res <- parse_box(box_reply(list(top = 0.2, bottom = 0.7, left = 0, right = 0.8)))
  with_na <- res
  with_na$type[1] <- NA_character_
  px <- box_to_pixels(with_na, 1000, 600)
  expect_true(all(is.na(px[1, c("x_min", "x_max", "y_min", "y_max")])))
  expect_equal(px$x_max[2], 800)
  first <- box_to_pixels(res, 1000, 600)
  again <- box_to_pixels(first, 500, 300)
  expect_equal(names(again), names(first))
  expect_equal(again$x_max[again$question == "cat"], 400)
  inverted <- suppressWarnings(parse_box(box_reply(list(top = 0.2, bottom = 0.7, left = 0.9, right = 0.1))))
  flipped <- box_to_pixels(inverted, 100, 100)
  expect_gt(flipped$x_min[2], flipped$x_max[2])
  expect_equal(box_to_pixels(res, 100.5, 10.5)$x_max[2], round(0.8 * 100.5))
  both <- box_to_pixels(res, .width = 1, .height = 1, .image = fake_png(500, 300))
  expect_equal(both$x_max[2], 400)
})

test_that("a reply whose box is not an object gives the friendly message", {
  for (bad in list("left", 0.5, TRUE, list("a", "b"))) {
    expect_error(parse_box(box_reply(bad)), "lacks a numeric", info = format(bad))
  }
})
