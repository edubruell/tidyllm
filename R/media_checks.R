DECIDE_LIMITS <- data.frame(
  provider = c("jev", "openai", "gemini", "ollama"),
  max_images = c(Inf, Inf, Inf, 4),
  image_exts = c("png,jpg,jpeg", "", "", "png,jpg,jpeg,webp"),
  max_image_mb = c(16, Inf, Inf, 4),
  max_total_mb = c(24, Inf, 100, 8),
  min_side = c(280, 0, 0, 0),
  max_side = c(1568, Inf, Inf, Inf),
  min_pixels = c(78400, 0, 0, 0),
  max_pixels = c(2458624, Inf, Inf, 16e6),
  max_questions = c(Inf, 400, 1000, 64),
  max_score_levels = c(Inf, 10, 255, 10),
  name_regex = c("^.+$", "^.+$", "^[A-Za-z0-9_-]{1,64}$", "^[A-Za-z0-9_.-]{1,100}$"),
  stringsAsFactors = FALSE
)

GEMINI_DECIDE_MIMES <- c(
  paste0("image/", c("png", "jpeg", "webp", "heic", "heif", "gif", "bmp", "tiff")),
  paste0("audio/", c("wav", "mp3", "mpeg", "aiff", "aac", "ogg", "flac", "m4a", "opus", "webm", "l16", "alaw", "mulaw")),
  paste0("video/", c("mp4", "mpeg", "mov", "avi", "webm", "wmv", "x-flv", "3gpp")),
  "application/pdf"
)

#' The format a file's first bytes announce: png, jpeg or unknown
#' @param .path A file path.
#' @noRd
image_kind <- function(.path) {
  head <- readBin(.path, "raw", n = 8)
  if (identical(head[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47)))) return("png")
  if (identical(head[1:2], as.raw(c(0xff, 0xd8)))) return("jpeg")
  "unknown"
}

#' Width and height of a PNG from its IHDR chunk
#' @param .bytes The file as a raw vector.
#' @noRd
png_dims <- function(.bytes) {
  if (length(.bytes) < 24) return(NULL)
  as.integer(c(sum(as.integer(.bytes[17:20]) * 256^(3:0)), sum(as.integer(.bytes[21:24]) * 256^(3:0))))
}

#' Position of the first start-of-frame marker in a JPEG, or NA
#' @param .bytes The file as a raw vector.
#' @noRd
jpeg_sof_position <- function(.bytes) {
  pos <- 3L
  n <- length(.bytes)
  while (pos + 9L <= n) {
    if (.bytes[[pos]] != as.raw(0xff)) return(NA_integer_)
    while (pos + 9L <= n && .bytes[[pos + 1L]] == as.raw(0xff)) pos <- pos + 1L
    marker <- as.integer(.bytes[[pos + 1L]])
    if (marker %in% c(0xc0, 0xc1, 0xc2)) return(pos)
    pos <- pos + 2L + as.integer(.bytes[[pos + 2L]]) * 256L + as.integer(.bytes[[pos + 3L]])
  }
  NA_integer_
}

#' Width and height of a JPEG from its first start-of-frame marker
#' @param .bytes The file as a raw vector.
#' @noRd
jpeg_dims <- function(.bytes) {
  pos <- jpeg_sof_position(.bytes)
  if (is.na(pos)) return(NULL)
  as.integer(c(as.integer(.bytes[[pos + 7L]]) * 256L + as.integer(.bytes[[pos + 8L]]),
               as.integer(.bytes[[pos + 5L]]) * 256L + as.integer(.bytes[[pos + 6L]])))
}

#' Pixel width and height of a PNG or JPEG file, or NULL when unreadable
#' @param .path A file path.
#' @return `c(width, height)` or `NULL`.
#' @noRd
image_dims <- function(.path) {
  kind <- image_kind(.path)
  if (kind == "unknown") return(NULL)
  bytes <- readBin(.path, "raw", n = file.size(.path))
  if (kind == "png") png_dims(bytes) else jpeg_dims(bytes)
}

#' Limits row for a provider, or NULL when none are recorded
#' @noRd
decide_limits <- function(.provider) {
  row <- DECIDE_LIMITS[DECIDE_LIMITS$provider == .provider, ]
  if (nrow(row) == 0) NULL else as.list(row)
}

#' Stop when the questions break a backend's limits
#' @param .questions The list from `decide_questions()`.
#' @param .lim A limits row from `decide_limits()`.
#' @param .provider Provider name, for the message.
#' @noRd
check_decide_questions <- function(.questions, .lim, .provider) {
  bad_names <- names(.questions)[!grepl(.lim$name_regex, names(.questions))]
  levels_n <- vapply(.questions, \(q) if (q$type == "score") length(q$options) else 0L, integer(1))
  stats::setNames(
    c(length(.questions) <= .lim$max_questions,
      length(bad_names) == 0,
      all(levels_n <= .lim$max_score_levels)),
    c(glue::glue("{.provider}() accepts at most {.lim$max_questions} questions; .questions has {length(.questions)}."),
      glue::glue("{.provider}() does not accept the question name {dQuote(bad_names[1], FALSE)}; allowed pattern: {.lim$name_regex}"),
      glue::glue("{.provider}() accepts at most {.lim$max_score_levels} score levels per question."))
  ) |> validate_inputs()
}

#' Stop when one image breaks a backend's format, size or pixel limits
#' @param .part An image object.
#' @param .lim A limits row.
#' @param .provider Provider name, for the message.
#' @noRd
check_decide_image <- function(.part, .lim, .provider) {
  path <- embed_part_path(.part)
  file <- dQuote(basename(path), FALSE)
  ext <- tolower(tools::file_ext(path))
  allowed <- strsplit(.lim$image_exts, ",")[[1]]
  kind <- image_kind(path)
  expected <- c(png = "png", jpg = "jpeg", jpeg = "jpeg")[ext]
  size_mb <- round(file.size(path) / 1024^2, 1)
  stats::setNames(
    c(length(allowed) == 0 || ext %in% allowed,
      is.na(expected) || kind == expected,
      size_mb <= .lim$max_image_mb),
    c(glue::glue("{file} is a {toupper(ext)} file; {.provider}() reads {paste(allowed, collapse = ', ')} images only. Convert it, for example with magick::image_convert()."),
      glue::glue("{file} is not a valid {toupper(ext)} file: its bytes do not match the extension."),
      glue::glue("{file} is {size_mb} MB; {.provider}() reads images up to {.lim$max_image_mb} MB."))
  ) |> validate_inputs()
  if (kind != "unknown") check_decide_dims(basename(path), image_dims(path), .lim, .provider)
}

#' Stop when image dimensions break a backend's side or pixel limits
#' @param .file File name, for the message.
#' @param .dims `c(width, height)` or NULL.
#' @param .lim A limits row.
#' @param .provider Provider name, for the message.
#' @noRd
check_decide_dims <- function(.file, .dims, .lim, .provider) {
  file <- .file
  if (is.null(.dims)) cli::cli_abort("The size of {.val {file}} cannot be read; the file looks truncated or damaged.", call = NULL)
  sides_ok <- all(.dims >= .lim$min_side & .dims <= .lim$max_side)
  pixels_ok <- prod(.dims) >= .lim$min_pixels && prod(.dims) <= .lim$max_pixels
  if (!sides_ok || !pixels_ok) {
    msg <- glue::glue(
      "{dQuote(file, FALSE)} is {.dims[[1]]} x {.dims[[2]]} pixels; {.provider}() needs each side between {.lim$min_side} and {.lim$max_side} pixels and {.lim$min_pixels} to {.lim$max_pixels} pixels in all. Resize it, for example with magick::image_scale()."
    )
    cli::cli_abort("{msg}", call = NULL)
  }
}

#' Stop when a Gemini media file has a MIME type the endpoint does not list
#' @noRd
check_decide_gemini_mime <- function(.part) {
  mime <- embed_part_mime(.part)
  file <- basename(embed_part_path(.part))
  type <- embed_part_type(.part)
  family <- c(image = "image/", audio = "audio/", video = "video/", pdf = "application/pdf")[[type]]
  if (!mime %in% GEMINI_DECIDE_MIMES || !startsWith(mime, family)) {
    cli::cli_abort("{.val {file}} has the type {.val {mime}}, which gemini() does not read as {type} input.", call = NULL)
  }
}

#' Stop when the media together are larger than the backend takes
#' @noRd
check_decide_total_size <- function(.media, .lim, .provider) {
  total_mb <- sum(vapply(.media, \(p) file.size(embed_part_path(p)), numeric(1))) / 1024^2
  has_pdf <- any(vapply(.media, \(p) embed_part_type(p) == "pdf", logical(1)))
  limit <- if (has_pdf && .provider == "gemini") 50 else .lim$max_total_mb
  stats::setNames(
    total_mb <= limit,
    glue::glue("The media in .state total {round(total_mb, 1)} MB; {.provider}() takes at most {limit} MB per request.")
  ) |> validate_inputs()
}

#' Check the questions and media of a decide() call against the backend's limits
#'
#' Runs in R before any request, so `.dry_run = TRUE` shows the failure too.
#' @param .state A parsed state.
#' @param .questions The list from `decide_questions()`.
#' @param .meta Provider metadata from `provider_metadata()`.
#' @return `NULL`, invisibly, or an error.
#' @noRd
check_decide_state <- function(.state, .questions, .meta) {
  provider <- .meta$provider_name
  lim <- decide_limits(provider)
  if (is.null(lim)) return(invisible(NULL))
  check_decide_questions(.questions, lim, provider)
  media <- decide_media_parts(.state)
  images <- Filter(\(p) embed_part_type(p) == "image", media)
  stats::setNames(
    length(images) <= lim$max_images,
    glue::glue(".state holds {length(images)} images; {provider}() reads at most {lim$max_images}.")
  ) |> validate_inputs()
  purrr::walk(images, check_decide_image, lim, provider)
  if (provider == "gemini") purrr::walk(media, check_decide_gemini_mime)
  if (length(media) > 0) check_decide_total_size(media, lim, provider)
  invisible(NULL)
}
