#' Type of one embed part: text, image, audio or video
#' @param .part A character string or a tidyllm media object.
#' @return One of `"text"`, `"image"`, `"audio"`, `"video"`.
#' @noRd
embed_part_type <- function(.part) {
  if (is.character(.part)) return("text")
  if (S7_inherits(.part, tidyllm_image)) return("image")
  if (S7_inherits(.part, tidyllm_audio)) return("audio")
  if (S7_inherits(.part, tidyllm_video)) return("video")
  NA_character_
}

#' Row label for one embed part
#' @param .part A character string or a tidyllm media object.
#' @return A string: the text itself, or `[IMG]`, `[AUDIO]`, `[VIDEO]` and the file name.
#' @noRd
embed_part_label <- function(.part) {
  switch(embed_part_type(.part),
    text  = .part,
    image = paste0("[IMG] ", .part@imagename),
    audio = paste0("[AUDIO] ", .part@audioname),
    video = paste0("[VIDEO] ", .part@videoname)
  )
}

#' Path of the file behind a media part
#' @param .part A tidyllm image, audio or video object.
#' @return A file path.
#' @noRd
embed_part_path <- function(.part) {
  switch(embed_part_type(.part),
    image = .part@imagepath,
    audio = .part@audiopath,
    video = .part@videopath
  )
}

#' MIME type of a media part, taken from the file extension
#' @param .part A tidyllm image, audio or video object.
#' @return A MIME type string.
#' @noRd
embed_part_mime <- function(.part) {
  guess_mime_type(embed_part_path(.part))
}

#' Plain base64 of a media part, without a data URI header
#' @param .part A tidyllm image, audio or video object.
#' @return A base64 string.
#' @noRd
embed_part_base64 <- function(.part) {
  path <- embed_part_path(.part)
  base64enc::base64encode(readBin(path, what = "raw", n = file.size(path)))
}

#' Base64 of a media part as a data URI
#' @param .part A tidyllm image, audio or video object.
#' @return A string of the form `data:<mime>;base64,<data>`.
#' @noRd
embed_part_data_uri <- function(.part) {
  paste0("data:", embed_part_mime(.part), ";base64,", embed_part_base64(.part))
}

#' Audio format name from the file extension
#' @param .part A tidyllm audio object.
#' @return A lower-case extension such as `"wav"`.
#' @noRd
embed_audio_format <- function(.part) {
  tolower(tools::file_ext(.part@audiopath))
}

#' Turn one element of an embed() list into an item
#' @param .x A single string, a media object, or a list of those (one fused item).
#' @return A list with `label` and `parts`.
#' @noRd
embed_item_of <- function(.x) {
  parts <- if (is.list(.x) && !is.object(.x)) .x else list(.x)
  is_part <- function(p) {
    if (is.character(p)) length(p) == 1 && !is.na(p) else !is.na(embed_part_type(p))
  }
  c(
    "Each element of an embed() list must be a single string, an img(), audio_file() or video_file() object, or a list of those for one fused item" =
      length(parts) > 0 && all(vapply(parts, is_part, logical(1)))
  ) |> validate_inputs()
  list(
    label = paste(vapply(parts, embed_part_label, character(1)), collapse = " + "),
    parts = parts
  )
}

#' Parse embed() input into a list of items, once, at the boundary
#'
#' A character vector gives one text item per element. A list gives one item per
#' element; a nested list is one fused item. An `LLMMessage` gives its message
#' texts.
#'
#' @param .input A character vector, a list, or an `LLMMessage`.
#' @return A list of items, each a list with `label` and `parts`.
#' @noRd
embed_items <- function(.input) {
  c(
    "Input .input must be a character vector, a list or an LLMMessage object" =
      S7_inherits(.input, LLMMessage) || is.character(.input) || (is.list(.input) && !is.data.frame(.input))
  ) |> validate_inputs()
  elements <- if (S7_inherits(.input, LLMMessage)) {
    as.list(parse_embedding_input(.input))
  } else {
    as.list(.input)
  }
  c("Input .input must contain at least one element" = length(elements) > 0) |>
    validate_inputs()
  lapply(elements, embed_item_of)
}

#' Row labels for a list of items
#' @param .items Items from `embed_items()`.
#' @return A character vector, one label per item.
#' @noRd
embed_labels <- function(.items) {
  vapply(.items, \(i) i$label, character(1))
}

#' Media types used across a list of items
#' @param .items Items from `embed_items()`.
#' @return A character vector of the non-text types present.
#' @noRd
embed_media_used <- function(.items) {
  types <- purrr::map(.items, \(i) vapply(i$parts, embed_part_type, character(1)))
  setdiff(unique(unlist(types)), "text")
}

#' Whether every item is a single text part
#' @param .items Items from `embed_items()`.
#' @return A logical.
#' @noRd
embed_is_text_only <- function(.items) {
  all(vapply(.items, \(i) length(i$parts) == 1 && is.character(i$parts[[1]]), logical(1)))
}

#' Providers whose embed registry lists a media type
#' @param .type One of the embed media types.
#' @return A character vector of provider names.
#' @noRd
providers_embedding <- function(.type) {
  names <- all_provider_names()
  has <- vapply(names, \(n) .type %in% provider_metadata(n)$embed_media, logical(1))
  names[has]
}

#' Stop when a provider cannot embed what the input contains
#'
#' Runs before any request, from the provider's embed media registry.
#'
#' @param .items Items from `embed_items()`.
#' @param .meta Provider metadata from `provider_metadata()`.
#' @return `NULL`, invisibly, or an error.
#' @noRd
check_embed_media <- function(.items, .meta) {
  used <- embed_media_used(.items)
  missing_types <- setdiff(used, .meta$embed_media)
  if (length(missing_types) > 0) {
    type <- missing_types[1]
    provider <- .meta$provider_name
    capable <- providers_embedding(type)
    cli::cli_abort(
      "The {.val {provider}} provider cannot embed {type} input. Providers that can: {.val {capable}}.",
      call = NULL
    )
  }
  fused <- any(vapply(.items, \(i) length(i$parts) > 1, logical(1)))
  if (fused && length(.meta$embed_media) == 0) {
    provider <- .meta$provider_name
    cli::cli_abort(
      "The {.val {provider}} provider cannot embed fused (nested list) input. Pass a flat character vector or list instead.",
      call = NULL
    )
  }
  invisible(NULL)
}

#' Stop when a media file is larger than a provider accepts
#' @param .items Items from `embed_items()`.
#' @param .limit_mb The provider's documented size limit per file, in megabytes.
#' @param .provider The provider name, for the message.
#' @return `NULL`, invisibly, or an error.
#' @noRd
check_embed_media_size <- function(.items, .limit_mb, .provider) {
  parts <- purrr::list_flatten(purrr::map(.items, \(i) i$parts))
  media <- Filter(\(p) !is.character(p), parts)
  sizes <- vapply(media, \(p) file.size(embed_part_path(p)) / 1024^2, numeric(1))
  too_big <- media[sizes > .limit_mb]
  if (length(too_big) > 0) {
    file_label <- embed_part_label(too_big[[1]])
    limit_mb <- .limit_mb
    provider <- .provider
    cli::cli_abort(
      "{.val {file_label}} is larger than the {limit_mb} MB limit of {.val {provider}}.",
      call = NULL
    )
  }
  invisible(NULL)
}

#' Text prefix that EmbeddingGemma-style models read as the task
#' @param .task `NULL`, `"query"` or `"document"`.
#' @return A string; empty when `.task` is `NULL`.
#' @noRd
embed_task_prefix <- function(.task) {
  if (is.null(.task)) return("")
  c(query = "task: search result | query: ", document = "title: none | text: ")[[.task]]
}

#' Check the `.task` and `.dimensions` arguments of an embedding function
#' @param .task `NULL`, `"query"` or `"document"`.
#' @param .dimensions `NULL` or a positive whole number.
#' @return `NULL`, invisibly, or an error.
#' @noRd
validate_embed_options <- function(.task, .dimensions) {
  c(
    "Input .task must be NULL, \"query\" or \"document\"" =
      is.null(.task) || (is.character(.task) && length(.task) == 1 && .task %in% c("query", "document")),
    "Input .dimensions must be NULL or a positive whole number" =
      is.null(.dimensions) || (is.numeric(.dimensions) && length(.dimensions) == 1 &&
                                 .dimensions > 0 && .dimensions == round(.dimensions))
  ) |> validate_inputs()
}

#' Put the task prefix before the first text part of an item
#' @param .item An item from `embed_items()`.
#' @param .prefix The task prefix string.
#' @return The item with the first text part prefixed and an unchanged label.
#' @noRd
embed_apply_prefix <- function(.item, .prefix) {
  first_text <- match("text", vapply(.item$parts, embed_part_type, character(1)))
  if (is.na(first_text)) return(.item)
  .item$parts[[first_text]] <- paste0(.prefix, .item$parts[[first_text]])
  .item
}

#' The `input` field for an OpenAI-style embeddings endpoint
#'
#' Text-only input stays a character vector. Input with media becomes one
#' `content` array of typed parts per item.
#'
#' @param .items Items from `embed_items()`.
#' @param .part_fn Function that turns one part into a typed content part.
#' @param .prefix Task prefix applied to text parts.
#' @return A character vector or a list of `list(content = ...)` items.
#' @noRd
embed_content_input <- function(.items, .part_fn, .prefix = "") {
  items <- purrr::map(.items, embed_apply_prefix, .prefix = .prefix)
  if (embed_is_text_only(items)) {
    return(vapply(items, \(i) i$parts[[1]], character(1)))
  }
  purrr::map(items, \(i) list(content = purrr::map(i$parts, .part_fn)))
}
