#' Does one part count as valid decide() input
#' @param .part A string or a tidyllm media object.
#' @noRd
decide_part_ok <- function(.part) {
  if (is.character(.part)) return(length(.part) == 1 && !is.na(.part) && nzchar(.part))
  !is.na(embed_part_type(.part)) && file.exists(embed_part_path(.part))
}

#' The parts of the last user message: its text, then its media in order
#' @param .llm An `LLMMessage`.
#' @return A list of strings and media objects.
#' @noRd
decide_message_parts <- function(.llm) {
  users <- Filter(\(m) identical(m$role, "user"), .llm@message_history)
  c("An LLMMessage given as .state needs a user message" = length(users) > 0) |>
    validate_inputs()
  last <- users[[length(users)]]
  media <- Filter(\(p) !is.na(embed_part_type(p)), last$media %||% list())
  c(Filter(decide_part_ok, list(last$content)), media)
}

#' Parse one `.state` item into ordered, optionally named parts
#'
#' A string is one text part. A media object (`img()`, `audio_file()`,
#' `video_file()`, `pdf_file()`) is one part. A list is the parts of one item, one
#' level only; its names label the parts. An `LLMMessage` gives the text and media
#' of its last user message.
#'
#' @param .state One item: a string, a media object, a list of those, or an `LLMMessage`.
#' @return A list with `label` (one line describing the state), `parts` (an
#'   unnamed list) and `names` (a character vector, `""` for an unnamed part).
#' @noRd
decide_parts <- function(.state) {
  items <- if (S7_inherits(.state, LLMMessage)) {
    decide_message_parts(.state)
  } else if (is.list(.state) && !is.object(.state)) {
    .state
  } else {
    list(.state)
  }
  labels <- names(items) %||% rep("", length(items))
  named <- labels[nzchar(labels)]
  c("Each `.state` item must be a non-empty string, an img(), audio_file(), video_file() or pdf_file() object, an LLMMessage, or one list of such parts (no nesting, no tidyllm_file)" =
      length(items) > 0 && all(vapply(items, decide_part_ok, logical(1))),
    "Parts of a `.state` item need unique, non-missing names" =
      !anyNA(labels) && !anyDuplicated(named)) |>
    validate_inputs()
  parts <- unname(items)
  list(
    label = substr(paste(vapply(parts, embed_part_label, character(1)), collapse = " + "), 1, 60),
    parts = parts,
    names = unname(labels)
  )
}

#' Part types of a decide state, one per part
#' @param .state A parsed state from `decide_parts()`.
#' @noRd
decide_types <- function(.state) {
  vapply(.state$parts, embed_part_type, character(1))
}

#' The non-text parts of a decide state
#' @noRd
decide_media_parts <- function(.state) {
  Filter(\(p) !is.character(p), .state$parts)
}

#' Is the state one unnamed string
#' @noRd
decide_is_plain <- function(.state) {
  length(.state$parts) == 1 && is.character(.state$parts[[1]]) && !nzchar(.state$names[[1]])
}

#' Why a PDF has no text, naming only remedies that apply
#'
#' A provider that reads PDFs natively is only offered when the PDF was forced
#' to text; otherwise the provider itself is the one to blame.
#' @param .parts The state parts before conversion.
#' @param .native Whether the provider reads PDFs natively.
#' @return A single string.
#' @noRd
decide_pdf_empty_message <- function(.parts, .native) {
  forced <- any(vapply(.parts, \(p) S7_inherits(p, tidyllm_pdf) && p@text_extract, logical(1)))
  base <- "A PDF given as .state has no extractable text; it is probably a scan."
  if (.native && forced) return(paste(base, "Drop `.text_extract = TRUE` so the provider reads the file itself."))
  if (.native) return(paste(base, "Run OCR on it first."))
  paste(base, "Run OCR on it first, or use a provider that reads PDFs natively, such as gemini().")
}

#' Replace PDF parts by their extracted text where the backend cannot read PDFs
#'
#' A `pdf_file(.text_extract = TRUE)` always goes as text.
#' @param .state A parsed state.
#' @param .meta Provider metadata from `provider_metadata()`.
#' @return The state with the affected parts turned into strings.
#' @noRd
decide_pdf_fallback <- function(.state, .meta) {
  native <- "pdf" %in% .meta$decide_media
  as_text <- \(p) if (S7_inherits(p, tidyllm_pdf) && (p@text_extract || !native)) p@pdftext else p
  parts <- lapply(.state$parts, as_text)
  empty <- vapply(parts, \(p) is.character(p) && !nzchar(trimws(p)), logical(1))
  stats::setNames(!any(empty), decide_pdf_empty_message(.state$parts, native)) |>
    validate_inputs()
  .state$parts <- parts
  .state
}

#' Providers whose decide registry lists a media type
#' @param .type One of the decide media types.
#' @noRd
providers_deciding <- function(.type) {
  names <- all_provider_names()
  has <- vapply(names, \(n) .type %in% provider_metadata(n)$decide_media, logical(1))
  names[has]
}

#' Stop when a provider cannot decide on the media the state contains
#' @param .state A parsed state.
#' @param .meta Provider metadata from `provider_metadata()`.
#' @noRd
check_decide_media <- function(.state, .meta) {
  missing_types <- setdiff(unique(decide_types(.state)), c("text", .meta$decide_media))
  if (length(missing_types) > 0) {
    type <- missing_types[1]
    provider <- .meta$provider_name
    capable <- providers_deciding(type)
    cli::cli_abort("The {.val {provider}} provider cannot decide on {type} input. Providers that can: {.val {capable}}.", call = NULL)
  }
  invisible(NULL)
}

#' Build the blocks of a state: a label block before each named part
#' @param .state A parsed state.
#' @param .label Function from a part name to a block.
#' @param .part Function from a part to a block.
#' @return A flat list of blocks.
#' @noRd
decide_blocks <- function(.state, .label, .part) {
  purrr::map2(.state$parts, .state$names, \(p, n) c(if (nzchar(n)) list(.label(n)), list(.part(p)))) |>
    purrr::list_flatten()
}

#' One OpenAI user content part
#' @noRd
openai_decide_part <- function(.part, .detail) {
  if (is.character(.part)) return(list(type = "input_text", text = .part))
  purrr::compact(list(type = "input_image", image_url = embed_part_data_uri(.part), detail = .detail))
}

#' OpenAI `input`: the string for plain text, otherwise one user message of parts
#' @noRd
openai_decide_input <- function(.state, .detail = NULL) {
  if (decide_is_plain(.state)) return(.state$parts[[1]])
  content <- decide_blocks(
    .state,
    \(n) list(type = "input_text", text = paste0(n, ":")),
    \(p) openai_decide_part(p, .detail)
  )
  list(list(type = "message", role = "user", content = content))
}

#' One Gemini content block
#'
#' Adjacent text blocks concatenate without a separator, hence the newline.
#' @noRd
gemini_decide_part <- function(.part, .resolution) {
  if (is.character(.part)) return(list(type = "text", text = paste0(.part, "\n")))
  type <- embed_part_type(.part)
  purrr::compact(list(
    type = if (type == "pdf") "document" else type,
    data = embed_part_base64(.part),
    mime_type = embed_part_mime(.part),
    resolution = if (type %in% c("image", "video")) .resolution
  ))
}

#' Gemini `input`: the string for plain text, otherwise an array of blocks
#' @noRd
gemini_decide_input <- function(.state, .resolution = NULL) {
  if (decide_is_plain(.state)) return(.state$parts[[1]])
  decide_blocks(
    .state,
    \(n) list(type = "text", text = paste0(n, ":\n")),
    \(p) gemini_decide_part(p, .resolution)
  )
}

#' Reference string for one Jev image: position and a content hash
#' @noRd
jev_asset_ref <- function(.part, .i) {
  paste0("tidyllm-img-", .i, "-", substr(unname(tools::md5sum(embed_part_path(.part))), 1, 8))
}

#' One Jev asset entry
#' @noRd
jev_asset <- function(.part) {
  list(type = "image", format = tolower(tools::file_ext(embed_part_path(.part))),
       data_base64 = embed_part_base64(.part))
}

#' Jev `state` and `assets`: references in the state, bytes in the assets map
#'
#' One unnamed image is the whole state; otherwise the state is an object keyed
#' by part name, or `part_<i>` for an unnamed part.
#' @return A list with `state` and `assets` (empty when no part is an image).
#' @noRd
jev_state_assets <- function(.state) {
  if (decide_is_plain(.state)) return(list(state = .state$parts[[1]], assets = list()))
  is_media <- decide_types(.state) != "text"
  refs <- purrr::imap_chr(.state$parts, \(p, i) if (is_media[[i]]) jev_asset_ref(p, i) else NA_character_)
  assets <- stats::setNames(lapply(.state$parts[is_media], jev_asset), refs[is_media])
  if (length(.state$parts) == 1 && !nzchar(.state$names)) return(list(state = refs[[1]], assets = assets))
  keys <- make.unique(ifelse(nzchar(.state$names), .state$names, paste0("part_", seq_along(.state$parts))))
  values <- ifelse(is_media, refs, vapply(.state$parts, \(p) if (is.character(p)) p else "", character(1)))
  list(state = stats::setNames(as.list(values), keys), assets = assets)
}

#' Ollama `state` text and `images` array
#'
#' The `images` array is shared and has no positions, so the text marks each
#' image as `[image k]` in order.
#' @return A list with `state` and `images` (a character vector, possibly empty).
#' @noRd
ollama_state_images <- function(.state) {
  if (decide_is_plain(.state)) return(list(state = .state$parts[[1]], images = character(0)))
  is_media <- decide_types(.state) != "text"
  marks <- ifelse(is_media, paste0("[image ", cumsum(is_media), "]"), "")
  texts <- vapply(.state$parts, \(p) if (is.character(p)) p else "", character(1))
  lines <- paste0(ifelse(nzchar(.state$names), paste0(.state$names, ": "), ""), marks, texts)
  list(
    state = paste(lines, collapse = "\n"),
    images = vapply(.state$parts[is_media], embed_part_base64, character(1))
  )
}
