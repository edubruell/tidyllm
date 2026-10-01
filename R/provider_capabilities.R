INTERNAL_VERBS <- "build"

#' Find a provider function from a name, a call or the function itself
#' @noRd
provider_function_of <- function(.provider) {
  ns <- asNamespace("tidyllm")
  if (is.function(.provider)) {
    return(.provider)
  }
  name <- if (is.character(.provider) && length(.provider) == 1) {
    .provider
  } else if (is.call(.provider)) {
    head <- .provider[[1]]
    if (is.call(head)) as.character(head[[length(head)]]) else as.character(head)
  } else {
    NA_character_
  }
  fn <- if (is.na(name)) NULL else get0(name, envir = ns, mode = "function")
  if (is.null(fn) || is.null(attr(fn, "tidyllm_provider"))) {
    stop(glue::glue("`{deparse(.provider)[1]}` is not a tidyllm provider."), call. = FALSE)
  }
  fn
}

#' Metadata a provider registered: name, verbs, arguments, defaults, media
#' @noRd
provider_metadata <- function(.provider) {
  provider_function_of(.provider)(.called_from = "metadata")
}

#' All provider functions in the package, by name
#' @noRd
all_provider_names <- function() {
  ns <- asNamespace("tidyllm")
  objs <- mget(ls(ns), envir = ns)
  found <- vapply(objs, function(o) is.function(o) && !is.null(attr(o, "tidyllm_provider")),
                  logical(1))
  sort(unique(vapply(objs[found], function(o) attr(o, "tidyllm_provider"), character(1))))
}

#' What each provider supports
#'
#' Lists the verbs a provider implements and the arguments each verb accepts, or
#' the media types it accepts in a message. Use it to find out which providers can
#' do something before you write code that depends on it.
#'
#' With `.what = "arguments"` the result has one row per provider, verb and
#' argument. A verb that takes no arguments of its own gets one row with
#' `argument = NA`. The `default` column holds the argument's default as text, so
#' for `.model` it is the provider's default model. `fn` names the function that
#' implements the verb for that provider; its help page documents every argument.
#'
#' The table says that a provider's function accepts an argument. It does not say
#' that every model of that provider accepts it: for example, `.thinking` can be
#' accepted by `openai()` while a particular model rejects some effort levels.
#'
#' @param .provider A provider call such as `claude()`, a provider name such as
#'   `"claude"`, or `NULL` (default) for every provider.
#' @param .verb Character vector of verb names (such as `"chat"` or `"send_batch"`)
#'   to keep. `NULL` keeps all verbs.
#' @param .argument Character vector of argument names (such as `".thinking"`) to
#'   keep. `NULL` keeps all arguments.
#' @param .what `"arguments"` (default) for verbs and arguments, or `"media"` for
#'   one row per provider and media type.
#' @param .internal Logical; if `TRUE`, include the internal `build` verb that
#'   `send_chat()` and `parallel_chat()` use. Default `FALSE`.
#'
#' @return A tibble. For `.what = "arguments"`: `provider`, `verb`, `argument`,
#'   `default`, `fn`. For `.what = "media"`: `provider`, `media` (`image`, `pdf`,
#'   `audio`, `video`, `files`) and `supported`.
#'
#' @examples
#' provider_capabilities(claude(), .verb = "chat")
#'
#' provider_capabilities(.argument = ".thinking")
#'
#' provider_capabilities(.what = "media")
#' @export
provider_capabilities <- function(.provider = NULL,
                                  .verb = NULL,
                                  .argument = NULL,
                                  .what = c("arguments", "media"),
                                  .internal = FALSE) {
  .what <- rlang::arg_match(.what)
  c(".verb must be NULL or a character vector" = is.null(.verb) || is.character(.verb),
    ".argument must be NULL or a character vector" = is.null(.argument) || is.character(.argument),
    ".internal must be TRUE or FALSE" = is.logical(.internal) && length(.internal) == 1,
    ".verb and .argument apply to .what = \"arguments\" only" =
      .what == "arguments" || (is.null(.verb) && is.null(.argument))) |>
    validate_inputs()

  if (is.null(.provider)) {
    metas <- lapply(all_provider_names(), provider_metadata)
  } else {
    given <- if (is.call(.provider) || is.function(.provider) || is.character(.provider)) {
      if (is.character(.provider)) as.list(.provider) else list(.provider)
    } else {
      stop("`.provider` must be a provider call, a provider name or NULL.", call. = FALSE)
    }
    metas <- lapply(given, provider_metadata)
  }

  if (.what == "media") {
    out <- purrr::map(metas, function(m) {
      tibble::tibble(
        provider  = m$provider_name,
        media     = MEDIA_TYPES,
        supported = MEDIA_TYPES %in% m$media
      )
    }) |> purrr::list_rbind()
    return(out)
  }

  out <- purrr::map(metas, function(m) {
    verbs <- names(m$supported_args)
    if (!.internal) verbs <- setdiff(verbs, INTERNAL_VERBS)
    purrr::map(verbs, function(v) {
      args <- m$supported_args[[v]]
      if (length(args) == 0) {
        args <- NA_character_
        defaults <- NA_character_
      } else {
        defaults <- unname(m$supported_defaults[[v]])
      }
      tibble::tibble(
        provider = m$provider_name,
        verb     = v,
        argument = args,
        default  = defaults,
        fn       = unname(m$functions[v])
      )
    }) |> purrr::list_rbind()
  }) |> purrr::list_rbind()

  if (!is.null(.verb)) out <- out[out$verb %in% .verb, ]
  if (!is.null(.argument)) out <- out[out$argument %in% .argument, ]
  out
}
