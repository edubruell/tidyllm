devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("url_check")

SITE        <- "https://edubruell.github.io/tidyllm/"
CRAN_FILES  <- c("DESCRIPTION", "README.md", "NEWS.md",
                 list.files("man", pattern = "\\.Rd$", full.names = TRUE),
                 list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE))
ARTICLES    <- list.files("vignettes/articles", pattern = "\\.Rmd$", full.names = TRUE)
PLACEHOLDER <- "my-|example|abc123|llm\\.my-"
SKIP_HOSTS  <- c("localhost", "127.0.0.1", "example.com", "example.org")

extract_urls <- function(files) {
  rows <- lapply(files, function(f) {
    txt  <- readLines(f, warn = FALSE)
    hits <- regmatches(txt, gregexpr("https?://[^][[:space:]<>\"'`(){}\\\\]+", txt))
    urls <- unlist(hits)
    if (length(urls) == 0) return(NULL)
    data.frame(file = f, url = sub("[.,;:!?*_]+$", "", urls), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(data.frame(file = character(), url = character()))
  host <- sub("^https?://([^/:]+).*$", "\\1", out$url)
  unique(out[!host %in% SKIP_HOSTS, ])
}

llt_test("no http:// URLs in files CRAN reads", {
  urls <- extract_urls(CRAN_FILES)
  bad  <- urls[grepl("^http://", urls$url), ]
  llt_expect_true(nrow(bad) == 0,
                  paste0("Use https:// for:\n    ",
                         paste(sprintf("%s  (%s)", bad$url, bad$file), collapse = "\n    ")))
})

llt_test("no URL ends in punctuation or a line break artefact", {
  txt <- unlist(lapply(CRAN_FILES, function(f) {
    l <- readLines(f, warn = FALSE)
    m <- regmatches(l, gregexpr("https?://[^][[:space:]<>\"'`(){}\\\\]+[.,;:]+(?=[[:space:])}]|$)", l, perl = TRUE))
    unlist(m)
  }))
  llt_expect_true(length(txt) == 0,
                  paste0("Trailing punctuation inside URL:\n    ", paste(txt, collapse = "\n    ")))
})

llt_test("links to the package site point at pages that exist in docs/", {
  urls <- extract_urls(CRAN_FILES)
  own  <- urls[startsWith(urls$url, SITE), ]
  rel  <- sub("[#?].*$", "", sub(SITE, "", own$url, fixed = TRUE))
  rel[rel == "" | grepl("/$", rel)] <- paste0(rel[rel == "" | grepl("/$", rel)], "index.html")
  missing <- own[!file.exists(file.path("docs", rel)), ]
  llt_expect_true(nrow(missing) == 0,
                  paste0("Not in docs/, so a 404 on CRAN until the site is deployed:\n    ",
                         paste(sprintf("%s  (%s)", missing$url, missing$file), collapse = "\n    ")))
})

if (!requireNamespace("curl", quietly = TRUE) || !curl::has_internet()) {
  cat("  [skip] network checks: no internet connection\n")
  llt_report("url_check")
} else {
  llt_test("CRAN incoming URL check (urlchecker::url_check)", {
    if (!requireNamespace("urlchecker", quietly = TRUE)) {
      stop("install.packages('urlchecker') to run the check CRAN runs")
    }
    res <- urlchecker::url_check(".")
    llt_expect_true(nrow(res) == 0,
                    paste0("CRAN would flag:\n    ",
                           paste(sprintf("%s  [%s %s]  in %s%s", res$URL, res$Status, res$Message, res$From,
                                         ifelse(nzchar(res$New), paste0("  -> ", res$New), "")),
                                 collapse = "\n    ")))
  })

  llt_test("URLs in pkgdown articles answer and have not moved (not read by CRAN)", {
    urls <- extract_urls(ARTICLES)
    urls <- urls[!grepl(PLACEHOLDER, urls$url), ]
    uniq <- unique(urls$url)
    reqs <- lapply(uniq, function(u) {
      httr2::request(u) |>
        httr2::req_timeout(20) |>
        httr2::req_user_agent("Mozilla/5.0 (tidyllm url_check)") |>
        httr2::req_error(is_error = function(resp) FALSE)
    })
    resps <- httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE)
    problems <- vapply(seq_along(uniq), function(i) {
      r <- resps[[i]]
      if (!inherits(r, "httr2_response")) return("no answer")
      st <- httr2::resp_status(r)
      if (st %in% c(404L, 410L)) return(as.character(st))
      final <- httr2::resp_url(r)
      if (!identical(sub("/$", "", sub("#.*$", "", final)), sub("/$", "", sub("#.*$", "", uniq[i])))) {
        return(paste("moved to", final))
      }
      ""
    }, character(1))
    bad <- which(nzchar(problems))
    llt_expect_true(length(bad) == 0,
                    paste0("Stale in articles:\n    ",
                           paste(sprintf("%s  [%s]  in %s", uniq[bad], problems[bad],
                                         vapply(uniq[bad], function(u) paste(unique(urls$file[urls$url == u]), collapse = ", "), "")),
                                 collapse = "\n    ")))
  })

  llt_report("url_check")
}
