#' Write a header-only PNG with the given pixel size and return its path.
#' @noRd
fake_png <- function(.w, .h, .dir = withr::local_tempdir(.local_envir = parent.frame()), .name = "a.png") {
  be32 <- \(n) as.raw(c(n %/% 16777216, (n %/% 65536) %% 256, (n %/% 256) %% 256, n %% 256))
  path <- file.path(.dir, .name)
  writeBin(c(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)), be32(13L), charToRaw("IHDR"),
             be32(.w), be32(.h), as.raw(c(8, 2, 0, 0, 0))), path)
  path
}

#' Write a header-only JPEG with the given pixel size and return its path.
#' @noRd
fake_jpeg <- function(.w, .h, .dir = withr::local_tempdir(.local_envir = parent.frame()), .name = "a.jpg") {
  path <- file.path(.dir, .name)
  writeBin(as.raw(c(0xff, 0xd8, 0xff, 0xe0, 0x00, 0x04, 0x00, 0x00,
                    0xff, 0xc0, 0x00, 0x11, 0x08, .h %/% 256, .h %% 256, .w %/% 256, .w %% 256,
                    3, 1, 0x22, 0, 2, 0x11, 1, 3, 0x11, 1, 0xff, 0xd9)), path)
  path
}

#' Write the given bytes to a temporary file and return its path.
#' @noRd
fake_file <- function(.name, .bytes = as.raw(1:32), .dir = withr::local_tempdir(.local_envir = parent.frame())) {
  path <- file.path(.dir, .name)
  writeBin(.bytes, path)
  path
}

#' Write a one-page PDF with a line of text and return its path.
#' @noRd
fake_pdf <- function(.dir = withr::local_tempdir(.local_envir = parent.frame())) {
  path <- file.path(.dir, "p.pdf")
  grDevices::pdf(path)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "hello pdf")
  grDevices::dev.off()
  path
}
