
#' Batch Process PDF into LLM Messages
#'
#' This function processes a PDF file page by page. For each page, it extracts the text
#' and converts the page into an image. It creates a list of LLMMessage objects with
#' the text and the image for multimodal processing. Users can specify a range of pages
#' to process and provide a custom function to generate prompts for each page.
#'
#' @param .pdf Path to the PDF file.
#' @param .general_prompt A default prompt that is applied to each page if `.prompt_fn` is not provided.
#' @param .system_prompt Optional system prompt to initialize the LLMMessage (default is "You are a helpful assistant").
#' @param .page_range A vector of two integers specifying the start and end pages to process. If NULL, all pages are processed.
#' @param .prompt_fn An optional custom function that generates a prompt for each page. The function takes the page text as input 
#' and returns a string. If NULL, `.general_prompt` is used for all pages.
#' 
#' @return A list of LLMMessage objects, each containing the text and image for a page.
#' @import pdftools
#' @import base64enc
#' @export
pdf_page_batch <- function(.pdf, 
                           .general_prompt, 
                           .system_prompt = "You are a helpful assistant",
                           .page_range = NULL,
                           .prompt_fn = NULL) {
  
  # Validate the inputs using validate_inputs
  validate_inputs(c(
    "The 'pdftools' package is required to read PDF files. Please install it." = requireNamespace("pdftools", quietly = TRUE),
    "The .pdf file must exist" = file.exists(.pdf),
    "The .general_prompt must be a non-empty string" = is.character(.general_prompt) && nchar(.general_prompt) > 0,
    "The .system_prompt must be a non-empty string" = is.character(.system_prompt) && nchar(.system_prompt) > 0,
    "If provided, .page_range must be a vector of two integer-valued numerics" = is.null(.page_range) || (
      is.numeric(.page_range) && length(.page_range) == 2 && 
        is_integer_valued(.page_range[1]) && is_integer_valued(.page_range[2])
    ),
    "The page range must be positive and start before the end" = is.null(.page_range) || (.page_range[1] > 0 && .page_range[1] <= .page_range[2])
  ))
  
  # Allow for anonymous prompt functions
  if (!is.null(.prompt_fn)) {.prompt_fn <- rlang::as_function(.prompt_fn)}
  
  # Extract text from PDF
  pdf_text <- pdftools::pdf_text(.pdf)
  num_pages <- length(pdf_text)
  
  # Limit to specified page range
  if (!is.null(.page_range)) {
    pdf_text <- pdf_text[.page_range[1]:min(.page_range[2], num_pages)]
  }
  
  message_list <- purrr::imap(pdf_text, function(text, i) {
    # Render the page as an image
    rendered_page <- pdftools::pdf_render_page(.pdf, page = i)
    
    # Convert the image to PNG format and then encode it as base64
    image_file <- tempfile(fileext = ".png")
    png::writePNG(rendered_page, image_file)
    
    # Generate the prompt for the current page
    llm_prompt <- if (is.null(.prompt_fn)) {
      paste0("Here are a picture of a page in a pdf and its text in <pdf></pdf> delimiters. 
           Please answer the following prompt:\n", .general_prompt, "\n<pdf>", text, "</pdf>")
    } else {
      .prompt_fn(text)
    }
    
    # Create an LLMMessage object for the page
    llm_message(
      .llm = llm_prompt, 
      .imagefile = image_file,
      .role = "user", 
      .system_prompt = .system_prompt
    )
  })
  
  # Return the list of LLMMessage objects
  return(message_list)
}

  
  