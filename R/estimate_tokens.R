#' Fast GPT-4 Token Estimator
#'
#' Estimate the number of tokens in a given text using a fast tokenization algorithm.
#'
#' @param .text The input text to estimate tokens from.
#' @return The estimated number of tokens in the input text.
#' @export
fast_gpt4_token_estimator <- function(.text){
  # Regex pattern for tokenization
  split_regexp <- "(?i:'s|'t|'re|'ve|'m|'ll|'d)|[^\\r\\n\\p{L}\\p{N}]?\\p{L}+|\\p{N}{1,3}| ?[^\\s\\p{L}\\p{N}]+[\\r\\n]*|\\s*[\\r\\n]+|\\s+(?!\\S)|\\s+"
  
  # Extract potential tokens using regex
  word_list <- stringr::str_extract_all(.text, split_regexp)[[1]]
  
  # Replace leading spaces with Ġ
  word_list <- ifelse(stringr::str_detect(word_list, "^\\s"), stringr::str_replace(word_list, "^\\s", "Ġ"), word_list)
  
  # Count tokens found directly in the vocabulary
  n_found_directly <- length(word_list[word_list %in% names(gpt4_vocab)])
  
  # Estimate compound tokens
  n_compound_tokens <- word_list[word_list %in% names(gpt4_vocab) == FALSE] |>
    sapply(function(x) ceiling(nchar(x)/4)) |>
    sum()
  
  # Total estimated tokens
  n_found_directly + n_compound_tokens
}

#' Estimate Tokens
#'
#' Estimate the number of tokens for an .llm message history
#'
#' @param .llm A tidyllm object
#' @return The estimated number of tokens in the input list of language model responses.
#' @export
estimate_tokens <- function(.llm){
  #Very simnple estimator using the text only part of a message
  tokens <- sapply(.llm$to_api_format("groq"), function(x){
    fast_gpt4_token_estimator(x$content)}) |> sum()
  
  return(tokens)
}

#gpt4_vocab <- load(file = here::here("data","gpt4_vocab.rda"))
#fast_gpt4_token_estimator("We document a substantial decline in cognitive and social interactive abilities and in GPAs among entering teachers. Then, using matched student-teacher data, we find that teacher abilities have a negligible impact on average student achievement. This finding hides interesting heterogeneities. In particular, an increase in teachers' cognitive (social) abilities increases (reduces) the achievement gap between high- and low-aptitude students. Teacher cognitive and social abili")

#save(gpt4_vocab, file = here::here("data","gpt4_vocab.rda"))




