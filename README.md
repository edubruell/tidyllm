<div style="padding-top:1em; padding-bottom: 0.5em;">
<img src="man/tidyllm.png" width = 120 align="right" />
</div>

# TidyLLM: Tidy Large Language Model Integration for R
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


**TidyLLM** is an R package designed to access various large language model APIs, including **Claude**, **ChatGPT**, and **Groq** (using their unique dedicated hardware accelerators) or even local models via **Ollama**. This package is built with simplicity and functionality in mind. Whether you're looking to generate text, analyze media, or integrate rich model feedback into your RCode, **TidyLLM** provides a unified interface to get the job done.

## Features

- **Multiple Model Support**: Seamlessly switch between various model providers like Claude, ChatGPT, Groq or Ollama using the best of what each has to offer.
- **Media Handling**: Extract and process text from PDFs and capture console outputs for messaging. Upload imagefiles or the last plotpane to multimodal models.
- **Interactive Messaging History**: Manage an ongoing conversation with models, maintaining a structured history of messages and media interactions, which are automatically formatted for each API
- **Stateful handling of rate limits:** API rate limits are handled statefully within each R Session and API functions can wait automatically for rate limits to reset
- **Tidy Workflow**: Making use of R's functional programming features for a side-effect-free, pipeline-oriented operation style that feels natural to R users.

## Requirements

You either need a local version of [ollama](https://ollama.com/) with some models set up or you 
need to setup your API keys for use in R for each API you want to use. 

1. For Anthropic you can create an API key in your [Anthropic account settings
page](https://console.anthropic.com/settings/keys) and set it by

``` r
Sys.setenv(ANTHROPIC_API_KEY = "ANTHROPIC-API-KEY-GOES-HERE")
```

2. For ChatGPT you can create an API key in  [API pane of the OpenAI platform
page](https://platform.openai.com/api-keys) and set it by

``` r
Sys.setenv(OPENAI_API_KEY = "OPENAI-API-KEY-GOES-HERE")
```

3. For groq you can set the API key on the [Groq console page](https://console.groq.com/keys) and set it by
``` r
Sys.setenv(GROQ-API_KEY = "GROQ-API-KEY-GOES-HERE")
```

Or you can do it persistently (session-wide), by assigning the keys in your
`.Renviron` file. For this, execute `usethis::edit_r_environ()`, and add a
line with with an API key in this file, for example:

``` r
ANTHROPIC_API_KEY=XX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```

## Getting Started

To install **tidyllm** from CRAN , use:

```R
install.packages("tidyllm")
```

Or install a current development version directly from GitHub using devtools:

```R
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install TidyLLM from GitHub
devtools::install_github("edubruell/tidyllm")
```
## Basic Usage Example

Here’s a quick run-through using **tidyllm** to send image descriptions through the Claude model and then follow-up with task-specific queries on Groq's open-source models.
All communication to the APIs is done via the `llm_message()` function that allows you to add a message to a conversation with language models. Different model-specific functions like `chatgpt()` or `claude()` 
allow you to send message histories to different models. 

```R
library("tidyllm"")

# Describe an image with a llava model on ollama
conversation <- llm_message("Describe this image",
                      .imagefile = here("image.png")) |>
            ollama(.model="llava")

# Use the description to query further with groq
conversation |>
  llm_message("Based on the previous description, what could the research here be about?") |>
  groq()
```
## Functions Overview

- **`llm_message()`**: Create or update a message object, adding prompts or media content.
- **`claude()`**: Send and receive messages from Anthropic's Claude models
- **`chatgpt()`**: Send and receive messages from OpenAI's ChatGPT models.
- **`ollama()`**: Send and receive messages to the ollama API (to work with local models)
- **`groq()`**: Interact with Groq's fast open-source models, taking advantage of their dedicated hardware accelerators for efficient processing.
- **`last_reply()`**: Fetch the most recent assistant's response from a message history.
- **`rate_limit_info()`**: Get a tibble of current rate limits of `claude()`, `groq()` or `chatgpt()`
- **`ollama_list_models`**: Get a tibble of available ollama-models

### Sending R outputs to the language model

`llm_message()` has an optional argument `.f` in which you can specify a (anonymous) function, which 
will be run and which console output will be captured and appended to the message when you run it: 

```r
# Some example data to show how passing R outputs to models works.
library(tidyverse)

example_data <- tibble(gender              = sample(c("Male", "Female"), 5000, replace = TRUE),
                       occupational_status = sample(c("Employed", "Unemployed", "Student", "Retired"), 5000, replace = TRUE),
                       age                 = rnorm(5000, mean = 35, sd = 10), 
                       education           = sample(c("High School", "Some College", "Bachelor's Degree", "Master's Degree", "Doctorate"), 5000, replace = TRUE), 
                       years_of_experience = rnorm(5000, mean = 10, sd = 5), 
                       industry_sector     = sample(c("Manufacturing", "Finance", "Healthcare", "Retail", "Education"), 5000, replace = TRUE)) %>%
  mutate(wage = 50000 
         + 10000 * (gender == "Male") 
         + 2000 * (occupational_status == "Employed") 
         + 500 * age 
         + 5000 * as.integer(factor(education, levels = c("High School", "Some College", "Bachelor's Degree", "Master's Degree", "Doctorate")))
         + 1000 * years_of_experience 
         + 3000 * (industry_sector == "Finance")
         + 1500 * rnorm(5000))

#Make a simple regression table with etable that we will also use in the example
#This code can be added embeded in a function in the .f arguement. Then we can 
#automatically add its output to the prompt.
library("fixest")
feols(wage ~ age + csw0(gender,education), example_data, vcov = 'hetero') %>%
  etable()

#Let's add a function .f which console output is also sent to Claude
llm_message("Please give me an interpretation of the results in column 3 of the regression table below. 
      What do the coefficients of the education variable mean? 
      What is the baseline level for education the coefficients compare to?
",
     .f = ~{ #Here is how you add the output of code. All output from the function(){} is captured and added to the prompt
       print("Here are counts of all levels of the education variable including the baseline:")
       example_data %>%
         count(education) %>%
         print()
       
       print("Here is a regression table")
       feols(wage ~ age + csw0(gender,education), example_data, vcov = 'hetero') %>%
         etable()
     }) |>
     claude() 
```

### Sending images or captured plots to multimodal models

You can also send images to a multimodal models: 
```r
llm_message("Describe this image",
                      .imagefile = here("image.png")) |>
            ollama(.model="llava")
```

Or even capture the last plot pane and send it to a model: 

```r
library("tidyverse")
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "Car Weight vs. Miles per Gallon",
       x = "Weight (1000 lbs)",
       y = "Miles per Gallon")

# Call the function with the ggplot and a prompt
llm_message("Please analyze the relationship between car weight and miles per gallon based on the provided plot.",
  .capture_plot = TRUE) |>
  chatgpt()
```
### Adding PDFs to messages

The `llm_message()` function also supports extracting text from PDFs and including it in the message. This allows you to easily provide context from a PDF document when interacting with the AI assistant.

To use this feature, you need to have the `pdftools` package installed. If it is not already installed, you can install it with:

```r
install.packages("pdftools")
```

To include text from a PDF in your prompt, simply pass the file path to the `.pdf` argument of the `chat` function:

```r
llm_message("Please summarize the key points from the provided PDF document.", 
     .pdf = "path/to/example_document.pdf") |>
     ollama()
```

The package will automatically extract the text from the PDF  and include it in the prompt sent to the an API. The text will be wrapped in `<pdf>` tags to clearly indicate the content from the PDF:

```
Please summarize the key points from the provided PDF document.

<pdf filename="example_document.pdf">
Extracted text from the PDF file...
</pdf>
```

### Getting the last reply (as raw text or structured data)

You can retrieve the last assistant reply from a message chain with `last_reply()`. Typically, it returns a character vector with the text of the assistant's reply. However, if API functions have requested replies in JSON mode, it can directly validate and return them as structured output. The function handles these different response types automatically.

If a JSON reply is detected, it returns a list with the following fields:
- `parsed_content`: The parsed JSON content (or `NULL` in case of parsing errors).
- `raw_response`: The direct string format of the reply.
- `is_parsed`: A flag set to `TRUE` if JSON parsing was successful, or `FALSE` otherwise.

You can also force standard raw text replies, even when JSON mode is detected, using the `.raw` argument.

#### Example 1: Getting standard text replies

```r
reply_text <- llm_message("Please summarize the key points from the provided PDF document in 50 words.", 
     .pdf = "path/to/example_document.pdf") |>
     groq() |>
     last_reply()
```
#### Example 2: Getting structured replies from APIs in JSON mode

```r
address <- llm_message('Imagine an address in JSON format. Reply only with JSON.') |>
  ollama(.json = TRUE) |>  # API is asked to return JSON
  last_reply()

# Access the structured data
if (address$is_parsed) {
  address_data <- as.data.frame(address$parsed_content)
} else {
  # Fallback: Handle the raw text if parsing fails
  address_raw <- address$raw_response
}
```
All API functions have a `.json`-argument that enables JSON-mode. Note that `claude()` does not have an explicit JSON-mode in the API-request but you need to specify that you want only JSON-output and ideally your shema in the prompt to the assistant.

### API parameters

Different API functions support different model parameters like  how deterministic the response should be via parameters like temperature. Please read API-documentation and the documentation of the model functions for specific examples.

```r

temp_example <- llm_message("Explain how temperature parameters work in large language model.")

#per default it is non-zero
temp_example |> groq()

#Temperature sets the randomness of the answer
#0 is one extreme where the output becomes fully deterministic. 
#Else the next token is allways sampled from a list of the most likely tokens. Here only the most likely token is used every time.
temp_example |> groq(.temperature=0)
temp_example |> groq(.temperature=0)# Same answer

```
### Experimental features

At the moment `ollama()`, `chatgpt()` and `claude()` support real-time streaming of reply tokens to the console while the model works with the `.stream=TRUE` argument. This is not super useful in the context of  data-analysis centered workflows, but gives you slightly better feedback on how your model works. We recommend using non-streaming response for production tasks though. Error handling in the callback functions for streaming responses is implemented differently for each API and differs in quality at the moment. 

### Changes in the current development version 0.1.1: JSON Mode Enhancements and Changes

In version 0.1.1, JSON mode is now more widely supported across all API functions, allowing for structured outputs when APIs support them. The `.json` argument is now passed only to API functions, specifying how the API should respond, it is not needed anymore in `last_reply()`

Additionally, the behavior of the `last_reply()` function has changed. It now automatically handles JSON replies by parsing them into structured data and falling back to raw text in case of errors. You can still force raw text replies even for JSON output using the `.raw` argument.

**Note:** These changes may introduce breaking behavior in workflows that relied on the previous handling of JSON replies, so please review any code that depends on `last_reply()` or JSON-mode API responses.



 
