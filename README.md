<div style="padding-top:1em; padding-bottom: 0.5em;">
<img src="tidyllm.png" width = 120 align="right" />
</div>

# TidyLLM: Tidy Large Language Model Integration for R

**TidyLLM** is an R package designed to access various large language model APIs, including **Claude**, **ChatGPT**, and **Groq** (using their unique dedicated hardware accelerators) or even local models via **Ollama**. This package is built with simplicity and functionality in mind. Whether you're looking to generate text, analyze media, or integrate rich model feedback into your RCode, **TidyLLM** provides a unified interface to get the job done.

## Features

- **Multiple Model Support**: Seamlessly switch between various model providers like Claude, ChatGPT, Groq or Ollama using the best of what each has to offer.
- **Media Handling**: Extract and process text from PDFs and capture console outputs for messaging. Upload imagefiles or the last plotpane to multimodal models.
- **Interactive Messaging History**: Manage an ongoing conversation with models, maintaining a structured history of messages and media interactions, which are automatically formatted for each API
- **Statefull handling of rate limits:** API rate limits are handled statefully within each R Session and API functions can wait automatically for rate limits to reset
- **Tidy Workflow**: Making use of R's functional programming features for a side-effect-free, pipeline-oriented operation style that feels natural to R users.

## Requirements

You either need a local version of [ollama](https://ollama.com/) with some models set up or you 
need to setup your API keys for use in R for each API you want to use. 

1. For Anthropic you can create an API key in your [Anthropic account settings
page](https://console.anthropic.com/settings/keys) and set it with

``` r
Sys.setenv(ANTHROPIC_API_KEY = "ANTHROPIC-API-KEY-GOES-HERE")
```

2. For ChatGPT you can create an API key in  [API pane of the OpenAI platform
page](https://platform.openai.com/api-keys) and set it with

``` r
Sys.setenv(OPENAI_API_KEY = "OPENAI-API-KEY-GOES-HERE")
```

3. For groq you can set the API key on the [Groq console page](https://console.groq.com/keys) and set it with
``` r
Sys.setenv(GROQ-API_KEY = "GROQ-API-KEY-GOES-HERE")
```

Or you can do it persistent (session-wide), by assigning the keys in your
`.Renviron` file. For this, execute `usethis::edit_r_environ()`, and add a
line with with an API key in this file, for example:

``` r
ANTHROPIC_API_KEY=XX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```

## Getting Started

Install **TidyLLM** directly from GitHub using devtools:

```R
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install TidyLLM from GitHub
devtools::install_github("edubruell/tidyllm")
```
## Basic Usage Example

Here’s a quick run-through using **TidyLLM** to send image descriptions through the Claude model and then follow-up with task-specific queries on Groq's open-source models.
All communication to the APIs is done via the `llm_message()` function that allows you to add a message to a conversation with language models. Different model-specific functions like `chatgpt()` or `claude()` 
allow you to send message histories to different models. 

```R
library(tidyllm)

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
- **`claude()`**: Send and receive messages  Anthropic's Claude models
- **`chatgpt()`**: Send and receive messages from OpenAI's ChatGPT models.
- **`ollama()`**: Send and receive messages to the ollama API (to work with local models)
- **`groq()`**: Interact with Groq's fast open-source models, taking advantage of their dedicated hardware accelerators for efficient processing.
- **`last_reply()`**: Fetch the most recent assistant's response from a message history.
- **`estimate_tokens()`**: Estimate the number of gpt-4 tokens in a message history.

### Sending R outputs to the language model

`llm_message()` has an optional argument `.f` in which you can specify a (anonymous) function, which 
will be run and which console output will be captured and appended to the message, when you run it: 

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
library(tidyverse)
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

To include text from a PDF file in your prompt, simply pass the file path to the `.pdffile` argument of the `chat` function:

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

### Getting the last reply (as a string)

Getting the last reply from a message chain as a character vector is done with `last_reply()`. Other functions to extract useful parts from conversations will be added.

```r
reply_pdf <- llm_message("Please summarize the key points from the provided PDF document.", 
     .pdf = "path/to/example_document.pdf") |>
     groq() |>
     last_reply()
```

### Directly get structured data as reply
Using the ability of large language models to output valid JSON, tidyllm can also directly get replies directly as structured data, which is easy to use in R. 
```r
de_books <- llm_message('Imagine a list of german books in JSON-format following this example:
{
  "books": [
    {"title": "To Kill a Mockingbird", "author": "Harper Lee", "genre": "Fiction", "price": "$8.99"},
    {"title": "1984", "author": "George Orwell", "genre": "Dystopian", "price": "$7.99"},
    {"title": "The Great Gatsby", "author": "F. Scott Fitzgerald", "genre": "Fiction", "price": "$6.99"},
    {"title": "Pride and Prejudice", "author": "Jane Austen", "genre": "Romance", "price": "$5.99"},
    {"title": "Moby Dick", "author": "Herman Melville", "genre": "Adventure", "price": "$10.99"}
  ]
}
') |>
  ollama(.json=TRUE) |>
  last_reply(.json=TRUE)
  
as.data.frame(de_books) 
```
At the moment this feature is limited to `ollama()`-models that support json-mode.

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

At the moment `ollama()` supports realtime streaming of reply tokens to the console while the model works. This is not super useful in the context of a data-analysis centered workflow but might be added to other API-functions anyway. 

## Future Work

- **Tool use:** Add support for models ability to use tools
- **Expand ollama-API:** Add additional ollama functions and support for embedding models
 
