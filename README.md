# tidyllm <a href="https://edubruell.github.io/tidyllm/"><img src="man/figures/logo.png" align="right" height="139" alt="tidyllm website" /></a>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**tidyllm** is an R package designed to access various large language model APIs, including **Claude**, **ChatGPT**, **Groq**, **Mistral**, and local models via **Ollama**. Built for simplicity and functionality, it helps you generate text, analyze media, and integrate model feedback into your data workflows with ease.

## Features

- **Multiple Model Support**: Seamlessly switch between various model providers like Claude, ChatGPT, Groq, Mistral or Ollama using the best of what each has to offer.
- **Media Handling**: Extract and process text from PDFs and capture console outputs for messaging. Upload imagefiles or the last plotpane to multimodal models.
- **Interactive Messaging History**: Manage an ongoing conversation with models, maintaining a structured history of messages and media interactions, which are automatically formatted for each API
- **Batch processing:** Efficiently handle large workloads with Anthropic and OpenAI batch processing APIs, reducing costs by up to 50%.
- **Tidy Workflow**: Use R's functional programming features for a side-effect-free, pipeline-oriented operation style.

## Installation

To install **tidyllm** from CRAN, use:

```r
install.packages("tidyllm")
```

Or for the development version from GitHub:
```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("edubruell/tidyllm")
```

## Basic Example

Here’s a quick  example using tidyllm to describe an image using the Claude model 
to and follow up with local open-source models:

```R
library("tidyllm")

# Describe an image with  claude
conversation <- llm_message("Describe this image", 
                              .imagefile = here("image.png")) |>
  claude()

# Use the description to query further with groq
conversation |>
  llm_message("Based on the previous description,
  what could the research in the figure be about?") |>
  ollama(.model = "gemma2")
```

For more examples and advanced usage, check the [Get Started vignette](https://edubruell.github.io/tidyllm/articles/tidyllm.html).

Please note: To use **tidyllm**, you need either an installation of **ollama** or an active API key for one of the supported providers (e.g., Claude, ChatGPT). See the [Get Started vignette](https://edubruell.github.io/tidyllm/articles/tidyllm.html) for setup instructions.

## Learn More

For detailed instructions and advanced features, see:

- [Get Started with tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm.html)
- [Changelog](https://edubruell.github.io/tidyllm/news/)
- [Documentation](https://edubruell.github.io/tidyllm/)
- Use-case oriented articles: 
  - [Classifying Texts with tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_classifiers.html)
  - [Structured Question Answering from PDFs](https://edubruell.github.io/tidyllm/articles/tidyllm-pdfquestions.html)
  - [Generate Synthetic Survey Data](https://edubruell.github.io/tidyllm/articles/tidyllm-synthetic-data.html)
  
## Contributing

We welcome contributions! Feel free to open issues or submit pull requests on [GitHub](https://github.com/edubruell/tidyllm).

## License

This project is licensed under the MIT License - see the [LICENSE](https://opensource.org/licenses/MIT) file for details.

