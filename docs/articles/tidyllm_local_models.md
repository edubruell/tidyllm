# Local Models with tidyllm

Running large language models locally has become a practical option for
researchers. Modern small models handle most annotation, extraction, and
classification tasks that previously required a paid cloud API, while
keeping data entirely on your machine. The three concrete advantages
are: **privacy**, sensitive data such as patient records, survey
responses, or proprietary documents never leaves your infrastructure;
**cost at scale**, once a model is running there are no per-request
charges, so annotating millions of documents is essentially free; and
**reproducibility**, a saved model file produces identical outputs
indefinitely, whereas cloud API models are updated or retired without
notice.

This article walks through two local inference tools that tidyllm
supports: [Ollama](https://ollama.com/), which prioritises ease of
setup, and [llama.cpp](https://github.com/ggerganov/llama.cpp), which
exposes more control for advanced workflows. It also covers how to use
[OpenRouter](https://openrouter.ai/) to test open-source models via a
cloud API before committing to running them locally.

## Ollama

Ollama is the easiest path to running models locally. It packages model
downloads, storage, and a local server into a single application that
requires no configuration to get started.

### Setup

Install Ollama from [ollama.com](https://ollama.com/) or via Homebrew on
macOS:

``` bash
brew install ollama
```

On macOS, Ollama runs as a background service after installation. On
Linux and Windows, start it manually with `ollama serve`. For most
setups, no further configuration is needed; tidyllm connects to Ollama’s
default address automatically.

### Downloading and managing models

tidyllm exposes the full Ollama model lifecycle from R. The
[`list_models()`](https://edubruell.github.io/tidyllm/reference/list_models.md)
verb shows what is already available on the machine:

``` r

list_models(ollama())
```

    ## # A tibble: 4 × 6
    ##   name                 size   format family  parameter_size quantization_level
    ##   <chr>                <chr>  <chr>  <chr>   <chr>          <chr>             
    ## 1 qwen3.5:4b           2.6 GB gguf   qwen3.5 4.0B           Q4_K_M            
    ## 2 qwen3.5:14b          9.0 GB gguf   qwen3.5 14.0B          Q4_K_M            
    ## 3 qwen3-embedding:0.6b 522 MB gguf   qwen3   0.6B           F16               
    ## 4 smollm2:1.7b         1.0 GB gguf   smollm2 1.7B           Q4_K_M

[`ollama_download_model()`](https://edubruell.github.io/tidyllm/reference/ollama_download_model.md)
pulls a model by name from the Ollama registry and shows a progress bar:

``` r

ollama_download_model("qwen3.5:4b")
```

    ## Pulling qwen3.5:4b...
    ## ✓ Downloaded qwen3.5:4b (2.6 GB)

It also accepts a full HuggingFace URL. GGUF is the standard file format
for distributing local models, and HuggingFace hosts thousands of them.
Any GGUF-based model there can be installed directly into Ollama without
leaving R:

``` r

ollama_download_model("https://huggingface.co/nsatya/SmolLM2-1.7B-Instruct")
```

    ## Pulling from https://huggingface.co/nsatya/SmolLM2-1.7B-Instruct...
    ## ✓ Downloaded smollm2:1.7b (1.0 GB)

To remove a model when you no longer need it:

``` r

ollama_delete_model("smollm2:1.7b")
```

### Chatting and embeddings

Chat with the default model (`qwen3.5:4b`) using the standard tidyllm
verb pattern:

``` r

reply <- llm_message("What is the difference between a tibble and a data frame in R?") |>
  chat(ollama())

get_reply(reply)
```

    ## [1] "A tibble is a modern reimagining of the data frame from the tidyverse. The main
    ## differences are that tibbles never convert strings to factors, never change column
    ## names, and print only the first ten rows by default, showing column types beneath
    ## the header. They also give more informative error messages when you access a column
    ## that does not exist."

The `.num_ctx` parameter controls how much text the model can read at
once, known as its *context window*. Text is measured in tokens, which
are roughly three-quarters of a word each. The function default is
`2048` tokens, which is quite short and much smaller than what modern
models like Qwen3.5 actually support. For long documents, increase it to
fit your input:

``` r

long_document |>
  llm_message() |>
  chat(ollama(.num_ctx = 32768))
```

For embeddings, use the
[`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md)
verb. Embeddings convert text into numeric vectors that capture meaning,
making them useful for similarity search and clustering. The default
embedding model is `qwen3-embedding:0.6b`, a purpose-built model for
this task:

``` r

c("neural networks", "deep learning", "baking bread") |>
  embed(ollama())
```

    ## # A tibble: 3 × 2
    ##   input           embeddings   
    ##   <chr>           <list>       
    ## 1 neural networks <dbl [1,024]>
    ## 2 deep learning   <dbl [1,024]>
    ## 3 baking bread    <dbl [1,024]>

## Finding the Right Model

The open-source landscape moves fast enough that a model recommended a
few months ago may already have been superseded. Here is how to navigate
it.

### Where to look

**[Chatbot Arena](https://lmarena.ai)** is the most reliable signal for
general instruction-following quality. Visitors vote on blind pairwise
comparisons between randomly assigned models, using their own prompts.
Because models cannot overfit to the prompts, the resulting rankings
reflect what people actually prefer. It is the best single answer to
“which model should I try first?”

**[Hugging Face Open LLM
Leaderboard](https://huggingface.co/spaces/open-llm-leaderboard/open_llm_leaderboard)**
tracks automated test scores across a range of academic benchmarks.
Useful for filtering clearly weak models and spotting improvements
between versions; less reliable as a final verdict because scores can be
inflated by training specifically for these tests.

**Your own pilot beats both.** If your task is classification,
extraction, or annotation, run 50–100 real examples through two or three
candidate models. The results will tell you more than any leaderboard
because they reflect your specific domain, label set, and prompt style.

### Understanding quantization

Models on HuggingFace are distributed as GGUF files in various
*quantization* levels. Quantization is the process of compressing a
model by storing its internal numbers at lower precision, similar to
saving an image as a smaller JPEG instead of a full-resolution PNG. A
more compressed model uses less memory and runs faster, but may produce
slightly worse outputs on complex tasks.

The label in the filename tells you how aggressively the model has been
compressed:

| Label | Quality vs. full precision | Memory use | Typical use |
|----|----|----|----|
| Q8_0 | Near-identical | Largest | Reference quality; use when you have enough GPU memory |
| Q4_K_M | Small, often imperceptible loss | Medium | Best everyday tradeoff |
| Q3_K_M | Noticeable on complex reasoning | Smaller | Only when memory is very tight |
| Q2_K | Significant degradation | Smallest | Experimentation only |

For most research tasks, Q4_K_M on a model one size class larger than
your first instinct is a reliable starting point. If a 7B model at
Q4_K_M does not perform well enough, try a 13B at Q4_K_M rather than
upgrading to Q8_0 on the 7B.

### The current model landscape

Model size is measured in *parameters*, the numerical weights learned
during training. More parameters generally means more capable, but also
more memory required and slower inference. As of early 2026, strong
open-weight families come from a range of labs:

| Family | Lab | Sizes | Notes |
|----|----|----|----|
| **Qwen3.5** | Alibaba | 0.8B–9B; 27B, 35B-A3B, 122B-A10B; 397B-A17B | 201 languages, 1M context, thinking mode on by default; default in tidyllm |
| **Mistral Small 4** | Mistral AI | 119B total / 6B active (MoE) | Reasoning, vision, and agentic coding unified; 256K context; Apache 2.0 |
| **Llama 4 Scout** | Meta | 109B total / ~17B active (MoE) | Native multimodal, 10M token context |
| **Gemma 3** | Google | 1B, 4B, 12B, 27B | Compact multimodal models; permissive license |
| **Kimi K2.5** | Moonshot AI | 1T total / 32B active (MoE) | Strong coding and vision; MIT license |
| **MiniMax M2.5** | MiniMax | large MoE | Near-frontier quality at a fraction of proprietary API cost |
| **DeepSeek V3.2** | DeepSeek | 671B MoE | Open-weight general model; best when hardware is not the constraint |
| **Phi-4** | Microsoft | 14B | Punches well above weight class on reasoning tasks |

Several of these are *Mixture-of-Experts* (MoE) models. Instead of
activating the entire network for every word, MoE models route each step
through only a small subset of specialised sub-networks. This means the
memory and speed cost depends on the *active* parameter count, not the
total. In the table, notation like “35B-A3B” means 35 billion total
parameters but only 3 billion active per step; it runs much more like a
3B model than a 35B one.

### Testing with OpenRouter before committing hardware

Running a large model locally requires significant hardware: a 13B model
at Q4_K_M needs around 10 GB of memory; a 70B model needs 40 GB or more.
Before investing in hardware or reserving compute time, test the model
via a cloud API first to confirm it works for your task.

[OpenRouter](https://openrouter.ai) provides a single API key for
hundreds of models, including the Qwen, Mistral, Llama, Kimi, Gemma, and
MiniMax families. Because tidyllm uses the same verb-and-provider
pattern for every provider, switching between candidates requires only
changing the `.model` argument:

``` r

prompt <- llm_message("Classify this job description into a one-sentence occupation label:
                        'I oversee a team of data engineers building our company data platform'")

result_qwen    <- prompt |> chat(openrouter(.model = "qwen/qwen3.5-35b-a3b"))
result_mistral <- prompt |> chat(openrouter(.model = "mistralai/mistral-small-4"))
result_kimi    <- prompt |> chat(openrouter(.model = "moonshotai/kimi-k2.5"))

get_reply(result_qwen)
get_reply(result_mistral)
get_reply(result_kimi)
```

    ## [1] "Data Engineering Manager: oversees a team responsible for building and maintaining the company data platform."
    ## [1] "Data Engineering Manager: leads engineers developing the organisation's core data infrastructure."
    ## [1] "Data Engineering Manager: manages a team of data engineers constructing the company's data platform."

[`openrouter_list_models()`](https://edubruell.github.io/tidyllm/reference/openrouter_list_models.md)
returns a table of all available models with pricing. You can filter it
by context length (how much text the model can process at once) and sort
by cost to shortlist candidates:

``` r

openrouter_list_models() |>
  filter(context_length >= 32000) |>
  arrange(prompt_price_per_million) |>
  select(id, name, context_length, prompt_price_per_million, completion_price_per_million)
```

    ## # A tibble: 8 × 5
    ##   id          name  context_length prompt_price_per_mil…¹ completion_price_per…²
    ##   <chr>       <chr>          <int>                  <dbl>                  <dbl>
    ## 1 qwen/qwen3… Qwen…        1000000                   0.1                    0.3 
    ## 2 mistralai/… Mist…         256000                   0.1                    0.3 
    ## 3 google/gem… Gemm…         131072                   0.1                    0.2 
    ## 4 microsoft/… Phi-4         131072                   0.14                   0.14
    ## 5 meta-llama… Llam…       10000000                   0.17                   0.6 
    ## 6 minimax/mi… Mini…        1000000                   0.2                    0.55
    ## 7 moonshotai… Kimi…         131072                   0.5                    1.5 
    ## 8 deepseek/d… Deep…         163840                   0.55                   2.19
    ## # ℹ abbreviated names: ¹​prompt_price_per_million, ²​completion_price_per_million

Prices are in US dollars per million tokens. Once a model performs well
on your task, find the corresponding GGUF on HuggingFace and run it
locally for free.
[`openrouter_credits()`](https://edubruell.github.io/tidyllm/reference/openrouter_credits.md)
tracks how much of your balance the pilot has consumed:

``` r

openrouter_credits()
```

    ## # A tibble: 1 × 3
    ##   total_credits total_usage remaining
    ##           <dbl>       <dbl>     <dbl>
    ## 1            15       0.153      14.8

## llama.cpp

Ollama is built on top of llama.cpp and covers most common use cases
well. Going directly to llama.cpp gives you access to features Ollama
does not expose: hard guarantees on output format (grammar constraints),
confidence scores for each prediction (log-probabilities), and finer
control over how the model generates text. It is worth the extra setup
effort when your workflow needs any of these.

### Setup

**Installation.** On macOS, install via Homebrew:

``` bash
brew install llama.cpp
```

On Linux, build from source or use a pre-built release from the
[llama.cpp GitHub releases
page](https://github.com/ggerganov/llama.cpp/releases). On Windows,
pre-built binaries are available from the same page; pick the variant
that matches your hardware (GPU-accelerated with CUDA for Nvidia or
Vulkan for AMD/Intel, or CPU-only if you have no dedicated GPU).

**Starting a server.** Unlike Ollama, llama.cpp does not manage models
for you: you point it at a GGUF file on disk and it serves that model
until you stop the process. The server command is `llama-server`:

``` bash
llama-server -m ~/models/Qwen3.5-8B-Q4_K_M.gguf -c 32768
```

The `-c` flag sets the context window size in tokens (32768 is a good
default for most tasks). Once started, the server listens on
`http://localhost:8080` by default.

**Embeddings require a separate process.** The embedding endpoint uses a
dedicated embedding model and must run on a different port:

``` bash
llama-server \
  -m ~/models/Qwen3-Embedding-0.6B-Q8_0.gguf \
  -c 512 --port 8081 --embeddings
```

You can run both commands in separate terminal tabs, or keep them
running in the background using a terminal multiplexer like
[tmux](https://github.com/tmux/tmux).

**Telling tidyllm where to look.** Add the following to your `.Renviron`
file (run
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)
to open it, then restart R):

    LLAMACPP_SERVER=http://localhost:8080
    LLAMACPP_MODEL_DIR=~/models

`LLAMACPP_SERVER` is the address tidyllm uses to reach the chat server.
`LLAMACPP_MODEL_DIR` is the folder where model files are stored and
where the management functions below will look for them.

### Downloading models from HuggingFace

HuggingFace is the primary source for GGUF files. tidyllm provides
functions to browse and download directly from R.

[`list_hf_gguf_files()`](https://edubruell.github.io/tidyllm/reference/list_hf_gguf_files.md)
shows all the quantization variants available for a given model
repository:

``` r

list_hf_gguf_files("Qwen/Qwen3.5-8B-GGUF")
```

    ## # A tibble: 5 × 3
    ##   filename               size_gb url                                            
    ##   <chr>                    <dbl> <chr>                                          
    ## 1 Qwen3.5-8B-Q2_K.gguf      3.19 https://huggingface.co/Qwen/Qwen3.5-8B-GGUF/re…
    ## 2 Qwen3.5-8B-Q3_K_M.gguf    4.02 https://huggingface.co/Qwen/Qwen3.5-8B-GGUF/re…
    ## 3 Qwen3.5-8B-Q4_K_M.gguf    5.16 https://huggingface.co/Qwen/Qwen3.5-8B-GGUF/re…
    ## 4 Qwen3.5-8B-Q6_K.gguf      6.6  https://huggingface.co/Qwen/Qwen3.5-8B-GGUF/re…
    ## 5 Qwen3.5-8B-Q8_0.gguf      8.54 https://huggingface.co/Qwen/Qwen3.5-8B-GGUF/re…

[`llamacpp_download_model()`](https://edubruell.github.io/tidyllm/reference/llamacpp_download_model.md)
downloads a specific file with a progress bar:

``` r

llamacpp_download_model(
  .repo     = "Qwen/Qwen3.5-8B-GGUF",
  .filename = "Qwen3.5-8B-Q4_K_M.gguf"
)
```

    ## Downloading Qwen3.5-8B-Q4_K_M.gguf from Qwen/Qwen3.5-8B-GGUF...
    ## [==============================] 5.16 GB / 5.16 GB
    ## ✓ Saved to ~/models/Qwen3.5-8B-Q4_K_M.gguf

[`llamacpp_list_local_models()`](https://edubruell.github.io/tidyllm/reference/llamacpp_list_local_models.md)
shows what GGUF files are already in your model directory, without
needing a running server:

``` r

llamacpp_list_local_models()
```

    ## # A tibble: 2 × 4
    ##   filename                       size_gb modified            path               
    ##   <chr>                            <dbl> <chr>               <chr>              
    ## 1 Qwen3.5-8B-Q4_K_M.gguf            5.16 2026-03-17 09:14:22 ~/models/Qwen3.5-8…
    ## 2 Qwen3-Embedding-0.6B-Q8_0.gguf    0.62 2026-03-10 14:03:05 ~/models/Qwen3-Emb…

To verify the server is running and see which model is loaded:

``` r

llamacpp_health()
list_models(llamacpp())
```

    ## $status
    ## [1] "ok"
    ## 
    ## $slots_idle
    ## [1] 1
    ## 
    ## $slots_processing
    ## [1] 0
    ## # A tibble: 1 × 3
    ##   id          object    created
    ##   <chr>       <chr>       <int>
    ## 1 local-model model  1742204062

### Chatting

Basic chat works through the same verb pattern as all other providers:

``` r

reply <- llm_message("Explain in one sentence what a mixture-of-experts model is.") |>
  chat(llamacpp())

get_reply(reply)
```

    ## [1] "A mixture-of-experts model routes each token through only a subset of specialised
    ## networks called experts, keeping inference cost low while maintaining the capacity
    ## of a much larger model."

### Confidence scores for annotation quality control

When you use a model for classification or annotation, not all
predictions are equally confident. Log-probabilities give you a numeric
confidence score for each output token: a score close to zero means the
model was highly certain; a strongly negative score means it was
genuinely uncertain between several options.

Passing `.logprobs = TRUE` stores these scores in the response metadata
alongside the reply:

``` r

result <- llm_message(
    "Reply with exactly one word. Classify the sentiment of this review:
     'The delivery was two days late but the product itself is solid.'"
  ) |>
  chat(llamacpp(.logprobs = TRUE, .top_logprobs = 3))

get_reply(result)
get_metadata(result)$api_specific$logprobs
```

    ## [1] "neutral"
    ## $content
    ## $content[[1]]
    ## $content[[1]]$token
    ## [1] "neutral"
    ## $content[[1]]$logprob
    ## [1] -0.847
    ## $content[[1]]$top_logprobs
    ## # A tibble: 3 × 2
    ##   token    logprob
    ##   <chr>      <dbl>
    ## 1 neutral   -0.847
    ## 2 mixed     -1.20 
    ## 3 positive  -2.03

A score of -0.85 on “neutral” means the model had genuine uncertainty;
the review is mixed and the model knows it. In a large annotation batch,
you can flag predictions where the winning score is better than -1 (high
confidence) for direct use, and route the rest to a larger model or
human review. Log-probability scores are also available on
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
and
[`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md).

### Grammar constraints for reliable structured output

Instruction-tuned models generally follow format instructions well, but
they can occasionally produce malformed output, especially with smaller
models or unusual inputs. Grammar constraints enforce the output format
at the deepest level of the model’s text generation process: the model
is physically prevented from producing tokens that would violate the
grammar, regardless of its size or how well it follows instructions.

This means even a small model running on a laptop will always return
valid JSON when you pass a `.json_schema`. The `.grammar` parameter goes
further: it accepts a grammar definition that can constrain output to
any pattern, not just JSON:

``` r

sentiment_grammar <- r"(root ::= ("positive" | "negative" | "neutral"))"

result <- llm_message(
    "Classify the sentiment of this review. Reply with one word only:
     'Fast shipping, exactly as described, would buy again.'"
  ) |>
  chat(llamacpp(.grammar = sentiment_grammar))

get_reply(result)
```

    ## [1] "positive"

### Local embeddings and reranking

The embedding server provides local dense vectors for similarity search
and clustering. Point the
[`llamacpp()`](https://edubruell.github.io/tidyllm/reference/llamacpp.md)
provider at it via the `.server` argument:

``` r

c("transformer architecture", "attention mechanism", "baking sourdough") |>
  embed(llamacpp(.server = "http://localhost:8081"))
```

    ## # A tibble: 3 × 2
    ##   input                    embeddings   
    ##   <chr>                    <list>       
    ## 1 transformer architecture <dbl [1,024]>
    ## 2 attention mechanism      <dbl [1,024]>
    ## 3 baking sourdough         <dbl [1,024]>

If you are building a document search system,
[`llamacpp_rerank()`](https://edubruell.github.io/tidyllm/reference/llamacpp_rerank.md)
can re-order a set of candidate documents by how relevant they are to a
query. This is more accurate than sorting by raw embedding similarity
alone, and runs on the same local server with no additional model:

``` r

llamacpp_rerank(
  .query     = "how does self-attention work",
  .documents = c(
    "Attention is All You Need introduced the transformer architecture.",
    "Sourdough bread uses a wild yeast starter for leavening.",
    "Self-attention computes a weighted sum of all positions in a sequence.",
    "BERT uses bidirectional transformers pre-trained on masked language modelling."
  )
)
```

    ## # A tibble: 4 × 3
    ##   index document                                                 relevance_score
    ##   <int> <chr>                                                              <dbl>
    ## 1     2 Self-attention computes a weighted sum of all positions…           0.921
    ## 2     0 Attention is All You Need introduced the transformer ar…           0.783
    ## 3     3 BERT uses bidirectional transformers pre-trained on mas…           0.641
    ## 4     1 Sourdough bread uses a wild yeast starter for leavening.           0.021

Combined with
[`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md) for
retrieval and
[`llamacpp_rerank()`](https://edubruell.github.io/tidyllm/reference/llamacpp_rerank.md)
for re-ranking, you have a fully local document search pipeline: no
cloud calls, no token costs, and reproducible results.

## Any OpenAI-Compatible Endpoint

Many universities, hospitals, and cloud providers run their own LLM
servers using tools like [vLLM](https://github.com/vllm-project/vllm) or
[text-generation-inference](https://github.com/huggingface/text-generation-inference).
These services typically expose the same API format as OpenAI, so they
work directly with tidyllm’s
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
provider by pointing it at a different server address:

``` r

llm_message("Summarise this paragraph in one sentence.") |>
  chat(openai(.api_url = "http://my-internal-server:8000/v1"))
```

For llama.cpp specifically, using
[`llamacpp()`](https://edubruell.github.io/tidyllm/reference/llamacpp.md)
with `LLAMACPP_SERVER` set in `.Renviron` is the cleaner path since it
also exposes the llama.cpp-specific parameters covered above. The
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
route is there when you are connecting to a generic institutional
endpoint that does not need any of those extras.
