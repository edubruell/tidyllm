# Structured Question Answering from PDFs

Navigating through a large collection of academic papers can be
time-consuming, especially when you’re trying to extract specific
insights or determine the relevance of each document to your research.
With **tidyllm**, you can streamline this process by automating the
extraction of structured answers directly from PDF documents using large
language models.

Imagine you have a folder of papers on the economic effects of
generative AI, and you need to assess how each paper is related to your
own research interests. This article guides you through setting up a
workflow that processes the first few pages of each paper, asks an AI
model targeted questions, and returns the answers in a structured
format, perfect for converting into a table for easy review and
analysis.

## Example Workflow

Imagine your folder looks something like this, many downloaded papers
but no structure yet:

``` r

library(tidyverse)
library(tidyllm)
dir("aipapers")
##  [1] "2018_Felten_etal_AILinkOccupations.pdf"                                                           
##  [2] "2024_Bick_etal_RapidAdoption.pdf"                                                                 
##  [3] "2024_Caplin_etal_ABCsofAI.pdf"                                                                    
##  [4] "2301.07543v1.pdf"                                                                                 
##  [5] "2302.06590v1.pdf"                                                                                 
##  [6] "2303.10130v5.pdf"                                                                                 
##  [7] "488.pdf"                                                                                          
##  [8] "88684e36-en.pdf"                                                                                  
##  [9] "ABCs_AI_Oct2024.pdf"                                                                              
## [10] "acemoglu-restrepo-2019-automation-and-new-tasks-how-technology-displaces-and-reinstates-labor.pdf"
## [11] "BBD_GenAI_NBER_Sept2024.pdf"                                                                      
## [12] "Deming-Ong-Summers-AESG-2024.pdf"                                                                 
## [13] "dp22036.pdf"                                                                                      
## [14] "FeltenRajSeamans_AIAbilities_AEA.pdf"                                                             
## [15] "JEL-2023-1736_published_version.pdf"                                                              
## [16] "Noy_Zhang_1.pdf"                                                                                  
## [17] "pb01-25.pdf"                                                                                      
## [18] "sd-2024-09-falck-etal-kuenstliche-intelligenz-unternehmen.pdf"                                    
## [19] "ssrn-4700751.pdf"                                                                                 
## [20] "SSRN-id4573321.pdf"                                                                               
## [21] "The Simple Macroeconomics of AI.pdf"                                                              
## [22] "w24001.pdf"                                                                                       
## [23] "w24871.pdf"                                                                                       
## [24] "w31161.pdf"                                                                                       
## [25] "w32430.pdf"
```

Our goal is to get a first overview of these papers and to give them
good file names.

### Step 1: Generating Messages for Each Document

First, we prepare a list of messages for all PDFs in the folder by
applying
[`llm_message()`](https://edubruell.github.io/tidyllm/dev/reference/llm_message.md)
with a prompt to the first five pages of each document. Even though
`gpt-6-luna` can process much longer documents, we limit the input to
five pages for demonstration purposes; the introduction is usually
enough to get a first overview of a paper.

Attach PDFs with
[`pdf_file()`](https://edubruell.github.io/tidyllm/dev/reference/pdf_file.md)
and the `.media` argument. On providers that support native PDF
rendering (Claude, Gemini), the file is sent as binary, preserving
images, tables, and formatting. On all other providers the text is
extracted automatically and wrapped in `<pdf>` tags in the prompt. Pass
`.text_extract = TRUE` to always force text extraction regardless of the
provider:

``` r

files <- list.files("aipapers", full.names = TRUE, pattern = "\\.pdf$")

document_tasks <- files |>
  map(\(f) llm_message(
    "Below are the first 5 pages of a document.
     Summarise the document based on the provided schema.",
    .media = pdf_file(f, pages = 1:5)
  ))
```

### Step 2: Defining the Schema for Structured Output

We define a schema that outlines the expected data types for each field
in the model’s responses. This enables the model to return answers in a
consistent, structured format that converts directly into a tibble.

``` r

document_schema <- tidyllm_schema(
  name             = "DocumentAnalysisSchema",
  Title            = field_chr("The full title of the provided document"),
  Authors          = field_chr("A semicolon-separated list of authors"),
  SuggestedFilename = field_chr("Suggest a filename in the format \"ReleaseYear_Author_etal_ShortTitle.pdf\". Use the publication year if available; otherwise, use XXXX."),
  Type             = field_fct("Is the document a Policy Report or a Research Paper based on its style, structure, and purpose?",
                               .levels = c("Policy", "Research")),
  Empirics         = field_chr("A 100 word description of empirical methods used, including data collection, statistical techniques, or analysis methods mentioned."),
  Theory           = field_chr("A 100 word outline of the primary theoretical framework or models discussed, if any."),
  MainPoint        = field_chr("A one-sentence summary of the main point raised"),
  Contribution     = field_chr("A short explanation of the main contributions claimed in the document"),
  KeyCitations     = field_chr("The four most frequently cited or critical references in the first 5 pages")
)
```

[`field_chr()`](https://edubruell.github.io/tidyllm/dev/reference/field_chr.md)
returns text,
[`field_dbl()`](https://edubruell.github.io/tidyllm/dev/reference/field_chr.md)
returns numbers, and
[`field_fct()`](https://edubruell.github.io/tidyllm/dev/reference/field_chr.md)
restricts to allowed categories via the `.levels` argument. Setting
`.vector = TRUE` on any field returns a list of values; here we keep a
flat structure so each document maps to a single-row tibble. `name` is a
special field providing an identifier for the schema.

### Step 3: Running the Analysis on a Sample Document

To test the setup, we run the analysis on a single document with
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md)
and the schema:

``` r

example_task <- document_tasks[[1]] |>
  chat(openai(.json_schema = document_schema,
              .model       = "gpt-6-luna"))
```

### Step 4: Extracting and Formatting the Results

[`get_reply_data()`](https://edubruell.github.io/tidyllm/dev/reference/get_reply_data.md)
extracts the model’s structured response:

``` r

get_reply_data(example_task)
## $Title
## [1] "A Method to Link Advances in Artificial Intelligence to Occupational Abilities"
## 
## $Authors
## [1] "Edward W. Felten; Manav Raj; Robert Seamans"
## 
## $SuggestedFilename
## [1] "2018_Felten_etal_AI_Occupational_Abilities.pdf"
## 
## $Type
## [1] "Research"
## 
## $Empirics
## [1] "The study combines the Electronic Frontier Foundation’s AI Progress Measurement data with the US Department of Labor’s O*NET occupational database. For 16 AI categories, the authors scale different performance metrics, estimate progress trends using a common linear rate with metric-specific offsets, and set progress to zero where historical data are insufficient. Computer science PhD students help map AI categories to 52 O*NET abilities. The authors weight estimated ability effects by each ability’s importance and prevalence across nearly 1,000 occupations, then aggregate occupation scores. As a validation test, they correlate 2010–2015 scores with planned 2018 BLS definition revisions: the correlation is 0.074 (p = 0.041)."
## 
## $Theory
## [1] "The paper uses a skills- and abilities-based view of labor: an occupation is understood as a bundle of abilities, with each ability varying in importance and prevalence across jobs. Advances in particular AI domains may affect the abilities used in occupations, so mapping AI progress to abilities and then to occupational requirements provides a way to compare relative exposure. This framework complements approaches that predict future automation susceptibility at the occupation level and those emphasizing variation in tasks among workers with the same occupational title. It does not provide a formal model of labor-market adjustment or determine whether AI substitutes for or complements workers; either outcome may occur."
## 
## $MainPoint
## [1] "The authors propose a method for linking measured advances in specific AI domains to occupational abilities and aggregating those links to compare which occupations may be affected."
## 
## $Contribution
## [1] "The paper introduces a reproducible framework that combines historical AI performance trends with O*NET ability requirements, enabling researchers and policymakers to assess the relative exposure of occupations and industries to advances in particular AI technologies. It also offers an initial validation: occupations with higher impact scores were modestly more likely to be scheduled for updated BLS definitions."
## 
## $KeyCitations
## [1] "Frey and Osborne (2017), “The Future of Employment,” provides an occupation-level automation-susceptibility approach that this method complements. Arntz, Gregory, and Zierahn (2016), “The Risk of Automation for Jobs in OECD Countries,” highlights task variation within occupations. Autor and Handel (2013), “Putting Tasks to the Test,” supports describing labor through job tasks and skills. Brynjolfsson, Mitchell, and Rock (2018), “What Can Machines Learn, and What Does it Mean for the Occupations and Industries?,” is a related approach to assessing AI and occupational effects."
```

The model returns a named list matching our schema, which
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
converts to a single row. We can also inspect token usage:

``` r

get_metadata(example_task)
```

    ## # A tibble: 1 × 7
    ##   model  timestamp           prompt_tokens completion_tokens total_tokens stream
    ##   <chr>  <dttm>                      <int>             <int>        <int> <lgl> 
    ## 1 gpt-6… 2026-10-02 15:20:13          4223              1051         5274 FALSE 
    ## # ℹ 1 more variable: api_specific <list>

At current `gpt-6-luna` batch pricing (\$0.05 per million input and
\$0.25 per million output tokens) the per-document cost is a fraction of
a cent; processing the 25 papers in this folder cost about 1.5 cents,
and a folder of 50 papers costs a few cents.

### Step 5: Scaling Up to a Whole Batch of Papers

After confirming the single-document analysis works, we extend the
workflow to process a full batch. Batch APIs from Anthropic and OpenAI
offer up to 50% savings compared to single requests, and batch rate
limits are separate from standard per-model limits so they don’t affect
your regular quota.

``` r

document_tasks |>
  send_batch(openai(.json_schema = document_schema,
                    .model       = "gpt-6-luna")) |>
  write_rds("document_batch.rds")
```

The output of
[`send_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_batch.md)
contains the input message list plus batch metadata (a `batch_id`
attribute and unique per-message identifiers). Batches are processed
within 24 hours; the example here completed in about 10 minutes.

> ⚠️ **Note:** Save the RDS file to disk. It acts as a checkpoint,
> letting you reload and check batch status or retrieve results across R
> sessions without resubmitting the batch.

Check whether a batch has completed:

``` r

read_rds("document_batch.rds") |>
  check_job(openai())
```

You can also list all OpenAI batches with `list_batches(openai())` or
view them in the [OpenAI batches
dashboard](https://platform.openai.com/batches/).

### Step 6: Getting Data from the Entire Batch

Once the batch is complete, fetch all responses with
[`fetch_batch()`](https://edubruell.github.io/tidyllm/dev/reference/fetch_batch.md):

``` r

results <- read_rds("document_batch.rds") |>
  fetch_batch(openai())
```

Map
[`get_reply_data()`](https://edubruell.github.io/tidyllm/dev/reference/get_reply_data.md)
and
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
over the batch output to produce a tidy table:

``` r

document_table <- results |>
  map(get_reply_data) |>
  map_dfr(as_tibble)

document_table
## # A tibble: 25 × 9
##    Title  Authors SuggestedFilename Type  Empirics Theory MainPoint Contribution
##    <chr>  <chr>   <chr>             <chr> <chr>    <chr>  <chr>     <chr>       
##  1 A Met… Edward… 2018_Felten_etal… Rese… The aut… The p… The pape… The authors…
##  2 The R… Alexan… 2024_Bick_etal_R… Rese… The stu… The p… Generati… The paper p…
##  3 The A… Andrew… 2024_Caplin_etal… Rese… The aut… The p… AI assis… The study i…
##  4 Large… John J… 2023_Horton_Larg… Rese… The pap… The p… Large la… The paper d…
##  5 The I… Sida P… 2023_Peng_etal_A… Rese… Researc… No fo… In a con… The study p…
##  6 GPTs … Tyna E… 2023_Eloundou_et… Rese… The stu… The p… LLMs cou… The paper i…
##  7 Autom… Philip… 2023_Lergetporer… Rese… The aut… The p… Workers … The study d…
##  8 Artif… Andrew… 2024_Green_etal_… Rese… The rep… The p… AI is ch… The report …
##  9 The A… Andrew… 2024_Caplin_etal… Rese… The aut… The p… AI benef… The study p…
## 10 Autom… Daron … 2019_Acemoglu_et… Rese… The pap… The f… Automati… The authors…
## # ℹ 15 more rows
## # ℹ 1 more variable: KeyCitations <chr>
```

This table can be exported to Excel with
[`writexl::write_xlsx()`](https://docs.ropensci.org/writexl//reference/write_xlsx.html)
for further review, or used to programmatically rename the PDFs using
the model’s suggested filenames.

## Further Notes on Working with Large Documents

### Context Length

When working with long documents, context length limits how much text
the model can process in a single query. If a document exceeds this
limit, the model only sees a portion of it and may miss important
sections.

Focusing on the first five pages, as in this workflow, typically
captures the abstract, introduction, and methodology; enough for a first
overview. For documents where later sections matter, split them into
smaller chunks and process each separately. The overlap between chunks
helps preserve continuity.

### Gemini and Claude for Image-Heavy PDFs

When you use
[`pdf_file()`](https://edubruell.github.io/tidyllm/dev/reference/pdf_file.md)
with
[`gemini()`](https://edubruell.github.io/tidyllm/dev/reference/gemini.md)
or
[`claude()`](https://edubruell.github.io/tidyllm/dev/reference/claude.md),
the PDF is sent as binary rather than extracted text. This means the
model sees the actual layout, tables, charts, and scanned pages, not
just the raw characters. For text-only workflows on any provider, pass
`.text_extract = TRUE` to
[`pdf_file()`](https://edubruell.github.io/tidyllm/dev/reference/pdf_file.md)
to force text extraction and skip binary encoding entirely.

For large or frequently reused documents you can also upload them once
to the provider’s servers with
[`upload_file()`](https://edubruell.github.io/tidyllm/dev/reference/upload_file.md)
and reference the returned handle in messages via `.files`. This avoids
re-sending the binary on every request. Provider file APIs often accept
a broader range of formats beyond PDFs: Gemini supports video, audio,
and various document types; OpenAI accepts Office files (DOCX, PPTX,
XLSX), source code, ZIP archives, and more. This makes
[`upload_file()`](https://edubruell.github.io/tidyllm/dev/reference/upload_file.md)
useful for workflows that go beyond plain PDFs.

``` r

report <- upload_file(claude(), .path = "annual_report.pdf")

llm_message("Summarise the key risks.", .files = report) |>
  chat(claude())
```

See the [Working with Files and
Media](https://edubruell.github.io/tidyllm/articles/tidyllm_video.html)
article for a full walkthrough of the file upload workflow across
providers.

### Local Models

Local models via
[`ollama()`](https://edubruell.github.io/tidyllm/dev/reference/ollama.md)
or
[`llamacpp()`](https://edubruell.github.io/tidyllm/dev/reference/llamacpp.md)
are a strong choice for privacy-sensitive documents, as no data leaves
your machine. Modern models like `qwen3.5` support a context window of
up to 262,144 tokens in principle; the practical limit on a typical
laptop is RAM. On a 16 GB machine a 9B model at Q4 quantisation already
uses roughly 6 GB for weights, leaving limited headroom for long
contexts before inference slows to a crawl or the system starts
swapping.

For most academic papers the first five pages fit comfortably within
what laptop hardware can handle. For longer documents, limit the page
range or reduce the context window explicitly via the `.num_ctx`
parameter in
[`ollama()`](https://edubruell.github.io/tidyllm/dev/reference/ollama.md).

> ⚠️ **Note:** When using paid remote APIs, consider data privacy,
> especially for sensitive documents. Local models via
> [`ollama()`](https://edubruell.github.io/tidyllm/dev/reference/ollama.md)
> or
> [`llamacpp()`](https://edubruell.github.io/tidyllm/dev/reference/llamacpp.md)
> keep all processing on-device.

## Outlook

This structured question-answering workflow streamlines extraction of
key insights from academic papers and adapts directly to other
document-heavy tasks; reports, policy documents, contracts, or news
archives can all be processed with the same pattern of schema
definition, single-document testing, and batch scale-up.
