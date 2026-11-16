# Classifying Texts with tidyllm

You have thousands of short free-text answers and a fixed list of
categories. A language model can label each answer in minutes. A single
label can differ from run to run, though, and a label that comes back
every time can still be wrong.

Here you write a codebook (a definition for each category), label every
answer several times, and compare the runs. You send the answers where
the runs disagree through a stronger second pass and check the result
against a sample of answers coded by people.

A faster, cheaper route is a *decision model*, a different kind of model
that you call with
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
instead of
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md). It
answers a closed question such as “which of these 23 codes?” with the
code and a probability, and writes no text. The section “An alternative:
decision models” runs it on the same sample.

Chunks that call an API (a model provider’s web service) are not
evaluated; the next chunk loads the saved results, so you can follow
along without an API key, the password for that service. If you rerun
them, they cost money. The [Get
Started](https://edubruell.github.io/tidyllm/articles/tidyllm.html)
article has a table of where to get a key for each provider.

## The task

Seven thousand survey respondents described their job in their own
words, which gives 2,209 distinct answers, since many people use the
same words (for example “lawyer”). We sort each answer into one of 22
two-digit groups of the SOC (Standard Occupational Classification, the
US scheme for job categories), or into code 99 when it names no
occupation.

``` r

library(tidyllm)
library(tidyverse)
library(glue)
library(irr)

occ_data  <- read_rds("occupation_data.rds")
occ_codes <- read_rds("occ_codes_2digits.rds")

distinct_occupations <- occ_data |>
  distinct(occupation = occupation_open) |>
  mutate(id = row_number())

print(distinct_occupations, n = 5)
## # A tibble: 2,209 × 2
##   occupation                     id
##   <chr>                       <int>
## 1 Ops oversight and strategy      1
## 2 Coordinating operations         2
## 3 Making sure everything runs     3
## 4 Building and demolition         4
## 5 Help lawyers with cases         5
## # ℹ 2,204 more rows
```

We classify each distinct answer once and join the labels back at the
end.

## Why a single answer is not enough

In 2026, most paid providers no longer let you set `temperature`, the
setting that controls how much a model’s answers vary for the same
input. Most current paid models reject it. OpenAI and Claude models
return an error, and recent Gemini models, including the default of
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md),
accept it and ignore it.
[`provider_capabilities()`](https://edubruell.github.io/tidyllm/reference/provider_capabilities.md)
lists which providers accept it.

So you cannot make a model repeat itself, and the same prompt on the
same text can return different labels, mostly on borderline texts
(Barrie, Palaiologou and Törnberg, 2024; Haldar and Hockenmaier, 2025).
The remedy is the one used with human coders: label each answer several
times, compare the runs, and check the labels against a validation
sample, a set of answers coded by people (Pangakis, Wolken and Fasching,
2023).

The chat examples use `claude(.model = "claude-sonnet-5-5")`.

## Step 1: Write a codebook, not a list

A codebook gives each label a definition, a rule for overlaps and one or
two examples. Without one the model guesses what “Management” or
“Missing” covers.

``` r

codebook <- read_file("classifier_codebook.txt") |> trimws()
cat(codebook)
## 11 Management: directs people, budgets or operations; the job is mainly deciding and supervising, not doing the trade itself. Example: "Ops oversight".
## 13 Business and Financial Operations: analysis, accounting, HR, purchasing, consulting. Example: "Handle construction budgets".
## 15 Computer and Mathematical: software, IT systems, data analysis, statistics. Example: "Systems administrator".
## 17 Architecture and Engineering: design and engineering of structures, machines and systems, and drafting.
## 19 Life, Physical, and Social Science: research in natural or social science, lab work.
## 21 Community and Social Service: counselling, social work, clergy.
## 23 Legal: lawyers, paralegals, judges.
## 25 Educational Instruction and Library: teachers, tutors, librarians, trainers.
## 27 Arts, Design, Entertainment, Sports, and Media: artists, designers, performers, athletes, journalists.
## 29 Healthcare Practitioners and Technical: doctors, nurses, therapists, technicians who diagnose or treat.
## 31 Healthcare Support: aides, assistants and orderlies who support practitioners.
## 33 Protective Service: police, fire, security, military guards.
## 35 Food Preparation and Serving: cooks, servers, bartenders.
## 37 Building and Grounds Cleaning and Maintenance: janitors, groundskeepers, cleaners. Example: "Oversee all cleaning operations" is still 37 unless the answer says managing a business.
## 39 Personal Care and Service: hair, beauty, child care, fitness, funeral service. Example: "Hair dresser".
## 41 Sales and Related: selling goods or services, cashiers, brokers.
## 43 Office and Administrative Support: clerks, secretaries, mail handling, data entry. Example: "Sort and deliver mail".
## 45 Farming, Fishing, and Forestry: crops, livestock, fishing, logging.
## 47 Construction and Extraction: building trades, mining. Example: "Fine Carpentry", "Layin' down shingles".
## 49 Installation, Maintenance, and Repair: fixing and installing equipment and vehicles. Example: "Bodyshop guy".
## 51 Production: manufacturing, assembly, machine operation, craft production. Example: "Eyeglass makin'".
## 53 Transportation and Material Moving: drivers, pilots, warehouse movers.
## 99 Missing: the answer names no occupation, or is too vague to place (for example "Doin' the numbers", "stuff"). Prefer 99 over a guess.
```

Keep the codebook in a file under version control. Rerun step 6 after
every edit, so the accuracy you report matches the codebook you use.

## Step 2: Constrain the answer with a schema

Asking for “only the code” breaks
[`parse_number()`](https://readr.tidyverse.org/reference/parse_number.html)
when the model adds a word. A schema, a fixed list of fields the reply
must fill, avoids this.
[`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
makes the label an enum, a field that takes only values from a fixed
list. We add a one-sentence rationale to read when you audit a row and a
three-level confidence to sort by.

``` r

label_levels <- as.character(c(occ_codes$occ2, 99)) |> unique()

occ_schema <- tidyllm_schema(
  name       = "occupation_label",
  rationale  = field_chr("One sentence: which words in the answer decide the group."),
  occ2       = field_fct("The SOC two-digit group from the codebook.", .levels = label_levels),
  confidence = field_fct("How clear the answer is.", .levels = c("low", "medium", "high"))
)
```

``` r

make_prompt <- function(occupation, codebook) {
  llm_message(glue(
    'You code survey answers into occupation groups.
     Use only the codebook below. Choose the single best group.
     If the answer names no occupation, use 99.

     Codebook:
     {codebook}

     Survey answer: "{occupation}"'
  ))
}
```

A dry run, `.dry_run = TRUE`, shows the request that would be sent
without sending it, so you can check it before spending anything.

``` r

make_prompt(distinct_occupations$occupation[1], codebook) |>
  chat(claude(.model = "claude-sonnet-5-5", .json_schema = occ_schema, .dry_run = TRUE))
```

Heavy output constraints can cost accuracy, the share of answers where
the model’s label equals the human label, on hard cases (Tam et al.,
2024); the rationale field is there for that reason.

One call looks like this:

``` r

make_prompt("Doin' the numbers", codebook) |>
  chat(claude(.model = "claude-sonnet-5-5", .json_schema = occ_schema)) |>
  get_reply_data()
```

[`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)
returns a named list with one element per schema field. The label stays
a character string, because “99” and “11” are labels.

## Step 3: Label every answer several times

We ask for each answer `k` times and keep every run. Three runs give a
majority; five give a finer agreement score at a higher price. We use a
random sample of 150 distinct answers.

[`parallel_chat()`](https://edubruell.github.io/tidyllm/reference/parallel_chat.md)
sends the requests concurrently and returns one reply per prompt, in the
same order.

The chunk below builds one ready-to-send prompt per answer and run and
keeps it in a `prompt` column next to the answer.

``` r

k <- 3

sample_df <- distinct_occupations |>
  slice_sample(n = 150)

tasks <- sample_df |>
  cross_join(tibble(run = 1:k)) |>
  mutate(prompt = map(occupation, make_prompt, codebook = codebook))
```

``` r

replies <- parallel_chat(
  tasks$prompt,
  claude(.model = "claude-sonnet-5-5", .json_schema = occ_schema),
  .max_active = 4
)
write_rds(replies, "runs_claude.rds")
```

Failed requests do not stop the call;
[`parallel_chat()`](https://edubruell.github.io/tidyllm/reference/parallel_chat.md)
warns how many failed, and you can run those prompts again.

[`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)
turns the list of replies into a tibble with one row per reply, in the
same order: `reply_id`, `error` and one column per schema field. A
failed request keeps its row with the message in `error`. The chunk
lists those rows in `failed`, so you can run them again, and drops them
from `runs`.

``` r

reply_data <- get_reply_data(replies)

failed <- filter(reply_data, !is.na(error))

runs <- tasks |>
  bind_cols(reply_data) |>
  filter(is.na(error)) |>
  select(id, run, label = occ2, rationale, confidence)
```

The saved results hold three labels for each of the 150 sampled answers.

``` r

sample_df <- read_rds("classifier_sample.rds")
runs      <- read_rds("classifier_runs.rds")
run_meta  <- read_rds("classifier_runs_meta.rds")
run_tokens <- run_meta$tokens |>
  rename(prompt_tokens = input_tokens, completion_tokens = output_tokens)

runs |>
  left_join(sample_df, by = "id") |>
  select(id, occupation, run, label, confidence) |>
  print(n = 6)
## # A tibble: 450 × 5
##      id occupation   run label confidence
##   <int> <chr>      <int> <chr> <chr>     
## 1    43 Server         1 35    medium    
## 2    43 Server         2 35    medium    
## 3    43 Server         3 35    medium    
## 4    53 Jailer         1 33    high      
## 5    53 Jailer         2 33    high      
## 6    53 Jailer         3 33    high      
## # ℹ 444 more rows
```

Tokens are the short pieces of text a model reads and writes, and
providers charge by them.
[`get_metadata()`](https://edubruell.github.io/tidyllm/reference/get_metadata.md)
returns one row per reply with its `prompt_tokens` and
`completion_tokens`:

``` r

run_tokens <- get_metadata(replies)
```

The codebook is part of every request, so prompts are far longer than
the answers. We count them for the 450 saved calls.

``` r

run_tokens |>
  summarise(
    calls               = n(),
    prompt_per_call     = round(mean(prompt_tokens)),
    completion_per_call = round(mean(completion_tokens)),
    prompt_tokens       = sum(prompt_tokens),
    completion_tokens   = sum(completion_tokens)
  ) |>
  relocate(calls, prompt_tokens, completion_tokens)
## # A tibble: 1 × 5
##   calls prompt_tokens completion_tokens prompt_per_call completion_per_call
##   <int>         <int>             <int>           <dbl>               <dbl>
## 1   450        626166             34518            1391                  77
```

### The same job as a batch

For a full dataset where a result an hour later is fine,
[`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md)
submits the same list as a batch, a job you collect later, at half the
price.
[`check_batch()`](https://edubruell.github.io/tidyllm/reference/check_batch.md)
reports the status and
[`fetch_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_batch.md)
returns the replies. We sent the first 20 requests as a test; the chunk
submits the batch job, checks it, fetches the replies and reads them
with
[`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)
as before.

``` r

first20 <- slice(tasks, 1:20)

job <- send_batch(first20$prompt, claude(.model = "claude-sonnet-5-5", .json_schema = occ_schema))
write_rds(job, "runs_batch_job.rds")

job <- read_rds("runs_batch_job.rds")
check_batch(job, claude())

replies <- fetch_batch(job, claude())

batch_runs <- bind_cols(first20, get_reply_data(replies))
```

The two
[`check_batch()`](https://edubruell.github.io/tidyllm/reference/check_batch.md)
outputs come from right after submission and three minutes later:

    # A tibble: 1 x 8
      batch_id          status      created_at          expires_at          req_succeeded
      <chr>             <chr>       <dttm>              <dttm>                      <int>
    1 msgbatch_017iBx1… in_progress 2026-10-02 13:22:40 2026-10-03 13:22:40             0
    # i 3 more variables: req_errored <int>, req_expired <int>, req_canceled <int>

    # A tibble: 1 x 8
      batch_id          status created_at          expires_at          req_succeeded
      <chr>             <chr>  <dttm>              <dttm>                      <int>
    1 msgbatch_017iBx1… ended  2026-10-02 13:22:40 2026-10-03 13:22:40            20
    # i 3 more variables: req_errored <int>, req_expired <int>, req_canceled <int>

The batch reported 27,823 prompt tokens for 20 requests, about 1,390
each, matching the live calls. Its labels compare with the first live
run:

``` r

batch_runs <- read_rds("classifier_batch_results.rds")

batch_runs |>
  select(id, occupation, label = occ2, confidence) |>
  inner_join(
    runs |> filter(run == 1) |> select(id, live_label = label),
    by = "id"
  ) |>
  summarise(answers = n(), same_label_as_live_run = sum(label == live_label))
## # A tibble: 1 × 2
##   answers same_label_as_live_run
##     <int>                  <int>
## 1      20                     20
```

### Using more than one model

A second model shows whether two independent systems read the codebook
the same way. Agreement across models is the stronger signal, because
unrelated models fail less alike (Ziems et al., 2024). Running the same
prompts through another provider adds its labels as extra runs. We did
not run this here.

``` r

replies_gemini <- parallel_chat(tasks$prompt, gemini(.json_schema = occ_schema), .max_active = 4)
```

## Step 4: Summarise agreement and flag disagreements

The next chunk counts the votes per label, puts the most-voted label
first, and keeps it with its vote share. The `unstable` flag marks
answers where the runs disagree.

``` r

vote <- runs |>
  count(id, label, name = "votes") |>
  arrange(id, desc(votes)) |>
  group_by(id) |>
  summarise(
    n_runs     = sum(votes),
    majority   = first(label),
    agreement  = first(votes) / sum(votes),
    candidates = paste(label, collapse = "/")
  ) |>
  left_join(sample_df, by = "id") |>
  mutate(unstable = agreement < 1)

vote |>
  count(unstable) |>
  mutate(share = n / sum(n))
## # A tibble: 2 × 3
##   unstable     n share
##   <lgl>    <int> <dbl>
## 1 FALSE      147  0.98
## 2 TRUE         3  0.02
```

Almost every answer gets the same label in all three runs. The rows
where the runs differ form a short list to read; step 6 shows it catches
only part of the errors.

A tie, such as one vote each for three labels, has no majority and
[`first()`](https://dplyr.tidyverse.org/reference/nth.html) picks one
arbitrarily, so treat rows with `agreement < 0.5` as unresolved. A
unanimous label can still be wrong.

Coders who pick from 22 categories agree sometimes by luck alone, more
often when a few categories are very frequent. Agreement statistics that
correct for this luck use a scale where 0 means no better than chance
and 1 means perfect agreement. The `irr` package computes both
statistics we use in this article:

- **Krippendorff’s alpha** measures how much several coders agree. Here
  the coders are the three runs and the units are the answers. Rules of
  thumb call 0.8 reliable and 0.667 the lowest value for tentative
  conclusions (Krippendorff, 2004).
- **Cohen’s kappa** measures how much two coders agree. We use it in
  step 6 for the model and the human coder.

[`kripp.alpha()`](https://rdrr.io/pkg/irr/man/kripp.alpha.html) wants
one row per coder and one column per answer, so we reshape the runs
first. Alpha measures agreement between runs only; human labels come in
step 6.

``` r

runs |>
  select(run, id, label) |>
  pivot_wider(names_from = id, values_from = label) |>
  select(-run) |>
  as.matrix() |>
  kripp.alpha(method = "nominal")
##  Krippendorff's alpha
## 
##  Subjects = 150 
##    Raters = 3 
##     alpha = 0.983
```

### Self-reported confidence

Does the confidence field carry a signal? We check against the labels
for the 150 answers, made with AI assistance and checked by a human
coder (step 6). We call them gold labels: the labels a person assigned,
which we treat as the truth to compare against.

``` r

gold <- read_rds("classifier_gold.rds")

scored <- vote |>
  left_join(gold, by = "id") |>
  mutate(match = majority == gold)

runs |>
  left_join(select(scored, id, match), by = "id") |>
  group_by(majority_matches_gold = match) |>
  summarise(runs = n(), share_low_confidence = mean(confidence == "low"))
## # A tibble: 2 × 3
##   majority_matches_gold  runs share_low_confidence
##   <lgl>                 <int>                <dbl>
## 1 FALSE                    75                0.413
## 2 TRUE                    375                0.032
```

Runs on rows where the majority label disagrees with the human label are
much more often marked “low”. The field helps rank rows to read, but
many mismatched rows still carry “medium” or “high”.

### Token probabilities, where the provider offers them

Some providers report how likely each piece of the reply (each token)
was, so the probability of the label token is a free confidence score.
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
and the Chat Completions based providers accept `.logprobs` and
`.top_logprobs`;
[`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md)
has no such argument. They describe one run of one model and say less
with structured output, where the label sits inside a JSON string.
Decision models avoid that limit; see the next section.

## An alternative: decision models

[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
sends the text and a closed question to a decision model, a model that
returns the chosen code and a probability (a score between 0 and 1) for
every code, with no text. It is much faster and cheaper than a chat
model. The [decision models
article](https://edubruell.github.io/tidyllm/articles/tidyllm_decide.html)
explains the question types and providers; here we use one choice
question.

The codebook becomes the question. In R a choice question is a factor
and its possible answers are levels: each level is an occupation code,
described by its codebook line.
[`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
takes the codes as `.levels` and the descriptions as
`.level_descriptions`.

This reads the codebook file, one line per code, and splits each line
into a code and a description.

``` r

codebook_levels <- tibble(line = read_lines("classifier_codebook.txt")) |>
  filter(str_trim(line) != "") |>
  separate(line, c("code", "description"), sep = " ", extra = "merge")

occ_questions <- tidyllm_schema(
  name = "occupation_label",
  occ2 = field_fct(
    "The SOC two-digit occupation group of this survey answer.",
    .levels             = codebook_levels$code,
    .level_descriptions = codebook_levels$description
  )
)
```

[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
takes the text in `.state`, the questions in `.questions` and a
provider. One line before each answer says what the text is; without it,
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
labelled answers such as “Doctor” as code 99 (“no occupation”).

``` r

frame_answer <- \(x) paste0("Survey question: What is your job? Answer: ", x)

decide(frame_answer("Residential roofer"), occ_questions, gemini()) |>
  select(question, type, answer, probability)
```

    # A tibble: 1 x 4
      question type   answer probability
      <chr>    <chr>  <chr>        <dbl>
    1 occ2     choice 47           0.998

Code 47 is Construction and Extraction, which fits a roofer. A list of
texts in `.state` returns one row per text and question. We run the same
150 human-labelled answers through
[`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) (the
provider of the Jev decision model),
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
and
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md);
the three runs took 6.9, 10.5 and 7.7 seconds. In the next chunk,
`label_all()` runs one provider over all 150 answers and joins the human
labels; the [`list()`](https://rdrr.io/r/base/list.html) call does this
for the three providers.

``` r

label_all <- function(provider) {
  res <- decide(as.list(frame_answer(sample_df$occupation)), occ_questions, provider)
  sample_df |>
    mutate(label = res$answer, probability = res$probability, probs = res$probabilities) |>
    inner_join(gold, by = "id")
}

decide_runs <- list(jev = label_all(jev()), openai = label_all(openai()), gemini = label_all(gemini()))
write_rds(decide_runs, "classifier_decide_runs.rds")
```

``` r

decide_runs <- read_rds("classifier_decide_runs.rds")
decide_all  <- bind_rows(decide_runs, .id = "provider")
```

First-pass accuracy against the human labels, next to the majority of
the three Claude runs from step 4:

``` r

claude_row <- scored |>
  summarise(provider = "claude, majority of 3 runs", answers = n(),
            correct = sum(majority == gold), accuracy = round(mean(majority == gold), 3))

decide_all |>
  group_by(provider) |>
  summarise(answers = n(), correct = sum(label == gold), accuracy = round(mean(label == gold), 3)) |>
  bind_rows(claude_row)
## # A tibble: 4 × 4
##   provider                   answers correct accuracy
##   <chr>                        <int>   <int>    <dbl>
## 1 gemini                         150     124    0.827
## 2 jev                            150     126    0.84 
## 3 openai                         150     125    0.833
## 4 claude, majority of 3 runs     150     125    0.833
```

All three decision models reach 0.83 to 0.84, level with the Claude
majority, in seconds and with one call per answer instead of three. The
table below groups the answers into probability bands. Each cell shows
the share labelled correctly and, in brackets, the number of answers in
the band.

``` r

decide_all |>
  mutate(band = cut(probability, c(0, 0.5, 0.8, 0.95, 1), include.lowest = TRUE)) |>
  group_by(provider, band) |>
  summarise(cell = str_glue("{sprintf('%.2f', mean(label == gold))} ({n()})"), .groups = "drop") |>
  pivot_wider(names_from = provider, values_from = cell, values_fill = "0 rows")
## # A tibble: 4 × 4
##   band       gemini    jev       openai   
##   <fct>      <glue>    <glue>    <glue>   
## 1 [0,0.5]    0.44 (16) 0.46 (13) 0.44 (9) 
## 2 (0.5,0.8]  0.69 (39) 0.65 (37) 0.52 (27)
## 3 (0.8,0.95] 0.89 (37) 0.89 (38) 0.86 (22)
## 4 (0.95,1]   0.98 (58) 1.00 (62) 0.96 (92)
```

Above 0.95, 96 to 100 per cent of the labels match the human label;
below 0.5, fewer than half do. At a cut-off of 0.8, rows at or above it
are right 93 to 96 per cent of the time and rows below it 51 to 62 per
cent. Send the rows below the cut-off to a person or to step 5.

``` r

decide_all |>
  mutate(flagged = probability < 0.8) |>
  group_by(provider, flagged) |>
  summarise(answers = n(), correct = sum(label == gold), accuracy = round(mean(label == gold), 2), .groups = "drop")
## # A tibble: 6 × 5
##   provider flagged answers correct accuracy
##   <chr>    <lgl>     <int>   <int>    <dbl>
## 1 gemini   FALSE        95      90     0.95
## 2 gemini   TRUE         55      34     0.62
## 3 jev      FALSE       101      97     0.96
## 4 jev      TRUE         49      29     0.59
## 5 openai   FALSE       115     107     0.93
## 6 openai   TRUE         35      18     0.51
```

The three providers are independent readers of the codebook, so the
agreement logic of step 4 applies.
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
and
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
choose the same code for 88 per cent of the answers,
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
and [`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) for
87 and
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
and [`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) for
82. All three agree on 119 of the 150 answers, and 92 per cent of those
match the human label. The 31 others are a short list to read by hand.

A decision model returns no reason; use the
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) pass
of step 5 for rows that need one. Its labels are only as good as the
codebook, so steps 6 and 7 apply here too.

### Hierarchical codes with a beam search

The 22 groups are the top level of a taxonomy, a tree of categories with
subcategories. Real codings, such as occupations or industries, go
several levels deeper with hundreds of codes, and one question with that
many levels is too long for the model to read well. The TypeSafe
documentation calls the remedy a beam search: ask a choice question at
the top level, keep the few top-scoring groups, ask a second question
inside each, and continue down. A path scores the product of the
probabilities along it, and you keep the few best paths at each level.
The `probabilities` column holds one probability per code, including the
runners-up.

A small example: for one answer, level one gives codes A 0.6 and B 0.3.
Inside A the best child scores 0.5 and inside B 0.9. The paths score 0.6
× 0.5 = 0.30 and 0.3 × 0.9 = 0.27, so keep both.

The chunk below takes the answer where
[`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) is least
sure and lists its three highest-probability codes.

``` r

doubtful <- decide_runs$jev |>
  slice_min(probability, n = 1, with_ties = FALSE)

enframe(doubtful$probs[[1]], name = "code", value = "probability") |>
  slice_max(probability, n = 3, with_ties = FALSE) |>
  mutate(occupation = doubtful$occupation, probability = round(probability, 3), .before = 1)
## # A tibble: 3 × 3
##   occupation  code  probability
##   <chr>       <chr>       <dbl>
## 1 Saftey tech 15           0.3 
## 2 Saftey tech 99           0.29
## 3 Saftey tech 33           0.24
```

A beam search would carry these three codes into the next level.

tidyllm has no helper for this yet, and we did not run a full beam
search on this two-digit example. A deeper codebook needs a few lines of
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
and [`map()`](https://purrr.tidyverse.org/reference/map.html) on top of
the code above.

## Step 5: Reclassify the disagreements

Rows where the runs disagreed or any run said “low” get a second look.
Instead of voting again, we show a more capable model the answer, the
earlier labels and their rationales, and turn reasoning on.

``` r

make_review_prompt <- function(occupation, rationales) {
  llm_message(glue(
    'Earlier independent attempts coded this survey answer; they disagreed or were unsure.
     Decide using the codebook. If the answer is genuinely too vague to place, use 99.

     Codebook:
     {codebook}

     Survey answer: "{occupation}"

     Earlier labels and reasons:
     {rationales}'
  ))
}
```

The next chunk adds the earlier labels and rationales to each flagged
row, sends the rows to the stronger model and reads the replies as
before.

The next chunk keeps the answers where the runs disagreed or any run
said “low”.

``` r

to_review <- runs |>
  group_by(id) |>
  summarise(any_low = any(confidence == "low")) |>
  left_join(vote, by = "id") |>
  filter(unstable | any_low)
```

``` r

rationales <- runs |>
  group_by(id) |>
  summarise(rationales = paste0("- ", label, ": ", rationale, collapse = "\n"))

review_prompts <- to_review |>
  left_join(rationales, by = "id") |>
  mutate(prompt = map2(occupation, rationales, make_review_prompt))

review_replies <- parallel_chat(
  review_prompts$prompt,
  claude(
    .model       = "claude-opus-5-5",
    .json_schema = occ_schema,
    .thinking    = TRUE,
    .max_tokens  = 4000
  ),
  .max_active = 4
)

reviewed <- review_prompts |>
  bind_cols(get_reply_data(review_replies)) |>
  select(id, review_label = occ2, review_rationale = rationale, review_confidence = confidence)
```

Keep `.max_tokens` generous with reasoning on, since thinking tokens
count against it.

``` r

reviewed <- read_rds("classifier_reviewed.rds")

tibble(
  flagged_rows        = nrow(to_review),
  runs_disagreed      = sum(to_review$unstable),
  only_low_confidence = sum(!to_review$unstable),
  reviewed_rows       = nrow(reviewed),
  same_ids            = setequal(to_review$id, reviewed$id)
)
## # A tibble: 1 × 5
##   flagged_rows runs_disagreed only_low_confidence reviewed_rows same_ids
##          <int>          <int>               <int>         <int> <lgl>   
## 1           18              3                  15            18 TRUE
```

This keeps the review label where there is one and the majority label
otherwise, and tags each row as unanimous, reviewed or human queue.

``` r

final <- vote |>
  left_join(reviewed, by = "id") |>
  left_join(gold, by = "id") |>
  mutate(
    occ2   = coalesce(review_label, majority),
    source = case_when(
      !is.na(review_label) & review_confidence == "low" ~ "human_queue",
      !is.na(review_label)                              ~ "reviewed",
      TRUE                                              ~ "unanimous"
    )
  )

final |> count(source)
## # A tibble: 3 × 2
##   source          n
##   <chr>       <int>
## 1 human_queue     5
## 2 reviewed       13
## 3 unanimous     132
```

The review changed the label of a few rows:

``` r

final |>
  filter(!is.na(review_label), review_label != majority) |>
  select(occupation, majority, review_label, gold, review_confidence)
## # A tibble: 3 × 5
##   occupation                majority review_label gold  review_confidence
##   <chr>                     <chr>    <chr>        <chr> <chr>            
## 1 Massage practitioner      29       31           39    medium           
## 2 Electrical stuff, up high 47       49           49    low              
## 3 Saftey tech               99       19           33    low

tibble(
  changed       = sum(final$review_label != final$majority, na.rm = TRUE),
  reviewed_rows = sum(!is.na(final$review_label))
)
## # A tibble: 1 × 2
##   changed reviewed_rows
##     <int>         <int>
## 1       3            18
```

The review is a second opinion from the same model family and can repeat
a shared misreading. Rows the reviewer marks as low confidence go to a
human queue:

``` r

final |>
  filter(source == "human_queue") |>
  select(occupation, candidates, review_label, gold)
## # A tibble: 5 × 4
##   occupation                     candidates review_label gold 
##   <chr>                          <chr>      <chr>        <chr>
## 1 Assisting families and schools 21/99      21           21   
## 2 Electrical stuff, up high      47         49           49   
## 3 Saftey tech                    99         19           33   
## 4 Network & stuff                15/99      15           15   
## 5 Shippin' stuff                 43         43           53
```

## Step 6: Validate against human labels

Only human coding shows whether labels are right. Draw a random sample
across all answers, since flagged rows alone make the estimate too
pessimistic. One or two people code it from the same codebook without
seeing the model labels and resolve their differences. Do not tune the
codebook on this sample and then report it as your accuracy.

These 150 answers were coded with AI assistance and each one was checked
by a human coder.

We measure accuracy, the share of answers whose label equals the human
label, and Cohen’s kappa from step 4. Kappa adds information because the
22 groups differ a lot in size: a coder who always picks the frequent
groups gets a high accuracy but a low kappa. The table compares the
first run, the majority of the three runs and the majority plus review.

``` r

first_run <- runs |>
  filter(run == 1) |>
  select(id, first_run = label) |>
  inner_join(gold, by = "id")

bind_rows(
  first_run |>
    summarise(pipeline = "first run only", accuracy = mean(first_run == gold), kappa = kappa2(cbind(gold, first_run))$value),
  final |>
    summarise(pipeline = "majority of 3 runs", accuracy = mean(majority == gold), kappa = kappa2(cbind(gold, majority))$value),
  final |>
    summarise(pipeline = "majority plus review", accuracy = mean(occ2 == gold), kappa = kappa2(cbind(gold, occ2))$value)
) |>
  mutate(accuracy = round(accuracy, 3), kappa = round(kappa, 3))
## # A tibble: 3 × 3
##   pipeline             accuracy kappa
##   <chr>                   <dbl> <dbl>
## 1 first run only          0.833 0.821
## 2 majority of 3 runs      0.833 0.821
## 3 majority plus review    0.84  0.829
```

The three rows are close, because the model gave the same label in
nearly all runs from the start. The review touches only the hardest
answers:

``` r

final |>
  filter(!is.na(review_label)) |>
  summarise(rows = n(), before_review = mean(majority == gold), after_review = mean(occ2 == gold))
## # A tibble: 1 × 3
##    rows before_review after_review
##   <int>         <dbl>        <dbl>
## 1    18         0.278        0.333
```

About one final label in six does not match the human label. The table
shows which route the mismatches took:

``` r

final |>
  group_by(source) |>
  summarise(answers = n(), mismatches = sum(occ2 != gold), accuracy = mean(occ2 == gold))
## # A tibble: 3 × 4
##   source      answers mismatches accuracy
##   <chr>         <int>      <int>    <dbl>
## 1 human_queue       5          2    0.6  
## 2 reviewed         13         10    0.231
## 3 unanimous       132         12    0.909
```

Where do the mismatches come from?

``` r

final |>
  filter(occ2 != gold) |>
  arrange(gold) |>
  select(occupation, model = occ2, human = gold, candidates) |>
  print(n = 30)
## # A tibble: 24 × 4
##    occupation                       model human candidates
##    <chr>                            <chr> <chr> <chr>     
##  1 Coach and mentor salespeople     25    11    25        
##  2 Lead mechanic supervisor         49    11    49        
##  3 manage donations                 99    11    99        
##  4 Workforce Optimization           99    13    99        
##  5 Day-to-day operations, logistics 11    13    11        
##  6 Work on operations projects      99    13    99        
##  7 Computer Aided Manufacturing     51    17    51        
##  8 Manufacturing Efficiency         99    17    99        
##  9 Temporary school staff           99    25    99        
## 10 Educational support, basically.  99    25    99        
## 11 Sewing and altering costumes     51    27    51        
## 12 Prophy tech                      29    31    29        
## 13 Saftey tech                      19    33    99        
## 14 I make food batches.             51    35    51        
## 15 Bakery worker                    51    35    51        
## 16 Massage practitioner             31    39    29/31/39  
## 17 Keep things organized            99    43    99        
## 18 Buildin and fixin things         99    47    99        
## 19 Civil works, ya know?            99    47    99        
## 20 Work with mechanical systems     99    49    99        
## 21 Shippin' stuff                   43    53    43        
## 22 Private dick                     33    99    33        
## 23 I haul                           53    99    53        
## 24 Barster                          35    99    35
```

Two groups dominate. The model gave vague answers such as “Keep things
organized”, “Workforce Optimization” or “Buildin and fixin things” code
99, because the codebook says to prefer 99 over a guess, while the human
placed them from context. For slang or terse answers such as “Private
dick”, “I haul” and “Barster” the model guessed a group and the human
coded 99. Some answers fit two groups under the codebook: whether bakery
work is production (51) or food service (35), where “Lead mechanic
supervisor” belongs, and whether massage is healthcare or personal care.

More runs would not average these errors away. Most are rows where the
model gave the same label every time, so step 3 cannot find them.

Compare the model with the agreement between two humans on the same
sample: if two careful coders give the same label for 85 per cent of the
answers, a model with an accuracy of 85 per cent matches them. A model’s
quality depends on the task, codebook and data, so check it each time
(Gilardi, Alizadeh and Kubli, 2023; Pangakis, Wolken and Fasching,
2023). Only one human checked the labels here, so the 83 per cent comes
with that limit.

Check also whether errors are spread evenly: if one group or phrasing
carries most of them, a downstream estimate can be biased even when
overall accuracy looks high. The validation sample can also correct for
misclassification.

``` r

final |>
  group_by(human_group = gold) |>
  summarise(answers = n(), accuracy = mean(occ2 == gold)) |>
  arrange(accuracy, desc(answers)) |>
  slice_head(n = 8)
## # A tibble: 8 × 3
##   human_group answers accuracy
##   <chr>         <int>    <dbl>
## 1 17                3    0.333
## 2 31                2    0.5  
## 3 99                8    0.625
## 4 47                6    0.667
## 5 25                9    0.778
## 6 13               14    0.786
## 7 49                5    0.8  
## 8 35               11    0.818
```

## Step 7: Do not tune the codebook on your validation sample

The mismatch table invites an edit to the codebook, so we made one.
Version 2 adds a rule for bakery and food batch work (51 unless it is
retail counter work), puts massage under personal care (39), counts lead
and supervisor titles and general operations work as management (11),
and counts electrical and mechanical upkeep as installation and repair
(49). We labelled the 150 answers three more times with it.

``` r

codebook_v2 <- read_file("classifier_codebook_v2.txt") |> trimws()

prompts_v2 <- tasks |>
  mutate(prompt = map(occupation, make_prompt, codebook = codebook_v2))

replies_v2 <- parallel_chat(
  prompts_v2$prompt,
  claude(.model = "claude-sonnet-5-5", .json_schema = occ_schema),
  .max_active = 4
)

runs_v2 <- prompts_v2 |>
  bind_cols(get_reply_data(replies_v2)) |>
  select(id, run, label = occ2, rationale, confidence)
```

The next chunk compares the majority label of each codebook version with
the human labels, on the sample we wrote the edit from:

``` r

runs_v2 <- read_rds("classifier_runs_v2.rds")

sample_comparison <- bind_rows(v1 = runs, v2 = runs_v2, .id = "codebook") |>
  count(codebook, id, label) |>
  group_by(codebook, id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  inner_join(gold, by = "id") |>
  group_by(codebook) |>
  summarise(answers = n(), accuracy = mean(label == gold))

sample_comparison
## # A tibble: 2 × 3
##   codebook answers accuracy
##   <chr>      <int>    <dbl>
## 1 v1           150    0.833
## 2 v2           150    0.86
```

We wrote the edit while looking at these errors, so this gain is
expected. To test the new codebook, we labelled 292 further distinct
answers once with each codebook. These held-out answers were set aside
and never used to build or change the codebook; the chunk that builds
them takes a `heldout` table of these answers, and the next one loads
their saved labels from `classifier_heldout_runs.rds` and the human
labels from `classifier_heldout_gold.rds`.

``` r

codebooks <- tibble(codebook = c("v1", "v2"), text = c(codebook, codebook_v2))

heldout_prompts <- heldout |>
  cross_join(codebooks) |>
  mutate(prompt = map2(occupation, text, make_prompt))

heldout_replies <- parallel_chat(
  heldout_prompts$prompt,
  claude(.model = "claude-sonnet-5-5", .json_schema = occ_schema),
  .max_active = 4
)

heldout_runs <- heldout_prompts |>
  bind_cols(get_reply_data(heldout_replies)) |>
  select(id, occupation, codebook, label = occ2, rationale, confidence)
```

``` r

heldout_runs <- read_rds("classifier_heldout_runs.rds")
heldout_gold <- read_rds("classifier_heldout_gold.rds")

heldout_comparison <- heldout_runs |>
  inner_join(select(heldout_gold, id, gold), by = "id") |>
  group_by(codebook) |>
  summarise(answers = n(), accuracy = mean(label == gold))

heldout_comparison
## # A tibble: 2 × 3
##   codebook answers accuracy
##   <chr>      <int>    <dbl>
## 1 v1           292    0.866
## 2 v2           292    0.860
```

On the held-out answers version 2 is slightly worse than version 1: the
edits fitted the quirks of the 150 answers. Tune on one part of the
human-labelled answers and report on another, or label a fresh sample
after every round of edits.

## Cost and size

The token table in step 3 gives the numbers for one prompt: about 1,400
prompt and 77 completion tokens. The next chunk multiplies the average
tokens per call by the number of calls and the price per token:

``` r

per_call <- run_tokens |>
  summarise(prompt = mean(prompt_tokens), completion = mean(completion_tokens))

price_input  <- 2
price_output <- 10

tibble(
  distinct_answers  = nrow(distinct_occupations),
  runs              = 3,
  requests          = distinct_answers * runs,
  prompt_tokens     = round(requests * per_call$prompt),
  completion_tokens = round(requests * per_call$completion),
  usd_standard      = round((prompt_tokens * price_input + completion_tokens * price_output) / 1e6, 2),
  usd_batch         = usd_standard / 2
)
## # A tibble: 1 × 7
##   distinct_answers  runs requests prompt_tokens completion_tokens usd_standard usd_batch
##              <int> <dbl>    <dbl>         <dbl>             <dbl>        <dbl>     <dbl>
## 1             2209     3     6627       9221338            508335         23.5      11.8
```

The prices are those of `claude-sonnet-5-5` on 2026-10-02: 2 USD per
million input tokens and 10 USD per million output tokens, half through
the batch API ([Anthropic
pricing](https://platform.claude.com/docs/en/about-claude/pricing)). At
these rates the 450 calls behind the three runs cost about 1.60 USD, the
second pass with `claude-opus-5-5` (4 and 20 USD) about 0.20 USD, and
the step 7 codebook experiment about 3.90 USD.

A decision model sends a shorter request: about 675 prompt tokens per
answer with
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
against about 1,400 for the chat prompt, and the reply is a code and a
probability. The full 2,209 answers take under two minutes per provider
at the speed measured above.

## What to take away

- Label each answer several times and estimate how stable the model is.
- Measure agreement between runs, and check accuracy against human
  labels. Here three runs gave the same label for nearly every answer,
  and about one in six of the final labels still differed from the human
  label.
- Write a codebook with definitions and decision rules. It does more for
  agreement than any prompt trick, and most remaining errors sit there.
- Use
  [`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
  to restrict labels to the codebook, and check accuracy in step 6.
- Use a decision model as the quick alternative: one call per answer, a
  probability for every code, and on this sample the accuracy of three
  Claude runs. Use the probability to pick rows to review.
- Repeat the call, vote and read the rows where runs disagree. Send
  those through a stronger second pass and keep a human queue for the
  rest.
- Validate on a random, held-out, human-coded sample, and report
  agreement with the number of coders behind it.
- Tune the codebook on one set of answers and validate on another.

## References

- Gilardi, F., Alizadeh, M. and Kubli, M. (2023). ChatGPT outperforms
  crowd workers for text-annotation tasks. *PNAS* 120(30).
- Pangakis, N., Wolken, S. and Fasching, N. (2023). Automated annotation
  with generative AI requires validation. arXiv:2306.00176.
- Ziems, C. et al. (2024). Can large language models transform
  computational social science? *Computational Linguistics* 50(1).
- Barrie, C., Palaiologou, E. and Törnberg, P. (2024). Prompt stability
  scoring for text annotation with large language models.
  arXiv:2407.02039.
- Haldar, R. and Hockenmaier, J. (2025). Rating Roulette:
  Self-inconsistency in LLM-as-a-judge frameworks. *Findings of EMNLP
  2025*; arXiv:2510.27106.
- Tam, Z. R. et al. (2024). Let me speak freely? A study on the impact
  of format restrictions on performance of large language models.
  arXiv:2408.02442.
- Krippendorff, K. (2004). *Content Analysis: An Introduction to Its
  Methodology*. 2nd ed. Sage.
