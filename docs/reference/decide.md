# Answer questions about a text, an image or a recording with a decision model

`decide()` sends a state (a text, a picture, a recording, a PDF or a
mix) and a set of closed questions to a decision model and returns, for
each question, the answer and the probability the model gives it. Use it
to classify, screen or score texts and images where you want a
calibrated number rather than free text. Give it many states at once and
it runs the requests in parallel.

## Usage

``` r
decide(
  .state,
  .questions,
  .provider = getOption("tidyllm_decide_default"),
  .dry_run = NULL,
  .timeout = NULL,
  .max_tries = NULL,
  .max_active = 5,
  .progress = FALSE
)
```

## Arguments

- .state:

  What to decide about. Every element is one item: a non-empty string, a
  media object, an `LLMMessage`, or a list of parts that belong together
  (one level only). Pass one item bare, a character vector, or a list of
  items; two or more items are decided in parallel. See the Many states
  section.

- .questions:

  A
  [`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)
  whose fields are the questions.

- .provider:

  A provider call such as
  [`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md).

- .dry_run:

  If `TRUE`, return the httr2 request without sending it (a list of
  requests, one per state, for many states).

- .timeout:

  Timeout in seconds. Provider default if `NULL`.

- .max_tries:

  Maximum attempts. Provider default if `NULL`.

- .max_active:

  Most requests in flight at once when `.state` holds many states. The
  default is 5; a free Gemini key allows about 200 requests a minute, so
  lower it if you see rate-limit warnings.

- .progress:

  Show a progress bar when `.state` holds many states.

## Value

A tibble with one row per question and the columns `question`, `type`
(`"yes_no"`, `"choice"`, `"score"` or `"box"`), `answer` (`"yes"` or
`"no"`, the chosen option, or the score level with the highest
probability), `probability` (of that answer), `confidence` (the
backend's confidence in the answer; for yes/no questions the larger of
the probability of yes and of no when the backend gives none, and `NA`
for choice and score questions) and `probabilities` (a list column with
one named numeric vector over the options for each row). A box question
gives one row with `answer` and `probability` `NA`, and the schema then
also adds the numeric columns `top`, `bottom`, `left` and `right`
(fractions of the image; `NA` on all other rows). For many states, one
block of rows per state with `state` and `state_id` in front.

The attributes `usage`, `latency` and `model` describe the call. `usage`
is a list with `input_tokens` and `output_tokens` (what the provider
billed; `NA` when it does not report them), plus `cached_tokens` and
`cache_write_tokens` for OpenAI and `tokens_by_modality` for Gemini.
`latency` is the elapsed seconds. For many states, `usage` sums the
states that succeeded, `latency` is the time of the whole parallel run,
and `model` is the model the first reply names. With `.dry_run = TRUE`,
the request.

## Details

Questions are the fields of a
[`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md).
[`field_lgl()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
is a yes/no question, `field_fct(.levels = )` is a choice question, and
`field_fct(.levels = , .ordered = TRUE)` is a score question over
ordered levels. The field description is the question text.
`.level_descriptions` in
[`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
tells the model what each option or level means; for a score question
the level order is the scale. `field_lgl(.true = , .false = )` defines
what yes and no mean; give both or neither.

## Media

A state can hold images, audio, video and PDFs next to text. A media
object ([`img()`](https://edubruell.github.io/tidyllm/reference/img.md),
[`audio_file()`](https://edubruell.github.io/tidyllm/reference/audio_file.md),
[`video_file()`](https://edubruell.github.io/tidyllm/reference/video_file.md),
[`pdf_file()`](https://edubruell.github.io/tidyllm/reference/pdf_file.md))
is an item, and so is an `LLMMessage` (the text of its last user
message, then its media). To put several parts into one item, wrap them
in a list inside `.state`:
`list(list(photo_a = img("a.jpg"), photo_b = img("b.jpg")))`. Names
inside the item label the parts, so a question can say "the animal in
photo_a". The providers read:

|  |  |  |
|----|----|----|
| Provider | Reads | Image option |
| [`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) | image | none |
| [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md) | image | `.detail` |
| [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md) | image, audio, video, PDF | `.resolution` |
| [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md) | image (not yet tested against a live server) | none |

`provider_capabilities(.what = "decide_media")` gives the same table as
a tibble. A provider that cannot read a media type stops with a message
before any request; `decide()` also checks image size, format and count
against the provider's limits and never resizes an image. A PDF sent to
a provider that does not read PDFs is replaced by its extracted text; a
PDF made with `pdf_file(.text_extract = TRUE)` always goes as text.
Either way `decide()` tells you once per call, naming the provider and
the files, that layout, tables and figures are lost. A PDF without
extractable text stops the call.

Images, audio and video leave your machine and go to the provider. Do
not send restricted or personal data, such as survey photos with faces.

## Many states

Every element of `.state` is one item. An item is a string, a media
object, an `LLMMessage`, or a list of parts that belong together
(strings and media objects, one level only; names label the parts).
`.state` itself is one item passed bare, a character vector (one item
per string), or a list of items (one item per element). So
`list(img("a.jpg"), img("b.jpg"))` is two items, and
`list(list("Caption", img("c.jpg")))` is one item with two parts.

A single item, however it is given, returns the plain tibble. Two or
more items give one result tibble with a leading `state` column (the
item's name when the outer list is named, else its text cut to 60
characters or a short description of its media) and an integer
`state_id`, then the usual columns.

The requests run in parallel, at most `.max_active` at a time, and are
retried like a single request. A state that fails after its retries, or
that the model refuses, gives NA rows and one warning; the other states
are kept. A malformed item stops the call before any request and names
its position.

## Finding things in images (experimental)

[`field_box()`](https://edubruell.github.io/tidyllm/reference/field_box.md)
asks where something is in an image and returns one box. Only
[`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) answers
it;
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md)
and
[`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md)
stop with a message before any request, and so does a state without an
image. A box question always returns a box, so pair it with a yes/no
presence question. The vendor labels this as preview and the format may
change. See
[`field_box()`](https://edubruell.github.io/tidyllm/reference/field_box.md)
and
[`box_to_pixels()`](https://edubruell.github.io/tidyllm/reference/box_to_pixels.md).

## Examples

``` r
if (FALSE) { # \dontrun{
questions <- tidyllm_schema(
  late = field_lgl("Was the payment late?"),
  tone = field_fct("Tone of the message", .levels = c("polite", "angry")),
  severity = field_fct("How severe is the complaint?",
    .levels = c("low", "medium", "high"), .ordered = TRUE)
)
decide("The invoice arrived three weeks late and nobody answers my mails.",
       questions, jev())

# Many texts: one tibble with a state and state_id column
mails <- c("Payment is three weeks late.", "Thanks, all fine.")
decide(mails, questions, openai())

# An image, and two labelled images in one item
cat_q <- tidyllm_schema(cat = field_lgl("Is the animal in the picture a cat?"))
decide(img("cat.jpg"), cat_q, openai(.detail = "low"))
pets <- tidyllm_schema(
  a_dog = field_lgl("Is the animal in photo_a a dog?"),
  b_dog = field_lgl("Is the animal in photo_b a dog?")
)
decide(list(list(photo_a = img("cat.jpg"), photo_b = img("dog.jpg"))), pets, jev())

# Many images: one item per element
files <- c("cat.jpg", "dog.jpg")
decide(purrr::map(files, img), cat_q, gemini(.resolution = "low"))
} # }
```
