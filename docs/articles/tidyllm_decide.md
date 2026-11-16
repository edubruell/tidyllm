# Decision Models with tidyllm

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyllm`](https://edubruell.github.io/tidyllm/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`

A chat model writes text, one word at a time. A *decision model* reads a
text and a fixed list of questions and answers each question without
writing anything. Think of a trained coder filling in a coding form: one
tick per question, either yes or no, one category, or a point on a
scale. A decision model does the same job.

For each question you get one answer and a probability for it, between 0
and 1. A text with three questions gives three rows. Because the model
writes no sentences, it is fast and cheap. Jev charges 0.042 US dollars
per million input tokens (the pieces of text a model reads) and nothing
for output tokens. A short survey answer plus our three questions is
about 500 tokens, so coding 10,000 answers costs about 20 cents.

A decision model cannot explain itself. It gives no reasons, summaries
or free text, and you must list every possible answer in advance. For
explanations, quotes or labels you cannot list, use
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md).

**tidyllm** gives these models one verb,
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md).
You write the questions once and change only the provider
([`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md),
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md) or
[`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md))
to switch models.
[`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) is the
decision model of TypeSafe AI and reads its key from `TYPESAFE_API_KEY`.
Here we code eight invented survey answers with three questions: whether
the answer is about economic policy, which topic it addresses, and how
dissatisfied the writer is. Calls that need an API key are shown but not
run; the next chunk loads the saved result. The [Get
Started](https://edubruell.github.io/tidyllm/articles/tidyllm.html) page
explains API keys.

## Write your questions as a schema

You write each question as one field of a schema, a named list of
questions. The field description is the question text, and the field
type fixes the kind of answer.

| Function | Question type | The answer is |
|----|----|----|
| [`field_lgl()`](https://edubruell.github.io/tidyllm/reference/field_chr.md) | yes/no | `yes` or `no` |
| `field_fct(.levels = )` | choice | one of your levels |
| `field_fct(.levels = , .ordered = TRUE)` | score | one of your levels; the order of the levels is the scale |

Only these helpers define questions for
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md);
[`field_box()`](https://edubruell.github.io/tidyllm/reference/field_box.md)
adds a third kind, described in the section on finding things in images.
The [Get
Started](https://edubruell.github.io/tidyllm/articles/tidyllm.html) page
explains schemas in general.

\
`questions`` ``<-`` `[`tidyllm_schema`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)`(`\
`  about_economy   ``=`` `[`field_lgl`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"Does the answer mainly concern economic policy such as wages, taxes, rents or pensions?"``)``,`\
`  topic           ``=`` `[`field_fct`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"Which topic does the answer mainly address?"``,`\
`    .levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"housing"``, ``"labour"``, ``"taxes"``, ``"pensions"``, ``"migration"``, ``"climate"``, ``"none"``)``,`\
`    .level_descriptions ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Rents, house prices, housing supply"``, ``"Wages, jobs, working conditions"``,`\
`      ``"Tax rates and who pays them"``, ``"Pensions and old-age provision"``,`\
`      ``"Immigration and integration"``, ``"Climate and energy policy"``, ``"No policy topic named"``)``)``,`\
`  dissatisfaction ``=`` `[`field_fct`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"How strongly does the writer express dissatisfaction with current policy?"``,`\
`    .levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"none"``, ``"mild"``, ``"strong"``)``, .ordered ``=`` ``TRUE``,`\
`    .level_descriptions ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"No complaint"``, ``"Some criticism, measured tone"``, ``"Anger or a demand for urgent change"``)``)`\
`)`

Three things to note in this schema:

- The model must pick one level, so add a `"none"` level for texts that
  fit no category.
- `.ordered = TRUE` makes a score question. List its levels from lowest
  to highest.
- `.level_descriptions` gives one short phrase per level. Models answer
  better when they know what each option means.

Very short texts need a framing sentence in front of the text that says
what the material is, such as “Survey question: What is your job?
Answer: …”. The [classifier
article](https://edubruell.github.io/tidyllm/articles/tidyllm_classifiers.html)
shows what happens without it.

## Ask one text

The eight invented survey answers are stored in `decide_answers.rds`
beside this article.

\
`answers`` ``<-`` `[`read_rds`](https://readr.tidyverse.org/reference/read_rds.html)`(``"decide_answers.rds"``)`\
\
`answers`\
`## ``# A tibble: 8 × 2`\
`##      id text                                                                              `\
`##   ``<int>`` ``<chr>``                                                                             `\
`## ``1``     1 Rents in my city have doubled and the government does nothing. We need a rent cap…`\
`## ``2``     2 I think the minimum wage is fine as it is; raising it would cost small shops thei…`\
`## ``3``     3 Honestly I do not follow politics, I just vote for whoever my parents vote for.   `\
`## ``4``     4 The pension system is unfair to young people, we pay in and will never get anythi…`\
`## ``5``     5 Taxes on big companies should go up, they hardly pay anything compared to my sala…`\
`## ``6``     6 Immigration is the main problem. Schools and hospitals are full and nobody asks u…`\
`## ``7``     7 Climate policy is important but heating rules were rushed and many families canno…`\
`## ``8``     8 No opinion.`

One call returns one row per question. Run the first answer through Jev.

\
`one`` ``<-`` `[`decide`](https://edubruell.github.io/tidyllm/reference/decide.md)`(``answers``$``text``[``1``]``, ``questions``, `[`jev`](https://edubruell.github.io/tidyllm/reference/jev.md)`(``)``)`

    ## # A tibble: 3 × 4
    ##   question        type   answer  probability
    ##   <chr>           <chr>  <chr>         <dbl>
    ## 1 about_economy   yes_no yes            0.96
    ## 2 topic           choice housing        1   
    ## 3 dissatisfaction score  strong         1

Read the rows like this. `type` says what kind of question it was. The
model says yes to the economy question with probability 0.96, names
housing as the topic at 1.00, and reports strong dissatisfaction at
1.00.

## Many texts at once

Pass a character vector and
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
codes every text. The calls run in parallel (at the same time) and come
back as one tibble. A leading `state` column holds the text (cut to 60
characters) and `state_id` numbers the texts in the order you gave them.

\
`many`` ``<-`` `[`decide`](https://edubruell.github.io/tidyllm/reference/decide.md)`(``answers``$``text``, ``questions``, `[`jev`](https://edubruell.github.io/tidyllm/reference/jev.md)`(``)``)`

Coding all eight answers with
[`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) took
under a second in our run. Coding with the fastest OpenAI chat model was
roughly ten times slower. A text that fails gives `NA` in `answer` and a
warning that counts the failures, so you can filter on `is.na(answer)`,
take the distinct `state_id` values and rerun only those texts.

Join the results to your data by `state_id`. The `coded` table has one
row per answer and question, with `id` linking it to the respondent.

\
`coded`` ``<-`` ``answers`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``state_id ``=`` `[`row_number`](https://dplyr.tidyverse.org/reference/row_number.html)`(``)``)`` ``|>`\
`  `[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``many``, by ``=`` ``"state_id"``)`\
\
`coded`` ``|>`\
`  `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``id``, ``question``, ``answer``, ``probability``)`` ``|>`\
`  `[`slice_head`](https://dplyr.tidyverse.org/reference/slice.html)`(``n ``=`` ``6``)`\
`## ``# A tibble: 6 × 4`\
`##      id question        answer  probability`\
`##   ``<int>`` ``<chr>``           ``<chr>``         ``<dbl>`\
`## ``1``     1 about_economy   yes            0.96`\
`## ``2``     1 topic           housing        1   `\
`## ``3``     1 dissatisfaction strong         1   `\
`## ``4``     2 about_economy   yes            0.97`\
`## ``5``     2 topic           labour         1   `\
`## ``6``     2 dissatisfaction none           0.97`

For a regression you want one row per respondent.
[`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
puts each question in its own column.

\
`wide`` ``<-`` ``coded`` ``|>`\
`  `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``id``, ``question``, ``answer``)`` ``|>`\
`  `[`pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html)`(``names_from ``=`` ``question``, values_from ``=`` ``answer``)`\
\
`wide`\
`## ``# A tibble: 8 × 4`\
`##      id about_economy topic     dissatisfaction`\
`##   ``<int>`` ``<chr>``         ``<chr>``     ``<chr>``          `\
`## ``1``     1 yes           housing   strong         `\
`## ``2``     2 yes           labour    none           `\
`## ``3``     3 no            none      none           `\
`## ``4``     4 yes           pensions  strong         `\
`## ``5``     5 yes           taxes     mild           `\
`## ``6``     6 no            migration strong         `\
`## ``7``     7 no            climate   mild           `\
`## ``8``     8 no            none      none`

Join `wide` to the respondent data by `id`. The probabilities stay in
`coded`, which the next section uses.

## Read the uncertain rows first

Treat the probability as a ranking: low means look twice. Sorting by it
shows where to read first. Here 4 of the 24 answers have a probability
below 0.8. That cut-off is a round number, not a rule.

\
`coded`` ``|>`\
`  `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``probability`` ``<`` ``0.8``)`` ``|>`\
`  `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``id``, ``question``, ``answer``, ``probability``)`` ``|>`\
`  `[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``probability``)`\
`## ``# A tibble: 4 × 4`\
`##      id question        answer probability`\
`##   ``<int>`` ``<chr>``           ``<chr>``        ``<dbl>`\
`## ``1``     5 dissatisfaction mild          0.65`\
`## ``2``     6 dissatisfaction strong        0.65`\
`## ``3``     7 about_economy   no            0.65`\
`## ``4``     4 dissatisfaction strong        0.7`

A probability is *calibrated* when it matches how often the answer is
right: of the answers given at 0.95, about 95 per cent should be
correct. Check this on your own data with a hand-labelled sample. In the
[classifier
article](https://edubruell.github.io/tidyllm/articles/tidyllm_classifiers.html),
96 to 100 per cent of the answers above 0.95 matched the human label,
against 51 to 62 per cent of the answers below 0.8.

## Images, audio and PDFs

[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
also reads photos, recordings and PDFs, with the same questions and the
same result tibble.
[`provider_capabilities()`](https://edubruell.github.io/tidyllm/reference/provider_capabilities.md)
shows what each provider reads.

\
[`provider_capabilities`](https://edubruell.github.io/tidyllm/reference/provider_capabilities.md)`(``.what ``=`` ``"decide_media"``)`` ``|>`\
`  `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``supported``)`` ``|>`\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``provider``)`` ``|>`\
`  `[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(``reads ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``media``, collapse ``=`` ``", "``)``)`\
`## ``# A tibble: 4 × 2`\
`##   provider reads                   `\
`##   ``<chr>``    ``<chr>``                   `\
`## ``1`` gemini   image, audio, video, pdf`\
`## ``2`` jev      image                   `\
`## ``3`` ollama   image                   `\
`## ``4`` openai   image`

A provider that cannot read a file type stops with a message before
anything is sent. Wrap a file in
[`img()`](https://edubruell.github.io/tidyllm/reference/img.md) to give
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md) a
picture. The question says “the picture”, because the picture is all the
model sees.

\
`cat_question`` ``<-`` `[`tidyllm_schema`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)`(`\
`  cat ``=`` `[`field_lgl`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"Is the animal in the picture a cat?"``)`\
`)`\
\
[`decide`](https://edubruell.github.io/tidyllm/reference/decide.md)`(`[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"photos/cat.jpg"``)``, ``cat_question``, `[`openai`](https://edubruell.github.io/tidyllm/reference/openai.md)`(``.detail ``=`` ``"low"``)``)`

`.detail = "low"` makes OpenAI read the picture at lower resolution,
which is cheaper. Gemini has the same switch under the name
`.resolution`.

To code many pictures, pass a list with one image per element;
`map(files, img)` wraps each file in
[`img()`](https://edubruell.github.io/tidyllm/reference/img.md). Each
element is its own item.

\
`files`` ``<-`` `[`list.files`](https://rdrr.io/r/base/list.files.html)`(``"photos"``, pattern ``=`` ``"jpg$"``, full.names ``=`` ``TRUE``)`\
\
[`decide`](https://edubruell.github.io/tidyllm/reference/decide.md)`(`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``files``, ``img``)``, ``cat_question``, `[`gemini`](https://edubruell.github.io/tidyllm/reference/gemini.md)`(``.resolution ``=`` ``"low"``)``)`

To ask about several files together, put them into one item: a named
list inside the outer [`list()`](https://rdrr.io/r/base/list.html). The
names label the parts, so the questions can say “photo_a” and “photo_b”.

\
`pets`` ``<-`` `[`tidyllm_schema`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)`(`\
`  a_dog ``=`` `[`field_lgl`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"Is the animal in photo_a a dog?"``)``,`\
`  b_dog ``=`` `[`field_lgl`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``"Is the animal in photo_b a dog?"``)`\
`)`\
\
[`decide`](https://edubruell.github.io/tidyllm/reference/decide.md)`(`[`list`](https://rdrr.io/r/base/list.html)`(`[`list`](https://rdrr.io/r/base/list.html)`(``photo_a ``=`` `[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"photos/cat.jpg"``)``, photo_b ``=`` `[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"photos/dog.jpg"``)``)``)``, ``pets``, `[`jev`](https://edubruell.github.io/tidyllm/reference/jev.md)`(``)``)`

## Finding things in images (experimental)

Sometimes the question is “where is it”, not “is there a cat”.
[`field_box()`](https://edubruell.github.io/tidyllm/reference/field_box.md)
asks for one box around the thing. This is experimental: only
[`jev()`](https://edubruell.github.io/tidyllm/reference/jev.md) answers
it, and the format may change in a later release. A box question always
returns a box, even when the thing is not in the picture, so pair it
with a yes/no question and read the box only where the answer is yes.
Name the picture in a backticked key and in the question text.

\
`where_cat`` ``<-`` `[`tidyllm_schema`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)`(`\
`  has_cat ``=`` `[`field_lgl`](https://edubruell.github.io/tidyllm/reference/field_chr.md)`(``` "Is there a cat in `photo`?" ```)``,`\
`  cat ``=`` `[`field_box`](https://edubruell.github.io/tidyllm/reference/field_box.md)`(``` "The cat in `photo`" ```)`\
`)`\
\
[`decide`](https://edubruell.github.io/tidyllm/reference/decide.md)`(`[`list`](https://rdrr.io/r/base/list.html)`(`[`list`](https://rdrr.io/r/base/list.html)`(``photo ``=`` `[`img`](https://edubruell.github.io/tidyllm/reference/img.md)`(``"photos/cat.jpg"``)``)``)``, ``where_cat``, `[`jev`](https://edubruell.github.io/tidyllm/reference/jev.md)`(``)``)`

\
`box_run``$``result`` ``|>`\
`  `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``question``, ``type``, ``answer``, ``probability``, ``top``, ``bottom``, ``left``, ``right``)`\
`## ``# A tibble: 2 × 8`\
`##   question type   answer probability   top bottom  left right`\
`##   ``<chr>``    ``<chr>``  ``<chr>``        ``<dbl>`` ``<dbl>``  ``<dbl>`` ``<dbl>`` ``<dbl>`\
`## ``1`` has_cat  yes_no yes           0.96  ``NA``    ``NA``       ``NA`` ``NA``   `\
`## ``2`` cat      box    ``NA``           ``NA``      0.2   0.68     0  0.79`

The box row has type `box` and no answer. `top`, `bottom`, `left` and
`right` are fractions of the picture, measured from the top-left corner.
Here the cat starts at the left edge (`left` is 0) and reaches 79 per
cent of the way to the right. The first row is the yes/no question, so
its box columns are empty.

To draw or crop, you need pixels.
[`box_to_pixels()`](https://edubruell.github.io/tidyllm/reference/box_to_pixels.md)
multiplies the fractions by the image size.

\
[`box_to_pixels`](https://edubruell.github.io/tidyllm/reference/box_to_pixels.md)`(``box_run``$``result``, .width ``=`` ``box_run``$``width``, .height ``=`` ``box_run``$``height``)`` ``|>`\
`  `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``question``, ``x_min``, ``x_max``, ``y_min``, ``y_max``)`\
`## ``# A tibble: 2 × 5`\
`##   question x_min x_max y_min y_max`\
`##   ``<chr>``    ``<dbl>`` ``<dbl>`` ``<dbl>`` ``<dbl>`\
`## ``1`` has_cat     ``NA``    ``NA``    ``NA``    ``NA`\
`## ``2`` cat          0   809   273   929`

We have not measured how accurate boxes are. Look at a sample by eye
before you rely on them.

## What to take away

- **Use
  [`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md)
  for closed questions:** each text, image or recording gets an answer
  from fixed options and a probability to sort by.
- **Write each question as a field:**
  [`field_lgl()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
  for yes/no,
  [`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
  for a choice, with `.ordered = TRUE` for a score; add level
  descriptions.
- **Pass many items at once:** a character vector or a list runs in
  parallel and returns one tibble with `state` and `state_id`.
- **Read the low-probability rows first,** and compare a hand-labelled
  sample with the model’s answers.

For a full workflow with codebooks, a second decision model and a
human-labelled sample, see [Classifying Texts with
tidyllm](https://edubruell.github.io/tidyllm/articles/tidyllm_classifiers.html).
