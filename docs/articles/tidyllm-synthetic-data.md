# Generate Synthetic Data with tidyllm

> ⚠️ **Note:** This article refers to the development version 0.2.5. of
> **tidyllm**, which has a chnaged interface compared to the last CRAN
> release. You can install the current development version directly from
> GitHub using devtools:

\
`# Install devtools if not already installed`\
`if`` ``(``!`[`requireNamespace`](https://rdrr.io/r/base/ns-load.html)`(``"devtools"``, quietly ``=`` ``TRUE``)``)`` ``{`\
`  `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"devtools"``)`\
`}`\
\
`# Install TidyLLM from GitHub`\
`devtools``::`[`install_github`](https://remotes.r-lib.org/reference/install_github.html)`(``"edubruell/tidyllm"``)`

As large language models (LLMs) evolve, their potential to simulate
human-like behavior offers an exciting opportunity for researchers in
various fields. Inspired by the work of [Horton et al
(2023)](https://arxiv.org/abs/2301.07543) on running behivoural
econonomics lab experiments with a “homo silicus”, LLMs acting as
computational models of humans, this article explores how synthetic data
generated from LLMs can be used to pretest survey designs before going
into the field.

Pretesting with synthetic data enables quick iterations on survey
questions, helping you spot potential issues early on and save time
during actual data collection. For example, if you’re preparing a survey
for lawyers on automation, you can use **tidyllm** to simulate survey
responses based on real profiles. This allows you to write analysis code
and identify ambiguities in question phrasing or logic flow, ensuring a
smoother deployment when surveying real participants.

Imagine for example that you are a researcher who just got contact info
for many lawyers you want to invite to a survey on automation, provided
by the bar association. You have already enriched this data with
information from web-based sources, such as advertising taglines from
law firm websites. Here is what such data could look like:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyllm`](https://edubruell.github.io/tidyllm/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`glue`](https://glue.tidyverse.org/)`)`\
\
`lawyers`` ``<-`` `[`read_rds`](https://readr.tidyverse.org/reference/read_rds.html)`(``"lawyer_profiles.rds"``)`\
`lawyers`\
`## ``# A tibble: 495 × 9`\
`##    first_name last_name     year_bar_admission specialization  location         `\
`##    ``<chr>``      ``<chr>``         ``<chr>``              ``<chr>``           ``<chr>``            `\
`## `` 1`` Solon      ``"``Predovic``"``    2018               Criminal Law    Denver, CO       `\
`## `` 2`` Linzy      ``"``Gleason``"``     2020               Tax Law         San Francisco, CA`\
`## `` 3`` Miss Amiya ``"``Braun``"``       2015               Family Law      Atlanta, GA      `\
`## `` 4`` Irvine     ``"``Kerluke PhD``"`` 2008               Corporate Law   Chicago, IL      `\
`## `` 5`` Marta      ``"``Waelchi ``"``    2012               Healthcare Law  Boston, MA       `\
`## `` 6`` Affie      ``"``Kreiger``"``     2018               Criminal Law    Dallas, TX       `\
`## `` 7`` Montie     ``"``Kunze``"``       2002               Tax Law         San Francisco, CA`\
`## `` 8`` Marleigh   ``"``Fahey``"``       2015               Family Law      Miami, FL        `\
`## `` 9`` Olene      ``"``Stokes``"``      2012               Corporate Law   Chicago, IL      `\
`## ``10`` Sonji      ``"``Stark ``"``      2009               Estate Planning Boston, MA       `\
`## ``# ℹ 485 more rows`\
`## ``# ℹ 4 more variables: bar_admission_state <chr>, place_of_education <chr>,`\
`## ``#   email <chr>, advertising_tagline <chr>`

To generate synthetic survey responses based on real profiles, we first
need to format the lawyer profiles into a human-readable prompt that the
language model can use to simulate responses. This step involves
transforming the data to create a synthetic “person” that the model will
assume when responding to the survey.

The following code prepares the second lawyer profile as a role for the
language model. So as a first step we get each individual profile into
list of single-row tibbles that we can modify with further functions:

\
`profiles`` ``<-`` ``lawyers`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`group_split`](https://dplyr.tidyverse.org/reference/group_split.html)`(``)`

Each of these invidual profile tibbles can be prepared as a potential
input into the model prompt. Here is an example step to get the profile
for the first person in our data in a well-readable format:

\
`profiles``[[``1``]``]`` ``|>`\
`  `[`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)`(``cols ``=`` `[`everything`](https://tidyselect.r-lib.org/reference/everything.html)`(``)``)`` ``|>`\
`  `[`glue_data`](https://glue.tidyverse.org/reference/glue.html)`(``"{str_replace_all(name, '_', ' ') |> str_to_sentence()}: {value}"``)`` ``|>`\
`  `[`str_c`](https://stringr.tidyverse.org/reference/str_c.html)`(``collapse ``=`` ``"\n"``)`` ``|>`\
`  `[`cat`](https://rdrr.io/r/base/cat.html)`(``)`\
`## First name: Solon`\
`## Last name: Predovic`\
`## Year bar admission: 2018`\
`## Specialization: Criminal Law`\
`## Location: Denver, CO`\
`## Bar admission state: Colorado`\
`## Place of education: University of Colorado Law School`\
`## Email: solonpredovic@co-legal.com`\
`## Advertising tagline: Protecting Your Rights in the Mile High City`

- This code uses
  [`pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  to convert the profile into key-value pairs.
- It then applies
  [`glue_data()`](https://glue.tidyverse.org/reference/glue.html) to
  format each row neatly, replacing underscores with spaces and
  capitalizing field names.
- Lastly it Collapses the profile into a single string with each
  attribute on a new line, which can then be used as input for the
  model.

With a way to get reasonable person profiles, we can put a first
questionaire flow into a function:

\
`generate_synthetic_study_opener`` ``<-`` ``function``(``lawyer_profile``)`` ``{`\
`  ``#Get the person prompt to glue into the initial setup`\
`  ``person_prompt`` ``<-`` ``lawyer_profile`` ``|>`\
`    `[`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)`(``cols ``=`` `[`everything`](https://tidyselect.r-lib.org/reference/everything.html)`(``)``)`` ``|>`\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``name ``=`` `[`str_replace_all`](https://stringr.tidyverse.org/reference/str_replace.html)`(``name``,``"_"``, ``" "``)`` ``|>`` `[`str_to_sentence`](https://stringr.tidyverse.org/reference/case.html)`(``)``,`\
`           profile_row ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``name``, ``": "``, ``value``)``)`` ``|>`\
`    `[`pull`](https://dplyr.tidyverse.org/reference/pull.html)`(``profile_row``)`` ``|>`\
`    `[`str_c`](https://stringr.tidyverse.org/reference/str_c.html)`(``collapse ``=`` ``"\n"``)`\
\
`  ``#Setup an initial task and the first question     `\
`  ``initial_setup`` ``<-`` `[`glue`](https://glue.tidyverse.org/reference/glue.html)`(``'Imagine you are this person and you are participating in `\
`  a survey on automation and the future of legal occupations:`\
`  `\
`  {person_prompt}`\
`       `\
`  How would you answer the following questions. `\
`  ONLY ANSWER WITH A VALID RESPONSE BUTTON`\
`  ----`\
`  Question 1: What gender do you identify as?`\
`  1 = Male`\
`  2 = Female`\
`  3 = Diverse`\
`  99 = Prefer not to say`\
`  '``)`` `\
\
`  ``#Pipe the initial setup into the second and third question,`\
`  ``# as well as model answers. `\
`  `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``initial_setup``)`` ``|>`\
`    `[`ollama_chat`](https://edubruell.github.io/tidyllm/reference/ollama_chat.md)`(``.model ``=`` ``"gemma2"``)`` ``|>`\
`    `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Question 2: What's your year of birth?`\
`              Answer with a four-digit number.`\
`              "``)`` ``|>`\
`    `[`ollama_chat`](https://edubruell.github.io/tidyllm/reference/ollama_chat.md)`(``.model ``=`` ``"gemma2"``)`` ``|>`\
`    `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Question 3: How familiar are you with the term AI?`\
`              `\
`              1 = Not familiar at all`\
`              2 = Slightly familiar`\
`              3 = Somewhat familiar`\
`              4 = Moderately familiar`\
`              5 = Very familiar`\
`              `\
`            99 = Prefer not to say  `\
`              "``)`` ``|>`\
`    `[`ollama_chat`](https://edubruell.github.io/tidyllm/reference/ollama_chat.md)`(``.model ``=`` ``"gemma2"``)`\
`}`\
\
`  `\
`profile1_questionaire`` ``<-`` ``generate_synthetic_study_opener``(``profiles``[[``1``]``]``)`\
`profile1_questionaire`

    ## Message History:
    ## system: You are a helpful assistant
    ## --------------------------------------------------------------
    ## user: Imagine you are this person and you are participating in 
    ## a survey on automation and the future of legal occupations:
    ## 
    ## First name: Solon
    ## Last name: Predovic
    ## Year bar admission: 2018
    ## Specialization: Criminal Law
    ## Location: Denver, CO
    ## Bar admission state: Colorado
    ## Place of education: University of Colorado Law School
    ## Email: solonpredovic@co-legal.com
    ## Advertising tagline: Protecting Your Rights in the Mile High City
    ##      
    ## How would you answer the following questions. 
    ## ONLY ANSWER WITH A VALID RESPONSE BUTTON
    ## ----
    ## Question 1: What gender do you identify as?
    ## 1 = Male
    ## 2 = Female
    ## 3 = Diverse
    ## 99 = Prefer not to say
    ## --------------------------------------------------------------
    ## assistant: 1 
    ## 
    ## --------------------------------------------------------------
    ## user: Question 2: What's your year of birth?
    ##               Answer with a four-digit number.
    ##               
    ## --------------------------------------------------------------
    ## assistant: 1991
    ## --------------------------------------------------------------
    ## user: Question 3: How familiar are you with the term AI?
    ##               
    ##               1 = Not familiar at all
    ##               2 = Slightly familiar
    ##               3 = Somewhat familiar
    ##               4 = Moderately familiar
    ##               5 = Very familiar
    ##               
    ##             99 = Prefer not to say  
    ##               
    ## --------------------------------------------------------------
    ## assistant: 4
    ## --------------------------------------------------------------

An easy way to get the assistant answers from this message history is to
use the
[`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)-function
which can give you assistant messages based on their index in the
message history.

\
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`\
`  gender      ``=`` `[`get_reply`](https://edubruell.github.io/tidyllm/reference/get_reply.md)`(``profile1_questionaire``,``1``)`` ``|>`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(``)``,`\
`  birth_year  ``=`` `[`get_reply`](https://edubruell.github.io/tidyllm/reference/get_reply.md)`(``profile1_questionaire``,``2``)`` ``|>`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(``)``,`\
`  ai_familiar ``=`` `[`get_reply`](https://edubruell.github.io/tidyllm/reference/get_reply.md)`(``profile1_questionaire``,``3``)`` ``|>`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(``)`\
`)`\
`## ``# A tibble: 1 × 3`\
`##   gender birth_year ai_familiar`\
`##    ``<int>``      ``<int>``       ``<int>`\
`## ``1``      1       ``1``991           4`

We could now put this into our `generate_synthetic_study_opener()`
function and directly get the answers together with some information
from the lawyer profiles we put in. But there is still a part missing.
In surveys we often want to either have preset paths that differ (e.g. a
randomized information treatment) or questions that are only shown, when
some previous answer was chosen. Both are fairly straightforward to
implement with **tidyllm**.

## Implementing an information treatment

Our next step if to implement a `generate_synthetic_infotreatment()`
function that is designed to simulate how respondents might update their
answers after receiving new information, known as an information
treatment. In this example, the treatment is based on a study that
reports [high exposure of legal professionals to automation through
generative
AI](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4414065). The
function takes in two arguments: `conversation`, which represents the
ongoing interaction between the LLM and the questionaire based on the
responses to `generate_synthetic_answers()`, and `treated`, a boolean
flag indicating whether the respondent receives the information
treatment.

The function starts by retrieving the synthetic respondent’s previous
answers to key questions for output. It then provides an AI
automation-related prompt to gauge the respondent’s initial perception
of their occupation’s automatable potential (the “prior” belief). If the
respondent is in the treated group, they receive the additional
information about legal professionals’ AI exposure. After presenting
this information, the function prompts the respondent to reconsider
their initial answer, thus capturing the “posterior” belief. Finally,
the function returns both the prior and posterior beliefs along with the
respondent’s demographic information, offering insights into how the
information treatment affects perceptions of AI automation.

Here is how such a simplified version of such a function (without
error-handling or cleanup logic) might look like:

\
`generate_synthetic_infotreatment`` ``<-`` ``function``(``conversation``, ``treated``)`` ``{`\
`  `\
`  ``# Extract key initial answers (gender, birth year, familiarity with AI)`\
`  ``answers_opener`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`\
`    gender      ``=`` `[`get_reply`](https://edubruell.github.io/tidyllm/reference/get_reply.md)`(``conversation``, ``1``)``,`\
`    birth_year  ``=`` `[`get_reply`](https://edubruell.github.io/tidyllm/reference/get_reply.md)`(``conversation``, ``2``)``,`\
`    ai_familiar ``=`` `[`get_reply`](https://edubruell.github.io/tidyllm/reference/get_reply.md)`(``conversation``, ``3``)`\
`  ``)`\
`  `\
`  ``# Ask the prior belief question (before treatment)`\
`  ``prior`` ``<-`` ``conversation`` ``|>`\
`    `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Among all occupations, how automatable do you think is your occupation?`\
`                  `\
`                  0 = Not Automatable `\
`                  1 = Among the 10 lowest percent`\
`                  2 = Among the 20 lowest percent`\
`                  3 = Among the 30 lowest percent`\
`                  4 = Among the 40 lowest percent`\
`                  5 = Right in the middle`\
`                  6 = Among the top 40 percent`\
`                  7 = Among the top 30 percent`\
`                  8 = Among the top 20 percent`\
`                  9 = Among the top 10 percent`\
`                  10 = At the very top`\
`                  99 = Prefer not to say`\
`                  "``)`` ``|>`\
`    `[`ollama_chat`](https://edubruell.github.io/tidyllm/reference/ollama_chat.md)`(``.model ``=`` ``"gemma2"``)`\
`  `\
`  ``# Extract the prior answer (belief before the treatment)`\
`  ``prior_answer`` ``<-`` ``prior`` ``|>`` `[`last_reply`](https://edubruell.github.io/tidyllm/reference/get_reply.md)`(``)`` ``|>`` `[`str_squish`](https://stringr.tidyverse.org/reference/str_trim.html)`(``)`\
`  `\
`  ``# Default to use the conversation state of the prior answer for the untreated group`\
`  ``post_treatment`` ``<-`` ``prior`\
`  `\
`  ``# Initialize the info-updating variable (0 means no treatment)`\
`  ``info_updating`` ``<-`` ``"0"`\
`  `\
`  ``# Apply the information treatment if the treated flag is TRUE`\
`  ``if`` ``(``treated``)`` ``{`` `\
`    ``post_treatment`` ``<-`` ``prior`` ``|>`\
`      `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"A recent study titled *Occupational, Industry, and Geographic Exposure to Artificial Intelligence* by Ed Felten (Princeton), Manav Raj (University of Pennsylvania), and Robert Seamans (New York University) identified legal professionals, including lawyers and judges, as some of the occupations with the highest exposure to AI technologies. `\
\
`According to the study, legal professionals are among the top 20 among 774 occupations most exposed to generative AI, suggesting that tasks traditionally performed by lawyers, such as legal research and document review, could be increasingly automated in the coming years.`\
\
`Have you read this information?`\
`1 = YES`\
`2 = NO`\
`99 = Prefer not to say`\
`"``)`` ``|>`\
`      `[`ollama_chat`](https://edubruell.github.io/tidyllm/reference/ollama_chat.md)`(``.model ``=`` ``"gemma2"``)`\
`    `\
`    ``# Update info-updating based on whether the participant confirms reading the information`\
`    ``info_updating`` ``<-`` `[`last_reply`](https://edubruell.github.io/tidyllm/reference/get_reply.md)`(``post_treatment``)`\
`  ``}`\
`  `\
`  ``# Ask the posterior belief question (after treatment)`\
`  ``# Untreated ar also asked if they want to update`\
`  ``post_treatment`` ``|>`\
`    `[`llm_message`](https://edubruell.github.io/tidyllm/reference/llm_message.md)`(``"Do you want to correct your previous answer? Which of these do you pick?`\
`                  `\
`                  0 = Not Automatable `\
`                  1 = Among the 10 lowest percent`\
`                  2 = Among the 20 lowest percent`\
`                  3 = Among the 30 lowest percent`\
`                  4 = Among the 40 lowest percent`\
`                  5 = Right in the middle`\
`                  6 = Among the top 40 percent`\
`                  7 = Among the top 30 percent`\
`                  8 = Among the top 20 percent`\
`                  9 = Among the top 10 percent`\
`                  10 = At the very top`\
`                  99 = Prefer not to say`\
`                  "``)`` ``|>`\
`    `[`ollama`](https://edubruell.github.io/tidyllm/reference/ollama.md)`(``.model ``=`` ``"gemma2"``)`\
`  `\
`  ``# Extract the posterior answer (belief after the treatment)`\
`  ``posterior_answer`` ``<-`` `[`last_reply`](https://edubruell.github.io/tidyllm/reference/get_reply.md)`(``post_treatment``)`\
`  `\
`  ``# Combine demographic data, prior and posterior beliefs, and info-updating status`\
`  ``answers_opener`` ``|>`\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``prior ``=`` ``prior_answer``,`\
`           info_updating ``=`` ``info_updating``,`\
`           posterior ``=`` ``posterior_answer``)`\
`}`\
\
`#Let's generate this treatment under the assumption that our first example lawyer was treated`\
`profile1_info_treatment`` ``<-`` ``profile1_questionaire`` ``|>`` `\
`  ``generate_synthetic_infotreatment``(``treated ``=`` ``TRUE``)`\
\
`#Print the result tibble`\
`profile1_info_treatment`

    ## # A tibble: 1 × 6
    ##   gender birth_year ai_familiar prior info_updating posterior
    ##   <chr>  <chr>      <chr>       <chr> <chr>         <chr>    
    ## 1 "1 \n" 1991       4           3     2             2

Our lawyer is very familiar with AI, has a low prior on automatibility
of his occupation, choose not to read the info material and did update
his posterior. Ironically, for now the only lawyer we’ve fully
“replaced” with generative AI as synthetic survey respondent seems to
believe his job is safe from automation.

We could now loop over each lawyer profile, make each of them answer the
survey, or add further questions. From here we have a basic setup to
generate synthetic data with **tidyllm**

## Validity and Limitations

While synthetic data from LLMs offers valuable insights for pretesting
surveys, it’s important to recognize the limitations of this approach.
LLM-generated responses are approximations and might miss nuances that
come with real human respondents. For instance, the model might not
accurately reflect personal biases, experiences, or diverse legal
practices that influence real lawyers’ perspectives on automation.

Additionally, as AI models are trained on vast datasets, there might be
overgeneralization, especially for niche professions we have in our data
(i.e. very specialized lawyers). Therefore, while synthetic data can
streamline early iterations of survey design, it should complement, not
replace, actual human feedback during later stages of research.

## Outlook

Looking ahead, the integration of synthetic data generation with tools
like **tidyllm** into traditional survey workflows offers exciting
possibilities for researchers. As LLMs become more advanced and capable
of simulating nuanced human behaviors, the accuracy of synthetic
responses will likely improve. This could lead to faster, more efficient
iterations in survey design, enabling researchers to refine questions
and test hypotheses with diverse, simulated populations before
real-world deployment.

Moreover, future advancements may allow for greater customization of
synthetic respondents, capturing more complex demographic variables and
behavioral patterns. For instance, by enhancing the ability to simulate
specific professions, backgrounds, or even emotional states, synthetic
data could evolve into a robust tool for experimental pretesting in
fields beyond survey research, such as behavioral economics, political
polling, and educational assessment.
