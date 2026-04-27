# Embedding Models in tidyllm

While most **tidyllm** use cases revolve around chat models, it also
supports **embedding models**, another type of large language model.
These models generate numerical vectors that map input text to points in
a high-dimensional space, where each point represents the semantic
meaning of the text:

- **Similar meanings are close together:** A text about a “cat” and one
  about a “kitten” map to nearby points.
- **Different meanings are farther apart:** A text about a “cat” and one
  about a “car” map to points much farther apart.

## Semantic Search in Economics Paper Abstracts

To demonstrate embeddings in action, we implement a **semantic search**
on a dataset of **22,960 economics paper abstracts published between
2010 and 2024**. Instead of relying on keyword matching, we use
embeddings to find papers with similar topics based on their underlying
meaning.

``` r

library(tidyverse)
library(tidyllm)
library(here)

abstracts <- read_rds("abstracts_data.rds")
print(abstracts, width = 60)
## # A tibble: 22,960 × 8
##    year  journal  authors volume firstpage lastpage abstract
##    <chr> <chr>    <chr>   <chr>  <chr>     <chr>    <chr>   
##  1 2024  Journal… Bauer,… 22     2075      2107     This pa…
##  2 2019  The Rev… Karlan… 86     1704      1746     We use …
##  3 2022  Journal… Corset… 20     513       548      We stud…
##  4 2018  The Rev… Anagol… 85     1971      2004     We stud…
##  5 2024  America… Thores… 16     447       79       This pa…
##  6 2024  Journal… Ren, Y… 238    NA        NA       The rap…
##  7 2013  The Rev… Adhvar… 95     725       740      A key p…
##  8 2022  Econome… Brooks… 90     2187      2214     If expe…
##  9 2011  Health … Fletch… 20     553       570      We exam…
## 10 2010  Journal… Rohwed… 24     119       38       Early r…
## # ℹ 22,950 more rows
## # ℹ 1 more variable: pdf_link <chr>

target_abstract <- "We use the expansion of the high-speed rail network in Germany as a natural experiment to examine the causal effect of reductions in commuting time between regions on the commuting decisions of workers and their choices regarding where to live and where to work. We exploit three key features in this setting: i) investment in high-speed rail has, in some cases dramatically, reduced travel times between regions, ii) several small towns were connected to the high-speed rail network only for political reasons, and iii) high-speed trains have left the transportation of goods unaffected. Combining novel information on train schedules and the opening of high-speed rail stations with panel data on all workers in Germany, we show that a reduction in travel time by one percent raises the number of commuters between regions by 0.25 percent. This effect is mainly driven by workers changing jobs to smaller cities while keeping their place of residence in larger ones. Our findings support the notion that benefits from infrastructure investments accrue in particular to peripheral regions, which gain access to a large pool of qualified workers with a preference for urban life. We find that the introduction of high-speed trains led to a modal shift towards rail transportation in particular on medium distances between 150 and 400 kilometers."
```

With the dataset ready, we use tidyllm to generate embeddings and
perform a semantic search to find papers similar to the
`target_abstract` from a paper on commuting and high-speed rail by
[Heuermann and Schmieder
(2018)](https://drive.google.com/file/d/1rKhTsrc9Qsi2gRUQg86zKKuUWvzx5HE1/view).
For this task we use the `mxbai-embed-large` model, available via Ollama
with `ollama pull mxbai-embed-large`. It is important to choose your
embedding model carefully upfront, as each model produces unique
numerical representations that are not interchangeable between models.

Embedding APIs are also available for
[`voyage()`](https://edubruell.github.io/tidyllm/reference/voyage.md),
[`mistral()`](https://edubruell.github.io/tidyllm/reference/mistral.md),
[`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md),
and
[`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
(as well as
[`azure_openai()`](https://edubruell.github.io/tidyllm/reference/azure_openai.md)).

### Step 1: Computing Embeddings for One Abstract

To compute an embedding of the target abstract we use
[`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md) with
[`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md) as
the provider:

``` r

target_tbl <- target_abstract |>
  embed(ollama(.model = "mxbai-embed-large:latest"))

target_tbl
str(target_tbl)
```

    ## # A tibble: 1 × 2
    ##   input                                           embeddings
    ##   <chr>                                           <list>    
    ## 1 We use the expansion of the high-speed rail ne… <dbl>

The [`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md)
function returns a tibble with two columns:

- **input:** The original text provided for embedding.
- **embeddings:** A list column containing the numerical vector for each
  input.

In our case we have a single input and the embeddings column contains a
1,024-dimensional vector:

``` r

str(target_tbl$embeddings)
## List of 1
##  $ : num [1:1024] -0.838 0.669 0.202 -0.686 -0.985 ...
```

### Step 2: Computing Embeddings for the Entire Abstract Corpus

When working with a large corpus like our 22,960 abstracts, embedding
all entries in a single pass is impractical. Commercial APIs typically
cap inputs per request at 50 to 100; local APIs face memory and compute
constraints.

We batch the data into groups of 200 and write results to disk as we go.
On a MacBook Pro M1 Pro this completes in approximately 25 minutes and
writes 207 MB of data. To compare multiple target abstracts against the
collection, we only need to embed the corpus once.

``` r

generate_abstract_embeddings <- function(abstracts) {
  embedding_batches <- tibble(abstract = abstracts) |>
    group_by(batch = floor(1:n() / 200) + 1) |>
    group_split()

  n_batches <- length(embedding_batches)
  glue("Processing {n_batches} batches of 200 abstracts") |> cat("\n")

  embedding_batches |>
    walk(\(batch) {
      batch_number <- pull(batch, batch) |> unique()
      glue("Generate Text Embeddings for Abstract Batch: {batch_number}/{n_batches}") |> cat("\n")

      batch$abstract |>
        embed(ollama(.model = "mxbai-embed-large:latest")) |>
        write_rds(here("embedded_abstracts", paste0(batch_number, ".rds")))
    })
}

abstracts |>
  pull(abstract) |>
  generate_abstract_embeddings()
```

    ## Processing 115 batches of 200 abstracts
    ## Generate Text Embeddings for Abstract Batch: 1/115
    ## Generate Text Embeddings for Abstract Batch: 2/115
    ## Generate Text Embeddings for Abstract Batch: 3/115
    ## ...

After the function finishes, load the precomputed embeddings:

``` r

embedded_abstracts <- here("embedded_abstracts") |>
  dir() |>
  map_dfr(\(f) read_rds(here("embedded_abstracts", f)))
```

### Step 3: Performing the Semantic Search

With embeddings precomputed, we perform a semantic search using cosine
similarity. **Cosine similarity** measures the similarity between two
vectors by computing the cosine of the angle between them; it ranges
from **-1** (opposite directions) to **1** (identical directions):

``` math
\text{cosine\_similarity}(\mathbf{a}, \mathbf{b}) = \frac{\sum_{i=1}^n a_i b_i}{\sqrt{\sum_{i=1}^n a_i^2} \cdot \sqrt{\sum_{i=1}^n b_i^2}}
```

Vectors at 90 degrees (orthogonal, no semantic overlap) have cosine
similarity of 0; vectors pointing in the same direction have similarity
close to 1. We express this as a simple function:

``` r

cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}
```

We apply it to find the 10 most similar abstracts in the corpus:

``` r

top10_similar <- embedded_abstracts |>
  mutate(cosine_sim = map_dbl(embeddings,
                              \(e) cosine_similarity(e, target_tbl$embeddings[[1]]))) |>
  arrange(desc(cosine_sim)) |>
  slice(1:10) |>
  rename(abstract = input) |>
  left_join(abstracts |> select(year, authors, journal, abstract), by = "abstract") |>
  select(-embeddings)
```

The ten most similar articles in the corpus:

| abstract | cosine_sim | year | authors | journal |
|:---|---:|:---|:---|:---|
| We use the expansion of the high-speed rail (HSR) network in Germany as a natural experiment to examine the causal effect of reductions in commuting time between regions on the commuting decisions of workers and their choices regarding where to live and where to work. We exploit three key features in this setting: (i) investment in HSR has, in some cases dramatically, reduced travel times between regions, (ii) several small towns were connected to the HSR network only for political reasons, and (iii) high-speed trains have left the transportation of goods unaffected. Combining novel information on train schedules and the opening of HSR stations with panel data on all workers in Germany, we show that a reduction in travel time by 1% raises the number of commuters between regions by 0.25%. This effect is mainly driven by workers changing jobs to smaller cities while keeping their place of residence in larger ones. Our findings support the notion that benefits from infrastructure investments accrue in particular to peripheral regions, which gain access to a large pool of qualified workers with a preference for urban life. We find that the introduction of high-speed trains led to a modal shift toward rail transportation in particular on medium distances between 150 and 400 km. | 0.9935939 | 2019 | Heuermann, Daniel; Schmieder, Johannes | Journal of Economic Geography |
| We analyze the economic impact of the German high-speed rail (HSR) connecting Cologne and Frankfurt, which provides plausibly exogenous variation in access to surrounding economic mass. We find a causal effect of about 8.5% on average of the HSR on the GDP of three counties with intermediate stops. We make further use of the variation in bilateral transport costs between all counties in our study area induced by the HSR to identify the strength and spatial scope of agglomeration forces. Our most careful estimate points to an elasticity of output with respect to market potential of 12.5%. The strength of the spillover declines by 50% every 30 min of travel time, diminishing to 1% after about 200 min. Our results further imply an elasticity of per-worker output with respect to economic density of 3.8%, although the effects seem driven by worker and firm selection. | 0.8984364 | 2018 | Ahlfeldt, Gabriel; Feddersen, Arne | Journal of Economic Geography |
| We investigate whether localities gain or lose employment when there are connected to a transportation network, such as a high-speed railway line. We argue that long-haul economies—implying that the marginal transportation cost decreases with network distance—play a pivotal role in understanding the location choices of firms. We develop a new spatial model to show that improvements in transportation infrastructure have nontrivial impacts on the location choices of firms. Using data on Japan’s Shinkansen, we show that ‘in-between’ municipalities that are connected to the Shinkansen witness a sizable decrease in employment. | 0.8691969 | 2022 | Koster, Hans; Tabuchi, Takatoshi; Thisse, Jacques | Journal of Economic Geography |
| How does intercity passenger transportation shape urban employment and specialization patterns? To shed light on this question I study China’s High Speed Railway (HSR), an unprecedentedly large-scale network that connected 81 cities from 2003 to 2014 with trains running at speeds over 200 km/h. Using a difference-in-differences approach, I find that an HSR connection increases city-wide passenger flows by 10% and employment by 7%. To deal with the issues of endogenous railway placement and simultaneous public investments accompanying HSR connection, I examine the impact of a city’s market access changes purely driven by the HSR connection of other cities. The estimates suggest that HSR-induced expansion in market access increases urban employment with an elasticity between 2 and 2.5. Further evidence on sectoral employment suggests that industries with a higher reliance on nonroutine cognitive skills benefit more from HSR-induced market access to other cities. | 0.8684935 | 2017 | Lin, Yatang | Journal of Urban Economics |
| We use the natural experiment provided by the opening and progressive extension of the Regional Express Rail (RER) between 1970 and 2000 in the Paris metropolitan region, and in particular the departure from the original plans due to budget constraints and technical considerations, to identify the causal impact of urban rail transport on firm location, employment and population growth. We apply a difference-in-differences method to a particular subsample, selected to minimize the endogeneity that is routinely found in the evaluation of the effects of transport infrastructure. We find that the RER opening caused a 8.8% rise in employment in the municipalities connected to the network between 1975 and 1990. While we find no effect on overall population growth, our results suggest that the arrival of the RER may have increased competition for land, since high-skilled households were more likely to locate in the vicinity of a RER station. | 0.8664813 | 2017 | Mayer, Thierry; Trevien, Corentin | Journal of Urban Economics |
| Infrastructure investment may reshape economic activities. In this article, I examine the distributional impacts of high-speed rail upgrades in China, which have improved passengers’ access to high-speed train services in the city nodes but have left the peripheral counties along the upgraded railway lines bypassed by the services. By exploiting the quasi-experimental variation in whether counties were affected by this project, my analysis suggests that the affected counties on the upgraded railway lines experienced reductions in GDP and GDP per capita following the upgrade, which was largely driven by the concurrent drop in fixed asset investments. This article provides the first empirical evidence on how transportation costs of people affect urban peripheral patterns. | 0.8579637 | 2017 | Qin, Yu | Journal of Economic Geography |
| Many US cities have made large investments in light rail transit in order to improve commuting networks. I analyse the labour market effects of light rail in four US metros. I propose a new instrumental variable to overcome endogeneity in transit station location, enabling causal identification of neighbourhood effects. Light rail stations are found to drastically improve employment outcomes in the surrounding neighbourhood. To incorporate endogenous sorting by workers, I estimate a structural neighbourhood choice model. Light rail systems tend to raise rents in accessible locations, displacing lower skilled workers to isolated neighbourhoods, which reduces aggregate metropolitan employment in equilibrium. | 0.8559837 | 2021 | Tyndall, Justin | Journal of Urban Economics |
| I study Los Angeles Metro Rail’s effects using panel data on bilateral commuting flows, a quantitative spatial model, and historically motivated quasi-experimental research designs. The model separates transit’s commuting effects from local productivity or amenity effects, and spatial shift-share instruments identify inelastic labor and housing supply. Metro Rail connections increase commuting by 16% but do not have large effects on local productivity or amenities. Metro Rail generates \$94 million in annual benefits by 2000 or 12â€“25% of annualized costs. Accounting for reduced congestion and slow transit adoption adds, at most, another \$200 million in annual benefits. | 0.8432086 | 2023 | Severen, Christopher | The Review of Economics and Statistics |
| We examine the effect of commuting distance on workers’ labour supply patterns, distinguishing between weekly labour supply, number of workdays per week and daily labour supply. We account for endogeneity of distance by using employer-induced changes in distance. In Germany, distance has a slight positive effect on daily and weekly labour supply, but no effect on the number of workdays. The effect of distance on labour supply patterns is stronger for female workers, but it is still small. | 0.8416284 | 2010 | Gutiérrez-i-Puigarnau, Eva; van Ommeren, Jos | Journal of Urban Economics |
| We estimate the causal impact of wage variations on commuting distance of workers. We test whether higher wages across years lead workers to live further away from their working place. We use employer–employee data for the French Ile-de-France region (surrounding Paris), from 2003 to 2008, and we deal with the endogenous relation between income and commuting using an instrumental variable strategy. We estimate that increases in wages coming from exogenous exposure to trade activities lead workers to increase their commuting distance and to settle closer to the city of Paris historical center. Our results cast novel insights upon the causal mechanisms from wage to spatial allocation of workers. | 0.8367841 | 2022 | Aboulkacem, El-Mehdi; Nedoncelle, Clément | Journal of Economic Geography |

Top 10 Most Similar Abstracts {.table .table .table-striped .table-hover
.table-condensed .table-responsive
style="width: auto !important; margin-left: auto; margin-right: auto;"}

Unsurprisingly, the top result is the target abstract itself. The
remaining results strongly align with the target’s focus on
transportation infrastructure and its economic effects; the second
result discusses German high-speed rail, while the third and fourth
cover Japan’s Shinkansen and China’s HSR network. This illustrates the
model’s ability to capture semantic connections across diverse
geographic and policy contexts.

## Multimodal Embeddings

While most embedding functions work only on text, the Voyage AI provider
in tidyllm supports embedding text and images in the same space. This is
particularly useful for **cross-modal search**, where text descriptions
and images need to be compared semantically.

Use [`img()`](https://edubruell.github.io/tidyllm/reference/img.md) to
create image objects and mix them with text in a list. When passing such
a list to `embed(voyage())`, the function automatically routes to
Voyage’s multimodal API:

``` r

list("a dish consisting of a sausage served in the slit of a partially sliced bun",
     img(here("hotdog.jpg"))) |>
  embed(voyage())
```

    ## # A tibble: 2 × 2
    ##   input                                                               embeddings
    ##   <chr>                                                               <list>    
    ## 1 a dish consisting of a sausage served in the slit of a partially s… <dbl>     
    ## 2 [IMG] hotdog.jpg                                                    <dbl>

Since the text description and the image refer to the same concept,
their embeddings will be close together in the high-dimensional space,
enabling similarity-based retrieval across modalities.

## Reranking with Voyage

Embedding-based search is fast for large corpora but not always precise;
cosine similarity scores can be noisy when the query and documents
differ in style or length. **Reranking** complements the retrieval step:
given a short candidate list, a reranker scores each document against
the query with a more powerful cross-attention model.

[`voyage_rerank()`](https://edubruell.github.io/tidyllm/reference/voyage_rerank.md)
wraps Voyage’s `/v1/rerank` endpoint and returns a tibble sorted by
relevance score:

``` r

candidates <- top10_similar$abstract

voyage_rerank(
  .query     = target_abstract,
  .documents = candidates,
  .model     = "rerank-2",
  .top_k     = 5
)
```

    ## # A tibble: 5 × 3
    ##   index relevance_score document                                                
    ##   <int>           <dbl> <chr>                                                   
    ## 1     0           0.921 We study the economic effects of German High-Speed Rail…
    ## 2     4           0.887 This paper examines how Shinkansen expansion affected...
    ## 3     1           0.854 We estimate the effect of China's high-speed rail netwo…
    ## 4     7           0.731 Infrastructure investment and regional labor markets... 
    ## 5     2           0.698 Commuting zones and the spatial structure of local labo…

A typical workflow combines both: embed the full corpus once, retrieve
the top 50 candidates with cosine similarity, then rerank to surface the
best 5 to 10. This keeps compute costs low while improving final result
quality.

## Outlook: Clustering and Beyond

Embeddings open the door to a wide range of analytical techniques beyond
semantic search:

- **Clustering for topic discovery:** Methods like K-means or
  hierarchical clustering can automatically group texts into themes,
  useful for exploratory research on large corpora without predefined
  labels.
- **Dimensionality reduction and visualisation:** PCA, t-SNE, or UMAP
  project embeddings into 2D or 3D, revealing clusters, trends, and
  outliers at a glance.
- **Retrieval-augmented generation (RAG):** An embedding search
  retrieves semantically similar documents, which are then appended to
  the model’s prompt to ground responses in relevant evidence.
- **Supporting qualitative research workflows:** As discussed in [Kugler
  et
  al. (2023)](https://www.iaw.edu/iaw-diskussionspapiere.html?file=files/dokumente/ab%20Januar%202023/iaw_dp_143.pdf),
  embeddings can automate coding steps in qualitative research, though
  models can still struggle with implicit references and closely related
  themes that human researchers handle naturally.
