library(dplyr)
library(tidyr)

#' Share of respondents by group
#'
#' @param df A data frame with a `group` column.
#' @return A tibble with one row per group.
share_by_group <- function(df) {
  df |>
    count(group) |>
    mutate(share = n / sum(n))
}

wide_table <- function(df, by, value) {
  df |>
    pivot_wider(names_from = {{ by }}, values_from = {{ value }})
}

results <- share_by_group(read.csv("output/survey_clean.csv"))
saveRDS(results, "/tmp/results.rds")
