setwd("C:/Users/anna/Documents/survey_project")
library(dplyr)

survey <- read.csv("data/raw/survey_2025.csv")

clean_income <- function(x, cap) {
  x[x < 0] <- NA
  x[x > cap] <- cap
  x
}

survey <- survey |>
  filter(!is.na(id)) |>
  mutate(income = clean_income(income, 250000))

write.csv(survey, "C:/Users/anna/Documents/survey_project/data/clean/survey.csv")
