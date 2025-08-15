library(tidyverse)

set.seed(123)

toy_df <- tibble(
  age = sample(1:100, size = 3, replace = TRUE),
  pct_scores = sample(0:100, size = 3, replace = TRUE),
  n_stage = sample(c("N0", "N1"), size = 3, replace = TRUE)
)

dim(toy_df)

# Build a function, function takes a df
# Depending on binning of variables, it assigns a value and then sums all values into 1 single score called a Clinical Nomogram Function

# getStarCap() should provide a Total Score which should range from 0 - 12

# Simplified Inputs:

# Age = age
# Percent Positive Cores = pct_cores
# Clinical N-stage = n_stage

# Simplified Rules:

# Age:
# 0-50 – Score 1
# 51-70 – Score 0
# 71+ – Score 1

# Percent Positive Core:
# 0-50% – Score 0
# 51-75% – Score 2
# 76-100% – Score 3

# Clinical N Stage:
# N0 – Score 0
# N1 – Score 8

# Modified Outputs:
# Total Score = total_score: 0 – 12
# Stage Group = stage_group: I, II, III+

# Total Score 0-4: I
# Total Score 5-10: II
# Total Score 11-12: III+

getStarCap <- function(input) {
  if (any(input$age < 0 | input$age > 120, na.rm = TRUE)) {
    stop("Invalid age values: must be between 0 and 120")
  }
  
  input %>%
    mutate(
      age_score = case_when(
        age <= 50 ~ 1,
        age <= 70 ~ 0,
        age > 70 ~ 1
      ),
      pct_score_score = case_when(
        pct_scores <= 50 ~ 0,
        pct_scores <= 75 ~ 2,
        pct_scores <= 100 ~ 3
      ),
      n_stage_score = ifelse(n_stage == "N0", 0, 8),
      total_score = age_score + pct_score_score + n_stage_score,
      
      total_score_score = case_when(
        total_score <= 4 ~ "I",
        total_score > 5 & total_score <= 10 ~ "II",
        total_score >= 11 ~ "III+"
    )
  )
}

result <- getStarCap(toy_df)
result

install.packages("testthat")
library("testthat")

test_df <- tibble(
  age = sample(c(-10, 50, NA), size = 3, replace = TRUE),
  pct_scores = sample(0:100, size = 3, replace = TRUE),
  n_stage = sample(c("N0", "N1"), size = 3, replace = TRUE)
)

test_that("Test total_score output", {
  test_df <- tibble(
    age = c(-50, 100, NA),
    pct_scores = sample(0:100, size = 3, replace = TRUE),
    n_stage = sample(c("N0", "N1"), size = 3, replace = TRUE)
  )
  
  result <- getStarCap(test_df)
  
})
