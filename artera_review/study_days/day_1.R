install.packages(c("tidyverse", "lubridate", "psych", "survival"))

library(tidyverse)
library(lubridate)
library(psych)
library(survival)

# tibble() creates modern, improved dataframe
# rnorm() generates consecutive numbers from 1 to 100
# sample() choose from numbers 1 - 5, make 100 selections, and after...
# ...picking a number, put it back to be picked again
df <- tibble(
  patient_id = 1:100,
  age = rnorm(100, mean = 65, sd = 7),
  biopsy_score = sample(1:5, 100, replace = TRUE),
  clinical_risk = sample(c("low", "intermediate", "high"),
                         100, replace = TRUE,
                         prob = c(0.3, 0.4, 0.3)) # weights for each option, where int risk is more common
)

head(df)

# %>% take output from left side, df and then feed it as first argument to function, filter()
# original df remains unchanged
df %>%
  filter(clinical_risk == "high")

# start with existing dataframe and pip it forward, creating a new column called "combined_score"
# mutate() creates derived metrics
# original df is changed as the result is saved back to the df variable
df <- df %>%
  mutate(combined_score = age * biopsy_score / 10)

head(df)

# Exercise 1: Create a new column age_group with 3 levels:
# <60, 60-70, >70
# Hint: Use mutate() + case_when()
df <- df %>%
  mutate(age_group = case_when(
    age < 60 ~ "<60",
    age >= 60 & age <= 70 ~ "60-70",
    age > 70 ~ ">70"
  ))

head(df)


# Exercise 2: Calculate the mean biopsy_score for each clinical_risk group:
# Hint: Use group_by() + summarize()
df %>%
  group_by(clinical_risk) %>%
  summarize(Avg_Biopsy_Score = mean(biopsy_score), .groups = "drop")


# Exercise 3: Rank patients from highest to lowest based on combined_score:
# Hint: Use arrange() with desc()
df %>%
  arrange(desc(combined_score))


# Scenario 1: Do patients in the high-risk group have significantly...
# higher combined_score than those in low-risk group?

# 1. Subset data to only low-risk and high-risk patients
# 2. Check normality of combined_score in each group
# 3. Decide between two-sample t-test or a Wilcoxon rank-sum test
# 4. Run the appropriate test
# 5. Interpret the results in plain language (clinical/statistical)