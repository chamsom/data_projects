# Scenario 3: Simulate a clinical trial dataset for evaluating AI-based prostate cancer risk prediction

library(tidyverse)
library(lubridate)
library(psych)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(123)

surv_df <- tibble(
  patient_id = 1:120, # sequential IDs
  age = rnorm(120, mean = 65, sd = 6), # normal distribution
  biopsy_score = round(runif(120, min = 4, max = 10), digits = 1),
  mri_score = round(runif(120, min = 0, max = 5), digits = 1),
  clinical_risk = sample(c("low", "intermediate", "high"), 120, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  time_to_event = round(rexp(120, rate = 0.05), digits = 1),
  event = sample(c(1, 0), 120, replace = TRUE, prob = c(0.6, 0.4))
)

head(surv_df)


# Scenario 4: Fit a Kaplan-Meier survival curve comparing clinical_risk groups

surv_obj <- Surv(surv_df$time_to_event, surv_df$event)
surv_obj # used as response variable in model

fit <- survfit(surv_obj ~ clinical_risk, data = surv_df)

ggsurvplot(fit,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           xlab = "Time (months)",
           ylab = "Survival Probability",
           legend.title = "Clinical Risk",
           legend.labs = c("low", "intermediate", "high")
           )

# run log-rank test explicitly
survdiff(Surv(surv_df$time_to_event, surv_df$event) ~ clinical_risk, data = surv_df)
