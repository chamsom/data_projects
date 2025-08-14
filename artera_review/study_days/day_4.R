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
# p-val = 0.7 here which means that there's a 70% chances that differences are due to random variation
survdiff(Surv(surv_df$time_to_event, surv_df$event) ~ clinical_risk, data = surv_df)


# Scenario 5: Investigate whether clinical risk is associated with time to event but...
# ...adjust for age and biopsy score as potential confounders

cox_model <- coxph(Surv(surv_df$time_to_event, surv_df$event) ~ clinical_risk + age + biopsy_score, data = surv_df)

cox_tidy <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)
cox_tidy

library(broom)

# visualize adjusted survival curves for each clinical_risk group keeping age and biopsy_score fixed at their means
newdata <- tibble(
  clinical_risk = c("low", "intermediate", "high"),
  age = mean(surv_df$age, na.rm = TRUE),
  biopsy_score = mean(surv_df$biopsy_score, na.rm = TRUE)
)

ggsurvplot(
  survfit(cox_model, newdata = newdata),
  data = surv_df,
  conf.int = TRUE,
  xlab = "Time (months)",
  ylab = "Survival Probability",
  legend.title = "Clinical Risk",
  legend.labs = c("low", "intermediate", "high")
)

# p-val >= 0.05 for clinical_risk, age, biopsy_score meaning the assumption holds
# effect of clinical_risk, age, and biopsy_score stays consistent over time
cox_zph <- cox.zph(cox_model)
cox_zph
