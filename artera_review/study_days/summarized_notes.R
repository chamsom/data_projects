# General useful packages
# broom packages take messy output and turns them into tidy tibbles
# install.packages(c("survival", "survminer", "dplyr", "tidyr", "broom"))

# Remember to set the seed
# set.seed(123)


# Guidelines on working with data

# 1. Explore the data:
# str(data)
# summary(data)
# head(data)
# dim(data)

# 2. Look for outcome variables:
# time, status, survival, outcome, response, etc.

# 3. Look for grouping variables
# treatment, group, sex, stage, etc.

# 4. For any scenario involving low_risk, high_risk in terms of scoring
# Use shaprio.test() to test for normality
# p >= 0.05 is normal, p < 0.05 is not normal
# if normal, run a basic t-test: t.test(data$outcome ~ data$group)
# if not normal, run wilcox.test(low_risk, high_risk)

# 5. For any scenario involving testing correlation
# Use Pearson correlation test: cor_test <- cor.test()
# correlation values close to 0 are not meaningful, meaning any observed correlation is likely due to chance

# 6. For survival models
# Kaplan - Meier Curves + Cox Regression Model (Cox Proportional Hazards Model)
# After, use cox.zph() to test the proportional hazards assumption
# To test the predictive accuracy of a survival model use: concordance(cox_model) where 0.5 is no better than random guessing

library(tidyverse)
library(survival)
library(survminer)
library(broom)

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

dim(surv_df)

head(surv_df)

surv_obj <- Surv(surv_df$time_to_event, surv_df$event)

fit <- survfit(surv_obj ~ clinical_risk, data = surv_df)

ggsurvplot(fit,
           conf.int = TRUE,
           pval = TRUE,
           surv.median.line = "h",
           risk.table = TRUE,
           legend.labs = c("low", "intermediate", "high")
           )

# 10% chance differences are due to random chance
survdiff(Surv(surv_df$time_to_event, surv_df$event) ~ clinical_risk, data = surv_df)

cox_model <- coxph(surv_obj ~ age + biopsy_score + mri_score, data = surv_df)
cox_tidy <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

concordance(cox_model)
