library(survival)
library(survminer)
library(dplyr)
library(broom)

set.seed(123) # sequence of random numbers generated will be the same each time code is run

prostate_data <- data.frame(
  age = rnorm(300, 68, 8), # generate 300 random numbers, mean of 68 years, standard deviation of 8
  psa_baseline = rlnorm(300, 2, 0.8), # generate 300 random numbers from log-normal distribution, mean of 2, standard deviation of 0.8
  gleason_score = sample(6:10, 300, replace = TRUE), # randomly sample 300 values from 6 to 10 allowing for replacement
  treatment = sample(c("RT_alone", "RT_plus_ADT"), 300, replace = TRUE), # randomly assign treatment type to each of the 300 patients allowing for replacement
  time_to_recurrence = rexp(300, 0.05), # rexp() generates 300 random numbers from exponential distribution with rate parameter of 0.05, modeling time until event occurs
  recurred = rbinom(300, 1, 0.3) # rbinom() generates 300 random numbers for binomial distribution where each patient has 30% chance of recurrence (1) and 70% chance of no recurrence (0)
)

str(prostate_data)
summary(prostate_data)
colSums(is.na(prostate_data))
head(prostate_data)

# risk stratification
# creating new column risk_group
# ifelse(condition, true_value, false_value) function returns one value if condition is true and another value if false
# if the psa_baseline > 20 OR gleason_score >= 8, assign to "High" risk group
prostate_data$risk_group <- ifelse(
  prostate_data$psa_baseline > 20 | prostate_data$gleason_score >= 8,
  "High", "Low"
)

# survival analysis by risk
# create survival object or surv_prostate where $time_to_recurrence is time until event and $recurred is the censoring status/event ending
# survfit() estimates survival probability over time
# seeing the effects of the independent variable risk_group on surv_prostate AKA how does risk_group influence survival probability?
surv_prostate <- Surv(prostate_data$time_to_recurrence, prostate_data$recurred)
fit_risk <- survfit(surv_prostate ~ risk_group, data = prostate_data)

ggsurvplot(fit_risk, pval = TRUE, risk.table = TRUE)

# treatment benefit model
# how does age, psa_baseline_treatment, treatment:risk_group influence the risk of an event occurring?
cox_benefit <- coxph(Surv(time_to_recurrence, recurred) ~ age + psa_baseline + treatment + treatment:risk_group, data = prostate_data)
summary(cox_benefit)

# use broom's tidy() to construct a tibble to summarize model's statistical findings
hr_results <- tidy(cox_benefit, exponentiate = TRUE, conf.int = TRUE)
print(hr_results)

concordance(cox_benefit)
