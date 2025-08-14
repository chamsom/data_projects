# Useful Survival Analyis Methods in R

### R Setup + Data Exploration

`install.packages(c("survival", "survminer", "dplyr", "broom"))`

`broom` packages take messy output and turns them into tidy tibbles via: `tidy()`

Examples provided will be utilizing the lung dataset.

Load libraries and data using `library()` and `data()`

Start with these to inspect data and check for missing data:
`str()`
`summary()`
`colSums(is.na())`
`head()`

### Creating Survival Object and Plotting

**What is Survival Analysis for?**
1. Models time until an event occurs.
2. Provides information about **Censored Data** or some information about the individual's survival time without knowing exactly when the event occurred.
3. Estimates survival probabilities.

**Kaplan - Meier Curves**

The Kaplan-Meier curves visualize the survival probability for each clincal risk group over time. The p-values of the log-rank test show if there's a statistically significant different in survival distributions.

To create a survival object use: 
- `Surv(time, end_status)`
- `surv_obj <- Surv(lung$time, lung$status)`

To estimate survival probability and fit a Kaplan-Meier curve use: 
- `survfit(formula, data)`
For example: 
- `fit <- survfit(surv_obj ~ sex, data = lung)` **where in this case: How does sex influence survival probability?**

It is also possible to estimate survival probabilities for the entire data set as a single group for example:
- `one_year_fit <- summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365)`
- `~ 1` is an estimation of overall survival function without considering independent variables/covariates.
- We use `times = 365` to denote 365 days or 1 year.

Use `survdiff()` or a log-rank test to see whether there was a difference in survival time according to an independent variable for example:
- `survdiff(Surv(time, status) ~ sex, data = lung)`

### Creating Cox Regression Model (Cox Prorportional Hazards Model)

**What is Cox Regression Model for?**
1. Analyze and understand the relationship between survival time of subjects and one or more predictor variables (covariates).
2. Focuses on the **Hazard Function** or the risk of an event occurring given the subject has survived up to that time.
3. Estimates the hazard ratios for each predictor variable.

To create a cox regression model use:
- `coxph(Surv(time, end_status) ~ predictors, data)`
- `cox_model <- coxph(Surv(lung$time, lung$status) ~ age + sex, data = lung`

To get the hazard ratio (HR) and summarize results use:
**NOTE:** This example uses `exponentiate = TRUE` as we want the hazard ratio rather than the log hazard ratio.
- `tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)`

To assess the predictive accuracy of a survival model use:
- `concordance(cox_model)`

### Interpreting Results: Important Values to Discuss

**p-values**

p < 0.05: Statistically significant (5% chance it's random)
p >= 0.05: Marginally significant or trending (Technically non-significant)

**cox.zph()**

Tests the proportional hazards assumption - the idea that the hazard ratio between groups stays constant over time. Checks whether the effect of each variable remains the same throughout the entire study period.
For example, an assumption is made that those in the high-risk group have 2x the hazard of an event compared to low-risk at the beginning of the study.
The high-risk group should then maintain that same 2x ratio throughout the entire follow-up period.

p < 0.05: Assumption is violated (bad - fix this)
p >= 0.05: Assumption holds (good - trust Cox model)