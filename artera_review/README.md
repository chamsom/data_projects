# Useful Survival Analyis Methods in R

### R Setup + Data Exploration

`install.packages(c("survival", "survminer", "dplyr", "broom"))`

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

To create a survival object use: `Surv(time, end_status)`
For example: `surv_obj <- Surv(lung$time, lung$status)`

To estimate survival probability and fit a Kaplan-Meier curve use: 
- `survfit(formula, data)`
For example: 
- `fit <- survfit(surv_obj ~ sex, data = lung)` where in this case: How does sex influence survival probability?

It is also possible to estimate survival probabilities for the entire data set as a single group for example:
- `one_year_fit <- summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365)`
- `~ 1` is an estimation of overall survival function without considering independent variables/covariates.
- We use `times = 365` to denote 365 days or 1 year.

Use `survdiff()` or a log-rank test to see whether there was a difference in survival time according to an independent variable for example:
- `survdiff(Surv(time, status) ~ sex, data = lung)`

### Creating Cox Regression Model 