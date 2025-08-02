# c() combines values into a vector
install.packages(c("survival", "survminer", "dplyr", "broom"))

# library() used to load packages, making functions available for use
# survival package contains core survival analysis routines, including definition of Surv objects, etc.
# dplyr package is a tool that allows for working with data frame like objects in-memory/out-memory
library(survival)
library(dplyr)
data(lung)

# once loading the survival package, the lung dataset is already in the env but invisible
lung <- lung

# -------------------- ALWAYS START WITH THESE TO EXPLORE! --------------------

# str(object), in this case lung is the object as it was assigned via: lung <- lung
# think of str as STRucture
# displays the internal structure of the lung object with a summary including object's type, dimensions, preview of contents, etc.
str(lung)

# summary() provides statistical overview of data returning minimum, maximum, median, and quartiles
summary(lung)

# colSums() calculates sum of each column
# is.na() checks for missing values in the lung object
# colSums(is.na()) checks for missing values in the lung object and then sums those missing values in colSums()
colSums(is.na(lung))

# head(x, n=number) returns the first n rows of the dataset
# tail(x, n=number) returns the last n rows of the dataset
head(lung)

# str(), summary() colSums(is.na()), head()

# -------------------- ALWAYS START WITH THESE TO EXPLORE! --------------------




# -------------------- SURVIVAL CURVES --------------------

library(survminer)

# how to create a survival object
# '$' used to create and access variables in lists and dataframes
head(lung$time)
head(lung$status)

# Surv() creates a survival object, in this case Surv(time, time2)
# time is for right censored data, or true survival times equal to or greater than the observed
# $time is the survival time in days
# $status is the censoring status where 1=censored, 2=dead
# time2 is the ending time of the interval, assumed to be open on the left and closed on the right (start, end]
surv_obj <- Surv(lung$time, lung$status)
surv_obj # surv_obj will be used as response variable in model formula

# fit Kaplan-Meier curves, survfit() computes survival curves for censored data using the Kaplan-Meier method
# estimates probability of survival over time based on survival data
# survfit(formula, data) requires Surv object (surv_object) as the response to the left of the '~' operator
# the right hand side of the tilde is the independent variable, in this case sex
fit <- survfit(surv_obj ~ sex, data = lung)

# p <= 0.05 reject the null hypothesis
# p > 0.05 insignificant, fail to reject the null hypothesis
# CI represents an estimation of range or where the sample is likely to fall (reflects uncertainty in sampling)
# CI example: estimate = 70, margin of error = 2, thus, CI is 68 inches to 72 inches
# in this case, if the interval is 95%, you can be 95% confident that the population falls between 68 inches to 72 inches
# strata represents subgroups within a population that share similar characteristics such as sampling by age, gender, etc.
ggsurvplot(fit,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           legend.labs = c("Male", "Female"))

summary(fit)

# -------------------- SURVIVAL CURVES --------------------




# -------------------- USING PH.ECOG --------------------

# NULL HYPOTHESIS: As time progresses, there is no difference in the likelihood of becoming bedbound between males and females.
# lung$time: time until an event and the lung$ph.ecog: becoming bedbound/event indicator
# 0 = asymptomatic, 1 = symptomatic but completely ambulatory, 2 = in bed < 50% of the day, 3 = in bed > 50% of the day but not bedbound, 4 = bedbound
surv_bed_obj <- Surv(lung$time, lung$ph.ecog)

surv_bed_obj

# use survfit() to compute survival curves for censored data/estimate probability of survival over time
# surv_bed_obj is the response whereas sex is the independent variable
bed_fit <- survfit(surv_bed_obj ~ sex, data = lung)

ggsurvplot(bed_fit,
           title = "Probability Until Becoming Bedbound",
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           legend.labs = c("Male", "Female"))

summary(bed_fit)

# -------------------- USING PH.ECOG --------------------




# -------------------- COX REGRESSION --------------------

