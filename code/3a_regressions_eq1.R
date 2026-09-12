library(mice)
library(survey)
# library(lme4)
library(modelsummary)
library(marginaleffects)
library(data.table)
# library(forcats)
# library(stringi)
# library(ggplot2)
# library(patchwork)
# library(ggh4x)
library(howManyImputations)
library(future)
library(future.apply)
g <- glue::glue

setwd("~/Projects/IRL/RCVS/call_the_cops")

source("code/coef_map.R")
source("code/regression_helpers/X_definition.R")
source("code/regression_helpers/functions.R")

# Dir to store results
out <- file.path("data", "temp", "multilevel")
dir.create(out, showWarnings = F, recursive = T)

# Set random seed for reproducibility
set.seed(0)

n_workers <- floor(parallelly::availableCores() * 2/3)
plan(multisession, workers = n_workers)

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_2024 <- readRDS("data/processed/victims_2024.Rds")

# Define vars to impute and those to use as predictors in imputation
vars_to_impute <- c("ses.household_income", "ses.settlement_type")
vars_to_use <- c("incident.crime_class",
                 "ses.male",
                 "ses.age_group",
                 "ses.has_partner",
                 "ses.edu",
                 "ses.household_size",
                 "ses.job",
                 "attitude.has_crime_record",
                 "weight.reg_scaled",
                 "region",
                 "year")

# Modeling
mlm_cyber <- pooled_regr_with_mult_imp(dat = victims_pooled,
                                     crime_class = "Cybercrime",
                                     y = "reported.police",
                                     X = c("social.told_someone", X_property),
                                     vars_to_impute = vars_to_impute,
                                     vars_to_use = vars_to_use,
                                     mdl_type = "mlm",
                                     marginal_effects = T
)
saveRDS(mlm_cyber, file.path(out, "mlm_cyber.Rds"))
summary(mlm_cyber)

mlm_prop <- pooled_regr_with_mult_imp(dat = victims_pooled,
                                     crime_class = "Property",
                                     y = "reported.police",
                                     X = c("social.told_someone", X_property),
                                     vars_to_impute = vars_to_impute,
                                     vars_to_use = vars_to_use,
                                     mdl_type = "mlm",
                                     marginal_effects = T
)
saveRDS(mlm_prop, file.path(out, "mlm_prop.Rds"))
summary(mlm_prop)

mlm_viol <- pooled_regr_with_mult_imp(dat = victims_pooled,
                                     crime_class = "Violent",
                                     y = "reported.police",
                                     X =  c("social.told_someone", X_violent),
                                     vars_to_impute = vars_to_impute,
                                     vars_to_use = vars_to_use,
                                     mdl_type = "mlm",
                                     marginal_effects = T
)
saveRDS(mlm_viol, file.path(out, "mlm_viol.Rds"))
summary(mlm_viol)

