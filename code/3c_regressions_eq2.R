library(data.table)
library(forcats)
library(stringi)
library(ggplot2)
library(patchwork)
library(ggh4x)
#library(future)
library(brms)
library(marginaleffects)
g <- glue::glue

setwd(Sys.getenv("CALL_THE_COPS"))

source("code/coef_map.R")
source("code/helpers/X_definition.R")
source("code/helpers/functions.R")

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_pooled[F1P1 == 1, path := "F1P1"]
victims_pooled[F0P1 == 1, path := "F0P1"]
victims_pooled[F1P0 == 1, path := "F1P0"]
victims_pooled[F0P0 == 1, path := "F0P0"]
victims_pooled[, path := fct_relevel(path, "F1P1")]

dat_cyber_pooled <- victims_pooled[incident.crime_class == "Cybercrime"]
dat_cyber_pooled <- dat_cyber_pooled[complete.cases(dat_cyber_pooled[, ..X_property])]
dat_cyber_pooled[, mean(is.na(path))]
dat_cyber_pooled[, region := fdroplevels(region)]
dat_cyber_pooled[, uniqueN(path)]

dat_prop_pooled <- victims_pooled[incident.crime_class == "Property"]
dat_prop_pooled <- dat_prop_pooled[complete.cases(dat_prop_pooled[, ..X_property])]
dat_prop_pooled[, mean(is.na(path))]
dat_prop_pooled[, region := fdroplevels(region)]
dat_prop_pooled[, uniqueN(path)]

dat_viol_pooled <- victims_pooled[incident.crime_class == "Violent"]
dat_viol_pooled <- dat_viol_pooled[complete.cases(dat_viol_pooled[, ..X_violent])]
dat_viol_pooled[, mean(is.na(path))]
dat_viol_pooled[, region := fdroplevels(region)]
dat_viol_pooled[, uniqueN(path)]

fml_prop_brm_multi <- as.formula(g("path | weights(weight) ~ {paste(c(X_property), collapse = '+')} + year + (1 | region)"))
fml_viol_brm_multi <- as.formula(g("path | weights(weight) ~ {paste(c(X_violent), collapse = '+')} + year + (1 | region)"))


# ==========================================================
# PART 2: CATEGORICAL MULTILEVEL MODEL (Multinomial)
# ==========================================================
# Fits true multinomial choices seamlessly. Runs in minutes, not hours!
brm_multi_cyber <- brm(
  formula = fml_prop_brm_multi,
  data    = dat_cyber_pooled,
  family  = categorical(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
brm_multi_prop <- brm(
  formula = fml_prop_brm_multi,
  data    = dat_prop_pooled,
  family  = categorical(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
brm_multi_viol <- brm(
  formula = fml_viol_brm_multi,
  data    = dat_viol_pooled,
  family  = categorical(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
summary(brm_multi_cyber)

save(list = grepv("^brm_", ls()), file = "temp/brm_multi_models.RData")

# AMEs -------------------------------------------------------------------------
ames_age <- avg_slopes(model_categorical_mcmc, variables = "ses.age_group", wts = "weight")
ames_gender <- avg_slopes(model_categorical_mcmc, variables = "ses.male", wts = "weight")

load("temp/brm_multi_models.RData")
ames_cyber_ses <- avg_slopes(brm_multi_cyber, variables = "ses.age_group", wts = "weight", re_formula = NULL)
as.data.table(ames_cyber_ses)

ames_cyber_ses <- avg_slopes(brm_multi_cyber, variables = grepv("^ses", X_property), wts = "weight")
ames_prop_ses <- avg_slopes(brm_multi_prop, variables = grepv("^ses", X_property), wts = "weight")
ames_viol_ses <- avg_slopes(brm_multi_viol, variables = grepv("^ses", X_property), wts = "weight")
