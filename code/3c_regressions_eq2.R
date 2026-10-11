library(data.table)
library(forcats)
library(stringi)
library(ggplot2)
library(patchwork)
library(ggh4x)
library(future)
library(future.apply)
library(brms)
library(marginaleffects)
library(modelsummary)
library(kableExtra)
g <- glue::glue

setwd(Sys.getenv("CALL_THE_COPS"))

source("code/helpers/coef_map.R")
source("code/helpers/X_definition.R")
source("code/helpers/functions.R")

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_pooled[F1P1 == 1, path := "F1P1"]
victims_pooled[F0P1 == 1, path := "F0P1"]
victims_pooled[F1P0 == 1, path := "F1P0"]
victims_pooled[F0P0 == 1, path := "F0P0"]
victims_pooled[, path := fct_relevel(path, "F1P1")]

# Enforce clean sequential conditional mapping block
# dat_viol_pooled[, path := as.character(NA)]

victims_pooled[, path_viol := as.character(NA)]

# 1. Complete Isolation
victims_pooled[incident.went_to_hospital == 0 & social.told_someone == 0 & reported.police == 0, path_viol := "F0P0H0"]

# 2. Pure Informal Track
victims_pooled[incident.went_to_hospital == 0 & social.told_someone == 1 & reported.police == 0, path_viol := "F1P0H0"]

# 3. Medical Social Track (Hospitalized, No Police)
victims_pooled[incident.went_to_hospital == 1 & reported.police == 0, path_viol := "FxP0H1"]

# 4. Pure Formal Track (Police involved, No Family)
victims_pooled[social.told_someone == 0 & reported.police == 1, path_viol := "F0P1Hx"]

# 5. Unified Joint Strategy (Both network and police involved, regardless of hospital)
victims_pooled[social.told_someone == 1 & reported.police == 1, path_viol := "F1P1Hx"]

# Convert to factor and explicitly anchor the unified joint strategy as your level 1 reference baseline
victims_pooled[, path_viol := fct_relevel(path_viol, "F1P1Hx")]

dat_cyber_pooled <- victims_pooled[incident.crime_class == "Cybercrime"]
dat_cyber_pooled <- dat_cyber_pooled[complete.cases(dat_cyber_pooled[, ..X_property])]
dat_cyber_pooled[, mean(is.na(path))]
dat_cyber_pooled[, region := fdroplevels(region)]
dat_cyber_pooled[, uniqueN(path)]
dat_cyber_pooled[, .N, path]

dat_prop_pooled <- victims_pooled[incident.crime_class == "Property"]
dat_prop_pooled <- dat_prop_pooled[complete.cases(dat_prop_pooled[, ..X_property])]
dat_prop_pooled[, mean(is.na(path))]
dat_prop_pooled[, region := fdroplevels(region)]
dat_prop_pooled[, uniqueN(path)]
dat_prop_pooled[, .N, path]

dat_viol_pooled <- victims_pooled[incident.crime_class == "Violent"]
dat_viol_pooled <- dat_viol_pooled[complete.cases(dat_viol_pooled[, ..X_violent])]
dat_viol_pooled[, mean(is.na(path_viol))]
dat_viol_pooled[, region := fdroplevels(region)]
dat_viol_pooled[, uniqueN(path_viol)]
dat_viol_pooled[, .N, path_viol]

fml_prop_brm_multi <- as.formula(g("path | weights(weight) ~ {paste(c(X_property), collapse = '+')} + year + (1 | region)"))
fml_viol_brm_multi <- as.formula(g("path_viol | weights(weight) ~ {paste(c(X_violent[X_violent != 'incident.went_to_hospital']), collapse = '+')} + year + (1 | region)"))

prop_priors <- c(
  # prior(normal(0, 3), class = "b", dpar = "muF0P0"),
  # prior(normal(0, 3), class = "b", dpar = "muF0P1"),
  # prior(normal(0, 3), class = "b", dpar = "muF1P0"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF0P0"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF0P1"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF1P0")
)

viol_priors <- c(
  # prior(normal(0, 3), class = "b", dpar = "muF0P0H0"),
  # prior(normal(0, 3), class = "b", dpar = "muF0P1Hx"),
  # prior(normal(0, 3), class = "b", dpar = "muF1P0H0"),
  # prior(normal(0, 3), class = "b", dpar = "muFxP0H1"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF0P0H0"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF0P1Hx"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF1P0H0"),
  prior(normal(0, 3), class = "Intercept", dpar = "muFxP0H1")
)

# ==========================================================
# PART 2: CATEGORICAL MULTILEVEL MODEL (Multinomial)
# ==========================================================
# Fits true multinomial choices seamlessly. Runs in minutes, not hours!
brm_multi_cyber <- brm(
  formula = fml_prop_brm_multi,
  data    = dat_cyber_pooled,
  family  = categorical(link = "logit"),
  prior = prop_priors,
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
brm_multi_cyber_null <- brms::brm(
  path | weights(weight) ~ (1 | region), 
  data = dat_cyber_pooled, 
  family = categorical(link = "logit"),
  prior = prop_priors,
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
brm_multi_prop <- brm(
  formula = fml_prop_brm_multi,
  data    = dat_prop_pooled,
  family  = categorical(link = "logit"),
  prior = prop_priors,
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
brm_multi_prop_null <- brms::brm(
  path | weights(weight) ~ (1 | region), 
  data = dat_prop_pooled, 
  family = categorical(link = "logit"),
  prior = prop_priors,
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
brm_multi_viol <- brm(
  formula = fml_viol_brm_multi,
  data    = dat_viol_pooled,
  family  = categorical(link = "logit"),
  prior = viol_priors,
  cores   = 4, chains = 4,
  backend = "cmdstanr",
  control = list(max_treedepth = 12)
)
brm_multi_viol_null <- brms::brm(
  path_viol | weights(weight) ~ (1 | region), 
  data = dat_viol_pooled, 
  family = categorical(link = "logit"),
  prior = viol_priors,
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
save(list = grep("^brm_", ls(), value = T), file = "temp/brm_multi_models.RData")

# AMEs -------------------------------------------------------------------------
# ames_age <- avg_slopes(model_categorical_mcmc, variables = "ses.age_group", wts = "weight")
# ames_gender <- avg_slopes(model_categorical_mcmc, variables = "ses.male", wts = "weight")

load("temp/brm_multi_models.RData")
library(future)
library(future.apply)

# 1. Open up 24 parallel worker threads on your 32-core Ubuntu box
plan(multisession, workers = 24) 

# 2. CRITICAL ENGINES SWITCH: Force marginaleffects to parallelize 
options(marginaleffects_parallel = TRUE)

ames_cyber_ses <- avg_slopes(brm_multi_cyber,
                             variables = grep("^(ses|attitude)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200, 
                             vcov       = FALSE )
ames_prop_ses <- avg_slopes(brm_multi_prop,
                             variables = grep("^(ses|attitude)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,
                             vcov       = FALSE )
ames_viol_ses <- avg_slopes(brm_multi_viol,
                             variables = grep("^(ses|attitude)", X_violent, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200, 
                             vcov       = FALSE )
save(list = grep("ames_.*_ses", ls(), value = T), file = "ames_multi_ses.RData")

ames_cyber_incident <- avg_slopes(brm_multi_cyber,
                             variables = grep("^(damage|incident)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_prop_incident <- avg_slopes(brm_multi_prop,
                             variables = grep("^(damage|incident)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_viol_incident <- avg_slopes(brm_multi_viol,
                             variables = grep("^(damage|incident)", X_violent[X_violent != "incident.went_to_hospital"], value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )

save(list = grep("ames_.*_incident", ls(), value = T), file = "ames_multi_incident.RData")

ames_cyber_full <- avg_slopes(brm_multi_cyber,
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_prop_full <- avg_slopes(brm_multi_prop,
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_viol_full <- avg_slopes(brm_multi_viol,
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )

save(list = grep("ames_.*_full", ls(), value = T), file = "ames_multi_full.RData")

# Full regressiona tables ===================================================== 

multi_ames_to_table(ames_cyber_full, brm_multi_cyber, brm_multi_cyber_null, "paper/tables/app_multi_cyber.tex")
multi_ames_to_table(ames_prop_full, brm_multi_prop, brm_multi_prop_null, "paper/tables/app_multi_prop.tex")
multi_ames_to_table(ames_viol_full, brm_multi_viol, brm_multi_viol_null, "paper/tables/app_multi_viol.tex")

# Plotting AMEs ====================================================================

load("ames_multi_ses.RData")
load("ames_multi_incident.RData")
load("ames_multi_full.RData")

# Non-violent
model_outputs_nonviol_ses <- list(
  "Cybercrime"     = as.data.table(ames_cyber_full)[grepl("^(ses|attitude)", term)],
  "Property" =  as.data.table(ames_prop_full)[grepl("^(ses|attitude)", term)]
)
model_outputs_nonviol_incident <- list(
  "Cybercrime"     = as.data.table(ames_cyber_full)[grepl("^(incident|damage)", term)],
  "Property" =  as.data.table(ames_prop_full)[grepl("^(incident|damage)", term)]
)

cleaned_plot_dt_nonviol_ses <- prep_profile_data(model_outputs_nonviol_ses)
cleaned_plot_dt_nonviol_incident <- prep_profile_data(model_outputs_nonviol_incident)

cleaned_plot_dt_nonviol_ses[, label := factor(label, levels = coef_map)]
cleaned_plot_dt_nonviol_incident[, label := factor(label, levels = coef_map)]

plot_multinomial_profiles(
  plot_data   = cleaned_plot_dt_nonviol_ses, 
  output_path = "paper/figures/multinomial_ses_profiles_nonviol.pdf"
)
plot_multinomial_profiles(
  plot_data   = cleaned_plot_dt_nonviol_incident, 
  output_path = "paper/figures/multinomial_incident_profiles_nonviol.pdf",
  width = 10, height = 5
)

# Violent
cleaned_plot_dt_viol_ses <- prep_profile_data(as.data.table(ames_viol_full)[grepl("^(ses|attitude)", term)])

cleaned_plot_dt_viol_ses[, label := factor(label, levels = coef_map)]

plot_multinomial_profiles(
  plot_data   = cleaned_plot_dt_viol_ses, 
  output_path = "paper/figures/multinomial_ses_profiles_viol.pdf",
)

cleaned_plot_dt_viol_incident <- prep_profile_data(as.data.table(ames_viol_full)[grepl("^(incident|damage)", term)])
cleaned_plot_dt_viol_incident[, label := factor(label, levels = coef_map)]

plot_multinomial_profiles(
  plot_data   = cleaned_plot_dt_viol_incident, 
  output_path = "paper/figures/multinomial_incident_profiles_viol.pdf",
)

