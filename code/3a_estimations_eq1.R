library(mice)
library(fixest)
library(data.table)
library(forcats)
library(stringi)
library(ggplot2)
library(patchwork)
library(ggh4x)
library(howManyImputations)
library(future)
g <- glue::glue
source("code/coef_map.R")
source("code/regression_helpers/X_definition.R")
source("code/regression_helpers/functions.R")

# options(browser="/usr/bin/google-chrome")

# Dir to store results
dir.create("data/temp/lpm", showWarnings = F, recursive = T)

# Set random seed for reproducibility
set.seed(0)
# Set number of workers for parallel multiple imputations
# <<<<<<< HEAD
# n_workers <- 7
# =======
# n_workers <- floor(parallelly::availableCores() * 2/3)
# >>>>>>> 763fa095c1a4fed5bd5bc1d5c80c776795a78aee
# plan(multisession, workers = n_workers)

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_2024 <- readRDS("data/processed/victims_2024.Rds")

# Regression formulas
fml_property <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | {paste(FE, collapse = '+')}"))
fml_violent <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | {paste(FE, collapse = '+')}"))

fml_property_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))
fml_violent_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))

# Define vars to impute and those to use as predictors
vars_to_impute <- c("ses.household_income", "ses.settlement_type")
vars_to_use <- c(
	"incident.crime_class",
	"ses.male",
	"ses.age_group",
	"ses.has_partner",
	"ses.edu",
	"ses.household_size",
	"ses.job",
	"attitude.has_crime_record",
	"weight",
	"region",
	"year")

# Models of reporting ==========================================================
## Base models
mdl_cyber <- pooled_reg_with_mult_imp(victims_pooled,
																			fml = fml_property,
																			crime_class = "Cybercrime",
																			vars_to_impute = vars_to_impute,
																			vars_to_use = vars_to_use)
saveRDS(mdl_cyber, file = "data/temp/lpm/mdl_cyber.Rds")

mdl_prop <- pooled_reg_with_mult_imp(victims_pooled,
																		 fml = fml_property,
																		 crime_class = "Property",
																		 vars_to_impute = vars_to_impute,
																		 vars_to_use = vars_to_use)
saveRDS(mdl_prop, file = "data/temp/lpm/mdl_prop.Rds")

mdl_viol <- pooled_reg_with_mult_imp(victims_pooled,
																		 fml = fml_violent,
																		 crime_class = "Violent",
																		 vars_to_impute = vars_to_impute,
																		 vars_to_use = vars_to_use)
saveRDS(mdl_viol, file = "data/temp/lpm/mdl_viol.Rds")


## Models augmented with emotional damage and measure of frequency
## of communication with relatives
mdl_cyber_2024 <- pooled_reg_with_mult_imp(victims_2024,
																					 fml = fml_property_2024,
																					 crime_class = "Cybercrime",
																					 vars_to_impute = vars_to_impute,
																					 vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_cyber_2024, file = "data/temp/lpm/mdl_cyber_2024.Rds")

mdl_prop_2024 <- pooled_reg_with_mult_imp(victims_2024,
																					fml = fml_property_2024,
																					crime_class = "Property",
																					vars_to_impute = vars_to_impute,
																					vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_prop_2024, file = "data/temp/lpm/mdl_prop_2024.Rds")

mdl_viol_2024 <- pooled_reg_with_mult_imp(victims_2024,
																					fml = fml_violent_2024,
																					crime_class = "Violent",
																					vars_to_impute = vars_to_impute,
																					vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_viol_2024, file = "data/temp/lpm/mdl_viol_2024.Rds")

## Models augmented with interaction of emotional damage and informing relatives
mdl_cyber_2024_moderation <- pooled_reg_with_mult_imp(victims_2024,
																										fml = as.formula(g("reported.police ~ social.told_someone * damage.emotional + {paste(X_property, collapse = '+')} | region")),
																										crime_class = "Cybercrime",
																										vars_to_impute = vars_to_impute,
																										vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_cyber_2024_moderation, file = "data/temp/lpm/mdl_cyber_2024_moderation.Rds")


mdl_prop_2024_moderation <- pooled_reg_with_mult_imp(victims_2024,
																									 fml = as.formula(g("reported.police ~ social.told_someone * damage.emotional + {paste(X_property, collapse = '+')} | region")),
																									 crime_class = "Property",
																									 vars_to_impute = vars_to_impute,
																									 vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_prop_2024_moderation, file = "data/temp/lpm/mdl_prop_2024_moderation.Rds")


mdl_viol_2024_moderation <- pooled_reg_with_mult_imp(victims_2024,
																									 fml = as.formula(g("reported.police ~ social.told_someone * damage.emotional + {paste(X_violent, collapse = '+')} | region")),
																									 crime_class = "Violent",
																									 vars_to_impute = vars_to_impute,
																									 vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_viol_2024_moderation, file = "data/temp/lpm/mdl_viol_2024_moderation.Rds")

# ==============================================================================

mdl_cyber <- readRDS( "data/temp/lpm/mdl_cyber.Rds")
mdl_prop <- readRDS( "data/temp/lpm/mdl_prop.Rds")
mdl_viol <- readRDS( "data/temp/lpm/mdl_viol.Rds")

modelsummary::modelsummary(list(Cyber = mdl_cyber$mdl,
																Property = mdl_prop$mdl,
																Violent = mdl_viol$mdl),
													 statistic = NULL,
													 coef_map = coef_map,
													 fmt = modelsummary::fmt_decimal(3),
													 estimate = "{estimate} ({std.error}){stars}",
													 stars = c("*" = .1, "**" = .05, "***" = .01),
# <<<<<<< HEAD
# 													 title = "Pooled data",
# 													 output = "latex")
# =======
													 title = "Pooled data")

# >>>>>>> 763fa095c1a4fed5bd5bc1d5c80c776795a78aee

mdl_cyber_2024 <- readRDS( "data/temp/lpm/mdl_cyber_2024.Rds")
mdl_prop_2024 <- readRDS( "data/temp/lpm/mdl_prop_2024.Rds")
mdl_viol_2024 <- readRDS( "data/temp/lpm/mdl_viol_2024.Rds")

modelsummary::modelsummary(list(Cyber = mdl_cyber_2024$mdl,
																Property = mdl_prop_2024$mdl,
																Violent = mdl_viol_2024$mdl),
													 statistic = NULL,
													 coef_map = coef_map,
													 fmt = modelsummary::fmt_decimal(3),
													 estimate = "{estimate} ({std.error}){stars}",
													 stars = c("*" = .1, "**" = .05, "***" = .01),
# <<<<<<< HEAD
													 # title = "Wave 2024: base model",
													 output = "latex"
													 )

mdl_cyber_2024_mediator <- readRDS( "data/temp/lpm/mdl_cyber_2024_mediator.Rds.Rds")
mdl_prop_2024_mediator <- readRDS( "data/temp/lpm/mdl_prop_2024_mediator.Rds.Rds")
mdl_viol_2024_mediator <- readRDS( "data/temp/lpm/mdl_viol_2024_mediator.Rds.Rds")

modelsummary::modelsummary(list(Cyber = mdl_cyber_2024_mediator$mdl,
																Property = mdl_prop_2024_mediator$mdl,
																Violent = mdl_viol_2024_mediator$mdl),
# =======
													 title = "Wave 2024")

mdl_cyber_2024_moderation <- readRDS( "data/temp/lpm/mdl_cyber_2024_moderation.Rds")
mdl_prop_2024_moderation <- readRDS( "data/temp/lpm/mdl_prop_2024_moderation.Rds")
mdl_viol_2024_moderation <- readRDS( "data/temp/lpm/mdl_viol_2024_moderation.Rds")

# modelsummary::modelsummary(list(Cyber = mdl_cyber_2024_moderation$mdl,
# 																Property = mdl_prop_2024_moderation$mdl,
# 																Violent = mdl_viol_2024_moderation$mdl),
# >>>>>>> 763fa095c1a4fed5bd5bc1d5c80c776795a78aee
# 													 statistic = NULL,
# 													 coef_map = coef_map,
# 													 fmt = modelsummary::fmt_decimal(3),
# 													 estimate = "{estimate} ({std.error}){stars}",
# 													 stars = c("*" = .1, "**" = .05, "***" = .01),
# <<<<<<< HEAD
# 													 # title = "Wave 2024: mediation",
# 													 output = "latex"
# 													 )
#
#
#
# =======
# 													 title = "Wave 2024")
#
#
# >>>>>>> 763fa095c1a4fed5bd5bc1d5c80c776795a78aee
