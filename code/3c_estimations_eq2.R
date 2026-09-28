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

setwd("~/Projects/IRL/RCVS/call_the_cops")

source("code/coef_map.R")
source("code/regression_helpers/X_definition.R")
source("code/regression_helpers/functions.R")

# options(browser="/usr/bin/google-chrome")

# Dir to store results
dir.create("data/temp/lpm", showWarnings = F)

# Set random seed for reproducibility
set.seed(0)
# Set number of workers for parallel multiple imputations
n_workers <- 7
plan(multisession, workers = n_workers)

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


# One VS All -----------------------------------------------------------------------------

path_outcomes <- c("F0P0", "F1P0", "F0P1", "F1P1")

rhs_cyber_path <- g("{paste(X_property, collapse = '+')} | region + year")
paths_mdls_cyber <- lapply(path_outcomes, function(path) {
                               fml <- as.formula(paste(path, "~", rhs_cyber_path))
                               pooled_reg_with_mult_imp(victims_pooled,
                                                        fml = fml,
                                                        crime_class = "Cybercrime",
                                                        vars_to_impute = vars_to_impute,
                                                        vars_to_use = vars_to_use)
                 })
saveRDS(paths_mdls_cyber, file = "data/temp/lpm/paths_mdls_cyber.Rds")

rhs_prop_path <- g("{paste(X_property, collapse = '+')} | region + year")
paths_mdls_prop <- lapply(path_outcomes, function(path) {
                              fml <- as.formula(paste(path, "~", rhs_prop_path))
                              pooled_reg_with_mult_imp(victims_pooled,
                                                       fml = fml,
                                                       crime_class = "Property",
                                                       vars_to_impute = vars_to_impute,
                                                       vars_to_use = vars_to_use)
                 })
saveRDS(paths_mdls_prop, file = "data/temp/lpm/paths_mdls_prop.Rds")

rhs_viol_path <- g("{paste(X_violent, collapse = '+')} | region + year")
paths_mdls_viol <- lapply(path_outcomes, function(path) {
                              fml <- as.formula(paste(path, "~", rhs_viol_path))
                              pooled_reg_with_mult_imp(victims_pooled,
                                                       fml = fml,
                                                       crime_class = "Violent",
                                                       vars_to_impute = vars_to_impute,
                                                       vars_to_use = vars_to_use)
                 })
saveRDS(paths_mdls_viol, file = "data/temp/lpm/paths_mdls_viol.Rds")


# One VS Reference ------------------------------------------------------------------
path_outcomes2 <- c("F0P0", "F1P0", "F0P1")

rhs_cyber_path <- g("{paste(X_property, collapse = '+')} | region + year")
paths_mdls_cyber <- lapply(path_outcomes2, function(path) {
                               fml <- as.formula(paste(path, "~", rhs_cyber_path))
                               dat <- victims_pooled[F1P1 == 1 | get(path) == 1]
                               pooled_reg_with_mult_imp(dat,
                                                        fml = fml,
                                                        crime_class = "Cybercrime",
                                                        vars_to_impute = vars_to_impute,
                                                        vars_to_use = vars_to_use)
                 })
saveRDS(paths_mdls_cyber, file = "data/temp/lpm/paths_mdls_cyber_OneVsRef.Rds")
plot_path_profile(paths_mdls_cyber, "Cybercrime")

paths_mdls_prop <- lapply(path_outcomes2, function(path) {
                              fml <- as.formula(paste(path, "~", rhs_cyber_path))
                              dat <- victims_pooled[F1P1 == 1 | get(path) == 1]
                              pooled_reg_with_mult_imp(dat,
                                                       fml = fml,
                                                       crime_class = "Property",
                                                       vars_to_impute = vars_to_impute,
                                                       vars_to_use = vars_to_use)
                 })
saveRDS(paths_mdls_prop, file = "data/temp/lpm/paths_mdls_prop_OneVsRef.Rds")
plot_path_profile(paths_mdls_prop, "Property")

rhs_viol_path <- g("{paste(X_violent, collapse = '+')} | region + year")
paths_mdls_viol <- lapply(path_outcomes2, function(path) {
                              fml <- as.formula(paste(path, "~", rhs_viol_path))
                              dat <- victims_pooled[F1P1 == 1 | get(path) == 1]
                              pooled_reg_with_mult_imp(dat,
                                                       fml = fml,
                                                       crime_class = "Violent",
                                                       vars_to_impute = vars_to_impute,
                                                       vars_to_use = vars_to_use)
                 })
saveRDS(paths_mdls_viol, file = "data/temp/lpm/paths_mdls_viol_OneVsRef.Rds")
plot_path_profile(paths_mdls_viol, "Violent")







rhs_cyber_path_2024 <- g("{paste(X_property_2024, collapse = '+')} | region")
paths_mdls_cyber_2024 <- lapply(path_outcomes, function(path) {
                                    fml <- as.formula(paste(path, "~", rhs_cyber_path_2024))
                                    pooled_reg_with_mult_imp(victims_2024,
                                                             fml = fml,
                                                             crime_class = "Cybercrime",
                                                             vars_to_impute = vars_to_impute,
                                                             vars_to_use = setdiff(vars_to_use, "year"))
                 })
saveRDS(paths_mdls_cyber_2024, file = "data/temp/lpm/paths_mdls_cyber_2024.Rds")


rhs_prop_path_2024 <- g("{paste(X_property_2024, collapse = '+')} | region")
paths_mdls_prop_2024 <- lapply(path_outcomes, function(path) {
                                   fml <- as.formula(paste(path, "~", rhs_prop_path_2024))
                                   pooled_reg_with_mult_imp(victims_2024,
                                                            fml = fml,
                                                            crime_class = "Property",
                                                            vars_to_impute = vars_to_impute,
                                                            vars_to_use = setdiff(vars_to_use, "year"))
                 })
saveRDS(paths_mdls_prop_2024, file = "data/temp/lpm/paths_mdls_prop_2024.Rds")


rhs_viol_path_2024 <- g("{paste(X_violent_2024, collapse = '+')} | region")
paths_mdls_viol_2024 <- lapply(path_outcomes, function(path) {
                                   fml <- as.formula(paste(path, "~", rhs_viol_path_2024))
                                   pooled_reg_with_mult_imp(victims_2024,
                                                            fml = fml,
                                                            crime_class = "Violent",
                                                            vars_to_impute = vars_to_impute,
                                                            vars_to_use = setdiff(vars_to_use, "year"))
                 })
saveRDS(paths_mdls_viol_2024, file = "data/temp/lpm/paths_mdls_viol_2024.Rds")




paths_mdls_cyber <- readRDS("data/temp/lpm/paths_mdls_cyber.Rds")
pdf(file = "paper/figures/paths_cyber.pdf", width = 8, height = 3.5)
plot_path_profile(paths_mdls_cyber, "Panel A. Cybercrime")
dev.off()

paths_mdls_prop <- readRDS("data/temp/lpm/paths_mdls_prop.Rds")
pdf(file = "paper/figures/paths_prop.pdf", width = 8, height = 3.5)
plot_path_profile(paths_mdls_prop, "Panel B. Property crime")
dev.off()

paths_mdls_viol <- readRDS("data/temp/lpm/paths_mdls_viol.Rds")
pdf(file = "paper/figures/paths_viol.pdf", width = 8, height = 4)
plot_path_profile(paths_mdls_viol, "Panel C. Violent crime")
dev.off()

paths_mdls_cyber <- readRDS("data/temp/lpm/paths_mdls_cyber.Rds")
paths_mdls_prop <- readRDS("data/temp/lpm/paths_mdls_prop.Rds")
paths_mdls_viol <- readRDS("data/temp/lpm/paths_mdls_viol.Rds")

cc <- plot_path_profile(paths_mdls_cyber, "Panel A. Cybercrime", 0.1)
pc <- plot_path_profile(paths_mdls_prop, "Panel B. Property crime", 0.1)
vc <- plot_path_profile(paths_mdls_viol, "Panel C. Violent crime", 0.1)

pdf(file = "paper/JQC/figures/paths_combined.pdf", width = 6, height = 11)
cc / pc / vc + plot_layout(heights = c(1, 1, 1.5), guides = "collect") &
    theme(text = element_text(size = 7), legend.position = "bottom") &
    guides(alpha = "none")
    # scale_color_manual(breaks = c("90%", "95%"),
                       # values = c("orange", "darkred"),
                       # name = "Conf. level:")
dev.off()

paths_mdls_cyber_ref <- readRDS("data/temp/lpm/paths_mdls_cyber_OneVsRef.Rds")
paths_mdls_prop_ref <- readRDS("data/temp/lpm/paths_mdls_prop_OneVsRef.Rds")
paths_mdls_viol_ref <- readRDS("data/temp/lpm/paths_mdls_viol_OneVsRef.Rds")

ccref <- plot_path_profile(paths_mdls_cyber_ref, "Panel A. Cybercrime", 0.1)
pcref <- plot_path_profile(paths_mdls_prop_ref, "Panel B. Property crime", 0.1)
vcref <- plot_path_profile(paths_mdls_viol_ref, "Panel C. Violent crime", 0.1)

pdf(file = "paper/JQC/figures/paths_combined_ref.pdf", width = 6, height = 11)
ccref / pcref / vcref + plot_layout(heights = c(1, 1, 1.5), guides = "collect") &
    theme(text = element_text(size = 7), legend.position = "bottom") &
    guides(alpha = "none")
    # scale_color_manual(breaks = c("90%", "95%"),
                       # values = c("orange", "darkred"),
                       # name = "Conf. level:")
dev.off()




# ===============================================================================


library(data.table)
library(glue)
library(WeMix)

source("typology_helpers.R")

# 1. Prepare raw wide columns into mutual binary types
setDT(victims_pooled)
victims_pooled[, `:=`(
  F1P1 = as.integer(social.told_someone == 1 & reported.police == 1),
  F0P1 = as.integer(social.told_someone == 0 & reported.police == 1),
  F1P0 = as.integer(social.told_someone == 1 & reported.police == 0),
  F0P0 = as.integer(social.told_someone == 0 & reported.police == 0),
  obs_id = as.character(.I)
)]

# 2. Reshape to long format via data.table melt
covariates_list <- c("obs_id", "region", "weight.reg_scaled", "reg_weight",
                     "ses.settlement_type", "ses.male", "ses.age_group", "ses.household_size_f",
                     "ses.household_income", "ses.higher_edu", "ses.employed", "damage.rub_thnd_binned",
                     "damage.physical", "damage.emotional", "incident.witness", "incident.resisted",
                     "incident.offender_had_weapon", "incident.went_to_hospital",
                     "incident.offender_relationship_grouped", "incident.self_blame",
                     "incident.sexualized", "incident.repeated", "attitude.familiar_police",
                     "attitude.has_crime_record")

dat_multinomial_long <- melt(
  victims_pooled,
  id.vars       = covariates_list,
  measure.vars  = c("F1P1", "F0P1", "F1P0", "F0P0"),
  variable.name = "category",
  value.name    = "choice_made"
)

# 3. Establish explicit factor levels setting F0P0 as baseline reference
pathways <- c("F0P0", "F1P1", "F0P1", "F1P0")
dat_multinomial_long[, category := factor(category, levels = pathways)]

# 4. Generate formula concisely using glue
covariates_str <- paste(covariates_list[!covariates_list %in% c("obs_id", "region", "weight.reg_scaled", "reg_weight")], collapse = " + ")

fml_poisson_concise <- as.formula(
  glue("choice_made ~ category * ({covariates_str}) + (1 | region) + (1 | obs_id)")
)

# 5. Fit the survey-weighted multi-level Poisson model
fit_typology_poisson <- mix(
  formula = fml_poisson_concise,
  data    = dat_multinomial_long,
  weights = c("weight.reg_scaled", "reg_weight"),
  family  = poisson(link = "log")
)

# 6. Run inference for your target variable (e.g., ses.male) across all pathways
typology_inference <- bootstrap_typology_ame(
  model      = fit_typology_poisson,
  data       = dat_multinomial_long,
  target_var = "ses.male",
  categories = c("F1P1", "F0P1", "F1P0", "F0P0"),
  B          = 500
)

print(typology_inference)

# 1. Source your helper script to load the new batch framework
source("typology_helpers.R")

# 2. Define the exact subset of key covariates you wish to calculate AMEs for
predictors_to_test <- c(
  "ses.male", 
  "ses.higher_edu", 
  "ses.household_income", 
  "attitude.familiar_police"
)

# 3. Execute the batch calculation matrix automatically
master_typology_inference <- batch_bootstrap_typology_ame(
  model      = fit_typology_poisson,
  data       = dat_multinomial_long,
  target_vars = predictors_to_test,
  categories = c("F1P1", "F0P1", "F1P0", "F0P0"),
  B          = 500,
  seed       = 42
)

# 4. View your complete publication matrix
print(master_typology_inference)

