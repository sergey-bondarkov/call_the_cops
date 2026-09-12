library(mice)
library(fixest)
library(data.table)
library(forcats)
library(stringi)
library(ggplot2)
library(mice)
library(survey)
library(fixest)
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
plan(list(sequential, multisession))
#plan(list(
  #tweak(multisession, workers = availableCores() %/% 4),
  #tweak(multisession, workers = I(4))
#))

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")

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

# Simulations
props <- seq(0.1, 0.5, by = 0.1)
n_rep <- 10

sims <- function(dat, crime_class, X, props, n_rep, mdl_type, sim_type) {

    mean_F1 <- dat[incident.crime_class == crime_class, mean(social.told_someone)]
    mean_P1 <- dat[incident.crime_class == crime_class, mean(reported.police)]
    
    rbindlist(
              lapply(props, function(prop) {
                         repls <- future_replicate(n_rep, 
                                                   {
                                                       F1P1_ids <- dat[F1P1 == 1 & incident.crime_class == crime_class]$id
                                                       rand_ids <- sample(F1P1_ids, floor(length(F1P1_ids) * prop))

                                                       if(sim_type == "noise") {
                                                           dat[id %in% rand_ids, `:=`(social.told_someone = rbinom(.N, 1, mean_F1),
                                                                                      reported.police = rbinom(.N, 1, mean_P1))]
                                                       }
                                                       if(sim_type == "drop") {
                                                           dat <- dat[id %notin% rand_ids] 
                                                       }
                                                       
                                                       pooled_regr_with_mult_imp(dat = dat,
                                                                                 crime_class = crime_class,
                                                                                 y = "reported.police",
                                                                                 X = c("social.told_someone", X, "year"),
                                                                                 vars_to_impute = vars_to_impute,
                                                                                 vars_to_use = vars_to_use,
                                                                                 mdl_type = mdl_type,
                                                                                 marginal_effects = F
                                                       )

                                                   }, simplify = F)
                         repls_dt <- rbindlist(lapply(repls, function(m) as.data.table(m$pooled[m$pooled$term == "social.told_someone", ])))

                         # 2. Apply Rubin's Rules grouped by each term
                         k <- length(repls)

                         final_results_dt <- repls_dt[, .(
                                                          pooled_estimate  = mean(estimate),
                                                          pooled_std_error = sqrt(mean(ubar) + (1 + (1 / k)) * var(estimate))
                                                          ), by = term]
                         if(sim_type == "noise") {
                             final_results_dt[, random_noise_share := prop]
                         }
                         if(sim_type == "drop") {
                             final_results_dt[, random_drop_share := prop]
                         }

                         final_results_dt
}))
}

sims_cyber_drop <- sims(dat = victims_pooled, crime_class = "Cybercrime", X = X_property, props = props, n_rep = n_rep, mdl_type = "mlm", sim_type = "drop")
sims_viol_drop <- sims(dat = victims_pooled, crime_class = "Property", X = X_violent, props = props, n_rep = n_rep, mdl_type = "mlm", sim_type = "drop")
sims_viol_drop <- sims(dat = victims_pooled, crime_class = "Violent", X = X_violent, props = props, n_rep = n_rep, mdl_type = "mlm", sim_type = "drop")

sims_cyber_noise <- sims(dat = victims_pooled, crime_class = "Cybercrime", X = X_property, props = props, n_rep = n_rep, mdl_type = "mlm", sim_type = "noise")
sims_prop_noise <- sims(dat = victims_pooled, crime_class = "Property", X = X_violent, props = props, n_rep = n_rep, mdl_type = "mlm", sim_type = "noise")
sims_viol_noise <- sims(dat = victims_pooled, crime_class = "Violent", X = X_violent, props = props, n_rep = n_rep, mdl_type = "mlm", sim_type = "noise")

sims_drop <- rbindlist(list(Cybercrime = sims_cyber_drop, Property = sims_prop_drop, Violent = sims_viol_drop), idcol = "crime_class")
sims_drop[, `:=`(cih = pooled_estimate + pooled_std_error * 1.96, cil = pooled_estimate - pooled_std_error * 1.96)]
fwrite(sims_drop, file.path("data", "temp", "multilevel", "sims_drop.csv"))

sims_noise <- rbindlist(list(Cybercrime = sims_cyber_drop, Property = sims_prop_drop, Violent = sims_viol_drop), idcol = "crime_class")
sims_noise[, `:=`(cih = pooled_estimate + pooled_std_error * 1.96, cil = pooled_estimate - pooled_std_error * 1.96)]
fwrite(sims_noise, file.path("data", "temp", "multilevel", "sims_noise.csv"))
