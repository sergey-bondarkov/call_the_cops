#library(WeMix)
library(brms)
library(fixest)
library(lme4)
#library(modelsummary)
library(marginaleffects)
library(data.table)
#library(howManyImputations)
library(future)
library(future.apply)
g <- glue::glue

setwd(Sys.getenv("CALL_THE_COPS"))

source("code/coef_map.R")
source("code/helpers/X_definition.R")
source("code/helpers/functions.R")

# Dir to store results
out_brm <- file.path("temp", "brm")
#out_mlm <- file.path("data", "temp", "mlm")
out_lpm <- file.path("temp", "lpm")
#dir.create(out_mlm, showWarnings = F, recursive = T)
dir.create(out_brm, showWarnings = F, recursive = T)
dir.create(out_lpm, showWarnings = F, recursive = T)

# Set random seed for reproducibility
set.seed(0)

#n_workers <- floor(parallelly::availableCores() * 2/3)
#plan(multisession, workers = n_workers)

# Load data 
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_2024 <- readRDS("data/processed/victims_2024.Rds")

# Formulas =====================================================================
## MLM
#fml_prop_mlm_pooled <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} + year + (1|region)"))
#fml_viol_mlm_pooled <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} + year + (1|region)"))
#fml_prop_mlm_2024 <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} + (1|region)"))
#fml_viol_mlm_2024 <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_violent_2024, collapse = '+')} + (1|region)"))

fml_prop_brm_pooled <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_property, collapse = '+')} + year + (1|region)"))
fml_viol_brm_pooled <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_violent, collapse = '+')} + year + (1|region)"))
fml_prop_brm_pooled_noWeights <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} + year + (1|region)"))
fml_viol_brm_pooled_noWeights <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} + year + (1|region)"))
fml_prop_brm_2024 <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_property_2024, collapse = '+')} + (1|region)"))
fml_viol_brm_2024 <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_violent_2024, collapse = '+')} + (1|region)"))

## LPM
fml_prop_lpm_pooled <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | year + region"))
fml_viol_lpm_pooled <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | year + region"))
fml_prop_lpm_2024 <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} | region"))
fml_viol_lpm_2024 <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_violent_2024, collapse = '+')} | region"))

# Data =========================================================================
#victims_pooled[, reg_weight := 1]
#victims_2024[, reg_weight := 1]

dat_cyber_pooled <- victims_pooled[incident.crime_class == "Cybercrime"]
dat_cyber_pooled <- dat_cyber_pooled[complete.cases(dat_cyber_pooled[, ..X_property])]
dat_cyber_pooled[, region := fdroplevels(region)]

dat_prop_pooled <- victims_pooled[incident.crime_class == "Property"]
dat_prop_pooled <- dat_prop_pooled[complete.cases(dat_prop_pooled[, ..X_property])]
dat_prop_pooled[, region := fdroplevels(region)]

dat_viol_pooled <- victims_pooled[incident.crime_class == "Violent"]
dat_viol_pooled <- dat_viol_pooled[complete.cases(dat_viol_pooled[, ..X_violent])]
dat_viol_pooled[, region := fdroplevels(region)]

dat_cyber_2024 <- victims_2024[incident.crime_class == "Cybercrime"]
dat_cyber_2024 <- dat_cyber_2024[complete.cases(dat_cyber_2024[, ..X_property_2024])]
dat_cyber_2024[, region := fdroplevels(region)]

dat_prop_2024 <- victims_2024[incident.crime_class == "Property"]
dat_prop_2024 <- dat_prop_2024[complete.cases(dat_prop_2024[, ..X_property_2024])]
dat_prop_2024[, region := fdroplevels(region)]

dat_viol_2024 <- victims_2024[incident.crime_class == "Violent"]
dat_viol_2024 <- dat_viol_2024[complete.cases(dat_viol_2024[, ..X_violent_2024])]
dat_viol_2024[, region := fdroplevels(region)]

# Modeling =====================================================================

## Panel A
brm_cyber_pooled <- brm(
  formula = fml_prop_brm_pooled,
  data    = dat_cyber_pooled,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"  
)
brm_prop_pooled <- brm(
  formula = fml_prop_brm_pooled,
  data    = dat_prop_pooled,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"  
)
brm_viol_pooled <- brm(
  formula = fml_viol_brm_pooled,
  data    = dat_viol_pooled,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"  
)


#mlm_cyber_pooled <- mix(fml_prop_mlm_pooled, 
                        #data = dat_cyber_pooled,
                        #weights = c("weight.reg_scaled", "reg_weight"))
#mlm_prop_pooled <- mix(fml_prop_mlm_pooled, 
                       #data = dat_prop_pooled,
                       #weights = c("weight.reg_scaled", "reg_weight"))
#mlm_viol_pooled <- mix(fml_viol_mlm_pooled, 
                       #data = dat_viol_pooled,
                       #weights = c("weight.reg_scaled", "reg_weight"))

lpm_cyber_pooled <- feols(fml_prop_lpm_pooled,
                          data = dat_cyber_pooled, 
                          weights= ~ weight.reg_scaled, 
                          cluster = ~region)
lpm_prop_pooled <- feols(fml_prop_lpm_pooled,
                         data = dat_prop_pooled, 
                         weights= ~ weight.reg_scaled, 
                         cluster = ~region)
lpm_viol_pooled <- feols(fml_viol_lpm_pooled,
                         data = dat_viol_pooled, 
                         weights= ~ weight.reg_scaled, 
                         cluster = ~region)

## Panel B
#mlm_cyber_2024 <- mix(fml_prop_mlm_2024, 
                      #data = dat_cyber_2024,
                      #weights = c("weight.reg_scaled", "reg_weight"))
#mlm_prop_2024 <- mix(fml_prop_mlm_2024, 
                     #data = dat_prop_2024,
                     #weights = c("weight.reg_scaled", "reg_weight"))
#mlm_viol_2024 <- mix(fml_viol_mlm_2024, 
                     #data = dat_viol_2024,
                     #weights = c("weight.reg_scaled", "reg_weight"))
brm_cyber_2024 <- brm(
  formula = fml_prop_brm_2024,
  data    = dat_cyber_2024,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"  
)
brm_prop_2024 <- brm(
  formula = fml_prop_brm_2024,
  data    = dat_prop_2024,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"  
)
brm_viol_2024 <- brm(
  formula = fml_viol_brm_2024,
  data    = dat_viol_2024,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"  
)


lpm_cyber_2024 <- feols(fml_prop_lpm_2024,
                        data = dat_cyber_2024, 
                        weights= ~ weight.reg_scaled, 
                        cluster = ~region)
lpm_prop_2024 <- feols(fml_prop_lpm_2024,
                       data = dat_prop_2024, 
                       weights= ~ weight.reg_scaled, 
                       cluster = ~region)
lpm_viol_2024 <- feols(fml_viol_lpm_2024,
                       data = dat_viol_2024, 
                       weights= ~ weight.reg_scaled, 
                       cluster = ~region)

## Panel C
#mlm_cyber_pooled_noWeights <- glmer(fml_prop_mlm_pooled, 
                                    #data = dat_cyber_pooled,
                                    #family = binomial(link = "logit"))
#mlm_prop_pooled_noWeights <- glmer(fml_prop_mlm_pooled, 
                                   #data = dat_prop_pooled,
                                   #family = binomial(link = "logit"))
#mlm_viol_pooled_noWeights <- glmer(fml_viol_mlm_pooled, 
                                   #data = dat_viol_pooled,
                                   #family = binomial(link = "logit"))
brm_cyber_pooled_noWeights <- brm(
  formula = fml_prop_brm_pooled_noWeights,
  data    = dat_cyber_pooled,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"  
)
brm_prop_pooled_noWeights <- brm(
  formula = fml_prop_brm_pooled_noWeights,
  data    = dat_prop_pooled,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"  
)
brm_viol_pooled_noWeights <- brm(
  formula = fml_viol_brm_pooled_noWeights,
  data    = dat_viol_pooled,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,
  backend = "cmdstanr"  
)


lpm_cyber_pooled_noWeights <- feols(fml_prop_lpm_pooled,
                                    data = dat_cyber_pooled, 
                                    cluster = ~region)
lpm_prop_pooled_noWeights <- feols(fml_prop_lpm_pooled,
                                   data = dat_prop_pooled, 
                                   cluster = ~region)
lpm_viol_pooled_noWeights <- feols(fml_viol_lpm_pooled,
                                   data = dat_viol_pooled, 
                                   cluster = ~region)

save(list = grepv("^(lpm|blm)_", ls()), file = "temp/models.RData")

# Calculate AMEs  ==============================================================

ames_brm_cyber_pooled <- avg_slopes(brm_cyber_pooled, variables = "social.told_someone", wts = "weight")
ames_brm_prop_pooled <- avg_slopes(brm_prop_pooled, variables = "social.told_someone", wts = "weight")
ames_brm_viol_pooled <- avg_slopes(brm_viol_pooled, variables = "social.told_someone", wts = "weight")

ames_brm_cyber_2024 <- avg_slopes(brm_cyber_2024, variables = "social.told_someone", wts = "weight")
ames_brm_prop_2024 <- avg_slopes(brm_prop_2024, variables = "social.told_someone", wts = "weight")
ames_brm_viol_2024 <- avg_slopes(brm_viol_2024, variables = "social.told_someone", wts = "weight")

ames_brm_cyber_pooled_noWeights <- avg_slopes(brm_cyber_pooled_noWeights, variables = "social.told_someone")
ames_brm_prop_pooled_noWeights <- avg_slopes(brm_prop_pooled_noWeights, variables = "social.told_someone")
ames_brm_viol_pooled_noWeights <- avg_slopes(brm_viol_pooled_noWeights, variables = "social.told_someone")

#ames_mlm_cyber_pooled <- bootstrap_wemix_ame(mlm_cyber_pooled, dat_cyber_pooled, "social.told_someone", weighted = T)
#ames_mlm_prop_pooled <- bootstrap_wemix_ame(mlm_prop_pooled, dat_prop_pooled, "social.told_someone", weighted = T)
#ames_mlm_viol_pooled <- bootstrap_wemix_ame(mlm_viol_pooled, dat_viol_pooled, "social.told_someone", weighted = T)

#ames_mlm_cyber_2024 <- bootstrap_wemix_ame(mlm_cyber_2024, dat_cyber_2024, "social.told_someone", weighted = T)
#ames_mlm_prop_2024 <- bootstrap_wemix_ame(mlm_prop_2024, dat_prop_2024, "social.told_someone", weighted = T)
#ames_mlm_viol_2024 <- bootstrap_wemix_ame(mlm_viol_2024, dat_viol_2024, "social.told_someone", weighted = T)

#ames_lpm_cyber_pooled <- avg_slopes(lpm_cyber_pooled, variables = "social.told_someone", wts = "weight.reg_scaled")
#ames_lpm_prop_pooled <- avg_slopes(lpm_prop_pooled, variables = "social.told_someone", wts = "weight.reg_scaled")
#ames_lpm_viol_pooled <- avg_slopes(lpm_viol_pooled, variables = "social.told_someone", wts = "weight.reg_scaled")

#ames_lpm_cyber_2024 <- avg_slopes(lpm_cyber_2024, variables = "social.told_someone", wts = "weight.reg_scaled")
#ames_lpm_prop_2024 <- avg_slopes(lpm_prop_2024, variables = "social.told_someone", wts = "weight.reg_scaled")
#ames_lpm_viol_2024 <- avg_slopes(lpm_viol_2024, variables = "social.told_someone", wts = "weight.reg_scaled")

#ames_mlm_cyber_pooled_noWeights <- avg_slopes(mlm_cyber_pooled_noWeights, variables = "social.told_someone")
#ames_mlm_prop_pooled_noWeights <- avg_slopes(mlm_prop_pooled_noWeights, variables = "social.told_someone")
#ames_mlm_viol_pooled_noWeights <- avg_slopes(mlm_viol_pooled_noWeights, variables = "social.told_someone")

#ames_lpm_cyber_pooled_noWeights <- avg_slopes(lpm_cyber_pooled_noWeights, variables = "social.told_someone")
#ames_lpm_prop_pooled_noWeights <- avg_slopes(lpm_prop_pooled_noWeights, variables = "social.told_someone")
#ames_lpm_viol_pooled_noWeights <- avg_slopes(lpm_viol_pooled_noWeights, variables = "social.told_someone")

save(list = grepv("^ames_", ls()), file = "temp/ames.RData")

# Goodness of Fit (GoF) Extraction --------------------------------------------

get_brms_r2 <- function(brm_model) {
  return(brms::bayes_R2(brm_model)[1, "Estimate"])
}

## Panel A
r2_brm_cyber_pooled <- get_brms_r2(brm_cyber_pooled)
r2_brm_prop_pooled  <- get_brms_r2(brm_prop_pooled)
r2_brm_viol_pooled  <- get_brms_r2(brm_viol_pooled)

r2_lpm_cyber_pooled <- r2(lpm_cyber_pooled, "wr2")
r2_lpm_prop_pooled  <- r2(lpm_prop_pooled, "wr2")
r2_lpm_viol_pooled  <- r2(lpm_viol_pooled, "wr2")

## Panel B
r2_brm_cyber_2024   <- get_brms_r2(brm_cyber_2024)
r2_brm_prop_2024    <- get_brms_r2(brm_prop_2024)
r2_brm_viol_2024    <- get_brms_r2(brm_viol_2024)

r2_lpm_cyber_2024   <- r2(lpm_cyber_2024, "wr2")
r2_lpm_prop_2024    <- r2(lpm_prop_2024, "wr2")
r2_lpm_viol_2024    <- r2(lpm_viol_2024, "wr2")

## Panel C
r2_brm_cyber_pooled_noWeights <- get_brms_r2(brm_cyber_pooled_noWeights)
r2_brm_prop_pooled_noWeights  <- get_brms_r2(brm_prop_pooled_noWeights)
r2_brm_viol_pooled_noWeights  <- get_brms_r2(brm_viol_pooled_noWeights)

r2_lpm_cyber_pooled_noWeights <- r2(lpm_cyber_pooled_noWeights, "wr2")
r2_lpm_prop_pooled_noWeights  <- r2(lpm_prop_pooled_noWeights, "wr2")
r2_lpm_viol_pooled_noWeights  <- r2(lpm_viol_pooled_noWeights, "wr2")

save(list = grepv("^r2_", ls()), file = "temp/r2s.RData")

# Table Processing System ======================================================

load("temp/ames.RData") 
load("temp/r2s.RData")

# 1. Cell formatter for BRM objects (Uses Dagger symbol if 95% CrI excludes zero)
format_brm_estimates <- function(ame_object) {
  est   <- as.numeric(ame_object$estimate)
  low   <- as.numeric(ame_object$conf.low)
  high  <- as.numeric(ame_object$conf.high)
  
  # Check if the 95% Bayesian Credible Interval excludes zero
  symb <- ""
  if ((low > 0 & high > 0) | (low < 0 & high < 0)) {
    symb <- "^\\dagger" # LaTeX dagger superscript
  }
  
  est_str <- sprintf("%.3f%s", est, symb)
  
  # Format explicit credible intervals inside brackets [Lower, Upper]
  ci_str  <- sprintf("[%.3f, %.3f]", low, high)
  
  return(list(est = est_str, se = ci_str)) # Maps cleanly to your existing table loops
}
format_brm_estimates(ames_brm_cyber_pooled)

# 2. Cell formatter for frequentist OLS / LPM summary matrices
format_lpm_estimates <- function(lpm_model, var_name = "social.told_someone") {
  coef_mat <- lpm_model$coeftable
  
  row_idx <- which(rownames(coef_mat) == var_name)
  if(length(row_idx) == 0) row_idx <- 2 
  
  est   <- coef_mat[row_idx, 1]
  se    <- coef_mat[row_idx, 2]
  p_val <- coef_mat[row_idx, 4]
  
  stars <- ""
  if (p_val < 0.001) stars <- "***"
  else if (p_val < 0.01)  stars <- "**"
  else if (p_val < 0.05)  stars <- "*"
  else if (p_val < 0.10)  stars <- "+"
  
  est_str <- sprintf("%.3f%s", est, stars)
  se_str  <- sprintf("(%.3f)", se)
  return(list(est = est_str, se = se_str))
}
format_lpm_estimates(lpm_cyber_pooled)

# 3. Row processing wrapper pairing BRM AMEs with Direct LPM Slopes
format_row_cells_brm_lpm <- function(brm_cyb, brm_prop, brm_viol, lpm_cyb, lpm_prop, lpm_viol) {
  c_brm <- format_brm_estimates(brm_cyb)
  p_brm <- format_brm_estimates(brm_prop)
  v_brm <- format_brm_estimates(brm_viol)
  
  c_lpm <- format_lpm_estimates(lpm_cyb)
  p_lpm <- format_lpm_estimates(lpm_prop)
  v_lpm <- format_lpm_estimates(lpm_viol)
  
  brm_est_row <- glue(" & {c_brm$est} & \t{p_brm$est} & {v_brm$est}\\\\")
  brm_se_row  <- glue(" & {c_brm$se} &  {p_brm$se} &  {v_brm$se} \\\\")
  
  lpm_est_row <- glue(" & {c_lpm$est} & \t{p_lpm$est} & {v_lpm$est}\\\\")
  lpm_se_row  <- glue(" & {c_lpm$se} &  {p_lpm$se} &  {v_lpm$se} \\\\")
  
  return(list(brm_est = brm_est_row, brm_se = brm_se_row, lpm_est = lpm_est_row, lpm_se = lpm_se_row))
}

# 4. Reporting rate calculator
get_direct_avg_rate <- function(data_obj, weighted = TRUE) {
  dt <- as.data.table(data_obj)
  if (weighted == TRUE) {
    return(weighted.mean(dt$reported.police, w = dt$weight, na.rm = TRUE))
  } else {
    return(mean(dt$reported.police, na.rm = TRUE))
  }
}

# ==============================================================================
# COMPILE CELL VALUES ACROSS ALL STUDY PANELS
# ==============================================================================

# Panel A Rows
cells_A <- format_row_cells_brm_lpm(
  ames_brm_cyber_pooled, ames_brm_prop_pooled, ames_brm_viol_pooled,
  lpm_cyber_pooled, lpm_prop_pooled, lpm_viol_pooled
)
avg_cyber_A <- sprintf("%.3f", get_direct_avg_rate(dat_cyber_pooled))
avg_prop_A  <- sprintf("%.3f", get_direct_avg_rate(dat_prop_pooled))
avg_viol_A  <- sprintf("%.3f", get_direct_avg_rate(dat_viol_pooled))

# Panel B Rows
cells_B <- format_row_cells_brm_lpm(
  ames_brm_cyber_2024, ames_brm_prop_2024, ames_brm_viol_2024,
  lpm_cyber_2024, lpm_prop_2024, lpm_viol_2024
)
avg_cyber_B <- sprintf("%.3f", get_direct_avg_rate(dat_cyber_2024))
avg_prop_B  <- sprintf("%.3f", get_direct_avg_rate(dat_prop_2024))
avg_viol_B  <- sprintf("%.3f", get_direct_avg_rate(dat_viol_2024))

# Panel C Rows
cells_C <- format_row_cells_brm_lpm(
  ames_brm_cyber_pooled_noWeights, ames_brm_prop_pooled_noWeights, ames_brm_viol_pooled_noWeights,
  lpm_cyber_pooled_noWeights, lpm_prop_pooled_noWeights, lpm_viol_pooled_noWeights
)
avg_cyber_C <- sprintf("%.3f", get_direct_avg_rate(dat_cyber_pooled, weighted = FALSE))
avg_prop_C  <- sprintf("%.3f", get_direct_avg_rate(dat_prop_pooled, weighted = FALSE))
avg_viol_C  <- sprintf("%.3f", get_direct_avg_rate(dat_viol_pooled, weighted = FALSE))

# ==============================================================================
# GENERATE LATEX TABULAR ENVIRONMENT & FOOTNOTE CLOSURE
# ==============================================================================
glue_tex <- function(x) glue::glue(x, .open = "<<", .close = ">>", .trim = FALSE)

regr_tab <- glue_tex(R"(\begin{tabular}{lrrr}
	\hline\hline
	\addlinespace[5pt] 
	& Cybercrime & Property crime & Violent crime \\  
	\hline
	\addlinespace[5pt] 
	\multicolumn{4}{l}{\textsc{Panel A. Base model, pooled data of three survey waves}} \\
	\addlinespace[2.5pt]
	BRM (AME)<<cells_A$brm_est>>
	CI         <<cells_A$brm_se>>
	LPM (Slope)<<cells_A$lpm_est>>
	SE            <<cells_A$lpm_se>>
	Avg. reporting rate & <<avg_cyber_A>> & <<avg_prop_A>> & <<avg_viol_A>> \\
	\addlinespace[2.5pt]   
	BRM Bayesian R$^2$ & <<sprintf("%.3f", r2_brm_cyber_pooled)>> & <<sprintf("%.3f", r2_brm_prop_pooled)>> & <<sprintf("%.3f", r2_brm_viol_pooled)>> \\
	LPM Within R$^2$ & <<sprintf("%.3f", r2_lpm_cyber_pooled)>> & <<sprintf("%.3f", r2_lpm_prop_pooled)>> & <<sprintf("%.3f", r2_lpm_viol_pooled)>> \\

	\addlinespace[10pt]
	\hline
	\addlinespace[5pt]

	\multicolumn{4}{l}{\textsc{Panel B. Base model augmented with level of emotional damage,}}\\
	\multicolumn{4}{l}{\hspace*{5em}\textsc{wave 2024 only}} \\
	\addlinespace[2.5pt]
	BRM (AME)<<cells_B$brm_est>>
	CI         <<cells_B$brm_se>>
	LPM (Slope)<<cells_B$lpm_est>>
	SE            <<cells_B$lpm_se>>
	Avg. reporting rate & <<avg_cyber_B>> & <<avg_prop_B>> & <<avg_viol_B>> \\
	\addlinespace[2.5pt]   
	BRM Bayesian R$^2$ & <<sprintf("%.3f", r2_brm_cyber_2024)>> & <<sprintf("%.3f", r2_brm_prop_2024)>> & <<sprintf("%.3f", r2_brm_viol_2024)>> \\
	LPM Within R$^2$ & <<sprintf("%.3f", r2_lpm_cyber_2024)>> & <<sprintf("%.3f", r2_lpm_prop_2024)>> & <<sprintf("%.3f", r2_lpm_viol_2024)>> \\

	\addlinespace[10pt]
	\hline
	\addlinespace[5pt]

	\multicolumn{4}{l}{\textsc{Panel C. Robustness check, pooled unweighted data}}\\
	\addlinespace[2.5pt]
	BRM (AME)<<cells_C$brm_est>>
	CI         <<cells_C$brm_se>>
	LPM (Slope)<<cells_C$lpm_est>>
	SE            <<cells_C$lpm_se>>
	Avg. reporting rate & <<avg_cyber_C>> & <<avg_prop_C>> & <<avg_viol_C>> \\
	\addlinespace[2.5pt]   
	BRM Bayesian R$^2$ & <<sprintf("%.3f", r2_brm_cyber_pooled_noWeights)>> & <<sprintf("%.3f", r2_brm_prop_pooled_noWeights)>> & <<sprintf("%.3f", r2_brm_viol_pooled_noWeights)>> \\
	LPM Within R$^2$ & <<sprintf("%.3f", r2_lpm_cyber_pooled_noWeights)>> & <<sprintf("%.3f", r2_lpm_prop_pooled_noWeights)>> & <<sprintf("%.3f", r2_lpm_viol_pooled_noWeights)>> \\
	\addlinespace[5pt]
	\hline\hline
	\addlinespace[4pt]
	\multicolumn{4}{p{11.5cm}}{\textit{Note}: BRM (AME) reports the Bayesian Multilevel Average Marginal Effect, where standard errors in parentheses indicate the exact posterior standard deviation of the marginal effect distribution. The \(\dag \) symbol denotes that the 95% Equal-Tailed Bayesian Credible Interval completely excludes zero. LPM (Slope) reports the unstandardized frequentist Ordinary Least Squares coefficients (functioning as linear marginal effects), with frequentist standard errors in parentheses. Frequentist significance is indicated by: + p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001. Estimates in Panels A and B incorporate post-stratification survey weights, normalized to the true sample size scale to preserve valid uncertainty bounds; Panel C displays unweighted estimates for robustness. Models globally control for unobserved regional heterogeneity across 85 distinct geographic clusters.}
\end{tabular})"
)

# Export complete structure to disk
writeLines(regr_tab, "paper/tables/regr_1_results.tex")
