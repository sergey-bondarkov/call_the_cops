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
dir.create(out_mlm, showWarnings = F, recursive = T)
dir.create(out_brm, showWarnings = F, recursive = T)
dir.create(out_lpm, showWarnings = F, recursive = T)

# Set random seed for reproducibility
set.seed(0)

n_workers <- floor(parallelly::availableCores() * 2/3)
plan(multisession, workers = n_workers)

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
fml_prop_brm_2024 <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_property_2024, collapse = '+')} + (1|region)"))
fml_viol_brm_2024 <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_violent_2024, collapse = '+')} + (1|region)"))

## LPM
fml_prop_lpm_pooled <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | year + region"))
fml_viol_lpm_pooled <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | year + region"))
fml_prop_lpm_2024 <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} | region"))
fml_viol_lpm_2024 <- as.formula(glue::glue("reported.police ~ social.told_someone + {paste(X_violent_2024, collapse = '+')} | region"))

# Data =========================================================================
victims_pooled[, reg_weight := 1]
victims_2024[, reg_weight := 1]

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
  formula = fml_property_bin,
  data    = dat_cyber_pooled,
  family  = bernoulli(link = "logit"),
  cores   = 4, chains = 4,   # Distributes computations across CPU cores
  backend = "cmdstanr"        # Uses Stan's highly optimized C++ engine
)
avg_slopes(model_binary_mcmc, variables = "social.told_someone", wts = "weight.reg_scaled")

mlm_cyber_pooled <- mix(fml_prop_mlm_pooled, 
                        data = dat_cyber_pooled,
                        weights = c("weight.reg_scaled", "reg_weight"))
mlm_prop_pooled <- mix(fml_prop_mlm_pooled, 
                       data = dat_prop_pooled,
                       weights = c("weight.reg_scaled", "reg_weight"))
mlm_viol_pooled <- mix(fml_viol_mlm_pooled, 
                       data = dat_viol_pooled,
                       weights = c("weight.reg_scaled", "reg_weight"))

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
mlm_cyber_2024 <- mix(fml_prop_mlm_2024, 
                      data = dat_cyber_2024,
                      weights = c("weight.reg_scaled", "reg_weight"))
mlm_prop_2024 <- mix(fml_prop_mlm_2024, 
                     data = dat_prop_2024,
                     weights = c("weight.reg_scaled", "reg_weight"))
mlm_viol_2024 <- mix(fml_viol_mlm_2024, 
                     data = dat_viol_2024,
                     weights = c("weight.reg_scaled", "reg_weight"))

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
mlm_cyber_pooled_noWeights <- glmer(fml_prop_mlm_pooled, 
                                    data = dat_cyber_pooled,
                                    family = binomial(link = "logit"))
mlm_prop_pooled_noWeights <- glmer(fml_prop_mlm_pooled, 
                                   data = dat_prop_pooled,
                                   family = binomial(link = "logit"))
mlm_viol_pooled_noWeights <- glmer(fml_viol_mlm_pooled, 
                                   data = dat_viol_pooled,
                                   family = binomial(link = "logit"))


lpm_cyber_pooled_noWeights <- feols(fml_prop_lpm_pooled,
                                    data = dat_cyber_pooled, 
                                    cluster = ~region)
lpm_prop_pooled_noWeights <- feols(fml_prop_lpm_pooled,
                                   data = dat_prop_pooled, 
                                   cluster = ~region)
lpm_viol_pooled_noWeights <- feols(fml_viol_lpm_pooled,
                                   data = dat_viol_pooled, 
                                   cluster = ~region)

save(list = grepv("^(lpm|mlm)_", ls()), file = "temp/models.RData")

# Calculate AMEs  ==============================================================

ames_mlm_cyber_pooled <- bootstrap_wemix_ame(mlm_cyber_pooled, dat_cyber_pooled, "social.told_someone", weighted = T)
ames_mlm_prop_pooled <- bootstrap_wemix_ame(mlm_prop_pooled, dat_prop_pooled, "social.told_someone", weighted = T)
ames_mlm_viol_pooled <- bootstrap_wemix_ame(mlm_viol_pooled, dat_viol_pooled, "social.told_someone", weighted = T)

ames_mlm_cyber_2024 <- bootstrap_wemix_ame(mlm_cyber_2024, dat_cyber_2024, "social.told_someone", weighted = T)
ames_mlm_prop_2024 <- bootstrap_wemix_ame(mlm_prop_2024, dat_prop_2024, "social.told_someone", weighted = T)
ames_mlm_viol_2024 <- bootstrap_wemix_ame(mlm_viol_2024, dat_viol_2024, "social.told_someone", weighted = T)

ames_lpm_cyber_pooled <- avg_slopes(lpm_cyber_pooled, variables = "social.told_someone", wts = "weight.reg_scaled")
ames_lpm_prop_pooled <- avg_slopes(lpm_prop_pooled, variables = "social.told_someone", wts = "weight.reg_scaled")
ames_lpm_viol_pooled <- avg_slopes(lpm_viol_pooled, variables = "social.told_someone", wts = "weight.reg_scaled")

ames_lpm_cyber_2024 <- avg_slopes(lpm_cyber_2024, variables = "social.told_someone", wts = "weight.reg_scaled")
ames_lpm_prop_2024 <- avg_slopes(lpm_prop_2024, variables = "social.told_someone", wts = "weight.reg_scaled")
ames_lpm_viol_2024 <- avg_slopes(lpm_viol_2024, variables = "social.told_someone", wts = "weight.reg_scaled")

ames_mlm_cyber_pooled_noWeights <- avg_slopes(mlm_cyber_pooled_noWeights, variables = "social.told_someone")
ames_mlm_prop_pooled_noWeights <- avg_slopes(mlm_prop_pooled_noWeights, variables = "social.told_someone")
ames_mlm_viol_pooled_noWeights <- avg_slopes(mlm_viol_pooled_noWeights, variables = "social.told_someone")

ames_lpm_cyber_pooled_noWeights <- avg_slopes(lpm_cyber_pooled_noWeights, variables = "social.told_someone")
ames_lpm_prop_pooled_noWeights <- avg_slopes(lpm_prop_pooled_noWeights, variables = "social.told_someone")
ames_lpm_viol_pooled_noWeights <- avg_slopes(lpm_viol_pooled_noWeights, variables = "social.told_someone")

save(list = grepv("^ames_", ls()), file = "temp/ames.RData")

# GoF's ------------------------------------------------------------------------

## Panel A
r2_mlm_cyber_pooled <- calculate_mcfadden_r2(mlm_cyber_pooled, dat_cyber_pooled) # 0.124
r2_mlm_prop_pooled <- calculate_mcfadden_r2(mlm_prop_pooled, dat_prop_pooled) # 0.097
r2_mlm_viol_pooled <- calculate_mcfadden_r2(mlm_viol_pooled, dat_viol_pooled) # 0.213

r2_lpm_cyber_pooled <- r2(lpm_cyber_pooled, "wr2") # 0.150
r2_lpm_prop_pooled <- r2(lpm_prop_pooled, "wr2") # 0.129
r2_lpm_viol_pooled <- r2(lpm_viol_pooled, "wr2") # 0.261

## Panel B
r2_mlm_cyber_2024 <- calculate_mcfadden_r2(mlm_cyber_2024, dat_cyber_2024) # 0.131
r2_mlm_prop_2024 <- calculate_mcfadden_r2(mlm_prop_2024, dat_prop_2024) # 0.136
r2_mlm_viol_2024 <- calculate_mcfadden_r2(mlm_viol_2024, dat_viol_2024) # 0.309

r2_lpm_cyber_2024 <- r2(lpm_cyber_2024, "wr2") # 0.183
r2_lpm_prop_2024 <- r2(lpm_prop_2024, "wr2") # 0.216
r2_lpm_viol_2024 <- r2(lpm_viol_2024, "wr2") # 0.356

## Panel C
r2_mlm_cyber_pooled_noWeights <- calculate_mcfadden_r2(mlm_cyber_pooled_noWeights, dat_cyber_pooled, weighted = F) # 0.129
r2_mlm_prop_pooled_noWeights <- calculate_mcfadden_r2(mlm_prop_pooled_noWeights, dat_prop_pooled, weighted = F) # 0.108
r2_mlm_viol_pooled_noWeights <- calculate_mcfadden_r2(mlm_viol_pooled_noWeights, dat_viol_pooled, weighted = F) # 0.195

r2_lpm_cyber_pooled_noWeights <- r2(lpm_cyber_pooled_noWeights, "wr2") # 0.155
r2_lpm_prop_pooled_noWeights <- r2(lpm_prop_pooled_noWeights, "wr2") # 0.138
r2_lpm_viol_pooled_noWeights <- r2(lpm_viol_pooled_noWeights, "wr2") # 0.235

save(list = grepv("^r2_", ls()), file = "temp/r2s.RData")

# Table ========================================================================

load("temp/ames.RData")
load("temp/r2s.RData")

# 1. Core cell formatter to handle estimates, standard errors, and significance stars
format_estimates <- function(ame_object) {
  est   <- as.numeric(ame_object$estimate)
  se    <- as.numeric(ame_object$std.error)
  p_val <- as.numeric(ame_object$p.value)
  
  # Standardize significance codes
  stars <- ""
  if (!is.na(p_val)) {
    if (p_val < 0.001) stars <- "***"
    else if (p_val < 0.01)  stars <- "**"
    else if (p_val < 0.05)  stars <- "*"
    else if (p_val < 0.10)  stars <- "+"
  }
  
  est_str <- sprintf("%.3f%s", est, stars)
  se_str  <- sprintf("(%.3f)", se)
  return(list(est = est_str, se = se_str))
}

# 2. Automation Wrapper to parse a full three-column set for a specific row type
format_row_cells <- function(cyb_obj, prop_obj, viol_obj) {
  c_cell <- format_estimates(cyb_obj)
  p_cell <- format_estimates(prop_obj)
  v_cell <- format_estimates(viol_obj)
  
  est_row <- glue(" & {c_cell$est} & \t{p_cell$est} & {v_cell$est}\\\\")
  se_row  <- glue(" & {c_cell$se} &  {p_cell$se} &  {v_cell$se} \\\\")
  return(list(est = est_row, se = se_row))
}

# 3. FIXED: Calculate average reporting rates directly from data objects
# Uses weighted.mean for weighted panels (A, B) and regular mean for unweighted panel (C)
get_direct_avg_rate <- function(data_obj, weighted = TRUE) {
  # Coerce to data.table to ensure safe syntax usage
  dt <- as.data.table(data_obj)
  
  if (weighted == T) {
    # Compute population-weighted average rate
    return(weighted.mean(dt$reported.police, w = dt$weight.reg_scaled, na.rm = TRUE))
  } else {
    # Compute simple unweighted arithmetic mean
    return(mean(dt$reported.police, na.rm = TRUE))
  }
}

# ==============================================================================
# GENERATE DYNAMIC TABLE ROWS FOR SECTIONS A, B, AND C
# ==============================================================================

# Panel A
cells_mlm_A <- format_row_cells(ames_mlm_cyber_pooled, ames_mlm_prop_pooled, ames_mlm_viol_pooled)
cells_lpm_A <- format_row_cells(ames_lpm_cyber_pooled, ames_lpm_prop_pooled, ames_lpm_viol_pooled)
avg_cyber_A <- sprintf("%.3f", get_direct_avg_rate(dat_cyber_pooled))
avg_prop_A <- sprintf("%.3f", get_direct_avg_rate(dat_prop_pooled))
avg_viol_A <- sprintf("%.3f", get_direct_avg_rate(dat_viol_pooled))

# Panel B
cells_mlm_B <- format_row_cells(ames_mlm_cyber_2024, ames_mlm_prop_2024, ames_mlm_viol_2024)
cells_lpm_B <- format_row_cells(ames_lpm_cyber_2024, ames_lpm_prop_2024, ames_lpm_viol_2024)
avg_cyber_B <- sprintf("%.3f", get_direct_avg_rate(dat_cyber_2024))
avg_prop_B <- sprintf("%.3f", get_direct_avg_rate(dat_prop_2024))
avg_viol_B <- sprintf("%.3f", get_direct_avg_rate(dat_viol_2024))

# Panel C
cells_mlm_C <- format_row_cells(ames_mlm_cyber_pooled_noWeights, ames_mlm_prop_pooled_noWeights, ames_mlm_viol_pooled_noWeights)
cells_lpm_C <- format_row_cells(ames_lpm_cyber_pooled_noWeights, ames_lpm_prop_pooled_noWeights, ames_lpm_viol_pooled_noWeights)
avg_cyber_C <- sprintf("%.3f", get_direct_avg_rate(dat_cyber_pooled, weighted = F))
avg_prop_C <- sprintf("%.3f", get_direct_avg_rate(dat_prop_pooled, weighted = F))
avg_viol_C <- sprintf("%.3f", get_direct_avg_rate(dat_viol_pooled, weighted = F))

# ==============================================================================
# COMPILE INTO INTERPOLATED LATEX LAYOUT (WITHOUT SAMPLE COUNTS)
# ==============================================================================
glue_tex <- function(x) glue::glue(x, .open = "<<", .close = ">>", .trim = F)

regr_tab <- glue_tex(R"(\begin{tabular}{lrrr}
	\hline\hline
	\addlinespace[5pt] 
	& Cybercrime & Property crime & Violent crime \\  
	\hline
	\addlinespace[5pt] 
	\multicolumn{4}{l}{\textsc{Panel A. Base model, pooled data of three survey waves}} \\
	\addlinespace[2.5pt]
	MLM<<cells_mlm_A$est>>
	   <<cells_mlm_A$se>>
	LPM<<cells_lpm_A$est>>
	   <<cells_lpm_A$se>>
	Avg. reporting rate & <<avg_cyber_A>> & <<avg_prop_A>> & <<avg_viol_A>> \\
	\addlinespace[2.5pt]   
	MLM McFadden's R$^2$ & <<sprintf("%.3f", r2_mlm_cyber_pooled)>> & <<sprintf("%.3f", r2_mlm_prop_pooled)>> & <<sprintf("%.3f", r2_mlm_viol_pooled)>> \\
	LPM Within R$^2$ & <<sprintf("%.3f", r2_lpm_cyber_pooled)>> & <<sprintf("%.3f", r2_lpm_prop_pooled)>> & <<sprintf("%.3f", r2_lpm_viol_pooled)>> \\

	\addlinespace[10pt]
	\hline
	\addlinespace[5pt]

	\multicolumn{4}{l}{\textsc{Panel B. Base model augmented with level of emotional damage,}}\\
	\multicolumn{4}{l}{\hspace*{5em}\textsc{wave 2024 only}} \\
	\addlinespace[2.5pt]
	MLM<<cells_mlm_B$est>>
	   <<cells_mlm_B$se>>
	LPM<<cells_lpm_B$est>>
	   <<cells_lpm_B$se>>
	Avg. reporting rate & <<avg_cyber_B>> & <<avg_prop_B>> & <<avg_viol_B>> \\
	\addlinespace[2.5pt]   
	MLM McFadden's R$^2$ & <<sprintf("%.3f", r2_mlm_cyber_2024)>> & <<sprintf("%.3f", r2_mlm_prop_2024)>> & <<sprintf("%.3f", r2_mlm_viol_2024)>> \\
	LPM Within R$^2$ & <<sprintf("%.3f", r2_lpm_cyber_2024)>> & <<sprintf("%.3f", r2_lpm_prop_2024)>> & <<sprintf("%.3f", r2_lpm_viol_2024)>> \\

	\addlinespace[10pt]
	\hline
	\addlinespace[5pt]

	\multicolumn{4}{l}{\textsc{Panel C. Robustness check, pooled unweighted data}}\\
	\addlinespace[2.5pt]
	MLM<<cells_mlm_C$est>>
	   <<cells_mlm_C$se>>
	LPM<<cells_lpm_C$est>>
	   <<cells_lpm_C$se>>
	Avg. reporting rate & <<avg_cyber_C>> & <<avg_prop_C>> & <<avg_viol_C>> \\
	\addlinespace[2.5pt]   
	MLM McFadden's R$^2$ & <<sprintf("%.3f", r2_mlm_cyber_pooled_noWeights)>> & <<sprintf("%.3f", r2_mlm_prop_pooled_noWeights)>> & <<sprintf("%.3f", r2_mlm_viol_pooled_noWeights)>> \\
	LPM Within R$^2$ & <<sprintf("%.3f", r2_lpm_cyber_pooled_noWeights)>> & <<sprintf("%.3f", r2_lpm_prop_pooled_noWeights)>> & <<sprintf("%.3f", r2_lpm_viol_pooled_noWeights)>> \\
	\addlinespace[5pt]
	\hline\hline
\end{tabular})"
)

# Output the clean tabular block
writeLines(regr_tab, "paper/tables/regr_1_results.tex")
