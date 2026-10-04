#library(mice)
#library(WeMix)
library(brms)
library(fixest)
library(data.table)
library(forcats)
library(stringi)
library(ggplot2)
library(patchwork)
#library(howManyImputations)
library(future)
library(future.apply)
g <- glue::glue

setwd(Sys.getenv("CALL_THE_COPS"))

source("code/coef_map.R")
source("code/helpers/X_definition.R")
source("code/helpers/functions.R")

# Dir to store results
out <- file.path("temp", "brm")
dir.create(out, showWarnings = F, recursive = T)

# Set random seed for reproducibility
set.seed(0)

n_workers <- floor(parallelly::availableCores() * 2/3)
plan(multicore, workers = n_workers)
#plan(list(
          #tweak(multicore, workers = unname(n_workers) %/% 5),
          #tweak(multicore, workers = I(5))
          #))

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")

dat_cyber_pooled <- victims_pooled[incident.crime_class == "Cybercrime"]
dat_cyber_pooled <- dat_cyber_pooled[complete.cases(dat_cyber_pooled[, ..X_property])]
dat_cyber_pooled[, region := fdroplevels(region)]

dat_prop_pooled <- victims_pooled[incident.crime_class == "Property"]
dat_prop_pooled <- dat_prop_pooled[complete.cases(dat_prop_pooled[, ..X_property])]
dat_prop_pooled[, region := fdroplevels(region)]

dat_viol_pooled <- victims_pooled[incident.crime_class == "Violent"]
dat_viol_pooled <- dat_viol_pooled[complete.cases(dat_viol_pooled[, ..X_violent])]
dat_viol_pooled[, region := fdroplevels(region)]

fml_prop_brm_pooled <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_property, collapse = '+')} + year + (1|region)"))
fml_viol_brm_pooled <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_violent, collapse = '+')} + year + (1|region)"))

# Simulations
props <- seq(0.05, 0.5, by = 0.05)
n_rep <- 100

sims <- function(fml, dat, props, n_rep, sim_type, weight_var = "weight") {

    mean_F1 <- dat[, mean(social.told_someone)]
    
    # 1. CRITICAL FIX: Pre-compile the model structure once on a single core
    message("Pre-compiling base Stan model structure to prevent worker race conditions...")
    base_mdl <- brm(
        formula = fml,
        data    = dat, # Use original dataset for structural skeleton
        family  = bernoulli(link = "logit"),
        chains  = 0,         # 0 chains means it ONLY compiles the structure and stops
        backend = "cmdstanr"
    )
    message("Pre-compilation complete. Launching parallel simulation loop...")

    rbindlist(
      lapply(props, function(prop) {
        # Pass future.seed = TRUE to ensure parallel MCMC reproducibility safely
        repls <- future_replicate(n_rep, {
            sim_dat <- copy(dat)
            F1P1_ids <- sim_dat[F1P1 == 1]$id
            rand_ids <- sample(F1P1_ids, floor(length(F1P1_ids) * prop))

            if(sim_type == "recode") {
                sim_dat[id %in% rand_ids, social.told_someone := rbinom(.N, 1, mean_F1)]
            }
            if(sim_type == "drop") {
                sim_dat <- sim_dat[id %notin% rand_ids] 
            }

            # 2. CRITICAL FIX: Use update() to bypass compilation inside the worker
            mdl <- update(
                base_mdl,
                newdata = sim_dat,   # Swaps the data matrix safely
                refresh = 0,         
                silent  = 2,         
                chains  = 2,         
                iter    = 1000,
                cores   = 1,         # Keeps worker bounded to 1 core
                threads = threading(1)
            )

            # 3. Extract Marginal Effects
            ame_res <- avg_slopes(mdl, variables = "social.told_someone", re_formula = NULL)
            
            excludes_zero <- (ame_res$conf.low > 0 & ame_res$conf.high > 0) | 
                             (ame_res$conf.low < 0 & ame_res$conf.high < 0)

            data.table(
              estimate      = ame_res$estimate,
              std.error     = ame_res$std.error, 
              excludes_zero = excludes_zero
            )

        }, simplify = FALSE, future.seed = TRUE) # Essential for parallel Stan seeds
        
        repls_dt <- rbindlist(repls)

        pooled_prop_res <- repls_dt[, .(
          proportion      = prop,
          sim_type        = sim_type,
          empirical_ame   = mean(estimate, na.rm = TRUE),
          mc_std_error    = sd(estimate, na.rm = TRUE), 
          avg_model_se    = mean(std.error, na.rm = TRUE),
          empirical_power = mean(excludes_zero, na.rm = TRUE), 
          n_replicates    = .N
        )]
        
        pooled_prop_res
      })
    )
}
sims_cyber_drop <- sims(fml_prop_brm_pooled, dat_cyber_pooled, props, n_rep, sim_type = "drop" )

sims_cyber_drop <- sims(fml_prop_mlm_pooled, dat_cyber_pooled, props, n_rep, sim_type = "drop" )
sims_prop_drop <- sims(fml_prop_mlm_pooled, dat_prop_pooled, props, n_rep, sim_type = "drop" )
sims_viol_drop <- sims(fml_viol_mlm_pooled, dat_viol_pooled, props, n_rep, sim_type = "drop" )
sims_drop <- rbindlist(list(Cybercrime = sims_cyber_drop, Property = sims_prop_drop, Violent = sims_viol_drop), idcol = "crime_class")
sims_drop[, `:=`(cil = empirical_ame - mc_std_error * 1.96, cih = empirical_ame + mc_std_error * 1.96)]
fwrite(sims_drop, file.path("data", "temp", "multilevel", "sims_drop.csv"))

sims_cyber_recode <- sims(fml_prop_mlm_pooled, dat_cyber_pooled, props, n_rep, sim_type = "recode" )
sims_prop_recode <- sims(fml_prop_mlm_pooled, dat_prop_pooled, props, n_rep, sim_type = "recode" )
sims_viol_recode <- sims(fml_viol_mlm_pooled, dat_viol_pooled, props, n_rep, sim_type = "recode" )
sims_recode <- rbindlist(list(Cybercrime = sims_cyber_recode, Property = sims_prop_recode, Violent = sims_viol_recode), idcol = "crime_class")
sims_recode[, `:=`(cil = empirical_ame - mc_std_error * 1.96, cih = empirical_ame + mc_std_error * 1.96)]
fwrite(sims_recode, file.path("data", "temp", "multilevel", "sims_recode.csv"))

pl_drop <-
    ggplot(sims_drop, aes(proportion, empirical_ame)) +
    geom_line() + geom_point() +
    geom_ribbon(aes(ymin = cil, ymax = cih), alpha = .1) +
    geom_hline(yintercept = 0) +
    facet_wrap(~crime_class, nrow = 1) +
    # scale_x_continuous(breaks = seq(0, 0.5, 0.05), labels = function(x) stringi::stri_sub(x, from = 2)) +
    labs(subtitle = "Panel A. Effect of removing a certain share of F1P1 observations\non AME of informing friends or family (MLM)", 
         y = "Pooled estimate with 95% CI\nfrom 100 replications", x = "Share of F1P1 observations removed")

pl_recode <- 
    ggplot(sims_recode, aes(proportion, empirical_ame)) +
    geom_line() + geom_point() +
    geom_ribbon(aes(ymin = cil, ymax = cih), alpha = .1) +
    geom_hline(yintercept = 0) +
    facet_wrap(~crime_class, nrow = 1) +
    # scale_x_continuous(breaks = seq(0, 0.5, 0.05), labels = function(x) stringi::stri_sub(x, from = 2)) +
    labs(subtitle = "Panel B. Effect of randomly recoding a certain share of F1P1 observations\non AME of informing friends or family (MLM)", 
         y = "Pooled estimate with 95% CI\nfrom 100 replications", x = "Share of F1P1 observations randomly recoded")

    pdf(file.path("paper", "figures", "simulations.pdf"), width = 10, height = 6)
    pl_drop / pl_recode & theme_bw()
    dev.off()
