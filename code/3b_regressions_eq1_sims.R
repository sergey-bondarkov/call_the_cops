library(brms)
library(fixest)
library(data.table)
library(forcats)
library(stringi)
library(ggplot2)
library(patchwork)
library(future)
library(future.apply)
library(marginaleffects)
g <- glue::glue

setwd(Sys.getenv("CALL_THE_COPS"))

source("code/helpers/coef_map.R")
source("code/helpers/X_definition.R")
source("code/helpers/functions.R")

# Dir to store results
out <- file.path("temp", "brm")
dir.create(out, showWarnings = F, recursive = T)

# Set random seed for reproducibility
set.seed(0)

n_workers <- floor(parallelly::availableCores() * 2/3)
plan(multicore, workers = n_workers)

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")

dat_cyber_pooled <- victims_pooled[incident.crime_class == "Cybercrime"]
dat_cyber_pooled <- dat_cyber_pooled[complete.cases(dat_cyber_pooled[, ..X_property])]
dat_cyber_pooled[, qegion := fdroplevels(region)]

dat_prop_pooled <- victims_pooled[incident.crime_class == "Property"]
dat_prop_pooled <- dat_prop_pooled[complete.cases(dat_prop_pooled[, ..X_property])]
dat_prop_pooled[, region := fdroplevels(region)]

dat_viol_pooled <- victims_pooled[incident.crime_class == "Violent"]
dat_viol_pooled <- dat_viol_pooled[complete.cases(dat_viol_pooled[, ..X_violent])]
dat_viol_pooled[, region := fdroplevels(region)]

fml_prop_brm_pooled <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_property, collapse = '+')} + year + (1|region)"))
fml_viol_brm_pooled <- as.formula(glue::glue("reported.police | weights(weight) ~ social.told_someone + {paste(X_violent, collapse = '+')} + year + (1|region)"))

# Simulations
# props <- seq(0.1, 0.5, by = 0.1)
# n_rep <- 5
props <- seq(0.05, 0.5, by = 0.05)
n_rep <- 100

sims <- function(fml, dat, props, n_rep, sim_type, weight_var = "weight") {

    mean_F1 <- dat[, mean(social.told_someone)]
    
    message("Pre-compiling base Stan model structure to prevent worker race conditions...")
    base_mdl <- brm(
        formula = fml,
        data    = dat, # Use original dataset for structural skeleton
        family  = bernoulli(link = "logit"),
        prior = prior(normal(0, 3), class = "b"),

        chains  = 1,         # 1 chain is enough to fully compile the dynamic library
        iter    = 50,        # Minimal iterations to make it take under 2 seconds
        warmup  = 25,
        refresh = 0,         # Mutes output
        silent  = 2,         # Mutes warnings    
        backend = "cmdstanr"
    )
    message("Pre-compilation complete. Launching parallel simulation loop...")

    rbindlist(
      lapply(props, function(prop) {
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
            # Fit the Bayesian Multilevel Model
            mdl <- update(
                base_mdl,
                newdata = sim_dat,   
                refresh = 0,         
                silent  = 2,         
                chains  = 2,         
                iter    = 1000,
                cores   = 1,         
                threads = threading(1)
            )

            # Extract Marginal Effects
            ame_res <- avg_slopes(mdl, 
                                  variables = "social.told_someone", 
                                  re_formula = NULL,
                                  wts = weight_var)
            
            # Evaluate bounds safely using Bayesian output structures
            excludes_zero <- (ame_res$conf.low > 0 & ame_res$conf.high > 0) | 
                             (ame_res$conf.low < 0 & ame_res$conf.high < 0)
            
            derived_se <- (ame_res$conf.high - ame_res$conf.low) / 3.92

            data.table(
              estimate      = ame_res$estimate,
              std.error     = derived_se, 
              excludes_zero = excludes_zero
            )

        }, simplify = FALSE, future.seed = TRUE)
        
        repls_dt <- rbindlist(repls)

        # This section will now execute flawlessly because std.error exists
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
sims_prop_drop <- sims(fml_prop_brm_pooled, dat_prop_pooled, props, n_rep, sim_type = "drop" )
sims_viol_drop <- sims(fml_viol_brm_pooled, dat_viol_pooled, props, n_rep, sim_type = "drop" )
sims_drop <- rbindlist(list(Cybercrime = sims_cyber_drop, Property = sims_prop_drop, Violent = sims_viol_drop), idcol = "crime_class")
sims_drop[, `:=`(cil = empirical_ame - mc_std_error * 1.96, cih = empirical_ame + mc_std_error * 1.96)]
fwrite(sims_drop, file.path(out, "sims_drop_weak_priors.csv"))

sims_cyber_recode <- sims(fml_prop_brm_pooled, dat_cyber_pooled, props, n_rep, sim_type = "recode" )
sims_prop_recode <- sims(fml_prop_brm_pooled, dat_prop_pooled, props, n_rep, sim_type = "recode" )
sims_viol_recode <- sims(fml_viol_brm_pooled, dat_viol_pooled, props, n_rep, sim_type = "recode" )
sims_recode <- rbindlist(list(Cybercrime = sims_cyber_recode, Property = sims_prop_recode, Violent = sims_viol_recode), idcol = "crime_class")
sims_recode[, `:=`(cil = empirical_ame - mc_std_error * 1.96, cih = empirical_ame + mc_std_error * 1.96)]
fwrite(sims_recode, file.path(out, "sims_recode_weak_priors.csv"))

sims_drop <- fread(file.path(out, "sims_drop_weak_priors.csv"))
sims_recode <- fread(file.path(out, "sims_recode_weak_priors.csv"))

pl_drop <-
    ggplot(sims_drop, aes(proportion, empirical_ame)) +
    geom_line() + geom_point() +
    geom_ribbon(aes(ymin = cil, ymax = cih), alpha = .1) +
    geom_hline(yintercept = 0) +
    facet_wrap(~crime_class, nrow = 1) +
    scale_y_continuous(breaks = seq(0, .25, .05)) +
    labs(subtitle = "Panel A. Effect of removing a certain share of N1P1 observations on AME of informing friends or family (BRM)", 
         y = "Mean AME with 95% Monte Carlo envelope from 100 replications", x = "Share of N1P1 observations removed")

pl_recode <- 
    ggplot(sims_recode, aes(proportion, empirical_ame)) +
    geom_line() + geom_point() +
    geom_ribbon(aes(ymin = cil, ymax = cih), alpha = .1) +
    geom_hline(yintercept = 0) +
    facet_wrap(~crime_class, nrow = 1) +
    scale_y_continuous(breaks = seq(0, .25, .05)) +
    labs(subtitle = "Panel B. Effect of randomly recoding a certain share of N1P1 observations on AME of informing friends or family (BRM)", 
         y = "Mean AME with 95% Monte Carlo envelope from 100 replications", x = "Share of N1P1 observations randomly recoded")


    pdf(file.path("paper", "figures", "simulations_weak_priors.pdf"), width = 10, height = 6)
    pl_drop / pl_recode + plot_layout(axis_titles = "collect_y") & theme_bw()
    dev.off()
