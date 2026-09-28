#library(mice)
library(WeMix)
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
source("code/regression_helpers/X_definition.R")
source("code/regression_helpers/functions.R")

# Dir to store results
out <- file.path("data", "temp", "multilevel")
dir.create(out, showWarnings = F, recursive = T)

# Set random seed for reproducibility
set.seed(0)

n_workers <- floor(parallelly::availableCores() * 2/3)
plan(multisession, workers = n_workers)
#plan(list(
          #tweak(multicore, workers = unname(n_workers) %/% 5),
          #tweak(multicore, workers = I(5))
          #))

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")

# Simulations
props <- seq(0, 0.5, by = 0.05)
n_rep <- 100

sims <- function(fml, dat, props, n_rep, sim_type) {

    mean_F1 <- dat[, mean(social.told_someone)]
    sim_dat <- copy(dat)

    rbindlist(
              lapply(props, function(prop) {
                         repls <- future_replicate(n_rep, 
                                                   {
                                                       F1P1_ids <- sim_dat[F1P1 == 1]$id
                                                       rand_ids <- sample(F1P1_ids, floor(length(F1P1_ids) * prop))

                                                       if(sim_type == "recode") {
                                                           sim_dat[id %in% rand_ids, social.told_someone := rbinom(.N, 1, mean_F1)]
                                                       }
                                                       if(sim_type == "drop") {
                                                           sim_dat <- sim_dat[id %notin% rand_ids] 
                                                       }

                                                       mdl <- mix(fml, 
                                                                  data = sim_dat,
                                                                  weights = c("weight.reg_scaled", "reg_weight"))

                                                       res <- bootstrap_wemix_ame(mdl, sim_dat, "social.told_someone", weighted = T)

                                                       res

                                                   }, simplify = F)
                         repls_dt <- rbindlist(repls)

                           
    # --- PROPER SIMULATION POOLING METRICS ---
    pooled_prop_res <- repls_dt[, .(
      proportion     = prop,
      sim_type       = sim_type,
      
      # 1. Empirical Point Estimate (Average AME across simulations)
      empirical_ame  = mean(estimate),
      
      # 2. Monte Carlo Standard Error (The true variance of the simulation distribution)
      mc_std_error   = sd(estimate), 
      
      # Average Model-Reported Standard Error (for bias comparison)
      avg_model_se   = mean(std.error),
      
      # 3. Empirical Statistical Power (Proportion of times the effect remained significant)
      empirical_power = mean(p.value < 0.05),
      
      n_replicates   = .N
    )]
    
    return(pooled_prop_res)
}
              ))}


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
