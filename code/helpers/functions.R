# ==============================================================================
# HELPER FUNCTIONS FOR POST-ESTIMATION MLM CALCULATIONS USING WEMIX
# ==============================================================================
calculate_mcfadden_r2 <- function(full_model, dat, weighted = T) {

    message("Fitting baseline null model to calculate McFadden's Pseudo R2...")

    # 1. Fit the reference Null Model containing ONLY alternative intercepts and random effects
    if(weighted == T) {
        null_model <- mix(
                          formula = reported.police ~ (1 | region),
                          data    = dat,
                          weights = c("weight.reg_scaled", "reg_weight")
        )

        # 2. Extract both Pseudo Log-Likelihood metrics
        lnL_full <- full_model$lnl
        lnL_null <- null_model$lnl

    } else {
        null_model <- glmer(
                            formula = reported.police ~ (1 | region),
                            data    = dat,
                            family = binomial(link = "logit")  
        )

        # 2. Extract both Pseudo Log-Likelihood metrics
        lnL_full <- as.numeric(summary(full_model)$logLik)
        lnL_null <- as.numeric(summary(null_model)$logLik)

    }

    # 3. Apply the ratio conversion formula
    pseudo_r2 <- 1 - (lnL_full / lnL_null)
    return(pseudo_r2)
}

# Calculate Average Marginal Effects (AME) for a Binary Variable in WeMix
calculate_wemix_ame <- function(model, data, target_var, weighted) {
  # 1. Create clean counterfactual data frame copies
  dat_cf_0 <- data
  dat_cf_1 <- data
  
  dat_cf_0[[target_var]] <- 0
  dat_cf_1[[target_var]] <- 1
  
  # 2. Add padding rows to bypass the constant-variable error rule in predict.WeMixResults
  dummy_0 <- dat_cf_0[1, , drop = FALSE]
  dummy_0[[target_var]] <- 1
  
  dummy_1 <- dat_cf_1[1, , drop = FALSE]
  dummy_1[[target_var]] <- 0
  
  dat_cf_0_padded <- rbind(dat_cf_0, dummy_0)
  dat_cf_1_padded <- rbind(dat_cf_1, dummy_1)
  
  # 3. Request native predictions from WeMix object
  preds_0_full <- predict(model, newdata = dat_cf_0_padded, type = "response")
  preds_1_full <- predict(model, newdata = dat_cf_1_padded, type = "response")
  
  # 4. Remove padding observations to return to original dimensions
  preds_0 <- preds_0_full[-length(preds_0_full)]
  preds_1 <- preds_1_full[-length(preds_1_full)]
  
  # 5. Return the point estimate
  if(weighted == T) {
      ame <- weighted.mean(preds_1 - preds_0, w = data$weight.reg_scaled )
  } else {
      ame <- mean(preds_1 - preds_0)
  }  

  return(ame)

}


# Parametric Bootstrap for WeMix AME Uncertainty Estimation (Krinsky-Robb Method)
bootstrap_wemix_ame <- function(model, data, target_var, weighted, B = 500, seed = 0) {

  set.seed(seed)
  
  # 1. Extract estimated parameters and variance-covariance structure
  beta_hat <- model$coef
  vcov_matrix <- model$cov_mat
  
  # Explicitly align matrices to avoid naming mismatches
  rownames(vcov_matrix) <- names(beta_hat)
  colnames(vcov_matrix) <- names(beta_hat)
  
  # 2. Calculate the baseline empirical point estimate using our helper function
  point_estimate <- calculate_wemix_ame(model, data, target_var, weighted)
  
  # 3. Generate alternative simulated coefficient paths
  simulated_betas <- MASS::mvrnorm(n = B, mu = beta_hat, Sigma = vcov_matrix)
  
  # 4. Initialize storage vector
  bootstrap_ames <- numeric(B)
  
  message(sprintf("Starting parametric bootstrap with %d replicates for variable '%s'...", B, target_var))
  
  # 5. Run the sampling simulation loop
  for (i in 1:B) {
    # Isolate a model clone and swap in the simulated coefficients
    boot_model <- model
    boot_model$coef <- simulated_betas[i, ]
    
    # Calculate AME for this simulated drawer
    bootstrap_ames[i] <- calculate_wemix_ame(boot_model, data, target_var, weighted)
    
    if (i %% 50 == 0) {
      cat(sprintf("  Completed replicate: %d / %d\n", i, B))
    }
  }
  
  # 6. Calculate inferential statistics
  ame_se <- sd(bootstrap_ames)
  ame_ci <- quantile(bootstrap_ames, probs = c(0.025, 0.975))
  z_stat <- point_estimate / ame_se
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  # 7. Package results cleanly
  results_summary <- data.frame(
    term = target_var,
    estimate = point_estimate,
    std.error = ame_se,
    conf.low = ame_ci[1],
    conf.high = ame_ci[2],
    statistic = z_stat,
    p.value = p_value,
    row.names = NULL
  )
  
  return(results_summary)
}
# ==============================================================================
# TYPOLOGY POST-ESTIMATION CALCULATORS (data.table + WeMix)
# ==============================================================================
library(data.table)
library(MASS)

#' Calculate Typology AMEs from a WeMix Poisson-Trick Model
calculate_typology_ame <- function(model, data, target_var, category_name) {
  # 1. Create counterfactual data.table copies
  dat_cf_0 <- copy(data); dat_cf_0[, (target_var) := 0]
  dat_cf_1 <- copy(data); dat_cf_1[, (target_var) := 1]
  
  # 2. Add a 4-row padding block (one full choice set) to bypass WeMix variance checks
  dummy_0 <- copy(dat_cf_0[1:4]); dummy_0[, (target_var) := 1]
  dummy_1 <- copy(dat_cf_1[1:4]); dummy_1[, (target_var) := 0]
  
  dat_padded_0 <- rbind(dat_cf_0, dummy_0)
  dat_padded_1 <- rbind(dat_cf_1, dummy_1)
  
  # 3. Pull native predictions (unconstrained counts)
  counts_0_full <- predict(model, newdata = dat_padded_0, type = "response")
  counts_1_full <- predict(model, newdata = dat_padded_1, type = "response")
  
  counts_0 <- counts_0_full[1:nrow(dat_cf_0)]
  counts_1 <- counts_1_full[1:nrow(dat_cf_1)]
  
  # 4. Normalization / Softmax step using data.table operations by reference
  eval_dt <- data[, .(obs_id, category = as.character(category))]
  eval_dt[, `:=`(c_0 = counts_0, c_1 = counts_1)]
  
  eval_dt[, `:=`(
    p_0 = c_0 / sum(c_0),
    p_1 = c_1 / sum(c_1)
  ), by = .(obs_id)]
  
  # 5. Compute mean difference for the target pathway
  return(eval_dt[category == category_name, mean(p_1 - p_0)])
}

#' Parametric Bootstrap for Typology AME Uncertainty 
bootstrap_typology_ame <- function(model, data, target_var, categories, B = 500, seed = 42) {
  set.seed(seed)
  
  beta_hat    <- model$coef
  vcov_matrix <- model$cov_mat
  rownames(vcov_matrix) <- names(beta_hat)
  colnames(vcov_matrix) <- names(beta_hat)
  
  # Draw B simulated coefficient vectors
  simulated_betas <- MASS::mvrnorm(n = B, mu = beta_hat, Sigma = vcov_matrix)
  
  # Matrix to store results: B rows, one column per category
  boot_results <- matrix(NA, nrow = B, ncol = length(categories))
  colnames(boot_results) <- categories
  
  message(sprintf("Running parametric bootstrap (%d replicates) for '%s'...", B, target_var))
  
  for (i in 1:B) {
    boot_model <- model
    boot_model$coef <- simulated_betas[i, ]
    
    for (cat_name in categories) {
      boot_results[i, cat_name] <- calculate_typology_ame(boot_model, data, target_var, cat_name)
    }
    if (i %% 50 == 0) cat(sprintf("  Completed replicate: %d / %d\n", i, B))
  }
  
  # Compile final summary dataframe
  summary_list <- lapply(categories, function(cat_name) {
    pt_est <- calculate_typology_ame(model, data, target_var, cat_name)
    sim_draws <- boot_results[, cat_name]
    se <- sd(sim_draws)
    ci <- quantile(sim_draws, probs = c(0.025, 0.975))
    z  <- pt_est / se
    p  <- 2 * (1 - pnorm(abs(z)))
    
    data.frame(
      pathway   = cat_name,
      estimate  = pt_est,
      std.error = se,
      conf.low  = ci[1],
      conf.high = ci[2],
      statistic = z,
      p.value   = p,
      row.names = NULL
    )
  })
  
  return(rbindlist(summary_list))
}

# ==============================================================================
# CORRECTED FACTOR-COMPATIBLE TYPOLOGY POST-ESTIMATION CALCULATORS
# ==============================================================================
library(data.table)
library(MASS)
library(glue)

#' Calculate Typology AMEs for Binary or Multilevel Factor Variables
calculate_typology_ame_factor <- function(model, data, target_var, category_name, focal_level = NULL, baseline_level = NULL) {
  
  # 1. Determine if we are handling a factor or a binary variable
  is_factor_var <- is.factor(data[[target_var]]) || is.character(data[[target_var]])
  
  # 2. Setup counterfactual data copies
  dat_cf_0 <- copy(data)
  dat_cf_1 <- copy(data)
  
  if (is_factor_var) {
    # For factors: cf_0 sets everyone to the baseline level; cf_1 sets everyone to the focal level
    dat_cf_0[, (target_var) := as.character(baseline_level)]
    dat_cf_1[, (target_var) := as.character(focal_level)]
    
    # Re-coerce back to factor using the exact levels structure of the original data
    orig_levels <- levels(as.factor(data[[target_var]]))
    dat_cf_0[, (target_var) := factor(get(target_var), levels = orig_levels)]
    dat_cf_1[, (target_var) := factor(get(target_var), levels = orig_levels)]
  } else {
    # Standard binary switch logic
    dat_cf_0[, (target_var) := 0]
    dat_cf_1[, (target_var) := 1]
  }
  
  # 3. Create a 4-row padding block (one full choice set) to bypass WeMix variance checks
  dummy_0 <- copy(data[1:4])
  dummy_1 <- copy(data[1:4])
  
  dat_padded_0 <- rbind(dat_cf_0, dummy_0)
  dat_padded_1 <- rbind(dat_cf_1, dummy_1)
  
  # 4. Request native predictions (unconstrained counts)
  counts_0_full <- predict(model, newdata = dat_padded_0, type = "response")
  counts_1_full <- predict(model, newdata = dat_padded_1, type = "response")
  
  counts_0 <- counts_0_full[1:nrow(dat_cf_0)]
  counts_1 <- counts_1_full[1:nrow(dat_cf_1)]
  
  # 5. Softmax / Normalization Step by reference
  eval_dt <- data[, .(obs_id, category = as.character(category))]
  eval_dt[, `:=`(c_0 = counts_0, c_1 = counts_1)]
  
  eval_dt[, `:=`(
    p_0 = c_0 / sum(c_0),
    p_1 = c_1 / sum(c_1)
  ), by = .(obs_id)]
  
  # 6. Extract the average probability difference for this specific pathway
  return(eval_dt[category == category_name, mean(p_1 - p_0)])
}


#' Batch Parametric Bootstrap for Binary and Multilevel Factor Typology AMEs
batch_bootstrap_typology_ame <- function(model, data, target_vars, categories, B = 500, seed = 42) {
  set.seed(seed)
  
  beta_hat    <- model$coef
  vcov_matrix <- model$cov_mat
  rownames(vcov_matrix) <- names(beta_hat)
  colnames(vcov_matrix) <- names(beta_hat)
  
  message(sprintf("Drawing %d simulated parameter vectors from vcov matrix...", B))
  simulated_betas <- MASS::mvrnorm(n = B, mu = beta_hat, Sigma = vcov_matrix)
  
  all_variables_results <- list()
  
  for (current_var in target_vars) {
    
    # Determine the comparison levels for the current variable
    is_factor_var <- is.factor(data[[current_var]]) || is.character(data[[current_var]])
    
    if (is_factor_var) {
      var_levels <- levels(as.factor(data[[current_var]]))
      base_level <- var_levels[1] # R treats the first level as the reference baseline
      levels_to_test <- var_levels[-1] # Evaluate all subsequent levels against the baseline
      message(sprintf("\n=== Processing Factor Variable: '%s' (Baseline: '%s') ===", current_var, base_level))
    } else {
      base_level <- NULL
      levels_to_test <- "1" # For binary, we just evaluate the 1 vs 0 shift
      message(sprintf("\n=== Processing Binary Variable: '%s' (0 vs 1) ===", current_var))
    }
    
    # Loop over each contrast level for the current variable
    for (focal_lvl in levels_to_test) {
      term_label <- if (is_factor_var) glue("{current_var} [{focal_lvl} vs {base_level}]") else current_var
      message(sprintf("  Simulating draws for contrast: %s", term_label))
      
      boot_results <- matrix(NA, nrow = B, ncol = length(categories))
      colnames(boot_results) <- categories
      
      for (i in 1:B) {
        boot_model <- model
        boot_model$coef <- simulated_betas[i, ]
        
        for (cat_name in categories) {
          boot_results[i, cat_name] <- calculate_typology_ame_factor(
            boot_model, data, current_var, cat_name, 
            focal_level = focal_lvl, baseline_level = base_level
          )
        }
      }
      
      # Compute final uncertainty matrix for this specific contrast
      var_summary_list <- lapply(categories, function(cat_name) {
        pt_est <- calculate_typology_ame_factor(
          model, data, current_var, cat_name, 
          focal_level = focal_lvl, baseline_level = base_level
        )
        sim_draws <- boot_results[, cat_name]
        se <- sd(sim_draws)
        ci <- quantile(sim_draws, probs = c(0.025, 0.975))
        z  <- pt_est / se
        p  <- 2 * (1 - pnorm(abs(z)))
        
        data.frame(
          term      = term_label,
          pathway   = cat_name,
          estimate  = pt_est,
          std.error = se,
          conf.low  = ci[1],
          conf.high = ci[2],
          statistic = z,
          p.value   = p,
          row.names = NULL
        )
      })
      
      all_variables_results[[paste0(current_var, "_", focal_lvl)]] <- rbindlist(var_summary_list)
    }
  }
  
  message("\n=== Batch Processing Complete! ===")
  return(rbindlist(all_variables_results))
}

