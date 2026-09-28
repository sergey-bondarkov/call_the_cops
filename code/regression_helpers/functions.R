# Wrapper for LPM regression with multiple imputations
pooled_regr_with_mult_imp <- function(dat, crime_class, 
                                     y, 
                                     X,
                                     vars_to_impute = vars_to_impute,
                                     vars_to_use = vars_to_use,
                                     mdl_type,
                                     marginal_effects) {

    if(mdl_type == "lpm") {
        fml <- as.formula(glue::glue("{y} ~ {paste(X, collapse = '+')} | {paste(intersect(c('region', 'year'), names(dat)), collapse = '+')}"))
        fit_reg <- function(dat_, fml, crime_class) {
            m <- feols(fml,
                  weights = ~weight,
                  cluster = "region",
                  data = dat_[dat_$incident.crime_class == crime_class, ])

            if(marginal_effects == T) {
                avg_slopes(m, wts = "weight")
            } else {
                m
            }
        }
    }
    if(mdl_type == "mlm") {
        fml <- as.formula(glue::glue("{y} ~ {paste(X, collapse = '+')}"))
        fit_reg <- function(dat_, fml, crime_class) {
            dat_ <- dat_[dat_$incident.crime_class == crime_class, ]
            dat_ <- dat_[complete.cases(dat_[, X]), ]
            survey_design <- svydesign(
                                       id = ~region + id,  
                                       weights = ~1 + weight.reg_scaled,  
                                       data = dat_  
            )

            m <- svyglm(fml, 
                   design = survey_design, 
                   family = binomial(link = "logit"))

            if(marginal_effects == T) {
                avg_slopes(m, newdata = dat_, wts = "weight.reg_scaled")
            } else {
                m
 }
        }
    }

    # Prepare arguments for mice
    ## 'where' matrix tells which rows and columns hold values that need be imputed,
    ## the default is is.na(data) but we need to impute missings only in two columns,
    ## so we have to define the matrix
    where_m <- matrix(0, nrow = nrow(dat), ncol = ncol(dat))
    for(v in vars_to_impute) {
        where_m[which(is.na(dat[[v]])), grep(v, names(dat))] <- 1
    }

    ## mice 'predictorMatrix' argument defines which variables (matrix cols) to use
    ## in imputation of a given variable (rows), the default behavior is to use
    ## all other vars,  we don't want this, so we have to define a custom matrix
    pred_m <- matrix(0, ncol(dat), ncol(dat), dimnames = list(names(dat), names(dat)))
    pred_m[, vars_to_use[vars_to_use %in% names(dat)]] <- 1
    diag(pred_m) <- 0

    # Do initial multiple imputations
    message("Parallel multiple imputations: 5")
    n_imp_initial <- 5
    imp1_list <- future_replicate(
                                  n_imp_initial, 
                                  expr = {
                                      mice(data = dat,
                                           m = 1, 
                                           meth = "rf",
                                           predictorMatrix = pred_m,
                                           where = where_m,
                                           printFlag = FALSE)
                                  },
                                  simplify = FALSE,
                                  future.seed = TRUE # FIX 1: Resolves the UNRELIABLE VALUE warning
    )
    # Combine imputed datasets
    imp1 <- Reduce(mice::ibind, imp1_list) 
    
    # Fit models using imputed data
    message("Running regressions on 5 completed datasets")
    imp1_dat <- complete(imp1, action = "all")
    imp1_mdls <- future_lapply(imp1_dat, fit_reg, fml = fml, crime_class = crime_class)

    # Check if the number of imputations is enough for robust inference
    message("How many imputations are needed for robust inference?")
    n_imp_empirical <- how_many_imputations(imp1_mdls)
    message(n_imp_empirical)

    if(n_imp_empirical <= n_imp_initial) {
        ## If yes, pool models
        message("Pooling 5 models...")
        pooled_mdl <- pool(imp1_mdls)
    } else {
        ## If no, rerun imputations and regressions, then pool
        message("Parallel multiple imputations: ", n_imp_empirical)
        imp2_list <- future_replicate(
                                      n_imp_empirical, 
                                      expr = {
                                          mice(data = dat,
                                               m = 1, 
                                               meth = "rf",
                                               predictorMatrix = pred_m,
                                               where = where_m,
                                               printFlag = FALSE)
                                      },
                                      simplify = FALSE,
                                      future.seed = TRUE # FIX 1: Resolves the UNRELIABLE VALUE warning
        )

        # Combine imputed datasets
        imp2 <- Reduce(mice::ibind, imp2_list) 
        imp2_dat <- complete(imp2, action = "all")
        message(glue::glue("Running regressions on {n_imp_empirical} completed datasets"))
        imp2_mdls <- future_lapply(imp2_dat, fit_reg, fml = fml, crime_class = crime_class)
        message("Pooling ", n_imp_empirical, " models...")
        pooled_mdl <- pool(imp2_mdls)
    }

    # Out
    pooled_mdl
}

# Wrapper for plotting path covariates patterns
plot_path_profile <- function(mdls_list, title, a_min = 0.05) {
    # mdls_list <- paths_mdls_cyber
    pl_data <-
        rbindlist(
                  lapply(mdls_list, function(x) {
                             d <- as.data.table(summary(x$mdl))
                             d[, path := x$info$outcome]
                             inv_coef_map <- names(coef_map)
                             names(inv_coef_map) <- coef_map
                             coef_map_present <- inv_coef_map[inv_coef_map %in% d$term]
                             d[, term := fct_rev(fct_relevel(as.factor(term), coef_map_present))]
                             d[, term := fct_recode(term, !!!inv_coef_map)]
                             d
                     })
        )
    pl_data[, CIh := estimate + std.error * 1.96]
    pl_data[, CIl := estimate - std.error * 1.96]

    ggplot(pl_data, aes(term, estimate,
                        alpha = ifelse(p.value <= a_min, "sign", "nonsign"),
                        col = fcase(p.value <= 0.05, "95%",
                                    p.value <= 0.1, "90%",
                                    default = NA))) +
geom_point() +
geom_errorbar(aes(ymin = CIl, ymax = CIh), width = 0) +
scale_alpha_manual(breaks = c("sign", "nonsign"), values = c(1, .3)) +
geom_hline(yintercept = 0, linetype = "dashed") +

coord_flip() +
facet_wrap(~path, nrow = 1#, #scales = "free_x"#,
           # labeller = labeller(path = f)
           ) +
theme_bw() +
theme(legend.position = "none",
      axis.text.y = element_text(family = "mono", face = "bold"),
      plot.margin = unit(c(0,.5,0,0), "cm")) +
labs(y = "Estimate with 95% CI", x = "", title = title) +
    # )
    scale_color_manual(breaks = c("90%", "95%"),
                       values = c("orange", "darkred"),
                       name = "Conf. level:")


}

wemix_ame_pvalue <- function(mdl, dat, target_var, n_boot = 200, seed = 0) {
  
  # ============================================================================
  # INTERNALS: CORE AME CALCULATOR ENGINE
  # ============================================================================
  calc_single_ame <- function(m, d, w_col) {
    beta <- m$coef
    fixed_fml <- lme4::nobars(formula(m))
    X_template <- model.matrix(fixed_fml, data = d)
    
    X_1 <- X_template
    X_0 <- X_template
    
    target_col <- grep(target_var, colnames(X_template), value = TRUE)
    if(length(target_col) == 0) {
      stop(paste("Could not find variable", target_var, "in the model matrix."))
    }
    
    X_1[, target_col] <- 1
    X_0[, target_col] <- 0
    
    common_cols <- intersect(names(beta), colnames(X_template))
    
    preds_1 <- drop(1 / (1 + exp(- (X_1[, common_cols] %*% beta[common_cols]))))
    preds_0 <- drop(1 / (1 + exp(- (X_0[, common_cols] %*% beta[common_cols]))))
    
    return(weighted.mean(preds_1 - preds_0, w = d[[w_col]]))
  }
  
  # ============================================================================
  # STEP 1: CALCULATE OBSERVED AME
  # ============================================================================
  parsed_weights <- as.character(mdl$call$weights)
  if (parsed_weights[1] == "c") {
    parsed_weights <- parsed_weights[-1] 
  }
  
  primary_weight_col <- parsed_weights[1] 
  ame_observed       <- calc_single_ame(mdl, dat, primary_weight_col)
  
  # ============================================================================
  # STEP 2: SETUP DATA METADATA (STREAMLINED FOR 'region')
  # ============================================================================
  # Hardcoded target clustering vector boundaries
  unique_clusters <- unique(dat$region)
  clean_formula   <- formula(mdl)
  
  # ============================================================================
  # STEP 3: PARALLELIZED BOOTSTRAP VIA future_lapply
  # ============================================================================
  cat("Running future-parallelized cluster bootstrap (", n_boot, " iterations) for: ", target_var, "...\n", sep="")
  
  boot_results <- future_lapply(1:n_boot, function(i) {
    
    # Resample regions
    boot_clusters <- sample(unique_clusters, replace = TRUE)
    draw_counts    <- table(boot_clusters)
    
    # Build frequency multiplier columns locally pointing to 'region'
    boot_mult <- as.numeric(draw_counts[as.character(dat$region)])
    boot_mult[is.na(boot_mult)] <- 0
    
    local_dat <- dat
    local_dat$b_weight_scaled <- local_dat[[parsed_weights[1]]] * boot_mult
    local_dat$b_weight_reg    <- local_dat[[parsed_weights[2]]] * boot_mult
    
    # Refit model locally on worker node
    boot_model <- tryCatch({
      suppressWarnings(
        do.call(WeMix::mix, list(
          formula = clean_formula,
          data    = local_dat,
          weights = c("b_weight_scaled", "b_weight_reg")
        ))
      )
    }, error = function(e) NULL)
    
    # Extract structural AME for this turn
    if (!is.null(boot_model)) {
      return(calc_single_ame(boot_model, local_dat, "b_weight_scaled"))
    } else {
      return(NA)
    }
  }, future.seed = seed) 
  
  boot_ames <- unlist(boot_results)
  
  # ============================================================================
  # STEP 4: STATISTICAL INFERENCE & OUTPUT GENERATION
  # ============================================================================
  boot_ames <- boot_ames[!is.na(boot_ames)]
  
  se_boot  <- sd(boot_ames)
  z_score  <- ame_observed / se_boot
  p_value  <- 2 * (1 - pnorm(abs(z_score)))
  
  res_table <- data.frame(
    Variable  = target_var,
    Estimate  = ame_observed,
    Std.Error = se_boot,
    z.value   = z_score,
    p.value   = p_value,
    stringsAsFactors = FALSE
  )
  
  return(res_table)
}
