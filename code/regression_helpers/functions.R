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
    # imp1 <- futuremice(dat,
                       # m = 5,
                       # meth = "rf",
                       # predictorMatrix = pred_m,
                       # where = where_m,
                       # # future.plan = "sequential",
                       # # future.plan = "multisession",
    # parallelseed = 0)
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

    # FIX 2: Safely merges the list of 5 mids objects step-by-step
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

        # FIX 2: Safely merges the list of 5 mids objects step-by-step
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




## Wrapper for LPM regression with multiple imputations
#pooled_reg_with_mult_imp2 <- function(dat, fml, crime_class,
                                      #vars_to_impute = vars_to_impute,
                                      #vars_to_use = vars_to_use) {
    ## Prepare arguments for mice
    ### 'where' matrix tells which rows and columns hold values that need be imputed,
    ### the default is is.na(data) but we need to impute missings only in two columns,
    ### so we have to define the matrix
    #where_m <- matrix(0, nrow = nrow(dat), ncol = ncol(dat))
    #for(v in vars_to_impute) {
        #where_m[which(is.na(dat[[v]])), grep(v, names(dat))] <- 1
    #}

    ### mice 'predictorMatrix' argument defines which variables (matrix cols) to use
    ### in imputation of a given variable (rows), the default behavior is to use
    ### all other vars,  we don't want this, so we have to define a custom matrix
    #pred_m <- matrix(0, ncol(dat), ncol(dat), dimnames = list(names(dat), names(dat)))
    #pred_m[, vars_to_use] <- 1
    #diag(pred_m) <- 0

    ## Do initial multiple imputations
    #message("Parallel multiple imputations: 5")
    #n_imp_initial <- 5
    #imp1 <- futuremice(dat,
                       #m = 5,
                       #meth = "rf",
                       #predictorMatrix = pred_m,
                       #where = where_m,
                       #parallelseed = 0)

    ## Fit models using imputed data
    #message("LPM regressions on 5 completed datasets")
    #imp1_dat <- complete(imp1, action = "all")
    #fit_reg <- function(dat_, fml, crime_class) {
        #feols(fml,
              #weights = ~ weight,
              #cluster = intersect(c("region", "year"), names(dat)),
              #data = dat_[dat_$incident.crime_class == crime_class, ])
    #}
    #imp1_mdls <- lapply(imp1_dat, fit_reg, fml = fml, crime_class = crime_class)

    ## Check if the number of imputations is enough for robust inference
    #message("How many imputations are needed for robust inference?")
    #n_imp_empirical <- how_many_imputations(imp1_mdls)
    #message(n_imp_empirical)

    #if(n_imp_empirical <= n_imp_initial) {
        ### If yes, pool models
        #message("Pooling 5 models...")
        #imp1_mdls
    #} else {
        ### If no, rerun imputations and regressions, then pool
        #message("Parallel multiple imputations: ", n_imp_empirical)
        #imp2 <- futuremice(dat,
                           #m = n_imp_empirical,
                           #meth = "rf",
                           #predictorMatrix = pred_m,
                           #where = where_m,
                           #parallelseed = 0)

        #imp2_dat <- complete(imp2, action = "all")
        #imp2_mdls <- lapply(imp2_dat, fit_reg, fml = fml, crime_class = crime_class)
        ## message("Pooling ", n_imp_empirical, " models...")
        ## pooled_mdl <- pool(imp2_mdls)
        #imp2_mdls
    #}

    ## # Save information on data
    ## outcome <- as.character(fml)[2]
    ## n_obs <- imp1_mdls[[1]]$nobs
    ## n_class1 <- {
    ## 	orig_data <- as.data.table(fixest_data(imp1_mdls[[1]]))
    ## 	y_name <- as.character(imp1_mdls[[1]]$fml)[2]
    ## 	orig_data[y == 1, .N, env = list(y = y_name)]
    ## }
    ##
    ## message("Finished")
    ##
    ## # Return pooled model alongside numbers of observations and class 1 cases
    ## list(mdl = pooled_mdl,
    ## 		 info = list(outcome = outcome,
    ## 		 						n_obs = n_obs,
    ## 		 						n_class1 = n_class1,
    ## 		 						len_X = {
    ## 		 							rhs <- as.character(fml)[3]
    ## 		 							rhs_woFE <- stri_replace_all_regex(rhs, "\\|", "")
    ## 		 							stri_count_fixed(rhs_woFE, "+") + 1
    ## 		 						}
    ## 		 ))

#}
