ames_to_table_crimeclass <- function(ames_objects, mdl_list, out_path) {
    ames <- rbindlist(ames_objects, idcol = "crime_class")

    # 1. Process terms and values
    ames[, term_contrast := paste0(term, gsub(" -.*", "", contrast))]
    ames[term_contrast != "year2021", term_contrast := gsub("1$", "", term_contrast)]
    ames[, label := coef_map[term_contrast]]

    ames[, c("estimate", "conf.low", "conf.high") := lapply(.SD, function(x) sprintf("%.3f", as.numeric(x))), .SDcols = c("estimate", "conf.low", "conf.high")]

    ames[, CI := paste0("[", conf.low, ", ", conf.high, "]")]
    ames[, AME := paste0(estimate, ifelse(as.numeric(conf.high) < 0 | as.numeric(conf.low) > 0, "$\\dagger$", ""))]
    
    long_table <- melt(
                       ames, 
                       id.vars = c("label", "crime_class"), 
                       measure.vars = c("AME", "CI"), 
                       variable.name = "metric", 
                       value.name = "value"
    )

    long_table[, label := factor(label, levels = coef_map)]
    res_table <- dcast(long_table, label + metric ~ crime_class, value.var = "value")
    
    # Order columns precisely
    setcolorder(res_table, c("label", "metric", "Cybercrime", "Property", "Violent"))

    # Clean up row layout
    res_table[metric == "CI", label := ""] 
    res_table[, metric := NULL] 
    
    # Convert label column to character so we can safely stack numeric row metrics
    res_table[, label := as.character(label)]

    # ---------------------------------------------------------
    # STABLE BINARY BAYESIAN GOF EXTRACTION (PURE S3 METHODS)
    # ---------------------------------------------------------
    target_cols <- c("Cybercrime", "Property", "Violent")
    
    gof_data <- lapply(target_cols, function(cc) {
      m <- mdl_list[[cc]]
      
      # 1. Standard brms S3 method to get observations (handles rstan & cmdstanr)
      obs <- as.character(summary(m)$nobs)
      
      # 2. Standard brms S3 summary method for R2
      r2_summary <- brms::bayes_R2(m, summary = TRUE)
      r2 <- sprintf("%.3f", r2_summary[1, "Estimate"])
      
      data.table(crime_class = cc, Observations = obs, Bayesian_R2 = r2)
    })
    
    dt_gof <- rbindlist(gof_data)
    
    # Pivot the metrics wide
    gof_wide <- dcast(
      melt(dt_gof, id.vars = "crime_class", variable.name = "metric", value.name = "value"),
      metric ~ crime_class, 
      value.var = "value"
    )
    
    # Set display names for GoF row headers
    setnames(gof_wide, "metric", "label")
    gof_wide[, label := c("Observations" = "Observations", "Bayesian_R2" = "Bayesian R$^2$")[label]]
    
    # Match the explicit table structure
    setcolorder(gof_wide, c("label", target_cols))
    
    # Track position indexes for midrules
    coef_rows_count <- nrow(res_table)
    
    # Merge terms and GoF rows together
    res_table <- rbind(res_table, gof_wide)

    # ---------------------------------------------------------
    # FINAL RENDER: Apply booktabs and export
    # ---------------------------------------------------------
    kbl(res_table,
        format = "latex",
        booktabs = T,
        escape = FALSE,
        linesep = "",
        col.names = c("Term", "Cybercrime", "Property", "Violent")) %>%
    row_spec(coef_rows_count, extra_latex_after = "\\midrule") %>%
    cat(file = out_path)
}







multi_ames_to_table <- function(ames_object, mdl, mdl_null, out_path) {
    setDT(ames_object)
    ames_object[, group := gsub("F", "N", group)]
    ames_object[, group := gsub("x", "*", group)]
    ngroups <- uniqueN(ames_object$group)
    if(ngroups == 4) {
        group_order <- c("N0P0", "N1P0", "N0P1", "N1P1")
    } else {
        group_order <- c("N0P0H0", "N1P0H0", "N*P0H1", "N0P1H*", "N1P1H*")
    }
    
    # 1. Process terms and values
    ames_object[, term_contrast := paste0(term, stri_replace_first_regex(contrast, " -.*", ""))]
    ames_object[term_contrast != "year2021", term_contrast := gsub("1$", "", term_contrast)]
    ames_object[, label := coef_map[term_contrast]]

    ames_object[, c("estimate", "conf.low", "conf.high") := lapply(.SD, function(x) sprintf("%.3f", as.numeric(x))), .SDcols = c("estimate", "conf.low", "conf.high")]

    ames_object[, CI := paste0("[", conf.low, ", ", conf.high, "]")]
    ames_object[, AME := paste0(estimate, ifelse(as.numeric(conf.high) < 0 | as.numeric(conf.low) > 0, "$\\dagger$", ""))]
    
    long_table <- melt(
                       ames_object, 
                       id.vars = c("label", "group"), 
                       measure.vars = c("AME", "CI"), 
                       variable.name = "metric", 
                       value.name = "value"
    )

    long_table[, label := factor(label, levels = coef_map)]
    res_table <- dcast(long_table, label + metric ~ group, value.var = "value")
    setcolorder(res_table, c("label", "metric", group_order))

    # Clean up row layout
    res_table[metric == "CI", label := ""] 
    res_table[, metric := NULL] 
    
    # Convert label column to character so we can append custom GoF rows safely
    res_table[, label := as.character(label)]

    # ---------------------------------------------------------
    # MULTINOMIAL BAYESIAN GOF: LOO METHOD
    # ---------------------------------------------------------
    obs_val <- as.character(nobs(mdl))
    
    # Compute LOO criteria natively, suppressing the expected Pareto k warnings
    loo_full <- suppressWarnings(brms::loo(mdl, save_psis = FALSE))
    loo_null <- suppressWarnings(brms::loo(mdl_null, save_psis = FALSE))
    
    # Extract the global elpd_loo point estimates
    elpd_full <- loo_full$estimates["elpd_loo", "Estimate"]
    elpd_null <- loo_null$estimates["elpd_loo", "Estimate"]
    
    # Calculate the LOO-based McFadden R2
    loo_pseudo_r2 <- 1 - (elpd_full / elpd_null)
    r2_val <- sprintf("%.3f", loo_pseudo_r2)
    
    span_width <- length(group_order)
    
    # Build the table rows with explicit structural alignment
    gof_rows <- data.table(label = c("Observations", "LOO Pseudo $R^2$"))
    
    # Target just the FIRST group column to house the merged LaTeX text block
    first_group_col <- group_order[1]
    gof_rows[, (first_group_col) := sprintf("\\multicolumn{%d}{c}{%s}", span_width, c(obs_val, r2_val))]
    
    # Leave the remaining trailing group columns completely empty
    for(col in group_order[-1]) {
      gof_rows[, (col) := ""]
    }
    
    # Sync column positions precisely with res_table structure
    setcolorder(gof_rows, c("label", group_order))

    # Track structural index rows
    coef_rows_count <- nrow(res_table)
    
    # Merge coefficients and GoF rows smoothly without structure mismatches
    res_table <- rbind(res_table, gof_rows)

    # ---------------------------------------------------------
    # FINAL RENDER: Apply booktabs and custom line breaks
    # ---------------------------------------------------------
    kable_out <- kbl(res_table,
        format = "latex",
        booktabs = T,
        escape = FALSE,
        linesep = "",
        col.names = c("Term", group_order)) %>%
      row_spec(coef_rows_count, extra_latex_after = "\\midrule")

    cat(kable_out, file = out_path)
}


library(data.table)
library(stringi)
library(ggplot2)

prep_profile_data <- function(ames) {
  
    # 1. FIX: Use inherits() for multi-class protection
    if(inherits(ames, "list")) {
        ames <- rbindlist(ames, idcol = "crime_class")
    } 

    ames[, group := gsub("F", "N", group)]
    ames[, group := gsub("x", "*", group)]
        
    # 2. Process terms and values
    ames[, term_contrast := paste0(term, gsub(" -.*", "", contrast))]
    ames[term_contrast != "year2021", term_contrast := gsub("1$", "", term_contrast)]
    ames[, label := coef_map[term_contrast]]
    ames[, label := factor(label, levels = coef_map)]
    # \u2013 is the internal hex representation for an en-dash
    ames[, label := gsub("\u2013", "-", label)] 
    ames[, label := gsub("–", "-", label)] # Catch literal variations too
    
    # Clean the factor levels vector exactly the same way to ensure a perfect match
    clean_levels <- gsub("\u2013", "-", coef_map)
    clean_levels <- gsub("–", "-", clean_levels)
    # FIX: Keep numeric columns raw so ggplot can map coordinates on a continuous axis.
    # Convert text calculations inline where needed.
    ames[, c("estimate", "conf.low", "conf.high") := lapply(.SD, as.numeric), .SDcols = c("estimate", "conf.low", "conf.high")]

    # FIX: Safely checks raw numeric thresholds accurately
    ames[, excludes_zero := conf.low > 0 | conf.high < 0]

    ngroups <- uniqueN(ames$group)
    if(ngroups == 4) {
        group_order <- c("N0P0", "N1P0", "N0P1", "N1P1")
    } else {
        group_order <- c("N0P0H0", "N1P0H0", "N*P0H1", "N0P1H*", "N1P1H*")
    }
    ames[, group := factor(group, levels = group_order)]

    return(ames)
}


plot_multinomial_profiles <- function(plot_data, output_path, width = 14, height = 7.5) {

    strict_dodge <- position_dodge(width = 0.6)

    # 3. FIX: Cleaned up the assignment expressions to map aesthetics safely
    if(is.null(plot_data$crime_class)) {
        p_base <- ggplot(plot_data, aes(x = estimate, y = label))
    } else {
        p_base <- ggplot(plot_data, aes(x = estimate, y = label, 
                                       color = crime_class, 
                                       group = crime_class))
    }

    p <- p_base +
    
    # Null Baseline Reference Coordinate
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
    
    # Horizontal Bayesian Credible Intervals (The Whiskers)
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, alpha = excludes_zero),
                   height = 0.3, 
                   position = strict_dodge,
                   linewidth = 0.5) +
    
    # Posterior Means (The Central Dots)
    geom_point(aes(alpha = excludes_zero),
               shape = 16, 
               position = strict_dodge,
               size = 2.2) +
    
    # Horizontal Multi-Category Strategy Path Splitting Facets
    facet_wrap(~group, nrow = 1) +
    
    # High-Contrast Parameter Isolation Modifiers
    scale_alpha_manual(values = c("FALSE" = 0.20, "TRUE" = 1.00), guide = "none") +
     scale_y_discrete(limits = rev) +
    
    # Universal layout theme parameters for clean LaTeX vector embedding
    theme_bw() +
    theme(legend.position = "bottom") +
    
    labs(
      x = "AME with 95% credible interval",
      y = "",
      color = "Victimization type:"
    )
  
  # Export clean high-resolution PDF vector geometry
  ggsave(output_path, plot = p, width = width, height = height, device = "pdf")
}

