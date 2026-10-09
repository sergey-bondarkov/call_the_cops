
multi_ames_to_table <- function(ames_object, out_path) {
    setDT(ames_object)
    ames_object[, group := gsub("F", "N", group)]
    ames_object[, group := gsub("x", "*", group)]
    ngroups <- uniqueN(ames_object$group)
    if(ngroups == 4) {
        group_order <- c("N0P0", "N1P0", "N0P1", "N1P1")
    } else {
        group_order <- c("N0P0H0", "N1P0H0", "N*P0H1", "N0P1H*", "N1P1H*")
    }
    ames_object[, term_contrast := paste0(term, stri_replace_first_regex(contrast, " -.*", ""))]
    ames_object[term_contrast != "year2021", term_contrast := gsub("1$", "", term_contrast)]
    ames_object[, label := coef_map[term_contrast]]

    ames_object[, c("estimate", "conf.low", "conf.high") := lapply(.SD, function(x) sprintf("%.3f", x)), .SDcols = c("estimate", "conf.low", "conf.high")]

    ames_object[, CI := paste0("[", conf.low, ", ", conf.high, "]")]
    ames_object[, AME := paste0(estimate, ifelse(conf.high < 0 | conf.low > 0, "$\\dagger$", ""))]
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

    res_table[metric == "CI", label := ""] 
    res_table[, metric := NULL] 

    kbl(res_table,
        format = "latex",
        booktabs = T,
        escape = FALSE,
        linesep = "",
        col.names = c("Term", group_order)) %>%
    cat(file = out_path)
}
#' Prep Profile Data Matrix
#' 
#' @param list_of_tables List of model datasets (data.tables)
#' @param target_var_order Character vector of original variable strings (e.g., c("ses.age_group", "ses.higher_edu")) specifying top-to-bottom order
#' @param rename_dict Named character vector for custom renaming (e.g., c("ses.higher_edu" = "Education"))
prep_profile_data <- function(list_of_tables, target_var_order, rename_dict = NULL) {
  
  # 1. Bind separate model datasets together using the list names as indicators
  plot_data <- rbindlist(list_of_tables, idcol = "crime_type")
  
  # 2. Apply optional user manual dictionary mapping at the original term level
  if (!is.null(rename_dict)) {
    match_idx <- match(plot_data$term, names(rename_dict))
    matched_rows <- !is.na(match_idx)
    if (any(matched_rows)) {
      plot_data[matched_rows, term := as.character(rename_dict[match_idx[matched_rows]])]
    }
  }
  
  # 3. Standardize variable strings (Title Case parsing)
  plot_data[, var_clean := gsub("^ses\\.", "", term)]
  plot_data[, var_clean := gsub("_", " ", var_clean)]
  plot_data[, var_clean := tools::toTitleCase(var_clean)]
  
  # Manual overrides for consistent shorthand labels (kept as defaults)
  plot_data[var_clean == "Higher Edu", var_clean := "Education"]
  plot_data[var_clean == "Household Size F", var_clean := "Household Size"]
  
  # 4. Cleanse comparison contrast descriptions
  plot_data[, contrast_clean := gsub(" - ", " vs. ", contrast, fixed = TRUE)]
  plot_data[, contrast_clean := gsub("1 vs. 0", "Yes vs. No", contrast_clean, fixed = TRUE)]
  plot_data[, contrast_clean := gsub("3--4", "3–4", contrast_clean, fixed = TRUE)]
  
  # Build a unified factor layout string combining terms and sub-levels
  plot_data[, display_label := paste0(var_clean, " (", contrast_clean, ")")]
  
  # 5. Enforce precise hierarchical ordering based on raw 'term' variables
  sorting_dt <- unique(plot_data[, .(display_label, term, contrast_clean)])
  
  # Match against target_var_order. If a term isn't found in the order vector, 
  # it defaults to Inf so it pushes to the bottom instead of breaking.
  sorting_dt[, var_rank := match(term, target_var_order)]
  sorting_dt[is.na(var_rank), var_rank := Inf]
  
  # Order hierarchically: Major group rank first, then sub-level contrast strings
  setorder(sorting_dt, var_rank, contrast_clean)
  
  # Invert order vector because ggplot prints discrete scale levels from bottom to top
  final_axis_levels <- rev(sorting_dt$display_label)
  plot_data[, display_label := factor(display_label, levels = final_axis_levels)]
  
  # Ensure facets and categories preserve structural presentation keys
  plot_data[, crime_type := factor(crime_type, levels = names(list_of_tables))]
  plot_data[, excludes_zero := (conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0)]
  
  return(plot_data)
}


plot_multinomial_profiles <- function(plot_data, output_path, width = 14, height = 7.5) {
  
  # Grouping by crime_type ensures dodging stays perfectly aligned regardless of significance
  strict_dodge <- position_dodge(width = 0.6)
  setkey(plot_data, group, display_label, crime_type)
  
  p <- ggplot(plot_data, aes(x = estimate, y = display_label, 
                            color = crime_type, 
                            group = crime_type)) + # Moved group definition here
    
    # Null Baseline Reference Coordinate
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
    
    # Horizontal Bayesian Credible Intervals (The Whiskers)
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, alpha = excludes_zero), # Map alpha locally
                   height = 0.3, 
                   position = strict_dodge, 
                   linewidth = 0.5) +
    
    # Posterior Means (The Central Dots)
    geom_point(aes(alpha = excludes_zero), # Map alpha locally
               shape = 16, 
               position = strict_dodge, 
               size = 2.2) +
    
    # Horizontal Multi-Category Strategy Path Splitting Facets
    facet_wrap(~group, nrow = 1) +
    
    # High-Contrast Parameter Isolation Modifiers
    scale_alpha_manual(values = c("FALSE" = 0.20, "TRUE" = 1.00), guide = "none") +
    
    # Expand and split legend elements over exactly two stacked rows
    # guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    
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
  message(paste("Flawlessly synchronized profile matrix exported to:", output_path))
}

