library(data.table)
library(forcats)
library(stringi)
library(ggplot2)
library(patchwork)
library(ggh4x)
library(future)
library(future.apply)
library(brms)
library(marginaleffects)
library(modelsummary)
library(kableExtra)
g <- glue::glue

setwd(Sys.getenv("CALL_THE_COPS"))

source("code/helpers/coef_map.R")
source("code/helpers/X_definition.R")
source("code/helpers/functions.R")

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_pooled[F1P1 == 1, path := "F1P1"]
victims_pooled[F0P1 == 1, path := "F0P1"]
victims_pooled[F1P0 == 1, path := "F1P0"]
victims_pooled[F0P0 == 1, path := "F0P0"]
victims_pooled[, path := fct_relevel(path, "F1P1")]

# Enforce clean sequential conditional mapping block
# dat_viol_pooled[, path := as.character(NA)]

victims_pooled[, path_viol := as.character(NA)]

# 1. Complete Isolation
victims_pooled[incident.went_to_hospital == 0 & social.told_someone == 0 & reported.police == 0, path_viol := "F0P0H0"]

# 2. Pure Informal Track
victims_pooled[incident.went_to_hospital == 0 & social.told_someone == 1 & reported.police == 0, path_viol := "F1P0H0"]

# 3. Medical Social Track (Hospitalized, No Police)
victims_pooled[incident.went_to_hospital == 1 & reported.police == 0, path_viol := "FxP0H1"]

# 4. Pure Formal Track (Police involved, No Family)
victims_pooled[social.told_someone == 0 & reported.police == 1, path_viol := "F0P1Hx"]

# 5. Unified Joint Strategy (Both network and police involved, regardless of hospital)
victims_pooled[social.told_someone == 1 & reported.police == 1, path_viol := "F1P1Hx"]

# Convert to factor and explicitly anchor the unified joint strategy as your level 1 reference baseline
victims_pooled[, path_viol := fct_relevel(path_viol, "F1P1Hx")]

dat_cyber_pooled <- victims_pooled[incident.crime_class == "Cybercrime"]
dat_cyber_pooled <- dat_cyber_pooled[complete.cases(dat_cyber_pooled[, ..X_property])]
dat_cyber_pooled[, mean(is.na(path))]
dat_cyber_pooled[, region := fdroplevels(region)]
dat_cyber_pooled[, uniqueN(path)]
dat_cyber_pooled[, .N, path]

dat_prop_pooled <- victims_pooled[incident.crime_class == "Property"]
dat_prop_pooled <- dat_prop_pooled[complete.cases(dat_prop_pooled[, ..X_property])]
dat_prop_pooled[, mean(is.na(path))]
dat_prop_pooled[, region := fdroplevels(region)]
dat_prop_pooled[, uniqueN(path)]
dat_prop_pooled[, .N, path]

dat_viol_pooled <- victims_pooled[incident.crime_class == "Violent"]
dat_viol_pooled <- dat_viol_pooled[complete.cases(dat_viol_pooled[, ..X_violent])]
dat_viol_pooled[, mean(is.na(path_viol))]
dat_viol_pooled[, region := fdroplevels(region)]
dat_viol_pooled[, uniqueN(path_viol)]
dat_viol_pooled[, .N, path_viol]

fml_prop_brm_multi <- as.formula(g("path | weights(weight) ~ {paste(c(X_property), collapse = '+')} + year + (1 | region)"))
fml_viol_brm_multi <- as.formula(g("path_viol | weights(weight) ~ {paste(c(X_violent[X_violent != 'incident.went_to_hospital']), collapse = '+')} + year + (1 | region)"))

prop_priors <- c(
  prior(normal(0, 3), class = "b", dpar = "muF0P0"),
  prior(normal(0, 3), class = "b", dpar = "muF0P1"),
  prior(normal(0, 3), class = "b", dpar = "muF1P0"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF0P0"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF0P1"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF1P0")
)

viol_priors <- c(
  prior(normal(0, 3), class = "b", dpar = "muF0P0H0"),
  prior(normal(0, 3), class = "b", dpar = "muF0P1Hx"),
  prior(normal(0, 3), class = "b", dpar = "muF1P0H0"),
  prior(normal(0, 3), class = "b", dpar = "muFxP0H1"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF0P0H0"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF0P1Hx"),
  prior(normal(0, 3), class = "Intercept", dpar = "muF1P0H0"),
  prior(normal(0, 3), class = "Intercept", dpar = "muFxP0H1")
)

# ==========================================================
# PART 2: CATEGORICAL MULTILEVEL MODEL (Multinomial)
# ==========================================================
# Fits true multinomial choices seamlessly. Runs in minutes, not hours!
brm_multi_cyber <- brm(
  formula = fml_prop_brm_multi,
  data    = dat_cyber_pooled,
  family  = categorical(link = "logit"),
  prior = prop_priors,
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
brm_multi_prop <- brm(
  formula = fml_prop_brm_multi,
  data    = dat_prop_pooled,
  family  = categorical(link = "logit"),
  prior = prop_priors,
  cores   = 4, chains = 4,
  backend = "cmdstanr"
)
brm_multi_viol <- brm(
  formula = fml_viol_brm_multi,
  data    = dat_viol_pooled,
  family  = categorical(link = "logit"),
  prior = viol_priors,
  cores   = 4, chains = 4,
  backend = "cmdstanr",
  control = list(max_treedepth = 12)
)

save(list = grep("^brm_", ls(), value = T), file = "temp/brm_multi_models.RData")

# AMEs -------------------------------------------------------------------------
# ames_age <- avg_slopes(model_categorical_mcmc, variables = "ses.age_group", wts = "weight")
# ames_gender <- avg_slopes(model_categorical_mcmc, variables = "ses.male", wts = "weight")

load("temp/brm_multi_models.RData")
library(future)
library(future.apply)

# 1. Open up 24 parallel worker threads on your 32-core Ubuntu box
plan(multisession, workers = 24) 

# 2. CRITICAL ENGINES SWITCH: Force marginaleffects to parallelize 
options(marginaleffects_parallel = TRUE)

ames_cyber_ses <- avg_slopes(brm_multi_cyber,
                             variables = grep("^(ses|attitude)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200, 
                             vcov       = FALSE )
ames_prop_ses <- avg_slopes(brm_multi_prop,
                             variables = grep("^(ses|attitude)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,
                             vcov       = FALSE )
ames_viol_ses <- avg_slopes(brm_multi_viol,
                             variables = grep("^(ses|attitude)", X_violent, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200, 
                             vcov       = FALSE )
save(list = grep("ames_.*_ses", ls(), value = T), file = "ames_multi_ses.RData")

ames_cyber_incident <- avg_slopes(brm_multi_cyber,
                             variables = grep("^(damage|incident)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_prop_incident <- avg_slopes(brm_multi_prop,
                             variables = grep("^(damage|incident)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_viol_incident <- avg_slopes(brm_multi_viol,
                             variables = grep("^(damage|incident)", X_violent[X_violent != "incident.went_to_hospital"], value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )

save(list = grep("ames_.*_incident", ls(), value = T), file = "ames_multi_incident.RData")

ames_cyber_full <- avg_slopes(brm_multi_cyber,
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_prop_full <- avg_slopes(brm_multi_prop,
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_viol_full <- avg_slopes(brm_multi_viol,
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )

save(list = grep("ames_.*_full", ls(), value = T), file = "ames_multi_full.RData")


# Plotting AMEs ====================================================================

load("ames_multi_ses.RData")
load("ames_multi_incident.RData")
load("ames_multi_full.RData")


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

multi_ames_to_table(ames_cyber_full, "paper/tables/app_multi_cyber.tex")
multi_ames_to_table(ames_prop_full, "paper/tables/app_multi_prop.tex")
multi_ames_to_table(ames_viol_full, "paper/tables/app_multi_viol.tex")



setDT(ames_viol_full)
ames_cyber_full[, group := gsub("F", "N", group)]
ames_cyber_full[, term_contrast := paste0(term, stri_replace_first_regex(contrast, " -.*", ""))]
ames_cyber_full[term_contrast != "year2021", term_contrast := gsub("1$", "", term_contrast)]
ames_cyber_full[, label := coef_map[term_contrast]]
View(unique(ames_cyber_full[, .(term_contrast, label)]))

ames_cyber_full[, c("estimate", "conf.low", "conf.high") := lapply(.SD, function(x) round(x, 3)), .SDcols = c("estimate", "conf.low", "conf.high")]

ames_cyber_full[, CI := paste0("[", conf.low, ", ", conf.high, "]")]

ames_cyber_full[, AME := paste0(estimate, ifelse(conf.high < 0 | conf.low > 0, "$\\dagger$", ""))]

test_table <- ames_cyber_full |> dcast(label ~ group, value.var = c("AME", "CI"))

setcolorder(test_table, c("label", 
                          "AME_N0P0", "CI_N0P0",
                          "AME_N1P0", "CI_N1P0",
                          "AME_N0P1", "CI_N0P1",
                          "AME_N1P1", "CI_N1P1"))

kbl(test_table,
    format = "latex",
    escape = F,
    col.names = c("Term", rep(c("AME", "CI"), 4))) %>%
    add_header_above(c(" " = 1, "N0P0" = 2, "N1P0" = 2, "N0P1" = 2, "N1P1" = 2))



# 2. Assemble your data tables into a named list
# The list names will become the clean visual text on your plot labels automatically
model_outputs_nonviol_ses <- list(
  "Cybercrime"     = ames_cyber_ses,
  "Property" = ames_prop_ses
  # "Violent Crime"  = ames_viol_ses
)
model_outputs_nonviol_incident <- list(
  "Cybercrime"     = ames_cyber_incident,
  "Property" = ames_prop_incident
  # "Violent Crime"  = ames_viol_ses
)

# 3. Explicitly define your desired vertical row sequence
ses_var_hierarchy <- c(
  # "Male",
  # "Age Group",
  # "Household Size",
  # "Education",
  # "Employed",
  # "Household Income",
  # "Settlement Type"
                       "ses.male",
                       "ses.age_group",
                       "ses.settlement_type",
                       "ses.household_size_f",
                       "ses.household_income",
                       "ses.heigher_edu",
                       "ses.employed",
                       "attitude.familiar_police",
                       "attitude.has_crime_record"

)

# 4. Execute the backend processing pipeline functions sequentially
cleaned_plot_dt <- prep_profile_data(model_outputs_nonviol_ses, ses_var_hierarchy)
cleaned_plot_dt <- prep_profile_data(model_outputs_nonviol_incident, c("damage.rub_thnd_binned", "incident.repeated", "incident.self_blame"))
# shading_bands   <- calculate_zebra_shading(cleaned_plot_dt)

# 5. Output the high-contrast chart directly to your manuscript directory
plot_multinomial_profiles(
  plot_data   = cleaned_plot_dt, 
  # shading_data = shading_bands,
  output_path = "paper/figures/multinomial_ses_profiles_nonviol.pdf"
)
plot_multinomial_profiles(
  plot_data   = cleaned_plot_dt, 
  # shading_data = shading_bands,
  output_path = "paper/figures/multinomial_incident_profiles_nonviol.pdf",
  width = 10, height = 5
  
)


# 4. Execute the backend processing pipeline functions sequentially
cleaned_plot_dt <- prep_profile_data(list("Violent" = ames_viol_ses), ses_var_hierarchy)
# shading_bands   <- calculate_zebra_shading(cleaned_plot_dt)

cleaned_plot_dt[, group := fct_relevel(group, c("F0P0H0", "F1P0H0", "FxP0H1", "F0P1Hx", "F1P1Hx"))]
# 5. Output the high-contrast chart directly to your manuscript directory
plot_multinomial_profiles(
  plot_data   = cleaned_plot_dt, 
  # shading_data = shading_bands,
  output_path = "paper/figures/multinomial_ses_profiles_viol.pdf",

)

incident_var_hierarchy <- c(
                             "damage.physical",                    
 "damage.rub_thnd_binned"       ,         
 "incident.offender_had_weapon"    ,      
 "incident.offender_relationship_grouped",
 "incident.repeated"       ,              
 "incident.resisted"    ,                 
 "incident.self_blame"  ,                 
 "incident.sexualized"  ,                 
 "incident.witness"  
)
# 4. Execute the backend processing pipeline functions sequentially
cleaned_plot_dt <- prep_profile_data(list("Violent" = ames_viol_incident), incident_var_hierarchy)
# shading_bands   <- calculate_zebra_shading(cleaned_plot_dt)

cleaned_plot_dt[, group := fct_relevel(group, c("F0P0H0", "F1P0H0", "FxP0H1", "F0P1Hx", "F1P1Hx"))]
# 5. Output the high-contrast chart directly to your manuscript directory
plot_multinomial_profiles(
  plot_data   = cleaned_plot_dt, 
  # shading_data = shading_bands,
  output_path = "paper/figures/multinomial_incident_profiles_viol.pdf",

)














# 1. Bind your three model data tables together with explicit title strings
setDT(ames_cyber_ses)[, crime_type := "Cybercrime"]
setDT(ames_prop_ses)[,  crime_type := "Property"]
setDT(ames_viol_ses)[,  crime_type := "Violent"]
# plot_data_ses <- rbindlist(list(ames_cyber_ses, ames_prop_ses))#, ames_viol_ses))
plot_data_ses <- ames_viol_ses

# 2. Text Cleansing & Label Standardization
plot_data_ses[, var_clean := gsub("^ses\\.", "", term)]
plot_data_ses[, var_clean := gsub("_", " ", var_clean)]
plot_data_ses[, var_clean := tools::toTitleCase(var_clean)]

# Specific Title-Case manual overrides to ensure shorthand stays clean
plot_data_ses[var_clean == "Higher Edu", var_clean := "Education"]
plot_data_ses[var_clean == "Household Size F", var_clean := "Household Size"]

plot_data_ses[, contrast_clean := gsub(" - ", " vs. ", contrast, fixed = TRUE)]
plot_data_ses[, contrast_clean := gsub("1 vs. 0", "Yes vs. No", contrast_clean, fixed = TRUE)]
plot_data_ses[, contrast_clean := gsub("3--4", "3–4", contrast_clean, fixed = TRUE)]

# Create unified text label
plot_data_ses[, display_label := paste0(var_clean, " (", contrast_clean, ")")]

# ------------------------------------------------------------------------------
# CRITICAL FIX: Enforce Strict Hierarchical Variable & Ascending Level Ordering
# ------------------------------------------------------------------------------
# Define your desired top-to-bottom variable order
target_var_order <- c("Male", "Age Group", "Household Size", "Education", "Employed", "Household Income", "Settlement Type")

# Create a mapping table of unique labels to sort levels ascendingly
sorting_dt <- unique(plot_data_ses[, .(display_label, var_clean, contrast_clean)])

# Assign numeric ordering values to the variables based on your target list
sorting_dt[, var_rank := match(var_clean, target_var_order)]

# Explicitly sort contrast levels ascendingly within each variable block
# This handles alphanumeric level steps cleanly
setorder(sorting_dt, var_rank, contrast_clean)

# Invert the final vector so it reads top-to-bottom on the ggplot coordinate canvas
final_axis_levels <- rev(sorting_dt$display_label)

# Apply the strict structural factor sequence back onto your master plotting matrix
plot_data_ses[, display_label := factor(display_label, levels = final_axis_levels)]
# ------------------------------------------------------------------------------

plot_data_ses[, crime_type := factor(crime_type, levels = c("Cybercrime", "Property", "Violent"))]

plot_data_ses[, path_label := factor(group, 
                                 levels = c("F0P0", "F1P0", "F0P1", "F1P1"),
                                 labels = c("No Help Seeking (F0P0)", 
                                            "Informal Network Only (F1P0)", 
                                            "Formal Police Only (F0P1)", 
                                            "Joint Strategy (F1P1)"))]

plot_data_ses[, excludes_zero := (conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0)]
# ------------------------------------------------------------------------------
# THE ABSOLUTE ALIGNMENT REGIME
# Define a strict, unified position engine
# ------------------------------------------------------------------------------
strict_dodge <- position_dodge(width = 0.6)
# ------------------------------------------------------------------------------

# 3. Generate the Flawlessly Synchronized Profile Plot Matrix
pl_multinomial_profiles <- 
  ggplot(plot_data_ses, aes(x = estimate, y = display_label, 
                        color = crime_type, alpha = excludes_zero,
                        group = crime_type)) + 
  
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  
  # Horizontal Bayesian Credible Interval Bounds (The Whiskers)
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), # Explicitly isolate alpha via local aes
                 height = 0.3, 
                 position = strict_dodge, 
                 linewidth = 0.5) +
  
  # Central Point Posterior Estimates (The Dots)
  geom_point(position = strict_dodge, 
             size = 2.2) +
  
  # Facet setup split horizontally by help-seeking strategy pathways
  facet_wrap(~group, nrow = 1) +
  
  # Controlling highlight scales cleanly
  scale_alpha_manual(values = c("FALSE" = 0.20, "TRUE" = 1.00), guide = "none") +

  # guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  
  # Formatting configurations for high-quality LaTeX integration
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey95"), 
    panel.grid.major.x = element_line(color = "grey90"),
    strip.background = element_rect(fill = "grey95"),
    strip.text       = element_text(face = "bold", size = 10),
    axis.text.y      = element_text(size = 9, face = "plain", color = "black"),
    axis.text.x      = element_text(size = 9),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 9),
    legend.box       = "horizontal"
  ) +
  
  # Publication labels
  labs(
    x = "Average Marginal Effect with 95% Bayesian Credible Interval\n(Absolute Probability Shift)",
    y = "Socio-Economic Predictors (Contrast Levels)"
  )

print(pl_multinomial_profiles)
ggsave("paper/figures/multinomial_ses_profiles.pdf", plot = pl_multinomial_profiles, width = 10, height = 7.5, device = "pdf")



ames_cyber_incident <- avg_slopes(brm_multi_cyber,
                             variables = grep("^(damage|incident)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_prop_incident <- avg_slopes(brm_multi_prop,
                             variables = grep("^(damage|incident)", X_property, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )
ames_viol_incident <- avg_slopes(brm_multi_viol,
                             variables = grep("^(damage|incident)", X_violent, value = T),
                             wts = "weight",
                             re_formula = NULL,
                             ndraws     = 200,   # Subsamples 200 random draws from the massive posterior matrix
                             vcov       = FALSE )

save(list = grep("ames_.*_incident", ls(), value = T), file = "ames_multi_incident.RData")

# 1. Bind your three model data tables together with explicit title strings
setDT(ames_cyber_incident)[, crime_type := "Cybercrime"]
setDT(ames_prop_incident)[,  crime_type := "Property"]
setDT(ames_viol_incident)[,  crime_type := "Violent"]
plot_data_incident <- rbindlist(list(ames_cyber_incident, ames_prop_incident, ames_viol_incident))

# 2. Text Cleansing & Label Standardization
plot_data_incident[, var_clean := gsub("^incident\\.", "", term)]
plot_data_incident[, var_clean := gsub("_", " ", var_clean)]
plot_data_incident[, var_clean := tools::toTitleCase(var_clean)]

# Specific Title-Case manual overrides to ensure shorthand stays clean
plot_data_incident[var_clean == "Higher Edu", var_clean := "Education"]
plot_data_incident[var_clean == "Household Size F", var_clean := "Household Size"]

plot_data_incident[, contrast_clean := gsub(" - ", " vs. ", contrast, fixed = TRUE)]
plot_data_incident[, contrast_clean := gsub("1 vs. 0", "Yes vs. No", contrast_clean, fixed = TRUE)]
plot_data_incident[, contrast_clean := gsub("3--4", "3–4", contrast_clean, fixed = TRUE)]

# Create unified text label
plot_data_incident[, display_label := paste0(var_clean, " (", contrast_clean, ")")]

# ------------------------------------------------------------------------------
# CRITICAL FIX: Enforce Strict Hierarchical Variable & Ascending Level Ordering
# ------------------------------------------------------------------------------
# Define your desired top-to-bottom variable order
target_var_order <- c("Male", "Age Group", "Household Size", "Education", "Employed", "Household Income", "Settlement Type")

# Create a mapping table of unique labels to sort levels ascendingly
sorting_dt <- unique(plot_data_incident[, .(display_label, var_clean, contrast_clean)])

# Assign numeric ordering values to the variables based on your target list
sorting_dt[, var_rank := match(var_clean, target_var_order)]

# Explicitly sort contrast levels ascendingly within each variable block
# This handles alphanumeric level steps cleanly
setorder(sorting_dt, var_rank, contrast_clean)

# Invert the final vector so it reads top-to-bottom on the ggplot coordinate canvas
final_axis_levels <- rev(sorting_dt$display_label)

# Apply the strict structural factor sequence back onto your master plotting matrix
plot_data_incident[, display_label := factor(display_label, levels = final_axis_levels)]
# ------------------------------------------------------------------------------

plot_data_incident[, crime_type := factor(crime_type, levels = c("Cybercrime", "Property", "Violent"))]

plot_data_incident[, path_label := factor(group, 
                                 levels = c("F0P0", "F1P0", "F0P1", "F1P1"),
                                 labels = c("No Help Seeking (F0P0)", 
                                            "Informal Network Only (F1P0)", 
                                            "Formal Police Only (F0P1)", 
                                            "Joint Strategy (F1P1)"))]

plot_data_incident[, excludes_zero := (conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0)]
# ------------------------------------------------------------------------------
# THE ABSOLUTE ALIGNMENT REGIME
# Define a strict, unified position engine
# ------------------------------------------------------------------------------
strict_dodge <- position_dodge(width = 0.6)
# ------------------------------------------------------------------------------

# 3. Generate the Flawlessly Synchronized Profile Plot Matrix
pl_multinomial_profiles <- 
  ggplot(plot_data_incident, aes(x = estimate, y = display_label, 
                        color = crime_type, alpha = excludes_zero,
                        group = crime_type)) + 
  
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  
  # Horizontal Bayesian Credible Interval Bounds (The Whiskers)
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), # Explicitly isolate alpha via local aes
                 height = 0.3, 
                 position = strict_dodge, 
                 linewidth = 0.5) +
  
  # Central Point Posterior Estimates (The Dots)
  geom_point(position = strict_dodge, 
             size = 2.2) +
  
  # Facet setup split horizontally by help-seeking strategy pathways
  facet_wrap(~path_label, nrow = 1) +
  
  # Controlling highlight scales cleanly
  scale_alpha_manual(values = c("FALSE" = 0.20, "TRUE" = 1.00), guide = "none") +

  # guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  
  # Formatting configurations for high-quality LaTeX integration
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey95"), 
    panel.grid.major.x = element_line(color = "grey90"),
    strip.background = element_rect(fill = "grey95"),
    strip.text       = element_text(face = "bold", size = 10),
    axis.text.y      = element_text(size = 9, face = "plain", color = "black"),
    axis.text.x      = element_text(size = 9),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 9),
    legend.box       = "horizontal"
  ) +
  
  # Publication labels
  labs(
    x = "Average Marginal Effect with 95% Bayesian Credible Interval\n(Absolute Probability Shift)",
    y = "Socio-Economic Predictors (Contrast Levels)"
  )

# 1. Source background plotting engine utilities

# 2. Assemble your data tables into a named list
model_outputs <- list(
  "Cybercrime"     = ames_cyber_ses,
  "Property" = ames_prop_ses,
  "Violent"  = ames_viol_ses
)

# 3. Explicitly define your desired vertical row sequence
ses_var_hierarchy <- c(
  "Male", 
  "Age Group", 
  "Household Size", 
  "Education", 
  "Employed", 
  "Household Income", 
  "Settlement Type"
)

# 4. Execute the backend processing pipeline functions sequentially
cleaned_plot_dt <- prep_profile_data(model_outputs, ses_var_hierarchy)

# 5. Output the high-contrast chart directly to your manuscript directory
plot_multinomial_profiles(
  plot_data   = cleaned_plot_dt, 
  output_path = "paper/figures/multinomial_ses_profiles.pdf"
)
