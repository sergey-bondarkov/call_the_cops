library(data.table)
library(ggplot2)
library(forcats)
library(fixest)
library(modelsummary)
library(marginaleffects)
library(kableExtra) 
g <- glue::glue

# ================================================================================
# Load and prepare data

victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_pooled[, .N, incident.crime_type]
victims_2024 <- readRDS("data/processed/victims_2024.Rds")
victims_2024[, .N, incident.crime_type]
victims_2024[, .N, .(incident.crime_type, incident.last_12_months)] |> dcast(...~incident.last_12_months)
# Robbery and trad fraud are too rare

# Lump infrequent regions to prevent loss of observations due to a single outcome in a region
# victims_2024[, region_lumped := fct_lump_min(region, 20)]
# victims_2024[, n_cases_region := .N, region]
# victims_2024[, paste0("area_fe_", seq(10, 100, 10)) := lapply(seq(10, 100, 10), function(x) fifelse(n_cases_region >= x,
                                  # as.character(region),
                                  # as.character(federal_district)))]
# victims_2024[, .N, area_fe_50][order(-N)]
# victims_2024[, .N, region_lumped][order(-N)]
# victims_2024[, .N, federal_district]
# victims_2024[, social.communication_relatives := fct_collapse(social.communication_relatives_freq,
                                                                # No = "Never",
                                                                # other_level = "Yes")]


# Make subsets
victims_pooled.theft <- victims_pooled[incident.crime_type == "Theft"]
victims_pooled.cyber <- victims_pooled[incident.crime_type == "Cybercrime"]
victims_pooled.fraud <- victims_pooled[incident.crime_type == "Fraud"]
victims_pooled.all_property <- victims_pooled[incident.crime_type %in% c("Cybercrime", "Theft", "Fraud")]
victims_pooled.assault <- victims_pooled[incident.crime_type == "Assault"]
victims_pooled.robbery <- victims_pooled[incident.crime_type == "Robbery"]
victims_pooled.all_violent <- victims_pooled[incident.crime_type %in% c("Assault", "Robbery")]

victims_2024.theft <- victims_2024[incident.crime_type == "Theft"]
victims_2024.cyber <- victims_2024[incident.crime_type == "Cybercrime"]
victims_2024.all_property <- victims_2024[incident.crime_type %in% c("Cybercrime", "Theft", "Fraud")]
victims_2024.assault <- victims_2024[incident.crime_type == "Assault"]
victims_2024.all_violent <- victims_2024[incident.crime_type %in% c("Assault", "Robbery")]

# ================================================================================
# Two-stage residuals inclusion (2SRI) estimation 

# List covariates for property and violent crimes
X_property <- c("social.told_someone",
                "social.self_blame",
                "social.has_partner",
                "damage.rub_thnd_binned",
                # "damage.emotional",
                # "attitude.trusts_police",
                "attitude.knows_some_police",
                "attitude.has_crime_record",
                "ses.settlement_type",
                "ses.gender",
                "ses.age_group",
                "ses.income",
                # "ses.household_size",
                "ses.higher_edu",
                "ses.employed")
X_violence <- c(X_property, 
                "damage.physical",
                "incident.witness",
                "incident.resisted",
                # "incident.offender_is_male",
                "incident.offender_had_weapon",
                # "incident.threats",
                "social.offender_relationship_grouped"
)
IV <- c("social.communication_relatives_freq"#,
        # "social.communication_relatives_way"
)

# FE <- "region_lumped"
# FE <- "area_fe_50"
FE <- c("federal_district", "year")
glm_family <- binomial(link = "logit")


# Regression formulas

# Basic logit
fml_property <- as.formula(g("reported.police ~ {paste(X_property, collapse = '+')} | {paste(FE, collapse = '+')} "))
fml_violence <- as.formula(g("reported.police ~ {paste(X_violence, collapse = '+')} | {paste(FE, collapse = '+')} "))
fml_property_2024 <- as.formula(g("reported.police ~ {paste(X_property, collapse = '+')} + attitude.trusts_police + damage.emotional | {paste(FE[1], collapse = '+')} "))
fml_violence_2024 <- as.formula(g("reported.police ~ {paste(X_violence, collapse = '+')} + attitude.trusts_police + damage.emotional | {paste(FE[1], collapse = '+')} "))


# 2SLS
fml_property.2SLS <-
    as.formula(g("reported.police ~ {paste(grepv('told_someone', X_property, invert = T), collapse = '+')} | {FE} | social.told_someone ~ {IV}"))
fml_cyber.2SLS <-
    as.formula(g("reported.police ~ {paste(c('reported.bank', grepv('told_someone|attitude.has|ses', X_property, invert = T)), collapse = '+')} | {FE} | social.told_someone ~ social.communication_relatives_freq"))
fml_violence.2SLS <-
    as.formula(g("reported.police ~ {paste(grepv('told_someone', X_violence, invert = T), collapse = '+')} | {FE} | social.told_someone ~ {IV}"))
## First stage
fml_property.OLS <- as.formula(g("social.told_someone ~ social.communication_relatives_freq + {paste(X_property[-1], collapse = '+')} | {FE} "))
fml_violence.OLS <- as.formula(g("social.told_someone ~ social.communication_relatives_freq + {paste(X_violence[-1], collapse = '+')} | {FE} "))

# 2SRI
fml_property.2SRI_1st_stage <-
    as.formula(g("social.told_someone ~ {paste(grepv('told_someone', X_property, invert = T), collapse = '+')} + {paste(IV, collapse = '+')} | {FE}"))
fml_property.2SRI_2nd_stage <-
    as.formula(g("reported.police ~ {paste(X_property, collapse = '+')} + social.told_someone.1st_stage_resids | {FE}"))
fml_violence.2SRI_1st_stage <-
    as.formula(g("social.told_someone ~ {paste(grepv('told_someone', X_property, invert = T), collapse = '+')} + {paste(IV, collapse = '+')} | {FE}"))
fml_violence.2SRI_2nd_stage <-
    as.formula(g("reported.police ~ {paste(X_violence, collapse = '+')} + social.told_someone.1st_stage_resids | {FE}"))

# Term names dict
coef_map <- 
    c( "social.communication_relatives_freqRarely" = "Contact relatives freq.: Rarely",
      "social.communication_relatives_freqOften" = "Contact relatives freq.: Often",
      "social.told_someone" = "Told someone",
      "fit_social.told_someone" = "Told someone",
      "damage.rub_thnd_binned(1,10]" = "Financial: (1, 10] thnd rub.",
      "damage.rub_thnd_binned(10,50]" = "Financial: (10, 50] thnd rub.",
      "damage.rub_thnd_binned(50,Inf]" = "Financial: (50, Inf] thnd rub.",
      "damage.physicalLight" = "Physical: Light",
      "damage.physicalHeavy" = "Physical: Heavy",
      "damage.emotionalMedium" = "Emotional: Medium",
      "damage.emotionalHeavy" = "Emotional: Heavy",
      "incident.witnessYes" = "Incident: Witness",
      "incident.resistedYes" = "Incident: Victim resisted",
      "incident.offender_had_weaponYes" = "Incident: Weapon",
      "social.self_blame" = "Self-blame",
      "social.has_partnerYes" = "Has partner",
      "social.offender_relationship_groupedAcquaintance or colleague" = "Off. rel.: Acquaintance or colleague",
      "social.offender_relationship_groupedNeighbour" = "Off. rel.: Neighbour",
      "social.offender_relationship_groupedSpouse, partner, relative, friend" = "Off. rel.: Spouse, partner, relative, friend",
      "ses.genderMale" = "Gender: Male",
      "ses.incomeEnough to buy furniture/electonics" = "Income: Can buy furniture/electonics", 
      "ses.incomeEnough to buy a car, or higher" = "Income: Can buy a car, or higher",
      "ses.higher_eduYes" = "Highly educated",
      "ses.settlement_typeSmall town" = "Settlement: Small town",
      "ses.settlement_typeVillage" = "Settlement: Village",
      "ses.settlement_typeUnknown" = "Settlement: Unknown",
      "attitude.trusts_police" = "Attitude: Victim trusts the police",
      "attitude.knows_some_police" = "Attitude: Victim knows a police",
      "attitude.has_crime_record" = "Attitude: Victim has crime record")

mdls_logit <- 
    mapply(
           list(
                Theft = victims_pooled.theft, 
                Fraud = victims_pooled.fraud,
                Cybercrime = victims_pooled.cyber, 
                `All property` = victims_pooled.all_property,
                Assault = victims_pooled.assault,
                `All violent` = victims_pooled.all_violent),
           list(
                fml_property, fml_property, fml_property, fml_property,
                fml_violence, fml_violence),
           USE.NAMES = T, SIMPLIFY = F,
           FUN = function(data_subset, fml) {
               feglm(fml, family = glm_family, cluster = FE, weights = ~weight, data = data_subset)
           }
    )
etable(mdls_logit)
modelsummary(mdls_logit, stars = T, statistic = NULL, coef_map = coef_map, gof_omit = "AIC|BIC",
output = "gt") |> as_latex() |> as.character() |> cat()

predictions_dt <- plot_predictions(mdls_logit[["All violent"]], by = c("damage.physical", "social.told_someone", "social.offender_relationship_grouped"), draw = F)

diffs_dt <- as.data.table(
                          avg_predictions(mdls_logit$`All violent`, 
                                          by = c("damage.physical", 
                                                 "social.told_someone", 
                                                 "social.offender_relationship_grouped"), 
                                          wts = "weight",
                                          hypothesis = difference ~ reference | social.offender_relationship_grouped + damage.physical))

diffs_dt[, label := paste0("Diff: ", format(abs(estimate), digits = 2, nsmall = 2), 
                           fcase(
                                 p.value <= .001, "***",
                                 p.value <= .01, "**",
                                 p.value <= .05, "*",
                                 p.value <= .1, "+",
                                 default = ""
                                 ))]
diffs_dt[is.na(std.error), label := NA]

pl <- ggplot(predictions_dt,
             aes(x = fct_relevel(damage.physical, c("No", "Light", "Heavy")), 
                 y = estimate, 
                 col = factor(social.told_someone, c(0, 1), c("No", "Yes")))) +
        geom_point(position = position_nudge(ifelse(predictions_dt$social.told_someone == 1, .1, -.1))) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      position = position_nudge(ifelse(predictions_dt$social.told_someone == 1, .1, -.1)),
                      width = .1) +
        geom_text(data = diffs_dt, inherit.aes = F, 
                  aes(x = damage.physical, y = -.15, label = label),
                  size = 3) +
        facet_wrap(~fct_relevel(social.offender_relationship_grouped, c("Stranger", "Acquaintance or colleague" , "Neighbour", "Spouse, partner, relative, friend")),
                   labeller = as_labeller(function(x) paste("Offender: ", x))) +
        scale_y_continuous(breaks = seq(0, 1, .2),
                           limits = c(-.3, 1.1), 
                           expand = c(0, 0)) +
        scale_color_discrete(breaks = c(0, 1), labels = c("No", "Yes")) +
        labs(x = "Physical damage", 
             y = "Avg. predicted probability", 
             col = "Victim talked with someone about the incident",
             caption = "+ p < 0.1$, * p < 0.05$, ** p < 0.01$, *** p < 0.001") +
        theme_bw() +
        theme(legend.position = "bottom", 
              panel.grid.minor = element_blank(),
              plot.caption = element_text(hjust = 0))

ggsave("output/avg_predicted_probs_grouped.pdf", width = 9, height = 6, scale = .75)


ggplot(pl_data, aes(x = fct_relevel(damage.physical, c("No", "Light", "Heavy")),
                    y = estimate, col = factor(social.told_someone, c(0, 1), c("No", "Yes")))) +
        geom_point() +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .2) +
        facet_wrap(~fct_relevel(social.offender_relationship_grouped, c("Stranger", "Acquaintance or colleague" , "Neighbour", "Spouse, partner, relative, friend"))) +
        theme_minimal()
        # lemon::facet_rep_wrap(~social.offender_relationship_grouped)
        # lemon::facet_rep_wrap(~fct_relevel(social.offender_relationship_grouped, c("Stranger", "Acquaintance or colleague" , "Neighbour", "Spouse, partner, relative, friend")))

mdls_logit_2024 <- 
    mapply(
           list(
                Theft = victims_2024.theft, 
                # Fraud = victims_2024.fraud,
                Cybercrime = victims_2024.cyber, 
                `All property` = victims_2024.all_property,
                Assault = victims_2024.assault,
                `All violent` = victims_2024.all_violent),
           list(
                fml_property_2024, fml_property_2024, fml_property_2024, 
                fml_violence_2024, fml_violence_2024),
           USE.NAMES = T, SIMPLIFY = F,
           FUN = function(data_subset, fml) {
               feglm(fml, family = glm_family, cluster = FE[1], weights = ~weight, data = data_subset)
           }
    )
etable(mdls_logit_2024)
modelsummary(mdls_logit_2024, stars = T, statistic = NULL, coef_map = coef_map,
output = "gt") |> as_latex() |> as.character() |> cat()



feglm(fml_property, cluster = FE, weights = ~weight, data = victims_pooled.theft)




victims_2024[is.na(damage.physical), damage.physical := "No"]
mdls_all <- feglm(as.formula(g("reported.police ~ {paste(X_property, collapse = '+')} + incident.crime_type + damage.physical | {FE}")), 

                             data = victims_2024[social.offender_relationship_grouped %in% c("Spouse, partner, relative, friend", "Acquaintance or colleague")],
                             weights = ~weight)
etable(mdls_all)
summary(mdls_all)

victims_pooled[is.na(damage.physical), damage.physical := "No"]
mdls_all <- feglm(as.formula(g("reported.police ~ {paste(X_property, collapse = '+')} | {FE} + year")), 

                             data = victims_pooled.all_property[social.offender_relationship_grouped %in% c("Spouse, partner, relative, friend", "Acquaintance or colleague"
)],
                             weights = ~weight)
etable(mdls_all)
summary(mdls_all)

# victims_pooled[is.na(damage.physical), damage.physical := "No"]
victims_pooled[, ses.household_size := fct_collapse(as.factor(ses.household_size),
                                                    one = "1",
                                                    two = "2",
                                                    `3+` = as.character(3:8))]
mdls_all <- feglm(as.formula(g("reported.police ~ {paste(X_violence, collapse = '+')} | {FE} + year ")), 

                             data = victims_pooled.all_violent[social.offender_relationship_grouped %in% c("Spouse, partner, relative, friend", "Acquaintance or colleague"
                                                                                                           )],
                             weights = ~weight)
etable(mdls_all)
summary(mdls_all)

sapply(mdls_logit, r2)

victims_2024[social.offender_relationship_grouped %in% c("Spouse, partner, relative, friend", "Acquaintance or colleague")]

victims_2024[, .N, social.offender_relationship_grouped]

modelsummary(mdls_logit, 
             exponentiate = T, 
             stars = T,
             # estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL#, 
             # coef_map = coef_map
)#,
             # gof_map = c("nobs", "r.squared"),
             # output = "presentations/12nov2025/tables/logit.tex")

mdls_OLS <- 
    mapply(
           list(Theft = victims_2024.theft, Cybercrime = victims_2024.cyber, `All property` = victims_2024.all_property, 
                Assault = victims_2024.assault, `All violent` = victims_2024.all_violent),
    list(fml_property.OLS, fml_property.OLS, fml_property.OLS, fml_violence.OLS, fml_violence.OLS),
    USE.NAMES = T, SIMPLIFY = F,
    FUN = function(data_subset, fml) {
        feols(fml, cluster = FE, weights = ~weight, data = data_subset)
    }
    )
modelsummary(mdls_OLS, 
             # stars = T,
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL, 
             coef_map = coef_map)#,
             # gof_map = c("nobs", "r.squared"),
             # output = "presentations/12nov2025/tables/logit.tex")

mdls_2SLS <- 
    mapply(
           list(Theft = victims_2024.theft, Cybercrime = victims_2024.cyber, `All property` = victims_2024.all_property, 
                Assault = victims_2024.assault, `All violent` = victims_2024.all_violent),
    list(fml_property.2SLS, fml_property.2SLS, fml_property.2SLS, fml_violence.2SLS, fml_violence.2SLS),
    USE.NAMES = T, SIMPLIFY = F,
    FUN = function(data_subset, fml) {
        feols(fml, cluster = FE, weights = ~weight, data = data_subset)
    }
    )
modelsummary(mdls_2SLS, 
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL#, 
             # coef_map = c("fit_social.told_someone" = "Told someone"),
             # gof_map = c("nobs", "r.squared"),
             # output = "output/tables/2sls_2nd.tex"
)
etable(mdls_2SLS)
summary(mdls_2SLS[["All violent"]])

sapply(mdls_2SLS, fitstat, type = c("ivf", "ivwald", "cd", "kpr", "wh"))
sapply(mdls_2SLS, fitstat, type = c("ivf", "ivwald", "cd"), simplify = F)






mdl_all_violent.2SLS <- feols(fml_violence.2SLS, data = victims_2024.all_violent)
summary(mdl_all_violent.2SLS, stage = 2)
etable(mdl_all_violent.2SLS)







# ================================================================================

predictions_dt <- plot_predictions(mdls_logit$`All violent`, 
                                   by = c("damage.physical", 
                                          "social.told_someone", 
                                          "social.offender_relationship_grouped"), 
                                   wts = "weight",
                                   draw = F)
diffs_dt <- as.data.table(
                          avg_predictions(mdls_logit$`All violent`, 
                                          by = c("damage.physical", 
                                                 "social.told_someone", 
                                                 "social.offender_relationship_grouped"), 
                                          wts = "weight",
                                          hypothesis = difference ~ reference | social.offender_relationship_grouped + damage.physical))

diffs_dt[, label := paste0("Diff: ", format(abs(estimate), digits = 2, nsmall = 2), 
                           fcase(
                                 p.value <= .001, "***",
                                 p.value <= .01, "**",
                                 p.value <= .05, "*",
                                 p.value <= .1, "+",
                                 default = ""
                                 ))]
diffs_dt[is.na(std.error), label := NA]

pl <- ggplot(predictions_dt,
             aes(x = damage.physical, 
                 y = estimate, 
                 col = as.factor(social.told_someone))) +
        geom_point(position = position_nudge(ifelse(predictions_dt$social.told_someone == 1, .1, -.1))) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      position = position_nudge(ifelse(predictions_dt$social.told_someone == 1, .1, -.1)),
                      width = .1) +
        geom_text(data = diffs_dt, inherit.aes = F, 
                  aes(x = damage.physical, y = -.15, label = label),
                  size = 3) +
        lemon::facet_rep_wrap(~social.offender_relationship_grouped,
                              repeat.tick.labels = T,
                              labeller = as_labeller(function(x) paste("Offender: ", x))) +
        scale_y_continuous(breaks = seq(0, 1, .2),
                           limits = c(-.3, 1.1), 
                           expand = c(0, 0)) +
        scale_color_discrete(breaks = c(0, 1), labels = c("No", "Yes")) +
        labs(x = "Physical damage", 
             y = "Avg. predicted probability", 
             col = "Victim talked with someone about the incident",
             caption = "+ p < 0.1$, * p < 0.05$, ** p < 0.01$, *** p < 0.001") +
        theme_bw() +
        theme(legend.position = "bottom", 
              panel.grid.minor = element_blank(),
              plot.caption = element_text(hjust = 0))

ggsave("output/avg_predicted_probs_grouped.pdf", width = 9, height = 6, scale = .75)


# ================================================================================

predictions_dt <- plot_predictions(mdls_logit$`All violent`, 
                                   by = c("damage.physical", 
                                          "social.told_someone", 
                                          "social.offender_relationship_grouped"), 
                                   wts = "weight",
                                   draw = F)
diffs_dt <- as.data.table(
                          avg_predictions(mdls_logit$`All violent`, 
                                          by = c("damage.physical", 
                                                 "social.told_someone", 
                                                 "social.offender_relationship_grouped"), 
                                          wts = "weight",
                                          hypothesis = difference ~ reference | social.offender_relationship_grouped + damage.physical))

diffs_dt[, label := paste0("Diff: ", format(abs(estimate), digits = 2, nsmall = 2), 
                           fcase(
                                 p.value <= .001, "***",
                                 p.value <= .01, "**",
                                 p.value <= .05, "*",
                                 p.value <= .1, "+",
                                 default = ""
                                 ))]
diffs_dt[is.na(std.error), label := NA]

pl <- ggplot(predictions_dt,
             aes(x = damage.physical, 
                 y = estimate, 
                 col = as.factor(social.told_someone))) +
        geom_point(position = position_nudge(ifelse(predictions_dt$social.told_someone == 1, .1, -.1))) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      position = position_nudge(ifelse(predictions_dt$social.told_someone == 1, .1, -.1)),
                      width = .1) +
        geom_text(data = diffs_dt, inherit.aes = F, 
                  aes(x = damage.physical, y = -.15, label = label),
                  size = 3) +
        lemon::facet_rep_wrap(~social.offender_relationship_grouped,
                              repeat.tick.labels = T,
                              labeller = as_labeller(function(x) paste("Offender: ", x))) +
        scale_y_continuous(breaks = seq(0, 1, .2),
                           limits = c(-.3, 1.1), 
                           expand = c(0, 0)) +
        scale_color_discrete(breaks = c(0, 1), labels = c("No", "Yes")) +
        labs(x = "Physical damage", 
             y = "Avg. predicted probability", 
             col = "Victim talked with someone about the incident",
             caption = "+ p < 0.1$, * p < 0.05$, ** p < 0.01$, *** p < 0.001") +
        theme_bw() +
        theme(legend.position = "bottom", 
              panel.grid.minor = element_blank(),
              plot.caption = element_text(hjust = 0))

ggsave("output/avg_predicted_probs_grouped.pdf", width = 9, height = 6, scale = .75)




mdl_cyber.2SLS <- feols(fml_cyber.2SLS, data = victims_2024.cyber)
summary(mdl_cyber.2SLS)

mdl_cyber.2SLS$collin_vars

mdl_theft.2SLS <- feols(fml_property.2SLS, data = victims_2024.theft)
summary(mdl_theft.2SLS)
mdl_theft.2SLS$collin_vars

mdl_assault.2SLS <- feols(fml_violence.2SLS, data = victims_2024.assault)
summary(mdl_assault.2SLS)
mdl_assault.2SLS$collin.var

#
# mdls_2SLS.all_violent <-
    # feols(reported.police ~
          # social.self_blame +
          # social.has_partner +
          # social.offender_relationship +
          # incident.witness +
          # incident.resisted +
          # incident.offender_had_weapon +
          # damage.rub_thnd_binned +
          # damage.physical +
          # # damage.emotional +
          # # attitude.police +
          # # attitude.has_crime_record +
          # ses.gender +
          # ses.age_group +
          # ses.income +
          # # ses.employed +
          # ses.settlement_type +
          # # ses.higher_edu +
          # mvsw(
               # # social.self_blame,
               # # social.has_partner,
               # # social.offender_relationship_grouped,
               # damage.emotional,
               # # incident.witness,
               # # incident.resisted,
               # # incident.offender_had_weapon,
               # attitude.police,
               # attitude.has_crime_record,
               # ses.employed,
               # ses.higher_edu
          # )
          # # | sw(area_fe_20, area_fe_30, region_lumped)
          # | sw(area_fe_50, region_lumped)
          # |
              # social.told_someone ~ social.communication_relatives_freq,
          # data = victims_2024.assault[ses.settlement_type != "Unknown"],
          # # data = victims_2024.all_violent[social.offender_relationship_grouped != "Neighbour"],
      # weights = ~weight)
# summary(mdls_2SLS.all_violent, stage = 2)

best_model <- which.min(sapply(mdls_2SLS.all_violent, function(m) coeftable(m)[1,4]))
summary(mdls_2SLS.all_violent[[best_model]], stage = 2)
modelsummary(mdls_2SLS.all_violent, stars = T, statistic = NULL, vcov = "")
length(mdls_2SLS.all_violent)
summary(last(mdls_2SLS.all_violent), stage = 1)
last(mdls_2SLS.all_violent)$coeftable
coeftable(last(mdls_2SLS.all_violent))[1,4]
etable(mdls_2SLS.all_violent)
lapply(mdls_2SLS.assault, function(m) )




## 1st stage: estimate models for telling friends or family about victimization
mdls_1st_stage.property <-
    sapply(list(theft = victims_2024.theft,
                cyber = victims_2024.cyber,
                all_property = victims_2024.all_property),
           USE.NAMES = T, 
           simplify = F,
           FUN = function(data_subset) {
               feglm(fml_property.2SRI_1st_stage,
                     family = glm_family,
                     cluster = FE,
                     weights = ~weight,
                     data = data_subset
               )
           })
etable(mdls_1st_stage.property) # Instrument is weak
# modelsummary(mdls_1st_stage.property, exponentiate = F, statistic = NULL, stars = T)

mdls_1st_stage.violence <-
    sapply(list(assault = victims_2024.assault,
                all_violent = victims_2024.all_violent),
           USE.NAMES = T, 
           simplify = F,
           FUN = function(data_subset) {
               feglm(fml_violence.2SRI_1st_stage,
                     family = glm_family,
                     cluster = FE,
                     weights = ~weight,
                     data = data_subset
               )
           })
etable(mdls_1st_stage.violence) # Instrument is less weak
# modelsummary(mdls_1st_stage.violence, exponentiate = F, statistic = NULL, stars = T)

## 2nd stage: include residuals from the 1st stage alongside relations.told_someone and other covariates
mdls_2SRI.property <-
    mapply(list(theft = victims_2024.theft,
                cyber = victims_2024.cyber,
                all_property = victims_2024.all_property),
           mdls_1st_stage.property,
           USE.NAMES = T,
           SIMPLIFY = F,
           FUN = function(data_subset, mdl_1st_stage) {
               data_subset[unlist(mdl_1st_stage$obs_selection), 
                           social.told_someone.1st_stage_resids := mdl_1st_stage$re]
               feglm(fml_property.2SRI_2nd_stage,
                     family = glm_family,
                     cluster = FE,
                     weights = ~weight,
                     data = data_subset
               )

           })
etable(mdls_2SRI.property)
summary(mdls_2SRI.property$theft)
# modelsummary(mdls_2SRI.property, exponentiate = T, estimate = "{estimate} ({std.error}){stars}", statistic = NULL)

mdls_2SRI.violence <-
    mapply(list(assault = victims_2024.assault, 
                all_violent = victims_2024.all_violent),
           mdls_1st_stage.violence,
           USE.NAMES = T,
           SIMPLIFY = F,
           FUN = function(data_subset, mdl_1st_stage) {
               data_subset[unlist(mdl_1st_stage$obs_selection), 
                           social.told_someone.1st_stage_resids := mdl_1st_stage$re]
               feglm(fml_violence.2SRI_2nd_stage,
                     family = glm_family,
                     cluster = FE,
                     weights = ~weight,
                     data = data_subset
               )
           })
etable(mdls_2SRI.violence)
summary(mdls_2SRI.violence$assault)
modelsummary(mdls_2SRI.violence, exponentiate = T, stars = T, statistic = "{p.value} ({std.error})", shape = term ~ model + statistic)
# notes = "p-value: + 0.1, * 0.05, ** 0.01, *** 0.\cs001")
# In both models betas at social.told_someone are large and significant




# ================================================================================
# Plots
# library(patchwork)


predictions_dt <- plot_predictions(mdls_2SLS.violence$all_violent, 
                                   by = c("damage.physical", 
                                          "social.told_someone", 
                                          "social.offender_relationship_grouped"), 
                                   wts = "weight",
                                   draw = F)
diffs_dt <- as.data.table(
                          avg_predictions(mdls_2SLS.violence$all_violent, 
                                          by = c("damage.physical", 
                                                 "social.told_someone", 
                                                 "social.offender_relationship_grouped"), 
                                          wts = "weight",
                                          hypothesis = difference ~ reference | social.offender_relationship_grouped + damage.physical))

diffs_dt[, label := paste0("Diff: ", format(abs(estimate), digits = 2, nsmall = 2), 
                           fcase(
                                 p.value <= .001, "***",
                                 p.value <= .01, "**",
                                 p.value <= .05, "*",
                                 p.value <= .1, ".",
                                 default = ""
                                 ))]
diffs_dt[is.na(std.error), label := NA]

pl <- ggplot(predictions_dt,
             aes(x = damage.physical, 
                 y = estimate, 
                 col = as.factor(social.told_someone))) +
        geom_point(position = position_nudge(ifelse(predictions_dt$social.told_someone == 1, .1, -.1))) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      position = position_nudge(ifelse(predictions_dt$social.told_someone == 1, .1, -.1)),
                      width = .1) +
        geom_text(data = diffs_dt, inherit.aes = F, 
                  aes(x = damage.physical, y = -.4, label = label),
                  size = 3) +
        lemon::facet_rep_wrap(~social.offender_relationship_grouped,
                              repeat.tick.labels = T,
                              labeller = as_labeller(function(x) paste("Offender: ", x))) +
        scale_y_continuous(breaks = seq(0, 1, .2),
                           limits = c(-.6, 1.5), 
                           expand = c(0, 0)) +
        scale_color_discrete(breaks = c(0, 1), labels = c("No", "Yes")) +
        labs(x = "Physical damage", 
             y = "Avg. predicted probability", 
             col = "Victim talked with someone about the incident",
             caption = "Significance levels: *** 0.001, ** 0.01, * 0.05, . 0.1") +
        theme_bw() +
        theme(legend.position = "bottom", 
              panel.grid.minor = element_blank(),
              plot.caption = element_text(hjust = 0))
        ggsave("output/avg_predicted_probs.pdf", width = 9, height = 6)

        # makeFootnote <- function(footnoteText=
        # format(Sys.time(), "%d %b %Y"),
        # size = .8, color= "black") {
        # require(grid)
        # pushViewport(viewport())
        # grid.text(label= footnoteText ,
        # x = unit(10, "mm"),
        # y = unit(4, "mm"),
        # just=c("left", "bottom"),
        # gp=gpar(cex= size, col=color))
        # popViewport()
        # }
        #
        # pdf("output/test.pdf")
        # print(pl)
        # makeFootnote("Significance levels: *** 0.001, ** 0.01, * 0.05, . 0.1")
        # dev.off()


        plot_predictions(mdls_2SLS.violence$all_violent, by = c("social.offender_relationship_grouped","social.told_someone","damage.physical"), wts = "weight") + coord_flip()
