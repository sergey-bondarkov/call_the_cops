library(data.table)
library(ggplot2)
library(forcats)
library(fixest)
library(gt)
library(modelsummary)
library(dominanceanalysis)
# library(marginaleffects)
# library(logistf)
# library(kableExtra) 
g <- glue::glue
options(browser="/usr/bin/google-chrome")

# ================================================================================
# Load and prepare data

victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_pooled[, ses.age_group := fct_relevel(ses.age_group, "35-49")]
victims_pooled[, year := as.factor(year)]
victims_pooled[, year2 := paste0(year, fifelse(incident.last_12_months == 1, "precise", "approx"))]
victims_2024 <- readRDS("data/processed/victims_2024.Rds")
victims_2024[, ses.age_group := fct_relevel(ses.age_group, "35-49")]



# Make subsets
victims_pooled.theft <- victims_pooled[incident.crime_type == "Theft"]
victims_pooled.cyber <- victims_pooled[incident.crime_type == "Cybercrime"]
victims_pooled.fraud <- victims_pooled[incident.crime_type == "Fraud"]
victims_pooled.all_property <- victims_pooled[incident.crime_type2 == "Property"]
victims_pooled.assault <- victims_pooled[incident.crime_type == "Assault"]
# victims_pooled.robbery <- victims_pooled[incident.crime_type == "Burglary or robbery"]
victims_pooled.all_violent <- victims_pooled[incident.crime_type2 == "Violent"]

victims_2024.theft <- victims_2024[incident.crime_type == "Theft"]
victims_2024.cyber <- victims_2024[incident.crime_type == "Cybercrime"]
victims_2024.all_property <- victims_2024[incident.crime_type2 == "Property"]
victims_2024.assault <- victims_2024[incident.crime_type == "Assault"]
victims_2024.all_violent <- victims_2024[incident.crime_type2 == "Violent"]

# ================================================================================

# victims_pooled[incident.crime_type2 == "Violent", sapply(.SD, function(x) mean(is.na(x))), .SDcols = c(outcome_var, focus_var, X_violence, FE)]

# List covariates for property and violent crimes
X_ses <- c(    "ses.settlement_type",
                "ses.gender",
                "ses.age_group",
                "ses.has_partner",
                "ses.household_income",
                "ses.higher_edu",
                "ses.employed",
                "ses.has_crime_record"
)

X_damage_property <- c("damage.rub_thnd_binned")
X_damage_violent <- c("damage.physical"#,
                    # "damage.needed_medical_help" 
)
X_incident_property <- "incident.self_blame"
X_incident_violent <- c(
                "incident.went_to_hospital",
                "incident.witness",
                "incident.resisted",
                "incident.offender_had_weapon",
                "incident.went_to_hospital",
                "incident.offender_relationship_grouped",
                "incident.self_blame"
)
X_attitude <- c("attitude.knows_an_officer")

X_property <- c(X_ses, X_damage_property, X_incident_property, X_attitude)
X_property_2024 <-  c(X_ses, X_damage_property, "damage.emotional", 
 X_incident_property, X_attitude
 # "attitude.trusts_police"
 )

X_violent <- c(X_ses, X_damage_property, X_damage_violent, X_incident_violent, X_attitude)
X_violent_2024 <-  c(X_ses, X_damage_property, X_damage_violent, "damage.emotional",
 X_incident_violent, X_attitude
 #"attitude.trusts_police"
 )

FE <- c("region", 
"year"#, 
# "year2"
)

# victims_pooled[incident.crime_type2 == "Violent", sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_violent]
# victims_pooled[incident.crime_type == "Assault", sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_violent]
# victims_pooled[incident.crime_type == "Theft", sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_property]

glm_family <- binomial(link = "logit")

# Regression formulas
fml_property <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | {paste(FE, collapse = '+')} "))
fml_violence <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | {paste(FE, collapse = '+')} "))

fml_property_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | {paste(FE[1], collapse = '+')} "))
fml_violence_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | {paste(FE[1], collapse = '+')} "))
fml_property_2024_emotional <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))
fml_violence_2024_emotional <- as.formula(g("reported.police ~ social.told_someone +{paste(X_violent_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))

# fml_property <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} + region + year "))
fml_violence <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} + region + year "))

# fml_property_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} + region "))
# fml_violence_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} + region"))
# fml_property_2024_emotional <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} + region"))
# fml_violence_2024_emotional <- as.formula(g("reported.police ~ social.told_someone +{paste(X_violent_2024, collapse = '+')} + region"))

# Term names dict
coef_map <- 
    c("social.communication_relatives_freqRarely" = "Contact relatives freq.: Rarely",
      "social.communication_relatives_freqOften" = "Contact relatives freq.: Often",
      "social.told_someone" = "Victim informed friends or family",
      "incident.went_to_hospitalYes" = "Victim went to hospital",
      "ses.genderMale" = "Male",
      "ses.age_group18-24" = " 18--24",
      "ses.age_group25-34" = " 25--34",
      "ses.age_group35-49" = " 35--49",
      "ses.age_group50-64" = " 50--64",
      "ses.age_group65+" = " 65+",
      "ses.has_partnerYes" = "Has a partner",
      "ses.household_incomeMiddle to upper" = "Middle to upper household income", 
    #   "ses.household_incomeEnough to buy a car, or higher" = "Income: Can buy a car, or higher",
      "ses.settlement_typeSmall town or village" = "Small town or village",
      "ses.higher_eduYes" = "Higher education",
      "ses.employmentEmployed or student" = "Employed or student",
      "ses.has_crime_record" = "Has crime record",
      "damage.rub_thnd_binned(1,10]" = " 1 to 10",
      "damage.rub_thnd_binned(10,50]" = " 10 to 50",
      "damage.rub_thnd_binned(50,Inf]" = " 50+",
      "damage.physicalLight" = "Light",
      "damage.physicalHeavy" = "Heavy",
      "damage.emotionalMedium" = " Medium",
      "damage.emotionalHeavy" = " Heavy",
      "incident.witnessYes" = "There was a witness",
      "incident.resistedYes" = "Victim resisted",
      "incident.offender_had_weaponYes" = "Offender had a weapon",
      "incident.self_blame" = "Self-blame",
    #   "incident.offender_relationshipAcquaintance" = "Offender: Acquaintance",
    #   "incident.offender_relationshipColleague" = "Offender: Colleague",
    #   "incident.offender_relationshipSpouse or partner" = "Offender: Spouse or partner",
    #   "incident.offender_relationshipFriend or relative" = "Offender: Friend or relative",
    #   "incident.offender_relationshipNeighbour" = "Offender: Neighbour",
      "incident.offender_relationship_groupedAcquaintance" = " Acquaintance",
      "incident.offender_relationship_groupedColleague or neighbor" = " Colleague or neighbor",
      "incident.offender_relationship_groupedFriend or relative" = " Friend or relative",
      "incident.offender_relationship_groupedSpouse or partner" = " Spouse or partner",
      "attitude.trusts_police" = "Trusts the police",
      "attitude.knows_an_officer" = "Knows an officer")

# Base models
mdls_logit <- 
    mapply(
           list(
                Theft = victims_pooled.theft, 
                Fraud = victims_pooled.fraud,
                Cybercrime = victims_pooled.cyber, 
                Assault = victims_pooled.assault,
                # `Burglary or robbery` = victims_pooled.robbery,
                `All property` = victims_pooled.all_property,
                `All violent` = victims_pooled.all_violent),
           list(
                fml_property, fml_property, fml_property,
                 fml_violence, #fml_violence,
                fml_property, fml_violence),
           USE.NAMES = T, SIMPLIFY = F,
           FUN = function(data_subset, fml) {
               feglm(fml, family = glm_family, cluster = FE, weights = ~weight, 
               data = data_subset
               )
           }
    )

m <- glm(fml_violence, data = victims_pooled.assault)
da <- dominanceAnalysis(m)

# options("modelsummary_format_numeric_latex" = "plain")
modelsummary(mdls_logit, 
# stars = c("*" = .1, "**" = .05, "***" = .01),
stars = T, 
statistic = NULL,
coef_map = coef_map, 
gof_omit = "AIC|BIC|Adj", 
# coef_omit = "region|year|Intercept",
# fmt = fmt_decimal(digits = 3, pdigits = 3),#: Decimal digits for estimate and p values
fmt = fmt_sprintf("%.3f"),#: See ?sprintf,
exponentiate = T,
output = "gt"
) #|> as_latex() |> as.character() |> cat()

ames <- lapply(
    mdls_logit,
    function(m) {
    res <- avg_slopes(m, wts = "weight")
    res <- as.data.table(get_estimates(res))
    res[term %notin% c("region", "year")]
    }
) 

save(ames, file = "paper/tables/ames.RData")

mdls_logit_firth <-  
    mapply(
           list(
                Theft = victims_pooled.theft, 
                Fraud = victims_pooled.fraud,
                Cybercrime = victims_pooled.cyber, 
                Assault = victims_pooled.assault,
                # `Burglary or robbery` = victims_pooled.robbery,
                `All property` = victims_pooled.all_property,
                `All violent` = victims_pooled.all_violent),
           list(
                fml_property, fml_property, fml_property,
                 fml_violence, #fml_violence,
                fml_property, fml_violence),
           USE.NAMES = T, SIMPLIFY = F,
           FUN = function(data_subset, fml) {
               logistf(fml, weights = weight, 
               data = data_subset
               )
           }
    )

lftest <- logistf(as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')}")), weights = weight, data = victims_pooled.all_violent)
summary(lftest)
lftest_ame <- avg_slopes(lftest, vcov = ~ region + year, variables = "social.told_someone")
# X_property_drop <- c(X_ses, X_damage_property, X_incident_property, X_attitude)
# X_violent_drop <- c(X_ses, X_damage_property, X_damage_violent, X_incident_violent[X_incident_violent != "incident.went_to_hospital"], 
# X_attitude)
# fml_property_drop <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | {paste(FE, collapse = '+')} "))
# fml_violence_drop <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent_drop, collapse = '+')} | {paste(FE, collapse = '+')} "))

# # Base models
# mdls_logit_drop <- 
#     mapply(
#            list(
#                 Theft = victims_pooled.theft, 
#                 Fraud = victims_pooled.fraud,
#                 Cybercrime = victims_pooled.cyber, 
#                 Assault = victims_pooled.assault,
#                 # `Burglary or robbery` = victims_pooled.robbery,
#                 `All property` = victims_pooled.all_property,
#                 `All violent` = victims_pooled.all_violent),
#            list(
#                 fml_property_drop, fml_property_drop, fml_property_drop,
#                  fml_violence_drop, #fml_violence,
#                 fml_property_drop, fml_violence_drop),
#            USE.NAMES = T, SIMPLIFY = F,
#            FUN = function(data_subset, fml) {
#                feglm(fml, family = glm_family, cluster = FE, weights = ~weight, 
#                data = data_subset
#                )
#            }
#     )
# # options("modelsummary_format_numeric_latex" = "plain")
# modelsummary(mdls_logit_drop, 
# # stars = c("*" = .1, "**" = .05, "***" = .01),
# stars = T, 
# statistic = NULL,
# coef_map = coef_map, 
# gof_omit = "AIC|BIC|Adj", 
# # coef_omit = "region|year|Intercept",
# # fmt = fmt_decimal(digits = 3, pdigits = 3),#: Decimal digits for estimate and p values
# fmt = fmt_sprintf("%.3f"),#: See ?sprintf,
# exponentiate = T,
# output = "gt"
# ) #|> as_latex() |> as.character() |> cat()

# ames_drop <- lapply(
#     mdls_logit_drop,
#     function(m) {
#     res <- avg_slopes(m, wts = "weight")
#     res <- as.data.table(get_estimates(res))
#     res[term %notin% c("region", "year")]
#     }
# ) 

coef_map_emotional <- 
    c("social.told_someone" = "Victim informed friends or family",
      "damage.emotionalMedium" = " Medium",
      "damage.emotionalHeavy" = " Heavy")
# Add trust in the police and emotional damage

mdls_logit_2024 <- 
    mapply(
           list(
                Theft = victims_2024.theft, 
                Cybercrime = victims_2024.cyber, 
                Assault = victims_2024.assault,
                `All property` = victims_2024.all_property,
                `All violent` = victims_2024.all_violent),
           list(
                fml_property_2024, fml_property_2024,  fml_violence_2024, 
               fml_property_2024, fml_violence_2024),
           USE.NAMES = T, SIMPLIFY = F,
           FUN = function(data_subset, fml) {
                # print(fml)
               feglm(fml, family = glm_family, cluster = FE[1], weights = ~weight,
                data = data_subset
                )
           }
    )

ames_2024 <- lapply(
    mdls_logit_2024,
    function(m) {
    res <- avg_slopes(m, wts = "weight", variables = "social.told_someone")
    res <- as.data.table(get_estimates(res))
    res[term %notin% c("region", "year")]
    }
) 
save(ames_2024, file = "paper/tables/ames_2024.RData")

mdls_logit_2024_emotional <- 
    mapply(
           list(
                Theft = victims_2024.theft, 
                Cybercrime = victims_2024.cyber, 
                Assault = victims_2024.assault,
                `All property` = victims_2024.all_property,
                `All violent` = victims_2024.all_violent),
           list(
                fml_property_2024_emotional, fml_property_2024_emotional,  fml_violence_2024_emotional, 
               fml_property_2024_emotional, fml_violence_2024_emotional),
           USE.NAMES = T, SIMPLIFY = F,
           FUN = function(data_subset, fml) {
                # print(fml)
               feglm(fml, family = glm_family, cluster = FE[1], weights = ~weight,
                data = data_subset[is.na(reported.case_initiated) | reported.case_initiated == "Да"]
                )
           }
    )

ames_2024_emotional <- lapply(
    mdls_logit_2024,
    function(m) {
    res <- avg_slopes(m, wts = "weight", variables = c("social.told_someone", "damage.emotional"))
    res <- as.data.table(get_estimates(res))
    res[term %notin% c("region", "year")]
    }
) 
save(ames_2024_emotional, file = "paper/tables/ames_2024_emotional.RData")



# modelsummary(mdls_logit_2024, stars = T, statistic = NULL, exponentiate = T, coef_map = coef_map_emotional) 
# victims_2024.theft[, sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_property_2024]
# victims_2024[, .N, incident.crime_type]

# modelsummary(mdls_logit_2024_emotional, 
# # stars = c("*" = .1, "**" = .05, "***" = .01), 
# stars = T,
# statistic = NULL,
# coef_map = coef_map_emotional, 
# fmt = fmt_sprintf("%.3f"),#: See ?sprintf,
# exponentiate = T,
# output = "gt"
# ) |> as_latex() |> as.character() |> cat()

# modelsummary(mdls_logit_2024, 
# # stars = c("*" = .1, "**" = .05, "***" = .01), 
# stars = T,
# statistic = NULL,
# coef_map = coef_map_emotional, 
# fmt = fmt_sprintf("%.3f"),#: See ?sprintf,
# exponentiate = T,
# output = "gt"
# ) |> as_latex() |> as.character() |> cat()
    
# Drop observations where no case was initiated after report
mdls_logit_caseinit <- 
    mapply(
           list(
                Theft = victims_pooled.theft, 
                Fraud = victims_pooled.fraud,
                Cybercrime = victims_pooled.cyber, 
                Assault = victims_pooled.assault,
                `All property` = victims_pooled.all_property,
                `All violent` = victims_pooled.all_violent),
           list(
                fml_property, fml_property, fml_property, fml_violence,
                fml_property, fml_violence),
           USE.NAMES = T, SIMPLIFY = F,
           FUN = function(data_subset, fml) {
               feglm(fml, family = glm_family, cluster = FE, weights = ~weight, 
               data = data_subset[is.na(reported.case_initiated) | reported.case_initiated == "Да"]
               )
           }
    )

ames_caseinit <- lapply(
    mdls_logit_caseinit,
    function(m) {
    res <- avg_slopes(m, wts = "weight", variables = "social.told_someone")
    res <- as.data.table(get_estimates(res))
    res[term %notin% c("region", "year")]
    }
) 
save(ames_caseinit, file = "paper/tables/ames_caseinit.RData")

# modelsummary(mdls_logit_caseinit, 
# # stars = c("*" = .1, "**" = .05, "***" = .01), 
# stars = T,
# statistic = NULL,
# coef_map = coef_map, 
# fmt = fmt_sprintf("%.3f"),#: See ?sprintf,
# exponentiate = T,
# output = "gt"
# ) |> as_latex() |> as.character() |> cat()


# Who asks for help

fml_property_told <- as.formula(g("social.told_someone ~ {paste(X_property, collapse = '+')} + region + year "))
fml_violence_told <- as.formula(g("social.told_someone ~ {paste(X_violent, collapse = '+')} + region + year "))

# fml_property_2024_told <- as.formula(g("{focus_var} ~ {paste(c(X_base, X_2024), collapse = '+')} | {paste(FE[1], collapse = '+')} "))
# fml_violence_2024_told <- as.formula(g("{focus_var} ~ {paste(c(X_violence, X_2024), collapse = '+')} | {paste(FE[1], collapse = '+')} "))
mdls_logit_told <- 
    mapply(
           list(
                Theft = victims_pooled.theft, 
                Fraud = victims_pooled.fraud,
                Cybercrime = victims_pooled.cyber, 
                Assault = victims_pooled.assault,
                `All property` = victims_pooled.all_property,
                `All violent` = victims_pooled.all_violent),
           list(
                fml_property_told, fml_property_told, fml_property_told, fml_violence_told,
                fml_property_told, fml_violence_told),
           USE.NAMES = T, SIMPLIFY = F,
           FUN = function(data_subset, fml) {
               feglm(fml, family = glm_family, cluster = FE, weights = ~weight, 
               data = data_subset
               )
           }
    )
# modelsummary(mdls_logit_told, 
# # stars = c("*" = .1, "**" = .05, "***" = .01), 
# stars = T,
# statistic = NULL,
# # coef_map = coef_map, 
# fmt = fmt_sprintf("%.3f"),#: See ?sprintf,
# exponentiate = T,
# output = "gt"
# ) |> as_latex() |> as.character() |> cat()


ames_told <- lapply(
    mdls_logit_told,
    function(m) {
    res <- avg_slopes(m, wts = "weight")
    res <- as.data.table(get_estimates(res))
    res[term %notin% c("region", "year")]
    }
) 
save(ames_told, file = "paper/tables/ames_told.RData")

library(kable)
knitr::kable(ames[[1]][, .(term, estimate, p.value)], "latex", booktabs = T)
for(a in ames) {
    print(
        knitr::kable(a[, .(term, estimate, p.value)])

    )
}
