library(data.table)
library(gt)
library(modelsummary)
library(ggplot2)
library(forcats)
library(circhelp)

victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
# victims_pooled <- victims_pooled[!is.na(ses.settlement_type) & as.character(ses.household_income2) != "<NA>"]
victims_2024 <- readRDS("data/processed/victims_2024.Rds")
# victims_2024 <- victims_2024[!is.na(ses.settlement_type) & as.character(ses.household_income2) != "<NA>"]


weighted_mean_sem <- function(x, w) {
    if(length(x) == 0) {
        "n/a"
    } else {
        m <- weighted.mean(x, w)
        sem <- weighted_sem(x, w)
        glue::glue("{round(m, 2)} ({round(sem, 2)})")
    }
}

datasummary(reported.police *
            (
             (Total = 1) +
            (`Survey wave` = Factor(year)) +
            (`Victim told friends or relatives` = factor(social.told_someone, c(0, 1), c("No", "Yes"))) +
            (`Offender` = incident.offender_relationship_grouped) +
             (Gender = factor(ses.male, c(0, 1), c("Female", "Male"))) +
             (Age = ses.age_group) +
             (`Household size` = ses.household_size_f) +
             (`Household income` = fdroplevels(ses.household_income)) +
             (`Settlement type` = ses.settlement_type) +
             (`Higher education` = factor(ses.higher_edu, c(0, 1), c("No", "Yes"))) +
             (Employment = factor(ses.employed, c(0, 1), c("No", "Yes"))) +
             (`Damage: money, thnd rub.` = damage.rub_thnd_binned) +
             (`Damage: physical` = damage.physical) +
             # (`Damage: emotional` = damage.emotional) +
              (`Incident: repeated` = factor(incident.repeated, c(0, 1), c("No", "Yes"))) +
             (`Incident: witness` = factor(incident.witness, c(0, 1), c("No", "Yes"))) +
             (`Incident: weapon` = factor(incident.offender_had_weapon, c(0, 1), c("No", "Yes"))) +
             (`Incident: resisted` = factor(incident.resisted, c(0, 1), c("No", "Yes"))) +
            (`Incident: sexualized` = factor(incident.sexualized, c(0, 1), c("No", "Yes"))) +
             (`Incident: went to hospital` = factor(incident.went_to_hospital, c(0, 1), c("No", "Yes"))) +
            (`Incident: self-blame` = factor(incident.self_blame, c(0, 1), c("No", "Yes"))) +
             (`Attitude: Familiar with the police` = factor(attitude.knows_an_officer, c(0, 1), c("No", "Yes"))) +
             (`Attitude: Has crime record` = factor(attitude.has_crime_record, c(0, 1), c("No", "Yes")))
             )
         ~ data.table::fdroplevels(incident.crime_class) * (N + (`Reported (SE)` = weighted_mean_sem * Arguments(w = weight))),
         output = "gt",
         fmt = 2,
         data = victims_pooled[!is.na(incident.crime_class)]) |>
    cols_hide(1) |> as_latex() |> as.character() |> cat()

datasummary(reported.police *
            (
             (Total = 1) +
            (`Victim told friends or relatives` = factor(social.told_someone, c(0, 1), c("No", "Yes"))) +
            (`Offender` = incident.offender_relationship_grouped) +
             (Gender = factor(ses.male, c(0, 1), c("Female", "Male"))) +
             (Age = ses.age_group) +
             (`Household size` = ses.household_size_f) +
             (`Household income` = fdroplevels(ses.household_income)) +
             (`Settlement type` = ses.settlement_type) +
             (`Higher education` = factor(ses.higher_edu, c(0, 1), c("No", "Yes"))) +
             (Employment = factor(ses.employed, c(0, 1), c("No", "Yes"))) +
             (`Damage: money, thnd rub.` = damage.rub_thnd_binned) +
             (`Damage: physical` = damage.physical) +
             (`Damage: emotional` = damage.emotional) +
              (`Incident: repeated` = factor(incident.repeated, c(0, 1), c("No", "Yes"))) +
             (`Incident: witness` = factor(incident.witness, c(0, 1), c("No", "Yes"))) +
             (`Incident: weapon` = factor(incident.offender_had_weapon, c(0, 1), c("No", "Yes"))) +
             (`Incident: resisted` = factor(incident.resisted, c(0, 1), c("No", "Yes"))) +
            (`Incident: sexualized` = factor(incident.sexualized, c(0, 1), c("No", "Yes"))) +
             (`Incident: went to hospital` = factor(incident.went_to_hospital, c(0, 1), c("No", "Yes"))) +
            (`Incident: self-blame` = factor(incident.self_blame, c(0, 1), c("No", "Yes"))) +
             (`Attitude: Familiar with the police` = factor(attitude.knows_an_officer, c(0, 1), c("No", "Yes"))) +
             (`Attitude: Has crime record` = factor(attitude.has_crime_record, c(0, 1), c("No", "Yes")))
             )
         ~ data.table::fdroplevels(incident.crime_class) * (N + (`Reported (SE)` = weighted_mean_sem * Arguments(w = weight))),
         output = "gt",
         fmt = 2,
         data = victims_2024[!is.na(incident.crime_class)]) |>
    cols_hide(1) |> as_latex() |> as.character() |> cat()
