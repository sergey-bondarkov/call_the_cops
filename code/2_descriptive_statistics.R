library(data.table)
library(gt)
library(modelsummary)
library(ggplot2)
library(forcats)
library(circhelp)

victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_pooled <- victims_pooled[!is.na(ses.settlement_type) & as.character(ses.household_income2) != "<NA>"]
victims_2024 <- readRDS("data/processed/victims_2024.Rds")
victims_2024 <- victims_2024[!is.na(ses.settlement_type) & as.character(ses.household_income2) != "<NA>"]


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
             (Gender = ses.gender) + 
             (Age = ses.age_group) +
             (`Has partner` = ses.has_partner) +
             (`Household income` = fdroplevels(ses.household_income)) +
             (`Settlement type` = ses.settlement_type) +
             (`Higher education` = ses.higher_edu) +
             (Employment = ses.employed) +
             (`Damage: money, thnd rub.` = damage.rub_thnd_binned) +
             (`Damage: physical` = damage.physical) +
             # (`Damage: emotional` = damage.emotional) +
             (`Incident: witness` = incident.witness) +
             (`Incident: weapon` = incident.offender_had_weapon) +
             (`Incident: resisted` = incident.resisted) +
             (`Incident: went to hospital` = incident.went_to_hospital) +
            (`Incident: self-blame` = factor(incident.self_blame, c(0, 1), c("No", "Yes"))) +
             (`Attitude: Knows an officer` = factor(attitude.knows_an_officer, c(0, 1), c("No", "Yes"))) +
             (`Attitude: Has crime record` = factor(ses.has_crime_record, c(0, 1), c("No", "Yes")))
             )
         ~ data.table::fdroplevels(incident.crime_type) * (N + (`Reported (SE)` = weighted_mean_sem * Arguments(w = weight))),
         output = "gt",
         fmt = 2,
         data = victims_pooled[incident.crime_type %notin% c("Other", "Not enough information", "Attempted cybercrime")]) |>
    cols_hide(1) |> as_latex() |> as.character() |> cat() 


datasummary(reported.police *
            (
             (`Damage: emotional` = damage.emotional) +
             (`Attitude: Trusts the police` = factor(attitude.trusts_police, c(0, 1), c("No", "Yes")))
             )
         ~ data.table::fdroplevels(incident.crime_type) * (N + (`Reported (SE)` = weighted_mean_sem * Arguments(w = weight))),
         output = "gt",
         fmt = 2,
         data = victims_2024[incident.crime_type %notin% c("Other", "Not enough information", "Attempted cybercrime")]) |>
    cols_hide(1) |> as_latex() |> as.character() |> cat() 


datasummary(reported.police *
             (
             (Total = 1) +
            (`Victim told friends or relatives` = factor(social.told_someone, c(0, 1), c("No", "Yes"))) +
            (`Offender` = incident.offender_relationship_grouped) +
             (Gender = ses.gender) + 
             (Age = ses.age_group) +
             (`Has partner` = ses.has_partner) +
             (`Household income` = fdroplevels(ses.household_income)) +
             (`Settlement type` = ses.settlement_type) +
             (`Higher education` = ses.higher_edu) +
             (Employment = ses.employed) +
             (`Damage: money, thnd rub.` = damage.rub_thnd_binned) +
             (`Damage: physical` = damage.physical) +
              (`Damage: emotional` = damage.emotional) +
            (`Incident: witness` = incident.witness) +
             (`Incident: weapon` = incident.offender_had_weapon) +
             (`Incident: resisted` = incident.resisted) +
            (`Incident: went to hospital` = incident.went_to_hospital) +
             (`Attitude: Knows an officer` = factor(attitude.knows_an_officer, c(0, 1), c("No", "Yes"))) +
             (`Attitude: Has crime record` = factor(ses.has_crime_record, c(0, 1), c("No", "Yes"))) +
             (`Attitude: Trusts the police` = factor(attitude.trusts_police, c(0, 1), c("No", "Yes")))
             )
         ~ data.table::fdroplevels(incident.crime_type) * (N + (`Reported (SE)` = weighted_mean_sem * Arguments(w = weight))),
         output = "gt",
         fmt = 2,
         data = victims_2024[incident.crime_type %notin% c("Other", "Not enough information", "Attempted cybercrime")]) |>
    cols_hide(1) |> as_latex() |> as.character() |> cat() 






