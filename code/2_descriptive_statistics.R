library(data.table)
library(gt)
library(modelsummary)
library(ggplot2)
library(forcats)

victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_2024 <- readRDS("data/processed/victims_2024.Rds")


weighted.sd <- function(x, w) {
    sw <- sum(w)
    xbar <- sum(w * x) / sw
    v <- sum(w * ((x - xbar)^2)) / (sw - 1)
    sqrt(v)
}

weighted_mean_sem <- function(x, w) {
    if(length(x) == 0) {
        ""
    } else {
        m <- weighted.mean(x, w)
        s <- weighted.sd(x, w)
        sem <- s / sqrt(sum(w))
        glue::glue("{round(m, 2)} ({round(sem, 2)})")

    }
}

reporting_agg <-
    victims_pooled[, .(estimate = weighted.mean(reported.police, weight),
                     sem = weighted.sd(reported.police, weight) / sqrt(sum(weight))), 
keyby = .(incident.crime_type, social.told_someone)]

ggplot(reporting_agg, 
       aes(fct_rev(incident.crime_type), 
           col = fct_recode(as.factor(social.told_someone), No = "0", Yes = "1"))) +
    geom_point(aes(y = estimate), position = position_nudge(ifelse(reporting_agg$social.told_someone == 1, .1, -.1))) +
    geom_errorbar(aes(ymin = estimate - sem * 1.96, 
                      ymax = estimate + sem * 1.96),
                  width = .2,
                  position = position_nudge(ifelse(reporting_agg$social.told_someone == 1, .1, -.1))) +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    labs(x = "", y = "Weighted probability of reporting", 
         col = "If victim spoke with somebody\nabout the incident") +
    coord_flip() 
ggsave("output/figures/raw_means_2024.pdf", width = 9, height = 6, scale = .5)

# ================================================================================
# Crime freq

datasummary(reported.police *
            (1 +
             (`Damage: money` = damage.rub_thnd_binned) +
            (`Damage: physical` = damage.physical)
        # +  (`Damage: emotional` = damage.emotional)
            )
~ incident.crime_type * (N + (`Reported (SE)` = weighted_mean_sem * Arguments(w = weight))),
data = victims_pooled,
output = "gt") |> cols_hide(1)

# ================================================================================
# Reporting
## SES
datasummary(reported.police *
            ((Total = 1) +
             (Gender = ses.gender) + 
             (Age = ses.age_group) +
             (`Has partner` = as.factor(social.has_partner)) +
            (`Settlement type` = ses.settlement_type) +
            (Education = ses.edu) +
            (Employment = ses.job) +
            (Income = ses.income) 
            )
            ~ incident.crime_type * (N + (`Reported (SE)` = weighted_mean_ci * Arguments(w = weight)) 
            )
            ,
        output = "gt",
        fmt = 2,
        data = victims_pooled) |> cols_hide(1) |> as_latex() |> as.character() |> cat()

## Damage
datasummary(reported.police *
            (
             (`Damage: money` = damage.rub_thnd_binned) +
                 (`Damage: physical` = damage.physical) +
                 # (`Damage: emotional` = damage.emotional) +
                 (`Incident: witness` = as.factor(incident.witness)) +
                 (`Incident: weapon` = as.factor(incident.offender_had_weapon)) +
                 (`Incident: resisted` = as.factor(incident.resisted))
            )
            ~ incident.crime_type * (N + (`Reported (SE)` = weighted_mean_ci * Arguments(w = weight))),
            output = "gt",
            fmt = 2,
            data = victims_pooled) |> cols_hide(1) |> as_latex() |> as.character() |> cat()


## Social
datasummary(reported.police *
            ((`Offender-victim relationship` = social.offender_relationship) +
(`Victim told friends/relatives` = factor(social.told_someone, c(0, 1), c("No", "Yes"))))
            ~ incident.crime_type * (N + (`Reported (SE)` = weighted_mean_ci * Arguments(w = weight))),
            output = "gt",
            fmt = 2,
            data = victims_pooled)|> cols_hide(1) |> as_latex() |> as.character() |> cat()

## Told someone
datasummary(reported.police *
            (`Victim told friends/relatives` = factor(social.told_someone, c(0, 1), c("No", "Yes")))
            ~ incident.crime_type * (N + (`Reported (SE)` = weighted_mean_ci * Arguments(w = weight))),
            output = "gt",
            fmt = 2,
            data = victims_pooled)|> cols_hide(1)

## All
datasummary(reported.police *
            (
             (Total = 1) +
            (Year = Factor(year)) +
             (Gender = ses.gender) + 
             (Age = ses.age_group) +
             (`Has partner` = social.has_partner) +
             (`Settlement type` = ses.settlement_type) +
             (Education = ses.edu) +
             (Employment = ses.job) +
             (`Damage: money` = damage.rub_thnd_binned) +
             (`Damage: physical` = damage.physical) +
             # (`Damage: emotional` = damage.emotional) +
             (`Incident: witness` = incident.witness) +
             (`Incident: weapon` = incident.offender_had_weapon) +
             (`Incident: resisted` = incident.resisted) +
             (`Offender-victim relationship` = social.offender_relationship) +
             (`Victim told friends/relatives` = factor(social.told_someone, c(0, 1), c("No", "Yes")))
             )
         ~ incident.crime_type * (N + (`Reported (SE)` = weighted_mean_ci * Arguments(w = weight))),
         output = "gt",
         fmt = 2,
         data = victims_pooled) |>
    cols_hide(1)|> as_latex() |> as.character() |> cat() #|> 
    # tab_row_group(label = "Socioeconomic status", rows = 2:23) |>
    # tab_row_group(label = "Incident charactertics", rows = 24:39) |>
    # tab_row_group(label = "Social factors", rows = 40:47) |>
    # row_group_order(c(NA, "Socioeconomic status", "Incident charactertics", "Social factors"))



