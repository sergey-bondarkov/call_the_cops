library(data.table)
library(forcats)

# data_panel <- readRDS("data/raw/rcvs_panels_21_24_2025-09-05.Rds")
# setnames(data_panel, "final_weight", "weight")

data_pooled <- readRDS("data/raw/rcvs_18_21_24_2025-07-02.Rds")
data_2024 <- readRDS("data/raw/rcvs_rdd_2024_2025-03-31.Rds")

vars_of_interest <- c(
                      # Reporting
                      victim_is_reporting = "reported.police",
                      Q47_5 = "reported.bank",
                      Q47_6 = "reported.criminal",

                      # Incident
                      Q76 = "incident.last_12_months",
                      Q76_1N = "incident.N_12_months",
                      Q75_1N = "incident.N_5_years",
                      crime_type = "incident.crime_type",
                      offender_is_male = "incident.offender_is_male",
                      crime_is_offender_seen = "incident.offender_seen",
                      crime_is_people_around = "incident.witness",
                      Q25_1 = "incident.no_weapon",
                      Q31 = "incident.resisted",
                      offender_alone = "incident.offender_alone",
                      crime_is_threat = "incident.threats",

                      # Damage
                      crime_is_property_damage = "damage.property_damage",
                      crime_is_property_acquisition = "damage.property_lost",
                      victim_damage_rub = "damage.rub",
                      victim_damage_physical = "damage.physical",
                      Q27 = "damage.needed_medical_help",
                      crime_is_seeking_medical_help = "damage.went_to_hospital",
                      victim_damage_emotional = "damage.emotional",

                      # Attitude toward police
                      resp_trust_police = "attitude.trusts_police",
                      resp_is_enforcers_familiar = "attitude.knows_some_police",
                      resp_is_crime_record = "attitude.has_crime_record",

                      # Relations
                      victim_is_told_anyone = "social.told_someone",
                      victim_is_self_accused = "social.self_blame",
                      resp_is_living_alone = "social.lives_alone",
                      resp_is_married = "social.has_partner",
                      resp_communication_relatives_freq = "social.communication_relatives_freq",
                      resp_communication_relatives_way = "social.communication_relatives_way",
                      offender_is_familiar = "social.offender_familiar",
                      offender_relationship_grouped = "social.offender_relationship",

                      # SES
                      resp_is_male = "ses.male",
                      Age_Groups = "ses.age_group",
                      resp_household_size = "ses.household_size", 
                      resp_income = "ses.income",
                      resp_edu = "ses.edu",
                      resp_ses = "ses.job",
                      Q2003 = "ses.settlement_type",

                      # Controls
                      Q1005 = "region",
                      Q2001 = "federal_district",
                      year = "year",

                      # Technical
                      weight = "weight"
)

crime_types <- c(`Cybercrime` = "Удаленное преступление",
                 `Fraud` = "Мошенничество",
                 `Theft` = "Кража",
                 Assault = "Нападение",
                 Robbery = "Грабеж и разбой"
)

prepare_data <- function(dt) {

    victims <- dt[crime_type %in% crime_types,
                  intersect(names(vars_of_interest), names(dt)),
                  with = F]

    setnames(victims, function(x) vars_of_interest[x])

    victims[, incident.crime_type := fct_recode(incident.crime_type, !!!crime_types)]
    victims[, incident.crime_type := fct_relevel(incident.crime_type, names(crime_types))]
    victims[, incident.crime_type := fdroplevels(incident.crime_type)]

    victims[, incident.last_12_months := ifelse(incident.last_12_months == "Да", 1, 0)]

    victims[, incident.resisted := fct_collapse(incident.resisted, 
                                                Yes = "Да", 
                                                `No/Doubt` = c("Нет","Затрудняюсь ответить"))]
    victims[, incident.resisted := fct_relevel(incident.resisted, "No/Doubt")]

    victims[, incident.offender_had_weapon := factor(incident.no_weapon, levels = c(1, 0), labels = c("No", "Yes"))]
    victims[, incident.no_weapon := NULL]

    victims[, incident.witness := factor(incident.witness, c(0, 1), c("No", "Yes"))]

    victims[, damage.rub_thnd_binned:= cut(damage.rub / 1000,
                                           breaks = c(0, 1, 10, 50, Inf),
                                           include.lowest = T)]

    victims[, damage.physical := fct_recode(damage.physical, 
                                            No = "Нет", 
                                            Light = "Да, синяки или ссадины", 
                                            Heavy = "Да, повреждения серьёзнее, чем синяки или ссадины")]
    victims[, damage.physical := fct_relevel(damage.physical, "No")]

    if ("damage.emotional" %in% names(victims)) {
        victims[, damage.emotional := fcase(damage.emotional >= 8, "Heavy",
                                            damage.emotional >= 4, "Medium",
                                            damage.emotional >= 1, "Light")]
        victims[, damage.emotional := fct_relevel(damage.emotional, c("Light", "Medium", "Heavy"))]
    }

    victims[, damage.needed_medical_help := fct_collapse(damage.needed_medical_help,
                                                         Yes = "Да",
                                                         other_level = "No/Doubt")]

    victims[, ses.gender := factor(ses.male, levels = c(0, 1), labels = c("Female", "Male"))]

    victims[, ses.employed := fct_collapse(ses.job, 
                                           `Non-employed or pensioner` = c("Безработный", "Пенсионер"),
                                           other_level = "Employed or student")]
    victims[, ses.job := fct_recode(ses.job, 
                                    Unemployed = "Безработный",
                                    Pensioner = "Пенсионер",
                                    Student = "Студент",
                                    Employee = "Служащий",
                                    Worker = "Рабочий",
                                    Manager = "Руководитель")]

    victims[, ses.income := fct_collapse(ses.income,
                                         `Enough to buy clothes, or lower` = c("Едва сводим концы с концами, денег не хватает на продукты",
                                                   "На продукты хватает, на одежду нет",
                                                   "На продукты и одежду хватает, на технику и мебель нет"),
                                         `Enough to buy furniture/electonics` = c("На технику и мебель хватает, на большее денег нет"),
                                         `Enough to buy a car, or higher` = c("Можем позволить автомобиль, но квартиру или дачу нет",
                                                   "Можем позволить себе практически все: квартиру и т.д.",
                                                   "Можем позволить только авто И можем позволить практически все"),
                                         other_level = NA)]

    victims[, ses.edu := fct_recode(ses.edu, 
                                    School = "Полное среднее и ниже",
                                    Vocational =  "Среднее спец-ное/техническое или нач-ное профес-ное",
                                    Tertiary = "Высшее и незаконченное высшее")]

    victims[, ses.higher_edu := fct_collapse(ses.edu,
                                             Yes = "Tertiary",
                                             other_level = "No")]
    victims[, ses.higher_edu := fct_relevel(ses.higher_edu, "No")]

    victims[, ses.settlement_type := fct_collapse(ses.settlement_type,
                                                  City = c("город более 1 млн",
                                                           "город от 500 тыс до 1 млн",
                                                           "город от 250 до 500 тыс",
                                                           "город от 100 до 250 тыс",
                                                           "город от 50 до 100 тыс",
                                                           "города 1 млн и более",
                                                           "города от 250 тыс. до 1 млн",
                                                           "города от 50 до 250 тыс."),
                                                  `Small town` = c("город менее 50 тыс", "пгт", "города менее 50 тыс., ПГТ"),
                                                  `Village` = c("сельский населенный пункт", "село"),
                                                  other_level = "Unknown") ]

    victims[, social.has_partner := factor(social.has_partner, c(0, 1), c("No", "Yes"))]


    if ("social.communication_relatives_freq" %in% names(victims)) {
        victims[, social.communication_relatives_freq := fct_collapse(social.communication_relatives_freq,
                                                                      Often = c("Один раз в неделю или несколько раз в месяц",
                                                                                "Каждый день", "Несколько раз в неделю"),
                                                                      Never = c("Не общаюсь", "Нет ближайших родственников"),
                                                                      Rarely = c("Несколько раз в год и реже", "Затрудняюсь ответить")
                                                                      )]
        victims[, social.communication_relatives_freq := fct_relevel(social.communication_relatives_freq, "Never")]
    }

    if ("social.communication_relatives_way" %in% names(victims)) {
        victims[, social.communication_relatives_way := fct_collapse(social.communication_relatives_way,
                                                                     Meet = "При личных встречах",
                                                                     Phone = "Созваниваетесь",
                                                                     `Text/Doubt` = c("Через СМС или сообщения в мессенджерах и социальных сетях", "Затрудняюсь ответить")
                                                                     )]
        victims[, social.communication_relatives_way := fct_relevel(social.communication_relatives_way, "Text/Doubt")]
    }

    victims[, social.offender_relationship := fct_recode(social.offender_relationship,
                                                         Colleague = "Коллега",
                                                         Neighbour = "Сосед или соседка",
                                                         `Friend or relative` = "Друзья и родственники",
                                                         `Spouse or partner` = "Супруги и сожители",
                                                         Acquaintance = "Дальние и случайные знакомые",
                                                         Stranger = "Незнакомец")]
    victims[, social.offender_relationship := fct_relevel(social.offender_relationship, "Stranger")]

    victims[, social.offender_relationship_grouped := fct_collapse(social.offender_relationship,
                                                                   `Acquaintance or colleague` = c("Acquaintance", "Colleague"),
                                                                   `Spouse, partner, relative, friend` = c("Friend or relative", "Spouse or partner"), 
                                                                   Neighbour = "Neighbour",
                                                                   Stranger = "Stranger")]


    if ("attitude.trusts_police" %in% names(victims)) {
        victims[, attitude.trusts_police := ifelse(attitude.trusts_police %in% c("Полностью согласен", "Скорее согласен"), 1, 0)]

    }

    victims[, `:=`(region = as.character(region), federal_district = as.character(federal_district))]
    victims[region %in% c("г. Москва", "г. Санкт-Петербург"), federal_district := region]
    victims[region %in% c("Донецкая Народная Республика", "Луганская Народная Республика", "Запорожская область", "Херсонская область"), federal_district := "Новые территории"]
    victims[, `:=`(region = as.factor(region), federal_district = as.factor(federal_district))]

    victims[, weight := as.numeric(weight)]

    return(victims)

}

# victims_panel <- prepare_data(data_panel)
victims_2024 <- prepare_data(data_2024)
victims_pooled <- prepare_data(data_pooled)

# saveRDS(victims_panel, "data/processed/victims_panel.Rds")
saveRDS(victims_2024, "data/processed/victims_2024.Rds")
saveRDS(victims_pooled, "data/processed/victims_pooled.Rds")
