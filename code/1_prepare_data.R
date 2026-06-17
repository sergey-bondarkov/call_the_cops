library(data.table)
library(forcats)

data_pooled <- readRDS("data/raw/rcvs_18_21_24_2026-03-14.Rds")
data_2024 <- readRDS("data/raw/rcvs_rdd_2024_2026-03-14.Rds")

data_pooled[!is.na(crime_type), lapply(.SD, function(x) round(mean(x, na.rm = T), 2)), by = crime_type, .SDcols = patterns("Q47")]


vars_of_interest <- c(

    Q75 = "victim",

    # Reporting
    victim_is_reporting = "reported.police",
    Q46 = "reported.case_initiated",
    #   Q47_5 = "reported.bank",
    #   Q47_6 = "reported.criminal",

    # Incident
    Q76 = "incident.last_12_months",
    #   Q76_1N = "incident.N_12_months",
    Q75_1N = "incident.N_5_years",
    crime_type = "incident.crime_type",
    offender_is_male = "incident.offender_is_male",
    crime_is_offender_seen = "incident.offender_seen",
    crime_is_people_around = "incident.witness",
    Q25_1 = "incident.no_weapon",
    Q31 = "incident.resisted",
    offender_alone = "incident.offender_alone",
    crime_is_threat = "incident.threat",
    offender_is_familiar = "incident.offender_familiar",
    offender_relationship_grouped = "incident.offender_relationship",
    victim_is_self_accused = "incident.self_blame",
    Q661 = "incident.via_phone",
    Q662 = "incident.via_internet",
    crime_is_seeking_medical_help = "incident.went_to_hospital",

    # Damage
    crime_is_property_damage = "damage.property_damage",
    crime_is_property_acquisition = "damage.property_loss",
    victim_damage_rub = "damage.rub",
    victim_damage_physical = "damage.physical",
    Q27 = "damage.needed_medical_help",
    victim_damage_emotional = "damage.emotional",

    # Attitude toward police
    resp_trust_police = "attitude.trusts_police",
    resp_is_enforcers_familiar = "attitude.knows_an_officer",

    # Social network
    victim_is_told_anyone = "social.told_someone",
    resp_is_living_alone = "social.lives_alone",
    resp_is_married = "ses.has_partner",
    resp_household_size = "social.household_size",
    Q801 = "social.communication_relatives_freq",
    Q802 = "social.communication_relatives_way",

    # SES
    resp_is_male = "ses.male",
    Age_Groups = "ses.age_group",
    Q57 = "ses.household_income",
    resp_edu = "ses.edu",
    resp_ses = "ses.job",
    Q2003 = "ses.settlement_type",
    resp_is_crime_record = "ses.has_crime_record",

    # Controls
    Q1005 = "region",
    Q2001 = "federal_district",
    year = "year",

    # Technical
    weight = "weight",
    deflator2024rub = "deflator2024rub"
)


crime_types <- c(
    `Cybercrime` = "Киберпреступление",
    `Attempted cybercrime` = "Покушение на киберпреступление",
    `Fraud` = "Мошенничество",
    `Theft` = "Кража",
    Assault = "Нападение",
    `Burglary or robbery` = "Грабеж и разбой",
    Other = "Прочее",
    `Not enough information` = "Недостаточно информации"
)

prepare_data <- function(dt) {
    dt <- dt[, intersect(names(vars_of_interest), names(dt)), with = F]

    setnames(dt, function(x) vars_of_interest[x])

    dt[, incident.crime_type := fct_recode(incident.crime_type, !!!crime_types)]
    dt[, incident.crime_type := fct_relevel(incident.crime_type, names(crime_types))]
    dt[, incident.crime_type := fdroplevels(incident.crime_type)]
    dt[incident.crime_type %in% c("Fraud", "Theft", "Cybercrime"), incident.crime_type2 := "Property"]
    dt[incident.crime_type %in% c("Assault", "Burglary or robbery"), incident.crime_type2 := "Violent"]

    dt[, incident.last_12_months := ifelse(incident.last_12_months == "Да", 1, 0)]

    dt[, incident.resisted := fct_collapse(incident.resisted,
        Yes = "Да",
        `No/Doubt` = c("Нет", "Затрудняюсь ответить")
    )]
    dt[, incident.resisted := fct_relevel(incident.resisted, "No/Doubt")]

    dt[, incident.offender_had_weapon := factor(incident.no_weapon, levels = c(1, 0), labels = c("No", "Yes"))]
    dt[, incident.no_weapon := NULL]

    dt[, incident.witness := factor(incident.witness, c(0, 1), c("No", "Yes"))]

    dt[, incident.went_to_hospital := factor(incident.went_to_hospital, c(0, 1), c("No", "Yes"))]

    dt[, incident.offender_relationship := fct_recode(incident.offender_relationship,
        Colleague = "Коллега",
        Neighbor = "Сосед или соседка",
        `Friend or relative` = "Друзья и родственники",
        `Spouse or partner` = "Супруги и сожители",
        Acquaintance = "Дальние и случайные знакомые",
        Stranger = "Незнакомец"
    )]
    dt[, incident.offender_relationship := fct_relevel(incident.offender_relationship, "Stranger")]

    dt[, incident.offender_relationship_grouped := fct_collapse(incident.offender_relationship,
        # Acquaintance = "Acquaintance", 
        `Colleague or neighbor` = c("Colleague", "Neighbor")
        # `Spouse or partner`, relative, friend` = c("Friend or relative", "Spouse or partner"),
        # Stranger = "Stranger"
    )]

    if("deflator2024rub" %in% names(dt))
    dt[, damage.rub := damage.rub / deflator2024rub]
    dt[, damage.rub_thnd_binned := cut(damage.rub / 1000,
        breaks = c(0, 1, 10, 50, Inf),
        include.lowest = T
    )]

    dt[, damage.physical := fct_recode(damage.physical,
        No = "Нет",
        Light = "Да, синяки или ссадины",
        Heavy = "Да, повреждения серьёзнее, чем синяки или ссадины"
    )]
    dt[, damage.physical := fct_relevel(damage.physical, c("No", "Light", "Heavy"))]

    if ("damage.emotional" %in% names(dt)) {
        dt[, damage.emotional := fcase(
            # damage.emotional == 11, "Light to medium",
            damage.emotional >= 9, "Heavy",
            damage.emotional >= 6, "Medium",
            damage.emotional >= 1, "Light"
        )]
        dt[, damage.emotional := fct_relevel(damage.emotional, c("Light", "Medium", "Heavy"))]
    }

    dt[, damage.needed_medical_help := fct_collapse(damage.needed_medical_help,
        Yes = "Да",
        other_level = "No/Doubt"
    )]

    dt[, ses.gender := factor(ses.male, levels = c(0, 1), labels = c("Female", "Male"))]

    dt[, ses.employed := fct_collapse(ses.job,
        `Non-employed or pensioner` = c("Безработный", "Пенсионер"),
        other_level = "Employed or student"
    )]
    dt[, ses.job := fct_recode(ses.job,
        Unemployed = "Безработный",
        Pensioner = "Пенсионер",
        Student = "Студент",
        Employee = "Служащий",
        Worker = "Рабочий",
        Manager = "Руководитель"
    )]

    dt[ses.household_income %in% c("Затрудняюсь ответить", "Отказ от ответа"), ses.household_income := NA]
    dt[!is.na(ses.household_income), ses.household_income := fct_collapse(as.factor(ses.household_income),
        `Lower` = c(
            "Едва сводим концы с концами, денег не хватает на продукты",
            "На продукты хватает, на одежду нет",
            "На продукты и одежду хватает, на технику и мебель нет"
        ),
        `Middle to upper` = c("На технику и мебель хватает, на большее денег нет",
        "Можем позволить автомобиль, но квартиру или дачу нет",
            "Можем позволить себе практически все: квартиру и т.д.",
            "Можем позволить только авто И можем позволить практически все"
        )
    )]
    dt[, ses.household_income := fdroplevels(ses.household_income)]
   

    dt[, ses.edu := fct_recode(ses.edu,
        School = "Полное среднее и ниже",
        Vocational = "Среднее спец-ное/техническое или нач-ное профес-ное",
        Higher = "Высшее и незаконченное высшее"
    )]

    dt[, ses.higher_edu := fct_collapse(ses.edu,
        Yes = "Higher",
        other_level = "No"
    )]
    dt[, ses.higher_edu := fct_relevel(ses.higher_edu, "No")]
        
        
    dt[ses.settlement_type == "нет данных", ses.settlement_type := NA]
    dt[!is.na(ses.settlement_type), ses.settlement_type := fct_collapse(ses.settlement_type,
        City = c(
            "город более 1 млн",
            "город от 500 тыс до 1 млн",
            "город от 250 до 500 тыс",
            "город от 100 до 250 тыс",
            "город от 50 до 100 тыс",
            "города 1 млн и более",
            "города от 250 тыс. до 1 млн",
            "города от 50 до 250 тыс."
        ),
        `Small town or village` = c("город менее 50 тыс", "пгт", "города менее 50 тыс., ПГТ", "сельский населенный пункт", "село")
        # `Village` = c("сельский населенный пункт", "село")
    )]
    dt[, ses.settlement_type := fdroplevels(ses.settlement_type)]

    dt[, ses.has_partner := factor(ses.has_partner, c(0, 1), c("No", "Yes"))]

    if ("social.communication_relatives_freq" %in% names(dt)) {
        dt[, social.communication_relatives_freq := fct_collapse(social.communication_relatives_freq,
            Often = c(
                "Один раз в неделю или несколько раз в месяц",
                "Каждый день", "Несколько раз в неделю"
            ),
            Never = c("Не общаюсь", "Нет ближайших родственников"),
            Rarely = c("Несколько раз в год и реже", "Затрудняюсь ответить")
        )]
        dt[, social.communication_relatives_freq := fct_relevel(social.communication_relatives_freq, c("Never", "Rarely", "Often"))]
    }

    if ("social.communication_relatives_way" %in% names(dt)) {
        dt[, social.communication_relatives_way := fct_collapse(social.communication_relatives_way,
            `In person` = "При личных встречах",
            `Remotely` = c("Созваниваетесь", "Через СМС или сообщения в мессенджерах и социальных сетях", "Затрудняюсь ответить")
        )]
        dt[, social.communication_relatives_way := fct_relevel(social.communication_relatives_way, "Remotely")]
    }

    if ("attitude.trusts_police" %in% names(dt)) {
        dt[, attitude.trusts_police := ifelse(attitude.trusts_police %in% c("Полностью согласен", "Скорее согласен"), 1, 0)]
    }

    dt[, `:=`(region = as.character(region), federal_district = as.character(federal_district))]
    dt[region %in% c("г. Москва", "г. Санкт-Петербург"), federal_district := region]
    dt[region %in% c("Донецкая Народная Республика", "Луганская Народная Республика", "Запорожская область", "Херсонская область"), federal_district := "Новые территории"]
    dt[, `:=`(region = as.factor(region), federal_district = as.factor(federal_district))]

    dt[, weight := as.numeric(weight)]

    return(dt)
}

data_pooled_prep <- prepare_data(data_pooled)
data_2024_prep <- prepare_data(data_2024)
saveRDS(data_pooled_prep, "data/processed/rcvs_pooled_prep.Rds")
saveRDS(data_2024_prep, "data/processed/rcvs_2024_prep.Rds")

victims_pooled <- data_pooled_prep[!is.na(incident.crime_type)]
victims_2024 <- data_2024_prep[!is.na(incident.crime_type)]
saveRDS(victims_2024, "data/processed/victims_2024.Rds")
saveRDS(victims_pooled, "data/processed/victims_pooled.Rds")

data_pooled_prep[, .N, incident.crime_type2]
