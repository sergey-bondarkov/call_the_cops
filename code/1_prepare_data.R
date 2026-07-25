library(data.table)
library(forcats)
# library(missForest)

data_pooled <- readRDS("data/raw/rcvs_18_21_24_2026-06-01.Rds")
data_2024 <- readRDS("data/raw/rcvs_rdd_2024_2026-03-14.Rds")
data_pooled[, unique(Q661)]
data_pooled[, unique(Q662)]


data_pooled[, damage.property_loss := as.numeric(rowSums(.SD, na.rm = T) > 0),
            .SDcols = paste0("Q19_", c(1:7, 999))]
data_2024[, damage.property_loss := as.numeric(rowSums(.SD, na.rm = T) > 0),
            .SDcols = paste0("Q19_", c(1:7, 999))]

vars_of_interest <- c(

    ID = "id",
    Q75 = "victim",

    # Reporting
    victim_is_reporting = "reported.police",
    Q46 = "reported.case_initiated",
    #   Q47_5 = "reported.bank",
    #   Q47_6 = "reported.criminal",

    # Incident
    Q76 = "incident.last_12_months",
    Q76_1N = "incident.N_12_months",
    Q75_1N = "incident.N_5_years",
    crime_type = "incident.crime_type",
    offender_is_male = "incident.offender_is_male",
    crime_is_offender_seen = "incident.offender_is_seen",
    crime_is_people_around = "incident.witness",
    Q23 = "incident.violent",
    Q25_1 = "incident.no_weapon",
    Q30 = "incident.could_die",
    Q31 = "incident.resisted",
    Q32 = "incident.sexualized",
    offender_alone = "incident.offender_alone",
    Q33 = "incident.threat_hurt",
    Q34 = "incident.threat_weapon",
    offender_is_familiar = "incident.offender_familiar",
    offender_relationship_grouped = "incident.offender_relationship",
    victim_is_self_accused = "incident.self_blame",
    Q66 = "incident.via_phone_internet",
    Q661 = "incident.via_phone",
    Q662 = "incident.via_internet",
    crime_is_seeking_medical_help = "incident.went_to_hospital",

    # Damage
    crime_is_property_damage = "damage.property_damage",
    damage.property_loss = "damage.property_loss",
    victim_damage_rub = "damage.rub",
    victim_damage_physical = "damage.physical",
    Q27 = "damage.needed_medical_help",
    victim_damage_emotional = "damage.emotional",

    # Attitude toward police
    Q573 = "attitude.general_trust",
    Q574 = "attitude.trusts_police",
    Q575 = "attitude.trusts_court",
    resp_is_enforcers_familiar = "attitude.knows_an_officer",
    resp_is_crime_record = "attitude.has_crime_record",

    # Social network
    victim_is_told_anyone = "social.told_someone",
    resp_is_living_alone = "social.lives_alone",
    Q801 = "social.communication_relatives_freq",
    Q802 = "social.communication_relatives_way",

    # SES
    resp_is_male = "ses.male",
    Age_Groups = "ses.age_group",
    Q57 = "ses.household_income",
    resp_is_married = "ses.has_partner",
    resp_household_size = "ses.household_size",
    resp_edu = "ses.edu",
    resp_ses = "ses.job",
    Q2003 = "ses.settlement_type",

    # Controls
    Q1005 = "region",
    Q2001 = "federal_district",
    year = "year",

    # Technical
    weight = "weight",
    deflator2024rub = "deflator2024rub"
)


# crime_types <- c(
#     `Cybercrime` = "Киберпреступление",
#     `Attempted cybercrime` = "Покушение на киберпреступление",
#     `Fraud` = "Мошенничество",
#     `Theft` = "Кража",
#     Assault = "Нападение",
#     `Burglary or robbery` = "Грабеж и разбой",
#     Other = "Прочее",
#     `Not enough information` = "Недостаточно информации"
# )

prepare_data <- function(dt) {
    dt <- dt[, intersect(names(vars_of_interest), names(dt)), with = F]

    setnames(dt, function(x) vars_of_interest[x])

    dt[, reported.case_initiated := fifelse(reported.case_initiated == "Да", 1, 0)]

    # Incident
    # dt[, incident.crime_type := fct_recode(incident.crime_type, !!!crime_types)]
    # dt[, incident.crime_type := fct_relevel(incident.crime_type, names(crime_types))]
    # dt[, incident.crime_type := fdroplevels(incident.crime_type)]
    # dt[incident.crime_type %in% c("Fraud", "Theft", "Cybercrime"), incident.crime_type2 := "Property"]
    # dt[incident.crime_type %in% c("Assault", "Burglary or robbery"), incident.crime_type2 := "Violent"]
    # dt[, incident.crime_type2 := as.factor(incident.crime_type2)]

    dt[, incident.last_12_months := ifelse(incident.last_12_months == "Да", 1, 0)]

    dt[, incident.via_phone := fifelse(incident.via_phone == "Да, через телефонный звонок", 1, 0)]
    dt[, incident.via_internet := fifelse(incident.via_internet == "Да, через интернет", 1, 0)]
    if("incident.via_phone_internet" %notin% names(dt)) {
        dt[, incident.via_phone_internet := as.numeric(incident.via_phone | incident.via_internet)]
    } else {
        dt[, incident.via_phone_internet := fifelse(incident.via_phone_internet == "Да, через телефон или интернет", 1, 0)]
    }
    dt[is.na(incident.via_phone_internet), incident.via_phone_internet := 0]

    dt[, incident.violent := fifelse(incident.violent == "Нет", 0, 1)]
    dt[, incident.threat_hurt := fifelse(incident.threat_hurt == "Да", 1, 0)]
    dt[, incident.threat_weapon := fifelse(incident.threat_weapon == "Да", 1, 0)]
    dt[, incident.threat := as.numeric(incident.threat_hurt | incident.threat_weapon)]
    dt[, incident.could_die := ifelse(incident.could_die == "Да", 1, 0)]
    dt[, incident.resisted := fifelse(incident.resisted == "Да", 1, 0)]
    dt[, incident.offender_had_weapon := abs(incident.no_weapon - 1)]
    dt[, incident.no_weapon := NULL]
    dt[, incident.offender_had_weapon := fcoalesce(incident.offender_had_weapon, incident.threat_weapon)]
    dt[, incident.sexualized := fifelse(incident.sexualized == "Да", 1, 0)]
    dt[, incident.repeated := fifelse(incident.N_5_years > 1, 1, 0)]
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
                                                                `Colleague or neighbor` = c("Colleague", "Neighbor")
    )]
    dt[, incident.offender_nonstranger := fifelse(incident.offender_relationship == "Stranger", 0, 1)]

    # face_to_face <- substitute(incident.via_phone_internet == 0 | is.na(incident.via_phone_internet))
    # violent <- substitute((!is.na(damage.physical) & damage.physical != "Нет")
    #                       | (!is.na(incident.violent) & incident.violent == 1))
    # threat <- substitute((!is.na(incident.offender_is_seen) & incident.offender_is_seen == 1)
    #                      & (!is.na(incident.threat) & incident.threat == 1))
    #
    # dt[, incident.crime_class := fifelse(eval(face_to_face)
    #                                      & (eval(violent) | eval(threat)),
    #                                      "Violent", "Property")]
    # dt[, incident.crime_class := as.factor(incident.crime_class)]

    # Damage
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

    dt[, damage.needed_medical_help := fifelse(damage.needed_medical_help == "Да", 1, 0)]


    # Crime class

    dt[incident.via_phone_internet == 1 & (damage.rub > 0 | damage.property_loss == 1), incident.crime_class := "Cybercrime"]
    dt[damage.physical != "No" | incident.violent == 1, incident.crime_class := "Violent"]
    dt[incident.offender_is_seen == 1 & incident.threat == 1 & (damage.rub > 0 | damage.property_loss == 1),
       incident.crime_class := "Violent"]
    dt[incident.offender_had_weapon == 1
       | (incident.offender_is_seen == 1 & incident.threat_weapon == 1),
       incident.crime_class := "Violent"]
    dt[incident.could_die == 1, incident.crime_class := "Violent"]
    dt[incident.resisted == 1, incident.crime_class := "Violent"]
    dt[incident.sexualized == 1, incident.crime_class := "Violent"]
    dt[damage.needed_medical_help == 1, incident.crime_class := "Violent"]
    dt[incident.went_to_hospital == 1, incident.crime_class := "Violent"]
    dt[is.na(incident.crime_class) #incident.crime_class %notin% c("Cybercrime", "Violent")
       & (damage.rub > 0 | damage.property_loss == 1),
       incident.crime_class := "Property"]

    #  Fill NAs for violent incident
    dt[incident.crime_class == "Violent" & is.na(incident.resisted), incident.resisted := 0]
    dt[incident.crime_class == "Violent" & is.na(damage.physical), damage.physical := "No"]


    # SES
    # dt[, ses.gender := factor(ses.male, levels = c(0, 1), labels = c("Female", "Male"))]
    dt[, ses.age_group := fct_relevel(ses.age_group, "35-49")]

    dt[, ses.employed := fifelse(ses.job %in% c("Безработный", "Пенсионер"), 0, 1)]

    dt[, ses.job := fct_recode(ses.job,
                               Unemployed = "Безработный",
                               Pensioner = "Пенсионер",
                               Student = "Студент",
                               Employee = "Служащий",
                               Worker = "Рабочий",
                               Manager = "Руководитель"
    )]

    dt[ses.household_income %in% c("Затрудняюсь ответить", "Отказ от ответа"), ses.household_income := NA]
    dt[!is.na(ses.household_income),
       ses.household_income := fct_collapse(as.factor(ses.household_income),
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

    dt[, ses.household_size_f := fct_collapse(as.factor(ses.household_size),
                                              "3--4" = as.character(3:4),
                                              "5+" = as.character(5:8))]


    dt[, ses.edu := fct_recode(ses.edu,
                               School = "Полное среднее и ниже",
                               Vocational = "Среднее спец-ное/техническое или нач-ное профес-ное",
                               Higher = "Высшее и незаконченное высшее"
    )]

    dt[, ses.higher_edu := fifelse(ses.edu == "Higher", 1, 0)]

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

    # dt[, ses.has_partner := factor(ses.has_partner, c(0, 1), c("No", "Yes"))]

    # Social network
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

    # Attitude
    if ("attitude.general_trust" %in% names(dt)) {
        dt[, attitude.general_trust := fct_collapse(attitude.general_trust,
                                                    `Most people are trustworthy` = "Большинству людей можно доверять",
                                                    `You can’t be too careful/Hard to tell` = c("Нужно быть осторожнее", "Затрудняюсь ответить")
        )]
        dt[, attitude.general_trust := fct_relevel(attitude.general_trust, "You can’t be too careful/Hard to tell")]
    }
    if ("attitude.trusts_police" %in% names(dt)) {
        dt[, attitude.trusts_police := ifelse(attitude.trusts_police %in% c("Полностью согласен", "Скорее согласен"), 1, 0)]
    }
    if ("attitude.trusts_police" %in% names(dt)) {
        dt[, attitude.trusts_court := ifelse(attitude.trusts_court %in% c("Полностью согласен", "Скорее согласен"), 1, 0)]
    }

    # Regions and weights

    dt[region == "Нет данных", region := NA]
    dt[region %in% c("Донецкая Народная Республика", "Луганская Народная Республика",
                     "Запорожская область", "Херсонская область"),
       region := "Новые территории"]
    # dt[, region := fdroplevels(region)]

    # dt[, `:=`(region = as.character(region), federal_district = as.character(federal_district))]
    # dt[region %in% c("г. Москва", "г. Санкт-Петербург"), federal_district := region]
    # dt[region %in% c("Донецкая Народная Республика", "Луганская Народная Республика", "Запорожская область", "Херсонская область"), federal_district := "Новые территории"]
    # dt[, `:=`(region = as.factor(region), federal_district = as.factor(federal_district))]

    dt[, weight := as.numeric(weight)]

    if("year" %in% names(dt)) {
        dt[, year := as.factor(year)]
        }

    return(dt)
}

data_pooled_prep <- prepare_data(data_pooled)
data_2024_prep <- prepare_data(data_2024)
saveRDS(data_pooled_prep, "data/processed/rcvs_pooled_prep.Rds")
saveRDS(data_2024_prep, "data/processed/rcvs_2024_prep.Rds")

victims_pooled <- data_pooled_prep[!is.na(incident.crime_class)]
victims_2024 <- data_2024_prep[!is.na(incident.crime_class)]
saveRDS(victims_2024, "data/processed/victims_2024.Rds")
saveRDS(victims_pooled, "data/processed/victims_pooled.Rds")

# data_pooled_prep[, .N, incident.crime_type2]
# victims_pooled[, .N, incident.crime_class]
# victims_pooled[is.na(incident.crime_class), .N, incident.crime_type]
# ch <- victims_pooled[is.na(incident.crime_class) & incident.crime_type %notin% c("Attempted cybercrime", "Not enough information", "Other"), ]
# ch[, .N, incident.crime_type]
# View(ch[incident.crime_type == "Assault"])
