library(data.table)

# Full sample
orig_pooled <- readRDS(file.path("data", "processed", "rcvs_pooled_prep.Rds"))
orig_pooled[, .N]
# 42572

# Victims only
victims_pooled <- orig_pooled[victim == "Да"]
victims_pooled[, .N]
# 8252

# Crime class defined
victims_pooled <- victims_pooled[!is.na(incident.crime_class)]
victims_pooled[, .N]
# 6194

# Split sample by crime class
victims_pooled[, .N, incident.crime_class]
# Cybercrime: 2020
# Property: 2814
# Violent: 1360
victims_cyber <- victims_pooled[incident.crime_class == "Cybercrime"]
victims_prop <- victims_pooled[incident.crime_class == "Property"]
victims_viol <- victims_pooled[incident.crime_class == "Violent"]

# Load predictor vectors (Cybercrime and Property crime share the same list of predictors)
source(file.path("code", "regression_helpers", "X_definition.R"))

# Caclulate missings by variable
missings_cyber <- victims_cyber[, sapply(.SD, function(x) sum(is.na(x))), .SDcols = X_property]
(missings_cyber <- missings_cyber[missings_cyber != 0])
      # ses.settlement_type      ses.household_size_f
                       # 54                         8
     # ses.household_income            ses.higher_edu
                       # 54                         4
 # attitude.familiar_police attitude.has_crime_record
                        # 3                         1

missings_prop <- victims_prop[, sapply(.SD, function(x) sum(is.na(x))), .SDcols = X_property]
(missings_prop <- missings_prop[missings_prop != 0])
      # ses.settlement_type             ses.age_group
                       # 92                         2
     # ses.household_size_f      ses.household_income
                       # 20                        87
           # ses.higher_edu  attitude.familiar_police
                        # 1                         1
# attitude.has_crime_record
                        # 3

missings_viol <- victims_viol[, sapply(.SD, function(x) sum(is.na(x))), .SDcols = X_violent]
(missings_viol <- missings_viol[missings_viol != 0])
                   # ses.settlement_type                          ses.age_group
                                    # 39                                      1
                  # ses.household_size_f                   ses.household_income
                                     # 2                                     55
                        # ses.higher_edu           incident.offender_had_weapon
                                     # 5                                      7
# incident.offender_relationship_grouped                    incident.sexualized
                                   # 161                                     10
              # attitude.familiar_police              attitude.has_crime_record
                                     # 4                                      3

# We will impute only two variables: Settlement type and Household income
# They are frequently missing and seem reasonably trivial to predict
#vars_to_impute <- c("ses.settlement_type", "ses.household_income")

# Share of data that would be lost due to missings in these variables
victims_cyber[is.na(ses.settlement_type) | is.na(ses.household_income), .N / nrow(victims_cyber)]
# 0.0509901
victims_prop[is.na(ses.settlement_type) | is.na(ses.household_income), .N / nrow(victims_prop)]
# 0.06183369
victims_viol[is.na(ses.settlement_type) | is.na(ses.household_income), .N / nrow(victims_viol)]
# 0.06691176

# Iterative listwise deletion
prev <- victims_cyber[, .N]
#for(v in setdiff(names(sort(missings_cyber, decreasing = T)), vars_to_impute)) {
for(v in names(sort(missings_cyber, decreasing = T))) {
           victims_cyber <- victims_cyber[!is.na(get(v))]
           message(c(v, ": ", missings_cyber[v], " | ", prev - victims_cyber[, .N], " | ", victims_cyber[, .N]))
           prev <- victims_cyber[, .N]
}
# Var | Miss. | Loss | N
#ses.settlement_type: 54 | 54 | 1966
#ses.household_income: 54 | 49 | 1917
#ses.household_size_f: 8 | 5 | 1912
#ses.higher_edu: 4 | 2 | 1910
#attitude.familiar_police: 3 | 3 | 1907
#attitude.has_crime_record: 1 | 1 | 1906

prev <- victims_prop[, .N]
#for(v in setdiff(names(sort(missings_prop, decreasing = T)), vars_to_impute)) {
for(v in names(sort(missings_prop, decreasing = T))) {
           victims_prop <- victims_prop[!is.na(get(v))]
           message(c(v, ": ", missings_prop[v], " | ", prev - victims_prop[, .N], " | ", victims_prop[, .N]))
           prev <- victims_prop[, .N]
}
# Var | Miss. | Loss | N
#ses.settlement_type: 92 | 92 | 2722
#ses.household_income: 87 | 82 | 2640
#ses.household_size_f: 20 | 16 | 2624
#attitude.has_crime_record: 3 | 3 | 2621
#ses.age_group: 2 | 2 | 2619
#ses.higher_edu: 1 | 0 | 2619
#attitude.familiar_police: 1 | 0 | 2619

prev <- victims_viol[, .N]
#for(v in setdiff(names(sort(missings_viol, decreasing = T)), vars_to_impute)) {
for(v in names(sort(missings_viol, decreasing = T))) {
           victims_viol <- victims_viol[!is.na(get(v))]
           message(c(v, ": ", missings_viol[v], " | ", prev - victims_viol[, .N], " | ", victims_viol[, .N]))
           prev <- victims_viol[, .N]
}
# Var | Miss. | Loss | N
#incident.offender_relationship_grouped: 161 | 161 | 1199
#ses.household_income: 55 | 46 | 1153
#ses.settlement_type: 39 | 34 | 1119
#incident.sexualized: 10 | 6 | 1113
#incident.offender_had_weapon: 7 | 0 | 1113
#ses.higher_edu: 5 | 2 | 1111
#attitude.familiar_police: 4 | 2 | 1109
#attitude.has_crime_record: 3 | 1 | 1108
#ses.household_size_f: 2 | 2 | 1106
#ses.age_group: 1 | 1 | 1105


