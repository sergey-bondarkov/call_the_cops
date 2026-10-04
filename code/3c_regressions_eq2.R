#library(fixest)
library(data.table)
library(WeMix)
library(forcats)
library(stringi)
library(ggplot2)
library(patchwork)
library(ggh4x)
library(future)
g <- glue::glue

setwd(Sys.getenv("CALL_THE_COPS"))

source("code/coef_map.R")
source("code/regression_helpers/X_definition.R")
source("code/helpers/functions.R")

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_pooled[, reg_weight := 1]
victims_pooled_long <- melt(victims_pooled, measure.vars = patterns("F.P."), 
                            variable.name = "alt", value.name = "chosen")

# Regression formulas
fml_property <- as.formula(g("chosen ~ alt + alt : ({paste(c(X_property, 'year'), collapse = '+')}) + id + (1 | region)"))
fml_violent <- as.formula(g("chosen ~ alt + alt : ({paste(c(X_violent, 'year'), collapse = '+')}) + (1 | id) + (1 | region)"))

dat_cyber_pooled_long <- victims_pooled_long[incident.crime_class == "Cybercrime", c("chosen", "alt", "region", "year", "id", X_property, "weight.reg_scaled", "reg_weight"), with = F]
dat_cyber_pooled_long <- dat_cyber_pooled_long[complete.cases(dat_cyber_pooled_long[, ..X_property])]
dat_cyber_pooled_long[, region := fdroplevels(region)]
dat_cyber_pooled_long[, obs_weight := 1]

dat_cyber_pooled_long <- victims_pooled_long[incident.crime_class == "Cybercrime", c("chosen", "alt", "region", "year", "id", X_property, "weight.reg_scaled", "reg_weight"), with = F]
dat_cyber_pooled_long <- dat_cyber_pooled_long[complete.cases(dat_cyber_pooled_long[, ..X_property])]
dat_cyber_pooled_long[, region := fdroplevels(region)]
dat_cyber_pooled_long[, obs_weight := 1]

dat_cyber_pooled_long <- victims_pooled_long[incident.crime_class == "Cybercrime", c("chosen", "alt", "region", "year", "id", X_property, "weight.reg_scaled", "reg_weight"), with = F]
dat_cyber_pooled_long <- dat_cyber_pooled_long[complete.cases(dat_cyber_pooled_long[, ..X_property])]
dat_cyber_pooled_long[, region := fdroplevels(region)]
dat_cyber_pooled_long[, obs_weight := 1]
dat_cyber_pooled_long[, id := as.factor(id)]

mlm_cyber <- mix(fml_property, 
                 data = dat_cyber_pooled_long,
                 family  = poisson(), # Explicitly declare Poisson for the trick
                 weights = c( "weight.reg_scaled", "reg_weight"),
                 cWeights = T, 
                 nQuad = 1)
