library(mice)
library(lme4)
library(modelsummary)
library(data.table)
# library(forcats)
# library(stringi)
# library(ggplot2)
# library(patchwork)
# library(ggh4x)
library(howManyImputations)
library(future)
library(future.apply)
g <- glue::glue

setwd("~/Projects/IRL/RCVS/call_the_cops")

source("code/coef_map.R")
source("code/regression_helpers/X_definition.R")
source("code/regression_helpers/functions.R")

# options(browser="/usr/bin/google-chrome")

# Dir to store results
out <- file.path("data", "temp", "ml")
dir.create(out, showWarnings = F, recursive = T)

# Set random seed for reproducibility
set.seed(0)

n_workers <- floor(parallelly::availableCores() * 2/3)
plan(multisession, workers = n_workers)

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_2024 <- readRDS("data/processed/victims_2024.Rds")


X_violent

fml_violent <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} + year + (1|region)"))
fml_property <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} + year + (1|region)"))
subsample <-  victims_pooled[incident.crime_class == "Violent"]
m1 <- glmer(fml_violent, weights = subsample$weight, family = binomial, nAGQ = 10, data = subsample)
summary(m1)

modelsummary(m1, output = "html", exponentiate = T, stars = T)

subsample <-  victims_pooled[incident.crime_class == "Property"]
m_prop <- glmer(fml_property, weights = subsample$weight, family = binomial, nAGQ = 10, data = subsample)
summary(m_prop)


fml_property_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} + (1|region)"))
subsample <-  victims_2024[incident.crime_class == "Cybercrime"]
m_cyber_2024 <- glmer(fml_property_2024, weights = subsample$weight, family = binomial, nAGQ = 10, data = subsample)
summary(m_cyber_2024)

subsample[, .N, region][, table(N)]

ggplot(subsample[, .N, region], aes(N)) +
    geom_histogram() +
    stat_summary(fun = "mean")
