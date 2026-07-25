library(mice)
library(fixest)
library(data.table)
library(forcats)
library(stringi)
library(ggplot2)
library(patchwork)
library(ggh4x)
library(howManyImputations)
library(kableExtra)
library(future)
g <- glue::glue
source("code/coef_map.R")
options(browser="/usr/bin/google-chrome")


# Dir to store results
dir.create("data/temp/lpm", showWarnings = F)

# Set random seed for reproducibility
set.seed(0)

# Set number of workers for parallel multiple imputations
plan(multisession, workers = 7)

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_2024 <- readRDS("data/processed/victims_2024.Rds")

# Define path outcomes
victims_pooled[, .N, .(social.told_someone, reported.police)
] |> dcast(social.told_someone ~ reported.police)
victims_pooled[, F1P1 := fifelse(social.told_someone == 1 & reported.police == 1, 1, 0)]
victims_pooled[, F0P1 := fifelse(social.told_someone == 0 & reported.police == 1, 1, 0)]
victims_pooled[, F1P0 := fifelse(social.told_someone == 1 & reported.police == 0, 1, 0)]
victims_pooled[, F0P0 := fifelse(social.told_someone == 0 & reported.police == 0, 1, 0)]

victims_2024[, .N, .(social.told_someone, reported.police)
] |> dcast(social.told_someone ~ reported.police)
victims_2024[, F1P1 := fifelse(social.told_someone == 1 & reported.police == 1, 1, 0)]
victims_2024[, F0P1 := fifelse(social.told_someone == 0 & reported.police == 1, 1, 0)]
victims_2024[, F1P0 := fifelse(social.told_someone == 1 & reported.police == 0, 1, 0)]
victims_2024[, F0P0 := fifelse(social.told_someone == 0 & reported.police == 0, 1, 0)]

# List covariates for property and violent crimes
X_ses <- c("ses.settlement_type",
					 "ses.male",
					 "ses.age_group",
					 "ses.household_size_f",
					 "ses.household_income",
					 "ses.higher_edu",
					 "ses.employed",
					 "social.communication_relatives_freq"

)

X_damage_property <-
	c("damage.rub_thnd_binned",
		"damage.emotional")
X_damage_violent <-
	c("damage.rub_thnd_binned",
		"damage.physical",
		"damage.emotional")

X_incident_property <- 	c(
	# "incident.via_phone_internet",
	"incident.self_blame",
	"incident.repeated"
)
X_incident_violent <- c(
	"incident.witness",
	"incident.resisted",
	"incident.offender_had_weapon",
	"incident.went_to_hospital",
	"incident.offender_relationship_grouped",
	"incident.self_blame",
	"incident.sexualized",
	"incident.repeated"
)
X_attitude <- c(#"attitude.general_trust",
	#"attitude.trusts_police",
	"attitude.knows_an_officer",
	"attitude.has_crime_record")

X_property_2024 <- c(X_ses, X_damage_property, X_incident_property, X_attitude)
X_property <- X_property_2024[X_property_2024 %in% names(victims_pooled)]

X_violent_2024 <- c(X_ses, X_damage_violent, X_incident_violent, X_attitude)
X_violent <- X_violent_2024[X_violent_2024 %in% names(victims_pooled)]

FE <- c("region",	"year")

# Regression formulas
fml_property <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | {paste(FE, collapse = '+')}"))

fml_violent <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | {paste(FE, collapse = '+')}"))

fml_property_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))

fml_violent_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))


# Define vars to impute and those to use as predictors
vars_to_impute <- c("ses.household_income", "ses.settlement_type")
vars_to_use <- c(
	# "victim",
	"incident.crime_class",
	"ses.male",
	"ses.age_group",
	"ses.has_partner",
	"ses.edu",
	"ses.household_size",
	"ses.job",
	# "social.lives_alone",
	# "social.communication_relatives_freq",
	"attitude.has_crime_record",
	# "weight",
	"region",
	"year")

sum(complete.cases(victims_pooled[incident.crime_class == "Violent", setdiff(X_violent, vars_to_impute), with = F]))
sum(complete.cases(victims_pooled[incident.crime_class == "Cybercrime", setdiff(X_property, vars_to_impute), with = F]))
sum(complete.cases(victims_pooled[incident.crime_class == "Property", setdiff(X_property, vars_to_impute), with = F]))

victims_2024[, mean(is.na(ses.household_income) | is.na(ses.settlement_type))]

# Wrapper for LPM regression with multiple imputations
pooled_reg_with_mult_imp <- function(dat, fml, crime_class,
																		 vars_to_impute = vars_to_impute,
																		 vars_to_use = vars_to_use) {
	# Prepare arguments for mice
	## 'where' matrix tells which rows and columns hold values that need be imputed,
	## the default is is.na(data) but we need to impute missings only in two columns,
	## so we have to define the matrix
	where_m <- matrix(0, nrow = nrow(dat), ncol = ncol(dat))
	for(v in vars_to_impute) {
		where_m[which(is.na(dat[[v]])), grep(v, names(dat))] <- 1
	}
	colSums(where_m)

	## mice 'predictorMatrix' argument defines which variables (matrix cols) to use
	## in imputation of a given variable (rows), the default behavior is to use
	## all other vars,  we don't want this, so we have to define a custom matrix
	pred_m <- matrix(0, ncol(dat), ncol(dat), dimnames = list(names(dat), names(dat)))
	pred_m[, vars_to_use] <- 1
	diag(pred_m) <- 0


	# Do initial multiple imputations
	message("Parallel multiple imputations: 5")
	n_imp_initial <- 5
	imp1 <- futuremice(dat,
										 m = 5,
										 meth = "rf",
										 predictorMatrix = pred_m,
										 where = where_m,
										 parallelseed = 0)

	# Fit models using imputed data
	message("LPM regressions on 5 completed datasets")
	imp1_dat <- complete(imp1, action = "all")
	fit_reg <- function(dat_, fml, crime_class) {
		feols(fml,
					weights = ~ weight,
					cluster = intersect(c("region", "year"), names(dat)),
					data = dat_[dat_$incident.crime_class == crime_class, ])
	}
	imp1_mdls <- lapply(imp1_dat, fit_reg, fml = fml, crime_class = crime_class)

	# Check if the number of imputations is enough for robust inference
	message("How many imputations are needed for robust inference?")
	n_imp_empirical <- how_many_imputations(imp1_mdls)
	message(n_imp_empirical)
	# print(n_imp_empirical)

	if(n_imp_empirical <= n_imp_initial) {
		## If yes, pool models
		message("Pooling 5 models...")
		pooled_mdl <- pool(imp1_mdls)
	} else {
		## If no, rerun imputations and regressions, then pool
		message("Parallel multiple imputations: ", n_imp_empirical)
		imp2 <- futuremice(dat,
											 m = n_imp_empirical,
											 meth = "rf",
											 predictorMatrix = pred_m,
											 where = where_m,
											 parallelseed = 0)

		imp2_dat <- complete(imp2, action = "all")
		imp2_mdls <- lapply(imp2_dat, fit_reg, fml = fml, crime_class = crime_class)
		message("Pooling ", n_imp_empirical, " models...")
		pooled_mdl <- pool(imp2_mdls)
	}

	# Save information on data
	outcome <- as.character(fml)[2]
	n_obs <- imp1_mdls[[1]]$nobs
	n_class1 <- {
		orig_data <- as.data.table(fixest_data(imp1_mdls[[1]]))
		y_name <- as.character(imp1_mdls[[1]]$fml)[2]
		orig_data[y == 1, .N, env = list(y = y_name)]
	}

	message("Finished")

	# Return pooled model alongside numbers of observations and class 1 cases
	list(mdl = pooled_mdl,
			 info = list(outcome = outcome,
			 						n_obs = n_obs,
			 						n_class1 = n_class1,
			 						len_X = {
			 							rhs <- as.character(fml)[3]
			 							rhs_woFE <- stri_replace_all_regex(rhs, "\\|", "")
			 							stri_count_fixed(rhs_woFE, "+") + 1
			 						}
			 ))

}

# Models of reporting ==========================================================
## Base models
mdl_cyber <- pooled_reg_with_mult_imp(victims_pooled,
																			fml = fml_property,
																			crime_class = "Cybercrime",
																			vars_to_impute = vars_to_impute,
																			vars_to_use = vars_to_use)
saveRDS(mdl_cyber, file = "data/temp/lpm/mdl_cyber.Rds")


mdl_prop <- pooled_reg_with_mult_imp(victims_pooled,
																		 fml = fml_property,
																		 crime_class = "Property",
																		 vars_to_impute = vars_to_impute,
																		 vars_to_use = vars_to_use)
saveRDS(mdl_prop, file = "data/temp/lpm/mdl_prop.Rds")

mdl_viol <- pooled_reg_with_mult_imp(victims_pooled,
																		 fml = fml_violent,
																		 crime_class = "Violent",
																		 vars_to_impute = vars_to_impute,
																		 vars_to_use = vars_to_use)
saveRDS(mdl_viol, file = "data/temp/lpm/mdl_viol.Rds")

m4 <- feols(fml_property_2024,
						data = victims_2024[incident.crime_class == "Property"],
						cluster = c("region"))
summary(m4)

m5 <- feols(fml_property,
						data = victims_pooled[incident.crime_class == "Cybercrime"],
						cluster = c("region", "year"))
summary(m5)

m <- feols(fml_violent,
					 data = victims_pooled[incident.crime_class == "Violent"],
					 cluster = c("region", "year"))
summary(m)

m <- feols(fml_violent,
					 data = victims_pooled[incident.crime_class == "Violent"],
					 cluster = c("region", "year"))
summary(m)

m2 <- feols(fml_violent,
						data = victims_pooled[incident.crime_class == "Violent"],
						cluster = ~ region + year)
summary(m2)

m3 <- feols(fml_violent,
						data = victims_pooled[incident.crime_class == "Violent"],
						cluster = ~ region ^ year)
summary(m3)


# mdl_viol_noregions <- pooled_reg_with_mult_imp(victims_pooled,
# 																							 fml = as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | year")),
# 																							 crime_class = "Violent",
# 																							 vars_to_impute = vars_to_impute,
# 																							 vars_to_use = vars_to_use)


## Base models
mdl_cyber_ignore <- pooled_reg_with_mult_imp(victims_pooled,
																						 fml = as.formula(g("reported.police ~ {paste(X_property, collapse = '+')} | region + year")),
																						 crime_class = "Cybercrime",
																						 vars_to_impute = vars_to_impute,
																						 vars_to_use = vars_to_use)
saveRDS(mdl_cyber_ignore, file = "data/temp/lpm/mdl_cyber_ignore.Rds")

mdl_prop_ignore <- pooled_reg_with_mult_imp(victims_pooled,
																						fml = as.formula(g("reported.police ~ {paste(X_property, collapse = '+')} | region + year")),
																						crime_class = "Property",
																						vars_to_impute = vars_to_impute,
																						vars_to_use = vars_to_use)
saveRDS(mdl_prop_ignore, file = "data/temp/lpm/mdl_prop_ignore.Rds")

mdl_viol_ignore <- pooled_reg_with_mult_imp(victims_pooled,
																						fml = as.formula(g("reported.police ~ {paste(X_violent, collapse = '+')} | region + year")),
																						crime_class = "Violent",
																						vars_to_impute = vars_to_impute,
																						vars_to_use = vars_to_use)
saveRDS(mdl_viol_ignore, file = "data/temp/lpm/mdl_viol_ignore.Rds")

## Models augmented with emotional damage and measure of frequency
## of communication with relatives
mdl_cyber_2024 <- pooled_reg_with_mult_imp(victims_2024,
																					 fml = fml_property_2024,
																					 crime_class = "Cybercrime",
																					 vars_to_impute = vars_to_impute,
																					 vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_cyber_2024, file = "data/temp/lpm/mdl_cyber_2024.Rds")


mdl_prop_2024 <- pooled_reg_with_mult_imp(victims_2024,
																					fml = fml_property_2024,
																					crime_class = "Property",
																					vars_to_impute = vars_to_impute,
																					vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_prop_2024, file = "data/temp/lpm/mdl_prop_2024.Rds")


mdl_viol_2024 <- pooled_reg_with_mult_imp(victims_2024,
																					fml = fml_violent_2024,
																					crime_class = "Violent",
																					vars_to_impute = vars_to_impute,
																					vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_viol_2024, file = "data/temp/lpm/mdl_viol_2024.Rds")

## Models augmented with emotional damage and measure of frequency
## of communication with relatives
mdl_cyber_2024_base <- pooled_reg_with_mult_imp(victims_2024,
																								fml = as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | region")),
																								crime_class = "Cybercrime",
																								vars_to_impute = vars_to_impute,
																								vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_cyber_2024_base, file = "data/temp/lpm/mdl_cyber_2024_base.Rds")


mdl_prop_2024_base <- pooled_reg_with_mult_imp(victims_2024,
																							 fml = as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | region")),
																							 crime_class = "Property",
																							 vars_to_impute = vars_to_impute,
																							 vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_prop_2024_base, file = "data/temp/lpm/mdl_prop_2024_base.Rds")


mdl_viol_2024_base <- pooled_reg_with_mult_imp(victims_2024,
																							 fml = as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | region")),
																							 crime_class = "Violent",
																							 vars_to_impute = vars_to_impute,
																							 vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_viol_2024_base, file = "data/temp/lpm/mdl_viol_2024_base.Rds")

## Models augmented with emotional damage and measure of frequency
## of communication with relatives
mdl_cyber_2024_mediator <- pooled_reg_with_mult_imp(victims_2024,
																										fml = as.formula(g("reported.police ~ social.told_someone * damage.emotional + {paste(X_property, collapse = '+')} | region")),
																										crime_class = "Cybercrime",
																										vars_to_impute = vars_to_impute,
																										vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_cyber_2024_mediator, file = "data/temp/lpm/mdl_cyber_2024_mediator.Rds")


mdl_prop_2024_mediator <- pooled_reg_with_mult_imp(victims_2024,
																									 fml = as.formula(g("reported.police ~ social.told_someone * damage.emotional + {paste(X_property, collapse = '+')} | region")),
																									 crime_class = "Property",
																									 vars_to_impute = vars_to_impute,
																									 vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_prop_2024_mediator, file = "data/temp/lpm/mdl_prop_2024_mediator.Rds")


mdl_viol_2024_mediator <- pooled_reg_with_mult_imp(victims_2024,
																									 fml = as.formula(g("reported.police ~ social.told_someone * damage.emotional + {paste(X_violent, collapse = '+')} | region")),
																									 crime_class = "Violent",
																									 vars_to_impute = vars_to_impute,
																									 vars_to_use = setdiff(vars_to_use, "year"))
saveRDS(mdl_viol_2024_mediator, file = "data/temp/lpm/mdl_viol_2024_mediator.Rds")


modelsummary::modelsummary(list(Cyber = mdl_cyber_2024_mediator$mdl,
																Property = mdl_prop_2024_mediator$mdl,
																Violent = mdl_viol_2024_mediator$mdl),
													 statistic = NULL,
													 coef_map = coef_map2,
													 fmt = modelsummary::fmt_decimal(3),
													 estimate = "{estimate} ({std.error}){stars}",
													 stars = c("*" = .1, "**" = .05, "***" = .01),
													 title = "Wave 2024: emotional damage as mediator")


## Base models on subset of data: reported-but-no-case observations removed
mdl_cyber_caseinit <- pooled_reg_with_mult_imp(victims_pooled[(reported.police == 1 & reported.case_initiated == 1)
																															| (reported.police == 0 & is.na(reported.case_initiated))],
																							 fml = fml_property,
																							 crime_class = "Cybercrime",
																							 vars_to_impute = vars_to_impute,
																							 vars_to_use = vars_to_use)
saveRDS(mdl_cyber_caseinit, file = "data/temp/lpm/mdl_cyber_caseinit.Rds")

mdl_prop_caseinit <- pooled_reg_with_mult_imp(victims_pooled[(reported.police == 1 & reported.case_initiated == 1)
																														 | (reported.police == 0 & is.na(reported.case_initiated))],
																							fml = fml_property,
																							crime_class = "Property",
																							vars_to_impute = vars_to_impute,
																							vars_to_use = vars_to_use)
saveRDS(mdl_prop_caseinit, file = "data/temp/lpm/mdl_prop_caseinit.Rds")

mdl_viol_caseinit <- pooled_reg_with_mult_imp(victims_pooled[
	(reported.police == 1 & reported.case_initiated == 1)
	| (reported.police == 0 & is.na(reported.case_initiated))],
	fml = fml_violent,
	crime_class = "Violent",
	vars_to_impute = vars_to_impute,
	vars_to_use = vars_to_use)
saveRDS(mdl_viol_caseinit, file = "data/temp/lpm/mdl_viol_caseinit.Rds")


# ==============================================================================

path_outcomes <- c("F0P0", "F1P0", "F0P1", "F1P1")
path_outcomes2 <- c("F0P0", "F1P0", "F0P1")

rhs_cyber_path <- g("{paste(X_property, collapse = '+')} | region + year")
paths_mdls_cyber <- lapply(path_outcomes2, function(path) {
	fml <- as.formula(paste(path, "~", rhs_cyber_path))
	dat <- victims_pooled[F1P1 == 1 | get(path) == 1]
	pooled_reg_with_mult_imp(dat,
													 fml = fml,
													 crime_class = "Cybercrime",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = vars_to_use)
})
plot_path_profile(paths_mdls_cyber, "123")
paths_mdls_prop <- lapply(path_outcomes2, function(path) {
	fml <- as.formula(paste(path, "~", rhs_cyber_path))
	dat <- victims_pooled[F1P1 == 1 | get(path) == 1]
	pooled_reg_with_mult_imp(dat,
													 fml = fml,
													 crime_class = "Property",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = vars_to_use)
})
plot_path_profile(paths_mdls_prop, "Property")
rhs_viol_path <- g("{paste(X_violent, collapse = '+')} | region + year")
paths_mdls_viol <- lapply(path_outcomes2, function(path) {
	fml <- as.formula(paste(path, "~", rhs_viol_path))
	dat <- victims_pooled[F1P1 == 1 | get(path) == 1]
	pooled_reg_with_mult_imp(dat,
													 fml = fml,
													 crime_class = "Violent",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = vars_to_use)
})
plot_path_profile(paths_mdls_viol, "Violent")


rhs_cyber_path <- g("{paste(X_property, collapse = '+')} | region + year")
paths_mdls_cyber <- lapply(path_outcomes, function(path) {
	fml <- as.formula(paste(path, "~", rhs_cyber_path))
	pooled_reg_with_mult_imp(victims_pooled,
													 fml = fml,
													 crime_class = "Cybercrime",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = vars_to_use)
})
saveRDS(paths_mdls_cyber, file = "data/temp/lpm/paths_mdls_cyber.Rds")

rhs_prop_path <- g("{paste(X_property, collapse = '+')} | region + year")
paths_mdls_prop <- lapply(path_outcomes, function(path) {
	fml <- as.formula(paste(path, "~", rhs_prop_path))
	pooled_reg_with_mult_imp(victims_pooled,
													 fml = fml,
													 crime_class = "Property",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = vars_to_use)
})
saveRDS(paths_mdls_prop, file = "data/temp/lpm/paths_mdls_prop.Rds")

rhs_viol_path <- g("{paste(X_violent, collapse = '+')} | region + year")
paths_mdls_viol <- lapply(path_outcomes, function(path) {
	fml <- as.formula(paste(path, "~", rhs_viol_path))
	pooled_reg_with_mult_imp(victims_pooled,
													 fml = fml,
													 crime_class = "Violent",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = vars_to_use)
})
saveRDS(paths_mdls_viol, file = "data/temp/lpm/paths_mdls_viol.Rds")


rhs_cyber_path_2024 <- g("{paste(X_property_2024, collapse = '+')} | region")
paths_mdls_cyber_2024 <- lapply(path_outcomes, function(path) {
	fml <- as.formula(paste(path, "~", rhs_cyber_path_2024))
	pooled_reg_with_mult_imp(victims_2024,
													 fml = fml,
													 crime_class = "Cybercrime",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = setdiff(vars_to_use, "year"))
})
saveRDS(paths_mdls_cyber_2024, file = "data/temp/lpm/paths_mdls_cyber_2024.Rds")


rhs_prop_path_2024 <- g("{paste(X_property_2024, collapse = '+')} | region")
paths_mdls_prop_2024 <- lapply(path_outcomes, function(path) {
	fml <- as.formula(paste(path, "~", rhs_prop_path_2024))
	pooled_reg_with_mult_imp(victims_2024,
													 fml = fml,
													 crime_class = "Property",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = setdiff(vars_to_use, "year"))
})
saveRDS(paths_mdls_prop_2024, file = "data/temp/lpm/paths_mdls_prop_2024.Rds")


rhs_viol_path_2024 <- g("{paste(X_violent_2024, collapse = '+')} | region")
paths_mdls_viol_2024 <- lapply(path_outcomes, function(path) {
	fml <- as.formula(paste(path, "~", rhs_viol_path_2024))
	pooled_reg_with_mult_imp(victims_2024,
													 fml = fml,
													 crime_class = "Violent",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = setdiff(vars_to_use, "year"))
})
saveRDS(paths_mdls_viol_2024, file = "data/temp/lpm/paths_mdls_viol_2024.Rds")


plot_path_profile <- function(mdls_list, title) {
	mdls_list <- paths_mdls_cyber
	pl_data <-
		rbindlist(
			lapply(mdls_list, function(x) {
				d <- as.data.table(summary(x$mdl))
				d[, path := x$info$outcome]
				inv_coef_map <- names(coef_map)
				names(inv_coef_map) <- coef_map
				coef_map_present <- inv_coef_map[inv_coef_map %in% d$term]
				d[, term := fct_rev(fct_relevel(as.factor(term), coef_map_present))]
				d[, term := fct_recode(term, !!!inv_coef_map)]
				d
			})
		)
	pl_data[, CIh := estimate + std.error * 1.96]
	pl_data[, CIl := estimate - std.error * 1.96]

	anno <-
		rbindlist(
			lapply(mdls_list, function(x) {
				data.table(path = x$info$outcome,
									 anno = paste0("N: ", x$info$n_obs,
									 							"\nCl. 1: ", x$info$n_class1,
									 							"\nR2: ", round(pool.r.squared(x$mdl)[1], 2)),
									 class1 = paste0(x$info$n_class1, "/", x$info$n_obs),
									 r2 = round(pool.r.squared(x$mdl)[1], 2),
									 ymin = {
									 	mdl_sum <- summary(x$mdl)
									 	min(summary(mdl_sum$estimate - mdl_sum$std.error * 1.96))
									 	},
									 ymax = {
									 	mdl_sum <- summary(x$mdl)
									 	max(summary(mdl_sum$estimate - mdl_sum$std.error * 1.96))
									 },
									 xmax = x$info$len_X
									)
			})
		)

	# f("F1P0")

	f <- function(p) {
		d <- anno[path == p, ]
		g("{p}\n{d$class1} | R2: {d$r2}")
	}

	f("F1P0")

	# print(
		ggplot(pl_data, aes(term, estimate, alpha = ifelse(p.value <= 0.1, "sign", "nonsign"))) +
			geom_point() +
			geom_errorbar(aes(ymin = CIl, ymax = CIh), width = 0) +
			scale_alpha_manual(breaks = c("sign", "nonsign"), values = c(1, .2)) +
			geom_hline(yintercept = 0, linetype = "dashed") +
			# geom_text(data = anno,
			# 					inherit.aes = F,
			# 					aes(x = 2.5, y = ymin, label = anno),
			# 					# aes(x = xmax - 2.5, y = ymax, label = anno),
			# 					size = 2.5,
			# 					hjust = 0) +
			coord_flip() +
			facet_wrap(~path, nrow = 1#, #scales = "free_x"#,
								# labeller = labeller(path = f)
			) +
			theme_bw() +
			theme(legend.position = "none",
						axis.text.y = element_text(family = "mono", face = "bold"),
						plot.margin = unit(c(0,.5,0,0), "cm")) +
			labs(y = "Estimate with 95% CI", x = "", title = title)
	# )

}
plot_path_profile(paths_mdls_cyber, "Panel A. Cybercrime")


paths_mdls_cyber <- readRDS("data/temp/lpm/paths_mdls_cyber.Rds")
pdf(file = "paper/figures/paths_cyber.pdf", width = 8, height = 3.5)
plot_path_profile(paths_mdls_cyber, "Panel A. Cybercrime")
dev.off()

paths_mdls_prop <- readRDS("data/temp/lpm/paths_mdls_prop.Rds")
pdf(file = "paper/figures/paths_prop.pdf", width = 8, height = 3.5)
plot_path_profile(paths_mdls_prop, "Panel B. Property crime")
dev.off()

paths_mdls_viol <- readRDS("data/temp/lpm/paths_mdls_viol.Rds")
pdf(file = "paper/figures/paths_viol.pdf", width = 8, height = 4)
plot_path_profile(paths_mdls_viol, "Panel C. Violent crime")
dev.off()

cc <- plot_path_profile(paths_mdls_cyber, "Panel A. Cybercrime")
pc <- plot_path_profile(paths_mdls_prop, "Panel B. Property crime")
vc <- plot_path_profile(paths_mdls_viol, "Panel C. Violent crime")

pdf(file = "paper/figures/paths_combined.pdf", width = 6, height = 11)
cc / pc / vc + plot_layout(heights = c(1, 1, 1.5)) &
	theme(text = element_text(size = 7))
dev.off()

paths_mdls_cyber_2024 <- readRDS("data/temp/lpm/paths_mdls_cyber_2024.Rds")
paths_mdls_prop_2024 <- readRDS("data/temp/lpm/paths_mdls_prop_2024.Rds")
paths_mdls_viol_2024 <- readRDS("data/temp/lpm/paths_mdls_viol_2024.Rds")

cc <- plot_path_profile(paths_mdls_cyber_2024, "Panel A. Cybercrime")
pc <- plot_path_profile(paths_mdls_prop_2024, "Panel B. Property crime")
vc <- plot_path_profile(paths_mdls_viol_2024, "Panel C. Violent crime")

pdf(file = "paper/figures/paths_combined_2024.pdf", width = 6, height = 11)
cc / pc / vc + plot_layout(heights = c(1, 1, 1.5)) &
	theme(text = element_text(size = 7))
dev.off()

rhs_cyber_path_2024_r <- g("{paste(X_property, collapse = '+')} | region")
paths_mdls_cyber_2024_r <- lapply(path_outcomes, function(path) {
	fml <- as.formula(paste(path, "~", rhs_cyber_path_2024_r))
	pooled_reg_with_mult_imp(victims_2024,
													 fml = fml,
													 crime_class = "Cybercrime",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = setdiff(vars_to_use, "year"))
})
saveRDS(paths_mdls_cyber_2024_r, file = "data/temp/lpm/paths_mdls_cyber_2024_r.Rds")


rhs_prop_path_2024_r <- g("{paste(X_property, collapse = '+')} | region")
paths_mdls_prop_2024_r <- lapply(path_outcomes, function(path) {
	fml <- as.formula(paste(path, "~", rhs_prop_path_2024_r))
	pooled_reg_with_mult_imp(victims_2024,
													 fml = fml,
													 crime_class = "Property",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = setdiff(vars_to_use, "year"))
})
saveRDS(paths_mdls_prop_2024_r, file = "data/temp/lpm/paths_mdls_prop_2024_r.Rds")


rhs_viol_path_2024_r <- g("{paste(X_violent, collapse = '+')} | region")
paths_mdls_viol_2024_r <- lapply(path_outcomes, function(path) {
	fml <- as.formula(paste(path, "~", rhs_viol_path_2024_r))
	pooled_reg_with_mult_imp(victims_2024,
													 fml = fml,
													 crime_class = "Violent",
													 vars_to_impute = vars_to_impute,
													 vars_to_use = setdiff(vars_to_use, "year"))
})
saveRDS(paths_mdls_viol_2024_r, file = "data/temp/lpm/paths_mdls_viol_2024_r.Rds")



cc <- plot_path_profile(paths_mdls_cyber_2024_r, "Panel A. Cybercrime")
pc <- plot_path_profile(paths_mdls_prop_2024_r, "Panel B. Property crime")
vc <- plot_path_profile(paths_mdls_viol_2024_r, "Panel C. Violent crime")

pdf(file = "paper/figures/paths_combined_2024_r.pdf", width = 6, height = 11)
cc / pc / vc + plot_layout(heights = c(1, 1, 1.5)) &
	theme(text = element_text(size = 7))
dev.off()









pdf(file = "paper/figures/paths_cyber.pdf", width = 8, height = 8)
plot_path_profile(paths_mdls_cyber)
dev.off()

pdf(file = "paper/figures/paths_prop.pdf", width = 7, height = 8)
plot_path_profile(mdls_lpm_property)
dev.off()

pdf(file = "paper/figures/paths_viol.pdf", width = 7, height = 9)
plot_path_profile(mdls_lpm_violent)
dev.off()

pdf(file = "paper/figures/paths_cyber_2024.pdf", width = 7, height = 8)
plot_path_profile(mdls_lpm_cybercrime_2024)
dev.off()

pdf(file = "paper/figures/paths_prop_2024.pdf", width = 7, height = 8.5)
plot_path_profile(mdls_lpm_property_2024)
dev.off()

pdf(file = "paper/figures/paths_viol_2024.pdf", width = 7, height = 9.5)
plot_path_profile(mdls_lpm_violent_2024)
dev.off()





# ==============================================================================

mdl_cyber <- readRDS( "data/temp/lpm/mdl_cyber.Rds")
mdl_prop <- readRDS( "data/temp/lpm/mdl_prop.Rds")
mdl_viol <- readRDS( "data/temp/lpm/mdl_viol.Rds")

modelsummary::modelsummary(list(Cyber = mdl_cyber$mdl,
																Property = mdl_prop$mdl,
																Violent = mdl_viol$mdl),
													 statistic = NULL,
													 coef_map = coef_map,
													 fmt = modelsummary::fmt_decimal(3),
													 estimate = "{estimate} ({std.error}){stars}",
													 stars = c("*" = .1, "**" = .05, "***" = .01),
													 title = "Pooled data")

modelsummary::modelsummary(list(Cyber = mdl_cyber_ignore$mdl,
																Property = mdl_prop_ignore$mdl,
																Violent = mdl_viol_ignore$mdl),
													 statistic = NULL,
													 coef_map = coef_map,
													 fmt = modelsummary::fmt_decimal(3),
													 estimate = "{estimate} ({std.error}){stars}",
													 stars = c("*" = .1, "**" = .05, "***" = .01),
													 title = "Pooled data: ignoring social network factor")

mdl_cyber_2024 <- readRDS( "data/temp/lpm/mdl_cyber_2024.Rds")
mdl_prop_2024 <- readRDS( "data/temp/lpm/mdl_prop_2024.Rds")
mdl_viol_2024 <- readRDS( "data/temp/lpm/mdl_viol_2024.Rds")

modelsummary::modelsummary(list(Cyber = mdl_cyber_2024_base$mdl,
																Property = mdl_prop_2024_base$mdl,
																Violent = mdl_viol_2024_base$mdl),
													 statistic = NULL,
													 coef_map = coef_map,
													 fmt = modelsummary::fmt_decimal(3),
													 estimate = "{estimate} ({std.error}){stars}",
													 stars = c("*" = .1, "**" = .05, "***" = .01),
													 title = "Wave 2024: base model")

modelsummary::modelsummary(list(Cyber = mdl_cyber_2024$mdl,
																Property = mdl_prop_2024$mdl,
																Violent = mdl_viol_2024$mdl),
													 statistic = NULL,
													 coef_map = coef_map,
													 fmt = modelsummary::fmt_decimal(3),
													 estimate = "{estimate} ({std.error}){stars}",
													 stars = c("*" = .1, "**" = .05, "***" = .01),
													 title = "Wave 2024: augmented model")

mdl_cyber_caseinit <- readRDS( "data/temp/lpm/mdl_cyber_caseinit.Rds")
mdl_prop_caseinit <- readRDS( "data/temp/lpm/mdl_prop_caseinit.Rds")
mdl_viol_caseinit <- readRDS( "data/temp/lpm/mdl_viol_caseinit.Rds")

modelsummary::modelsummary(list(Cyber = mdl_cyber_caseinit$mdl,
																Property = mdl_prop_caseinit$mdl,
																Violent = mdl_viol_caseinit$mdl),
													 statistic = NULL,
													 coef_map = coef_map,
													 fmt = modelsummary::fmt_decimal(3),
													 estimate = "{estimate} ({std.error}){stars}",
													 stars = c("*" = .1, "**" = .05, "***" = .01),
													 title = "Pooled data: reported-no-case observations dropped")

ggplot(victims_pooled[damage.rub < 1e6 & damage.rub > 0],
			 aes(damage.rub, fill = as.factor(reported.police))) +
	geom_histogram(bins = 30, alpha = .5) +
	# geom_density() +
	xlim(c(0, 2e4)) +
	facet_wrap(~reported.police)

















prop_grid <- seq(0.05, 0.5, by = 0.05)
n_rep   <- 50

sim <- lapply(prop_grid, function(p) {
	print(p)
	m_list <- lapply(1:n_rep, function(x) {
		d   <- victims_pooled[incident.crime_class == "Violent"]
		idx <- which(d$social.told_someone == 1 & d$reported.police == 1)
		drop <- sample(idx, floor(p * length(idx)))
		d   <- d[-drop, ]
		m <- pooled_reg_with_mult_imp(d,
																	fml = fml_violent,
																	crime_class = "Violent",
																	vars_to_impute = vars_to_impute,
																	vars_to_use = vars_to_use)
		m
	})
	m_pooled <- mice::pool(m_list)
	res <- as.data.table(summary(m_pooled)[1, c("estimate", "std.error")])
	res[, prop_dropped := p]
})
sim <- rbindlist(sim)
sim[, cil := estimate - std.error*1.96]
sim[, cih := estimate + std.error*1.96]

ggplot(sim, aes(prop_dropped, estimate)) +
	geom_line() +
	geom_ribbon(aes(ymin = cil, ymax = cih), alpha = .1) +
	geom_hline(yintercept = 0)








d <- mtcars

set.seed(0)
m1 <- feols(mpg ~ cyl, data = d[-sample(1:nrow(d), 5), ])
set.seed(1)
m2 <- feols(mpg ~ cyl, data = d[-sample(1:nrow(d), 10), ])
mice::pool(list(m1, m2))
