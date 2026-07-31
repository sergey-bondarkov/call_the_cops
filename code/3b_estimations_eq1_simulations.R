library(mice)
library(fixest)
library(data.table)
library(forcats)
library(stringi)
library(ggplot2)
library(patchwork)
library(ggh4x)
library(howManyImputations)
library(parallel)
library(future)
library(future.apply)
library(purrr)
library(pbapply)
g <- glue::glue
source("code/coef_map.R")
source("code/regression_helpers/X_definition.R")
source("code/regression_helpers/functions.R")

# options(browser="/usr/bin/google-chrome")

# Dir to store results
dir.create("data/temp/lpm", showWarnings = F, recursive = T)

# Set random seed for reproducibility
set.seed(0)
# Set number of workers for parallel multiple imputations
# n_workers <- 28

# plan(list(tweak(multisession, workers = 5), tweak(multisession, workers = I(5))))

cl <- makeCluster(4)

clusterEvalQ(cl,
	{
		library(mice)
		library(fixest)
		library(data.table)
		# library(forcats)
		# library(stringi)
		# library(ggplot2)
		# library(patchwork)
		# library(ggh4x)
		library(howManyImputations)
		# library(parallel)
		library(future)
		library(purrr)
		library(pbapply)
		g <- glue::glue
		source("code/coef_map.R")
		source("code/regression_helpers/X_definition.R")
		source("code/regression_helpers/functions.R")

		# plan(list(tweak(multisession, workers = 4)))
		plan(multisession, workers = 4)

	}
)

# Load data
victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_2024 <- readRDS("data/processed/victims_2024.Rds")

# Regression formulas
fml_property <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | {paste(FE, collapse = '+')}"))
fml_violent <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | {paste(FE, collapse = '+')}"))

fml_property_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))
fml_violent_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))

# Define vars to impute and those to use as predictors
vars_to_impute <- c("ses.household_income", "ses.settlement_type")
vars_to_use <- c(
	"incident.crime_class",
	"ses.male",
	"ses.age_group",
	"ses.has_partner",
	"ses.edu",
	"ses.household_size",
	"ses.job",
	"attitude.has_crime_record",
	"weight",
	"region",
	"year")

props <- seq(0.1, 0.5, by = 0.05)
n_rep <- 50

clusterExport(cl, c("victims_pooled", "fml_property", "fml_violent", "vars_to_impute", "vars_to_use"))


# library(parallel)
# cl <- makeCluster(10)
# clusterEvalQ(cl, {
# 	library(mice)
# 	library(fixest)
# 	library(data.table)
# 	library(howManyImputations)
# 	library(future)
# 	g <- glue::glue
# 	# source("code/coef_map.R")
# 	source("code/regression_helpers/X_definition.R")
# 	source("code/regression_helpers/functions.R")
# })
# clusterExport(cl, c("victims_pooled", "fml_property", "fml_violent", "vars_to_impute", "vars_to_use"))

# plan(list(tweak(multisession, workers = 2),
# 					tweak(multisession, workers = I(2))))

cyber_sims <-
	rbindlist(
		lapply(props, function(p) {
			m_list <- future_lapply(X = 1:n_rep,
												FUN = function(x) {
													d   <- victims_pooled[victims_pooled$incident.crime_class == "Cybercrime", ]
													idx <- which(d$social.told_someone == 1 & d$reported.police == 1)
													drop <- sample(idx, floor(p * length(idx)))
													d   <- d[-drop, ]
													dat <- d
													# m <- feols(fml_property, weights = ~weight, cluster = c("region", "year"),
													# 					 data = d)
													pooled_reg_with_mult_imp2(d,
																										fml = fml_property,
																										crime_class = "Cybercrime",
																										vars_to_impute = vars_to_impute,
																										vars_to_use = vars_to_use)
													m

												}#,
												# cl = cl
												#)
			)
			m_pooled <- pool(flatten(m_list))
			res <- as.data.table(summary(m_pooled)[1, c("estimate", "std.error")])
			res[, prop_dropped := p]
			res
		})
	)
cyber_sims[, cil := estimate - std.error*1.96]
cyber_sims[, cih := estimate + std.error*1.96]
fwrite(cyber_sims, "data/temp/lpm/cyber_sims.csv")

ggplot(cyber_sims, aes(prop_dropped, estimate)) +
	# geom_line() +
	geom_point() +
	geom_errorbar(aes(ymin = estimate - 1.65*std.error, ymax = estimate + 1.65*std.error), col = "red") +
	geom_errorbar(aes(ymin = cil, ymax = cih)) +
	# geom_ribbon(aes(ymin = cil, ymax = cih), alpha = .1) +
	geom_hline(yintercept = 0)

prop_sims <-
	rbindlist(
		lapply(props, function(p) {
			m_list <- pbapply::pblapply(cl = cl,
																	X = 1:n_rep,
																	FUN = function(x) {
																		d   <- victims_pooled[victims_pooled$incident.crime_class == "Property", ]
																		idx <- which(d$social.told_someone == 1 & d$reported.police == 1)
																		drop <- sample(idx, floor(p * length(idx)))
																		d   <- d[-drop, ]
																		# m <- feols(fml_property, weights = ~weight, cluster = c("region", "year"),
																		# 					 data = d)
																		pooled_reg_with_mult_imp2(d,
																															fml = fml_property,
																															crime_class = "Property",
																															vars_to_impute = vars_to_impute,
																															vars_to_use = vars_to_use)
																		# m
																	})
			m_pooled <- pool(flatten(m_list))
			res <- as.data.table(summary(m_pooled)[1, c("estimate", "std.error")])
			res[, prop_dropped := p]
			res
		}))
prop_sims[, cil := estimate - std.error*1.96]
prop_sims[, cih := estimate + std.error*1.96]
fwrite(prop_sims, "data/temp/lpm/prop_sims.csv")

ggplot(prop_sims, aes(prop_dropped, estimate)) +
	geom_line() +
	geom_ribbon(aes(ymin = cil, ymax = cih), alpha = .1) +
	geom_hline(yintercept = 0)

viol_sims <-
	rbindlist(
		lapply(props, function(p) {
			m_list <- pbapply::pblapply(cl = cl,
																	X = 1:n_rep,
																	FUN = function(x) {
																		d   <- victims_pooled[victims_pooled$incident.crime_class == "Violent", ]
																		idx <- which(d$social.told_someone == 1 & d$reported.police == 1)
																		drop <- sample(idx, floor(p * length(idx)))
																		d   <- d[-drop, ]
																		# m <- feols(fml_property, weights = ~weight, cluster = c("region", "year"),
																		# 					 data = d)
																		pooled_reg_with_mult_imp2(d,
																															fml = fml_violent,
																															crime_class = "Violent",
																															vars_to_impute = vars_to_impute,
																															vars_to_use = vars_to_use)
																		# m
																	})
			m_pooled <- pool(flatten(m_list))
			res <- as.data.table(summary(m_pooled)[1, c("estimate", "std.error")])
			res[, prop_dropped := p]
			res
		}))
viol_sims[, cil := estimate - std.error*1.96]
viol_sims[, cih := estimate + std.error*1.96]
fwrite(viol_sims, "data/temp/lpm/viol_sims.csv")

ggplot(viol_sims, aes(prop_dropped, estimate)) +
	geom_line() +
	geom_ribbon(aes(ymin = cil, ymax = cih), alpha = .1) +
	geom_hline(yintercept = 0)
cyber_sims <- fread("data/temp/lpm/cyber_sims.csv")
comb <- rbindlist(list(Cybercrime = cyber_sims, Property = prop_sims, Violent = viol_sims), idcol = "crime_class")

pdf("paper/figures/sims.pdf", width = 6, height = 2)
ggplot(comb, aes(prop_dropped, estimate)) +
	geom_point() + geom_line() +
	geom_ribbon(aes(ymin = cil, ymax = cih), linetype = "dashed", alpha = .1) +
	geom_hline(yintercept = 0) +
	labs(x = "Share of F1P1 cases dropped", y = "Estimate with 95% CI") +
	facet_wrap(~crime_class, nrow = 1) +
	theme_bw() +
	theme(panel.grid.minor.x = element_blank())
dev.off()
