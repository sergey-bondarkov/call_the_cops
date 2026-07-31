library(data.table)
library(ggplot2)
library(forcats)
library(fixest)
library(mice)
library(gt)
library(modelsummary)
library(ggh4x)
g <- glue::glue
options(browser="/usr/bin/google-chrome")
source("code/coef_map.R")

# ================================================================================
# Load and prepare data

victims_pooled <- readRDS("data/processed/victims_pooled.Rds")
victims_2024 <- readRDS("data/processed/victims_2024.Rds")

# Outcomes
victims_pooled[, .N, .(social.told_someone, reported.police)
] |> dcast(social.told_someone ~ reported.police)
victims_pooled[, F1P1 := fifelse(social.told_someone == 1 & reported.police == 1, 1, 0)]
victims_pooled[, F0P1 := fifelse(social.told_someone == 0 & reported.police == 1, 1, 0)]
victims_pooled[, F1P0 := fifelse(social.told_someone == 1 & reported.police == 0, 1, 0)]
victims_pooled[, F0P0 := fifelse(social.told_someone == 0 & reported.police == 0, 1, 0)]

victims_2024[, .N, .(social.told_someone, reported.police)
] |> dcast(social.told_someone ~ reported.police)
victims_2024[incident.crime_class == "Violent",
							 .N, .(social.told_someone, reported.police)
] |> dcast(social.told_someone ~ reported.police)
victims_2024[, F1P1 := fifelse(social.told_someone == 1 & reported.police == 1, 1, 0)]
victims_2024[, F0P1 := fifelse(social.told_someone == 0 & reported.police == 1, 1, 0)]
victims_2024[, F1P0 := fifelse(social.told_someone == 1 & reported.police == 0, 1, 0)]
victims_2024[, F0P0 := fifelse(social.told_someone == 0 & reported.police == 0, 1, 0)]

# ================================================================================

# List covariates for property and violent crimes
X_ses <- c("ses.settlement_type",
					 "ses.male",
					 "ses.age_group",
					 # "ses.has_partner",
					 "ses.household_size_f",
					 "ses.household_income",
					 "ses.higher_edu",
					 "ses.employed"
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



library(mlogit)

victims_pooled_long <- melt(victims_pooled,
														measure.vars = patterns("F.P."),
														variable.name = "path",
														value.name = "choice")
victims_pooled_long[, set := .GRP, by = id]
victims_pooled_long[, path := fct_relevel(path, "F0P0")]

dat <- dfidx(victims_pooled_long, idx = c("set", "path"))
head(dat, 3)

multilogit <- function(crime_class, X) {
	m <- mlogit(as.formula(g("choice ~ 0 | - 1 + {paste(X, collapse = '+')}")),
							weights = dat[dat$incident.crime_class == crime_class, ]$weight,
							data = dat[dat$incident.crime_class == crime_class, ])
	sm <- summary(m)
	res <- as.data.table(sm$CoefTable,keep.rownames = "term")
	setnames(res, c("term", "est", "se", "z", "p"))
	res
}

mv <- multilogit("Violent", X_violent)
mv[, c("term", "path") := tstrsplit(term, ":")]
mvw <- dcast(mv, term ~ path, value.var = c("est", "se", "p"))
mvw[, grepv("est", names(mvw)) := lapply(.SD, exp), .SDcols = patterns("est")]
mvw[, grepv("F.P.", names(mvw)) := lapply(.SD, function(x) round(x, 3)), .SDcols = patterns("F.P.")]
# setcolorder(mvw, c("term",
# 									 "est_F1P0", "se_F1P0",  "p_F1P0",
# 									 "est_F0P1",  "se_F0P1",  "p_F0P1",
# 									 "est_F0P0", "se_F0P0",  "p_F0P0"))
# View(mvw)

stars_fun <- function(x) {
	fcase(x <= 0.01, "***",
				x <= 0.05, "**",
				x <= 0.1, "*",
				default = "")
}

View(mvw[, .(coef_map[term],
				F1P0 = paste0(est_F1P0, " (", se_F1P0, ") ", stars_fun(p_F1P0)),
				F0P1 = paste0(est_F0P1, " (", se_F0P1, ") ", stars_fun(p_F0P1)),
				F1P1 = paste0(est_F1P1, " (", se_F1P1, ") ", stars_fun(p_F1P1)))])






m <- mlogit(as.formula(g("choice ~ 0 | {paste(X_property, collapse = '+')}")),
						weights = dat[dat$incident.crime_class == "Property", ]$weight,
						data = dat[dat$incident.crime_class == "Property", ])
modelsummary(m, exponentiate = T, stars = T, statistic = NULL)


library(marginaleffects)
predictions(m, newdata = dat, hypothesis = h)

View(
	victims_pooled[, .N, .(incident.crime_class, region)] |> dcast(...~incident.crime_class)
)

library(dplyr)
library(mlogit)
library(marginaleffects)

# Fishing mode‑choice data from mlogit
data(Fishing)
Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")

m <- mlogit(mode ~ price + catch | income, data = Fish)

library(dplyr)
h <- function(x) {
	x |>
		aggregate(estimate ~ term + group, data = _, FUN = mean) |>
		aggregate(estimate ~ group, data = _, FUN = diff) |>
		setNames(c("term", "estimate"))
}
predictions(m, newdata = dat, hypothesis = h)



# # Imputation
# imputed_data <- mice(victims_pooled, ...)
# mi_model_fit <- with(imputed_data, lm(y ~ x))
# how_many_imputations(mi_model_fit)


# ================================================================================

# List covariates for property and violent crimes
X_ses <- c("ses.settlement_type",
					 "ses.male",
					 "ses.age_group",
					 # "ses.has_partner",
					 "ses.household_size_f",
					 "ses.household_income",
					 "ses.higher_edu",
					 "ses.employed"
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

victims_pooled[incident.crime_class == "Cybercrime", sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_property]
victims_pooled[incident.crime_class == "Property", sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_property]
victims_pooled[incident.crime_class == "Violent", sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_violent]
victims_2024[incident.crime_class == "Cybercrime", sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_property_2024]
victims_2024[incident.crime_class == "Property", sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_property_2024]
victims_2024[incident.crime_class == "Violent", sapply(.SD, function(x) mean(is.na(x))), .SDcols = X_violent_2024]

# Regression formulas
fml_property <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property, collapse = '+')} | {paste(FE, collapse = '+')}"))
# fml_property_x <- as.formula(g("reported.police ~ social.told_someone * incident.via_phone_internet + {paste(X_property[X_property != 'incident.via_phone_internet'], collapse = '+')} | {paste(FE, collapse = '+')} "))

fml_violent <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent, collapse = '+')} | {paste(FE, collapse = '+')}"))
# fml_violent_x <- as.formula(g("reported.police ~ social.told_someone * incident.offender_nonstranger + {paste(X_violent[X_violent != 'incident.offender_relationship_grouped'], collapse = '+')} | {paste(FE, collapse = '+')}"))

fml_property_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_property_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))
# fml_property_2024_x <- as.formula(g("reported.police ~ social.told_someone * incident.via_phone_internet + {paste(X_property_2024[X_property_2024 != 'incident.via_phone_internet'], collapse = '+')} | {paste(FE[1], collapse = '+')} "))

fml_violent_2024 <- as.formula(g("reported.police ~ social.told_someone + {paste(X_violent_2024, collapse = '+')} | {paste(FE[1], collapse = '+')} "))
# fml_violent_2024_x <- as.formula(g("reported.police ~ social.told_someone * incident.offender_nonstranger + {paste(X_violent_2024[X_violent_2024 != 'incident.offender_relationship_grouped'], collapse = '+')} | {paste(FE[1], collapse = '+')} "))

mdl_lpm_cybercrime<-
	feols(fml_property,
				weights = ~weight,
				cluster = FE,
				data = victims_pooled[incident.crime_class == "Cybercrime"])

mdl_lpm_property <-
	feols(fml_property,
				weights = ~weight,
				cluster = FE,
				data = victims_pooled[incident.crime_class == "Property"])


mdl_lpm_property <-
	feols(fml_property,
				weights = ~weight,
				cluster = FE,
				data = victims_pooled[incident.crime_class == "Property"
															& (reported.case_initiated == "Да" | reported.police == 0)])

# etable(mdl_lpm_property, digits = 3)

# mdl_lpm_property_x <-
# 	feols(fml_property_x,
# 				weights = ~weight,
# 				cluster = FE,
# 				data = victims_pooled[incident.crime_class == "Property"])
#
# etable(mdl_lpm_property_x)

mdl_lpm_violent <-
	feols(fml_violent,
				weights = ~weight,
				cluster = FE,
				data = victims_pooled[incident.crime_class == "Violent"
															& (reported.case_initiated == "Да" | reported.police == 0)])

etable(list(mdl_lpm_cybercrime, mdl_lpm_property, mdl_lpm_violent))
etable(list(mdl_lpm_property, mdl_lpm_violent),
			 dict = coef_map, digits = 3, se.below = F, se.row = T, tex = T,
			 keep = "informed")


mdl_lpm_property_2024 <-
	feols(fml_property_2024,
				weights = ~weight,
				cluster = FE[1],
				data = victims_2024[incident.crime_class == "Property"])

etable(mdl_lpm_property_2024)

mdl_lpm_cybercrime_2024 <-
	feols(fml_property_2024,
				weights = ~weight,
				cluster = FE[1],
				data = victims_2024[incident.crime_class == "Cybercrime"])

etable(mdl_lpm_property_2024_cyber)

# mdl_lpm_property_2024_x <-
# 	feols(fml_property_2024_x,
# 				weights = ~weight,
# 				cluster = FE[1],
# 				data = victims_2024[incident.crime_class == "Property"])
#
# etable(mdl_lpm_property_2024_x)

mdl_lpm_violent_2024 <-
	feols(fml_violent_2024,
				weights = ~weight,
				cluster = FE[1],
				data = victims_2024[incident.crime_class == "Violent"])

etable(list(mdl_lpm_cybercrime_2024,
						mdl_lpm_property_2024,
						mdl_lpm_violent_2024),
			 digits = 3, tex = F)

etable(list(mdl_lpm_property_2024, mdl_lpm_violent_2024),
			 dict = coef_map, digits = 3, se.below = F, se.row = T, tex = T,
			 keep_raw = "told|emotional")

# mdl_lpm_violent_2024_x <-
# 	feols(fml_violent_2024_x,
# 				weights = ~weight,
# 				cluster = FE[1],
# 				data = victims_2024[incident.crime_class == "Violent"])
#
# etable(mdl_lpm_violent_2024_x)

mdl_lpm_violent_2024_sexualized <-
	feols(as.formula(g("reported.police ~ social.told_someone * incident.sexualized + {paste(X_violent_2024[X_violent_2024 %notin% c('incident.sexualized')], collapse = '+')} | {FE[1]}")),
				weights = ~weight,
				cluster = FE[1],
				data = victims_2024[incident.crime_class == "Violent"])

etable(mdl_lpm_violent_2024_sexualized)


# Paths ========================================================================

mdls_lpm_cybercrime <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_property, collapse = '+')} | {paste(FE, collapse = '+')}")),
				data = victims_pooled[incident.crime_class == "Cybercrime"],
				weights = ~weight,
				cluster = FE)
etable(mdls_lpm_cybercrime, digits = 3, dict = coef_map)

mdls_lpm_property <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_property, collapse = '+')} | {paste(FE, collapse = '+')}")),
				data = victims_pooled[incident.crime_class == "Property"  & year == 2021],
				weights = ~weight,
				cluster = FE)
etable(mdls_lpm_property, digits = 3, dict = coef_map)

mdls_lpm_violent <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_violent, collapse = '+')} | {paste(FE, collapse = '+')}")),
				data = victims_pooled[incident.crime_class == "Violent"  & year == 2021],
				weights = ~weight,
				cluster = FE)
etable(mdls_lpm_violent, digits = 3)

mdls_lpm_cybercrime_2024 <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_property_2024, collapse = '+')} | {FE[1]}")),
				data = victims_2024[incident.crime_class == "Cybercrime"],
				weights = ~weight,
				cluster = FE[1])

mdls_lpm_cybercrime_2021 <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_property, collapse = '+')} | {FE[1]}")),
				data = victims_pooled[incident.crime_class == "Cybercrime" & year == "2021"],
				weights = ~weight,
				cluster = FE[1])

mdls_lpm_property_2024 <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_property_2024, collapse = '+')} | {FE[1]}")),
				data = victims_2024[incident.crime_class == "Property"],
				weights = ~weight,
				cluster = FE[1])
etable(mdls_lpm_property_2024)

mdls_lpm_property_2021 <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_property, collapse = '+')} | {FE[1]}")),
				data = victims_pooled[incident.crime_class == "Property" & year == "2021"],
				weights = ~weight,
				cluster = FE[1])

mdls_lpm_violent_2024 <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_violent_2024, collapse = '+')} | {FE[1]}")),
				data = victims_2024[incident.crime_class == "Violent"],
				weights = ~weight,
				cluster = FE[1])
etable(mdls_lpm_violent_2024)

mdls_lpm_violent_2018 <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_violent, collapse = '+')} | {FE[1]}")),
				data = victims_pooled[incident.crime_class == "Violent" & year == "2018"],
				weights = ~weight,
				cluster = FE[1])

mdls_lpm_violent_2021 <-
	feols(as.formula(g("c(F1P1, F1P0, F0P1, F0P0) ~ {paste(X_violent, collapse = '+')} | {FE[1]}")),
				data = victims_pooled[incident.crime_class == "Violent" & year == "2021"],
				weights = ~weight,
				cluster = FE[1])

# etable(mdls_lpm_violent_2024, dict = coef_map, digits = 3, se.below = F, se.row = T, tex = T)


victims_pooled[incident.crime_class == "Violent", .N, incident.sexualized]
victims_2024[incident.crime_class == "Violent", .N, incident.sexualized]

# Plots ========================================================================

plot_path_profile <- function(mdls_list) {
	pl_data <-
		rbindlist(
			lapply(mdls_list, function(x) {
				d <- as.data.table(x$coeftable, keep.rownames = "var")
				setnames(d, c("var", "est", "se", "tval", "pval"))
				d[, path := x$model_info$lhs]
				inv_coef_map <- names(coef_map)
				names(inv_coef_map) <- coef_map
				coef_map_present <- inv_coef_map[inv_coef_map %in% d$var]
				d[, var := fct_rev(fct_relevel(as.factor(var), coef_map_present))]
				d[, var := fct_recode(var, !!!inv_coef_map)]
				d
			})
		)
	pl_data[, cih := est + se*1.96]
	pl_data[, cil := est - se*1.96]

	anno <-
		rbindlist(
			lapply(mdls_list, function(x) {
				data.table(path = x$model_info$lhs,
									 anno = paste0("N: ", x$nobs,
									 							"\nClass 1: ", {
									 								orig_data <- as.data.table(fixest_data(x))
									 								y_name <- as.character(x$fml)[2]
									 								orig_data[y == 1, .N, env = list(y = y_name)]
									 							},
									 							"\nR2: ", round(as.numeric(fitstat(x, ~r2)), 3)),
									 ymin = min(x$coefficients - x$se*1.96))
			})
		)

	print(

	ggplot(pl_data, aes(var, est)) +
		geom_point(aes(alpha = ifelse(pl_data$pval <= 0.1, "sign", "nonsign"))) +
		geom_errorbar(aes(ymin = cil, ymax = cih, alpha = ifelse(pl_data$pval <= 0.1, "sign", "nonsign")), width = 0,
									) +
		scale_alpha_manual(breaks = c("sign", "nonsign"),
											 values = c(1, .2)) +
		geom_hline(yintercept = 0, linetype = "dashed") +
		geom_text(data = anno, aes(x = 2.5, y = ymin, label = anno),
							size = 2.5,
							# family = "mono",
							hjust = 0) +
		coord_flip() +
		facet_wrap(~path, nrow = 2, scales = "free_x") +
		theme_bw() +
		theme(legend.position = "none",
					axis.text.y = element_text(family = "mono", face = "bold"),
					plot.margin = unit(c(0,.5,0,0), "cm")) +
		labs(y = "Estimate", x = "")

	)

}

pdf(file = "paper/figures/paths_cyber.pdf", width = 7, height = 8)
plot_path_profile(mdls_lpm_cybercrime)
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

dev.new()

plot_path_profile(mdls_lpm_cybercrime_2021)
plot_path_profile(mdls_lpm_property_2021)
plot_path_profile(mdls_lpm_violent_2021)

plot_path_profile(mdls_lpm_violent_2018)


dev.new()


# Sankey -----------------------------------------------------------------------

library(ggsankeyfier)
data(ecosystem_services)

## pivot to long format for plotting:
es_long <-
	pivot_stages_longer(
		## the data.frame we wish to pivot:
		data        = ecosystem_services,
		## the columns that represent the stages:
		stages_from = c("activity_type", "pressure_cat",
										"biotic_realm", "service_division"),
		## the column that represents the size of the flows:
		values_from = "RCSES"	)

victims_pooled[, victim := fifelse(victim == "Да", 1, 0)]
victims_pooled[, path := fcase(F1P1 == 1, "F1P1",
															 F1P0 == 1, "F1P0",
															 F0P1 == 1, "F0P1",
															 F0P0 == 1, "F0P0")]
victims_pooled_agg <- victims_pooled[victim == 1, .N, .(victim, social.told_someone, path, incident.crime_class)]

victims_long <- pivot_stages_longer(victims_pooled_agg,
										stages_from = c("victim", "social.told_someone", "path"),
										values_from = "N",additional_aes_from = "incident.crime_class")


ggplot(victims_long,
			 aes(x = stage, y = N, group = node,
			 		connector = connector, edge_id = edge_id)) +
	# geom_segment(stat = "sankeyedge", position = position_sankey(v_space = "auto")) +
	geom_sankeyedge(v_space = "auto") + #, aes(fill = social.told_someone)) +
	geom_sankeynode(v_space = "auto", split_nodes = F) +
	geom_text(aes(x = 1.5, y = 4000), label = 6271) +
	facet_wrap(~ incident.crime_class)


