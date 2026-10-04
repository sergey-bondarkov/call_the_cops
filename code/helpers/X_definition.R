# List covariates for property and violent crimes
X_ses <- c("ses.settlement_type",
					 "ses.male",
					 "ses.age_group",
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
X_attitude <- c(
	"attitude.familiar_police",
	"attitude.has_crime_record")

X_property <- c(X_ses, X_damage_property[X_damage_property != "damage.emotional"], X_incident_property, X_attitude)
X_property_2024 <- c(X_ses, X_damage_property, X_incident_property, X_attitude)

X_violent <- c(X_ses, X_damage_violent[X_damage_violent != "damage.emotional"], X_incident_violent, X_attitude)
X_violent_2024 <- c(X_ses, X_damage_violent, X_incident_violent, X_attitude)

FE <- c("region",	"year")
