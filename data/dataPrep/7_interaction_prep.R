# IMPORT DATA:
choice_data <- read_csv(here::here("6coded", "choice_data_coded.csv"))
demo_data <- read_csv(here::here("6coded", "demo_data_coded.csv"))

# REPLICATE DEMO INTERACTION VARIABLES FOR CHOICE DATA:
income <- rep(demo_data$income, each=45)
incomeHIGH <- rep(demo_data$income_HIGH, each=45)
incomeNA <- rep(demo_data$income_NA, each=45)
age <- rep(demo_data$age, each=45)
ageHIGH <- rep(demo_data$age_HIGH, each=45)
ageNA <- rep(demo_data$age_NA, each=45)
marriage <- rep(demo_data$marriage, each=45)
marriageNA <- rep(demo_data$marriage_NA, each=45)
female <- rep(demo_data$female, each=45)
femaleNA <- rep(demo_data$female_NA, each=45)
householdsize <- rep(demo_data$householdsize, each=45)
householdsizeHIGH <- rep(demo_data$householdsize_HIGH, each=45)
householdsizeNA <- rep(demo_data$householdsize_NA, each=45)
numchildren <- rep(demo_data$num_children, each=45)
havechild <- rep(demo_data$have_child, each=45)
havechildNA <- rep(demo_data$have_child_NA, each=45)
education <- rep(demo_data$education, each=45)
collegegrad <- rep(demo_data$college_grad, each=45)
collegegradNA <- rep(demo_data$college_grad_NA, each=45)
enthusiast <- rep(demo_data$enthusiast, each=45)
recentpastbuyer <- rep(demo_data$recent_past_buyer, each=45)
primarybuyer <- rep(demo_data$primarybuyer, each=45)
numvehicles <- rep(demo_data$num_vehicles, each=45)
vehicleownership <- rep(demo_data$vehicle_ownership, each=45)
vehicleownershipNA <- rep(demo_data$vehicle_ownership_NA, each=45)
firsttimebuyer <- rep(demo_data$first_time_buyer, each=45)
nodriveexpr <- rep(demo_data$no_drive_expr, each=45)
homecharging <- rep(demo_data$home_charge, each=45)
envappear <- rep(demo_data$env_appearance, each=45)
statsym <- rep(demo_data$status_symbol, each=45)

######## INTERACT ALL VARIABLES ########:
# Interact with Type, Price, and OpCost:

int_vars <- function(var, name) {
	HEV <- var*choice_data$HEV
	PHEV <- var*choice_data$PHEV
	BEV <- var*choice_data$BEV
	price <- var*choice_data$Price
	opcost <- var*choice_data$OpCost
	out <- cbind(HEV,PHEV, BEV, price, opcost)
	colnames(out) <- sub("^", name, colnames(out))
	return(out)
}

income <- int_vars(income, "income_")
incomeHIGH <- int_vars(incomeHIGH, "incomeHIGH_")
incomeNA <- int_vars(incomeNA, "incomeNA_")
age <- int_vars(age, "age_")
ageHIGH <- int_vars(ageHIGH, "ageHIGH_")
ageNA <- int_vars(ageNA, "ageNA_")
marriage <- int_vars(marriage, "marriage_")
marriageNA <- int_vars(marriageNA, "marriageNA_")
female <- int_vars(female, "female_")
femaleNA <- int_vars(femaleNA, "femaleNA_")
householdsize <- int_vars(householdsize, "householdsize_")
householdsizeHIGH <- int_vars(householdsizeHIGH, "householdsizeHIGH_")
householdsizeNA <- int_vars(householdsizeNA, "householdsizeNA_")
numchildren <- int_vars(numchildren, "numchildren_")
havechild <- int_vars(havechild, "havechild_")
havechildNA <- int_vars(havechildNA, "havechildNA_")
education <- int_vars(education, "education_")
collegegrad <- int_vars(collegegrad, "collegegrad_")
collegegradNA <- int_vars(collegegradNA, "collegegradNA_")
enthusiast <- int_vars(enthusiast, "enthusiast_")
recentpastbuyer <- int_vars(recentpastbuyer, "recentpastbuyer_")
primarybuyer <- int_vars(primarybuyer, "primarybuyer_")
numvehicles <- int_vars(numvehicles, "numvehicles_")
vehicleownership <- int_vars(vehicleownership, "vehicleownership_")
vehicleownershipNA <- int_vars(vehicleownershipNA, "vehicleownershipNA_")
firsttimebuyer <- int_vars(firsttimebuyer, "firsttimebuyer_")
nodriveexpr <- int_vars(nodriveexpr, "nodriveexpr_")
homecharging <- int_vars(homecharging, "homecharging_")
envappear <- int_vars(envappear, "envappear_")
statsym <- int_vars(statsym, "statsym_")

# ADD INTERACTIONS TO CHOICE DATASET:
choice_data <- cbind(choice_data,income,incomeHIGH,incomeNA,age,ageHIGH,ageNA,marriage,marriageNA,female,femaleNA,householdsize,householdsizeHIGH,householdsizeNA,numchildren,havechild,havechildNA,education,collegegrad,collegegradNA,enthusiast,recentpastbuyer,primarybuyer,numvehicles,vehicleownership,vehicleownershipNA,firsttimebuyer,nodriveexpr,homecharging,envappear,statsym)

# Export choice data
write_csv(choice_data, here::here("6coded", "choice_data_interactions.csv"))
