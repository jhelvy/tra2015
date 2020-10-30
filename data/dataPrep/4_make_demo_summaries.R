# Author: John P. Helveston
# First Coded: February 19, 2013
# Last Updated: October 29, 2020

# Description: This code imports the combined choice data from China, MTurk, and the Pittsburgh international auto show and exports summaries of different demographic variables

library(tidyverse)

# Import all demo data:
demo_data <- read_csv(here::here("3fixed", "demo_data_clean_fixed.csv"))

########################################################################
# GENERAL DEMO SUMMARIES
########################################################################

# Function to summarize the data:
summarize = function(X) {
	X <- cbind(X,demo_data$usa)
	C = X[which(X[,2]==0),1]
	U = X[which(X[,2]==1),1]
	c.med = median(na.omit(C))
	c.mean = mean(na.omit(C))
	c.sd = sd(na.omit(C))
	c.na <- length(which(is.na(C)==T))
	u.med = median(na.omit(U))
	u.mean = mean(na.omit(U))
	u.sd = sd(na.omit(U))
	u.na <- length(which(is.na(U)==T))
	c.out <- c(c.med, c.mean, c.sd, c.na)
	u.out <- c(u.med, u.mean, u.sd, u.na)
	out <- rbind(c.out, u.out)
	colnames(out) <- c("median", "mean", "sd", "NA's")
	return(out)
}

# Function to summarize the binary data:
summarize_binary = function(X) {
	X <- cbind(X,demo_data$usa)
	C = X[which(X[,2]==0),1]
	U = X[which(X[,2]==1),1]
	c.1 <- length(which(C==1))
	c.0 <- length(which(C==0))
	c.NA <- length(which(is.na(C)==T))
	u.1 <- length(which(U==1))
	u.0 <- length(which(U==0))
	u.NA <- length(which(is.na(U)==T))
	cpercent <- c.1/(c.1+c.0)*100
	upercent <- u.1/(u.1+u.0)*100
	C = cbind(cpercent, c.NA)
	U = cbind(upercent, u.NA)
	out <- rbind(C, U)
	colnames(out) <- c("Percent", "NA's")
	return(out)
}

household_income = summarize(demo_data$income)
age = summarize(demo_data$age)
num_children = summarize(demo_data$num_children)
num_vehicles = summarize(demo_data$num_vehicles)
dailyvmt = summarize(demo_data$dailyvmt)
annualvmt = summarize(demo_data$annualvmt)
householdsize = summarize(demo_data$householdsize)
education = summarize(demo_data$education)

china_dem <- rbind(household_income[1,], age[1,], num_children[1,], num_vehicles[1,], dailyvmt[1,], annualvmt[1,], householdsize[1,], education[1,])
usa_dem <- rbind(household_income[2,], age[2,], num_children[2,], num_vehicles[2,], dailyvmt[2,], annualvmt[2,], householdsize[2,], education[2,])
rownames(china_dem) <- c("Household Income", "Age", "Num Children", "Num Vehicles", "Daily VMT", "Annual VMT", "Household Size", "Years Education")
rownames(usa_dem) <- c("Household Income", "Age", "Num Children", "Num Vehicles", "Daily VMT", "Annual VMT", "Household Size", "Years Education")

female = summarize_binary(demo_data$female)
marriage = summarize_binary(demo_data$marriage)
recent_past_buyer = summarize_binary(demo_data$recent_past_buyer)
home_charge = summarize_binary(demo_data$home_charge)
work_charge = summarize_binary(demo_data$work_charge)
have_child = summarize_binary(demo_data$have_child)
college_grad = summarize_binary(demo_data$college_grad)
first_time_buyer = summarize_binary(demo_data$first_time_buyer)
no_drive_expr = summarize_binary(demo_data$no_drive_expr)

china_binary <- rbind(female[1,], marriage[1,], recent_past_buyer[1,], home_charge[1,], work_charge[1,], have_child[1,], college_grad[1,], first_time_buyer[1,], no_drive_expr[1,])
usa_binary <- rbind(female[2,], marriage[2,], recent_past_buyer[2,], home_charge[2,], work_charge[2,], have_child[2,], college_grad[2,], first_time_buyer[2,], no_drive_expr[2,])
rownames(china_binary) <- c("Percent Female", "Percent Married", "Recent Past Buyer", "Home Charging Ability", "Work Charging Ability", "Have Children", "College Graduate", "First Time Buyer", "No Driving Experience")
rownames(usa_binary) <- c("Percent Female", "Percent Married", "Recent Past Buyer", "Home Charging Ability", "Work Charging Ability", "Have Children", "College Graduate", "First Time Buyer", "No Driving Experience")

write_csv(as.data.frame(china_dem), here::here("demo_summary", "china_demographics_full.csv"))
write_csv(as.data.frame(usa_dem), here::here("demo_summary", "usa_demographics_full.csv"))
write_csv(as.data.frame(china_binary), here::here("demo_summary", "china_binary_demographics_full.csv"))
write_csv(as.data.frame(usa_binary), here::here("demo_summary", "usa_binary_demographics_full.csv"))

########################################################################
# SUMMARIES OF SCREENED CARS & WEIGHTED
########################################################################

# Screen out Beijing & SUVs:
demo_data <- demo_data[-which(demo_data$usa==0 & demo_data$shanghai==0 & demo_data$shenzhen==0 & demo_data$chengdu==0),]
demo_data <- demo_data[which(demo_data$SUVbuyer==0),]

household_income = summarize(demo_data$income)
age = summarize(demo_data$age)
num_children = summarize(demo_data$num_children)
num_vehicles = summarize(demo_data$num_vehicles)
dailyvmt = summarize(demo_data$dailyvmt)
annualvmt = summarize(demo_data$annualvmt)
householdsize = summarize(demo_data$householdsize)
education = summarize(demo_data$education)

china_dem <- rbind(household_income[1,], age[1,], num_children[1,], num_vehicles[1,], dailyvmt[1,], annualvmt[1,], householdsize[1,], education[1,])
usa_dem <- rbind(household_income[2,], age[2,], num_children[2,], num_vehicles[2,], dailyvmt[2,], annualvmt[2,], householdsize[2,], education[2,])
rownames(china_dem) <- c("Household Income", "Age", "Num Children", "Num Vehicles", "Daily VMT", "Annual VMT", "Household Size", "Years Education")
rownames(usa_dem) <- c("Household Income", "Age", "Num Children", "Num Vehicles", "Daily VMT", "Annual VMT", "Household Size", "Years Education")

female = summarize_binary(demo_data$female)
marriage = summarize_binary(demo_data$marriage)
recent_past_buyer = summarize_binary(demo_data$recent_past_buyer)
home_charge = summarize_binary(demo_data$home_charge)
work_charge = summarize_binary(demo_data$work_charge)
have_child = summarize_binary(demo_data$have_child)
college_grad = summarize_binary(demo_data$college_grad)
first_time_buyer = summarize_binary(demo_data$first_time_buyer)
no_drive_expr = summarize_binary(demo_data$no_drive_expr)

china_binary <- rbind(female[1,], marriage[1,], recent_past_buyer[1,], home_charge[1,], work_charge[1,], have_child[1,], college_grad[1,], first_time_buyer[1,], no_drive_expr[1,])
usa_binary <- rbind(female[2,], marriage[2,], recent_past_buyer[2,], home_charge[2,], work_charge[2,], have_child[2,], college_grad[2,], first_time_buyer[2,], no_drive_expr[2,])
rownames(china_binary) <- c("Percent Female", "Percent Married", "Recent Past Buyer", "Home Charging Ability", "Work Charging Ability", "Have Children", "College Graduate", "First Time Buyer", "No Driving Experience")
rownames(usa_binary) <- c("Percent Female", "Percent Married", "Recent Past Buyer", "Home Charging Ability", "Work Charging Ability", "Have Children", "College Graduate", "First Time Buyer", "No Driving Experience")

write_csv(as.data.frame(china_dem), here::here("demo_summary", "china_demographics_cars.csv"))
write_csv(as.data.frame(usa_dem), here::here("demo_summary", "usa_demographics_cars.csv"))
write_csv(as.data.frame(china_binary), here::here("demo_summary", "china_binary_demographics_cars.csv"))
write_csv(as.data.frame(usa_binary), here::here("demo_summary", "usa_binary_demographics_cars.csv"))


################# Weights:
china_weights <- as.matrix(read_csv(here::here("4weights", "china_car_weights_0.2.csv")))
us_weights <- as.matrix(read_csv(here::here("4weights", "us_car_weights_0.2.csv")))
weights <- rbind(china_weights, us_weights)*10

weighted.summary <- function(X) {
	X <- cbind(X,weights,demo_data$usa)
	X <- X[-which(is.na(X[,1])==T),]
	C = X[which(X[,3]==0),]
	U = X[which(X[,3]==1),]
	out <- rep(C[1,1], C[1,2])
	for (i in 2:nrow(C)) {
		out1 <- rep(C[i,1], C[i,2])
		out <- c(out, out1)
	}
	c.out <- c(median(out), mean(out), sd(out))
	out <- rep(U[1,1], U[1,2])
	for (i in 2:nrow(U)) {
		out1 <- rep(U[i,1], U[i,2])
		out <- c(out, out1)
	}
	u.out <- c(median(out), mean(out), sd(out))
	out <- rbind(c.out, u.out)
	colnames(out) <- c("median", "mean", "sd")
	return(out)
}

weighted.binary.summary <- function(X) {
	X <- cbind(X,weights,demo_data$usa)
	if (length(which(is.na(X[,1])==T))!=0) {
	X <- X[-which(is.na(X[,1])==T),]}
	C = X[which(X[,3]==0),]
	U = X[which(X[,3]==1),]
	out <- rep(C[1,1], C[1,2])
	for (i in 2:nrow(C)) {
		out1 <- rep(C[i,1], C[i,2])
		out <- c(out, out1)
	}
	c.out <- sum(out) / length(out)
	out <- rep(U[1,1], U[1,2])
	for (i in 2:nrow(U)) {
		out1 <- rep(U[i,1], U[i,2])
		out <- c(out, out1)
	}
	u.out <- sum(out) / length(out)
	out <- c(c.out,u.out)
}

household_income = weighted.summary(demo_data$income)
age = weighted.summary(demo_data$age)
num_children = weighted.summary(demo_data$num_children)
num_vehicles = weighted.summary(demo_data$num_vehicles)
dailyvmt = weighted.summary(demo_data$dailyvmt)
annualvmt = weighted.summary(demo_data$annualvmt)
householdsize = weighted.summary(demo_data$householdsize)
education = weighted.summary(demo_data$education)

china_dem <- rbind(household_income[1,], age[1,], num_children[1,], num_vehicles[1,], dailyvmt[1,], annualvmt[1,], householdsize[1,], education[1,])
usa_dem <- rbind(household_income[2,], age[2,], num_children[2,], num_vehicles[2,], dailyvmt[2,], annualvmt[2,], householdsize[2,], education[2,])
rownames(china_dem) <- c("Household Income", "Age", "Num Children", "Num Vehicles", "Daily VMT", "Annual VMT", "Household Size", "Years Education")
rownames(usa_dem) <- c("Household Income", "Age", "Num Children", "Num Vehicles", "Daily VMT", "Annual VMT", "Household Size", "Years Education")

write.table(china_dem, here::here("demo_summary", "china_demographics_weighted_cars.csv"), sep=",", row.names=T, col.names=T)
write.table(usa_dem, here::here("demo_summary", "usa_demographics_weighted_cars.csv"), sep=",", row.names=T, col.names=T)

female = weighted.binary.summary(demo_data$female)
marriage = weighted.binary.summary(demo_data$marriage)
recent_past_buyer = weighted.binary.summary(demo_data$recent_past_buyer)
home_charge = weighted.binary.summary(demo_data$home_charge)
work_charge = weighted.binary.summary(demo_data$work_charge)
have_child = weighted.binary.summary(demo_data$have_child)
college_grad = weighted.binary.summary(demo_data$college_grad)
first_time_buyer = weighted.binary.summary(demo_data$first_time_buyer)
no_drive_expr = weighted.binary.summary(demo_data$no_drive_expr)

china_binary <- rbind(female[1], marriage[1], recent_past_buyer[1], home_charge[1], work_charge[1], have_child[1], college_grad[1], first_time_buyer[1], no_drive_expr[1])
usa_binary <- rbind(female[2], marriage[2], recent_past_buyer[2], home_charge[2], work_charge[2], have_child[2], college_grad[2], first_time_buyer[2], no_drive_expr[2])
rownames(china_binary) <- c("Percent Female", "Percent Married", "Recent Past Buyer", "Home Charging Ability", "Work Charging Ability", "Have Children", "College Graduate", "First Time Buyer", "No Driving Experience")
rownames(usa_binary) <- c("Percent Female", "Percent Married", "Recent Past Buyer", "Home Charging Ability", "Work Charging Ability", "Have Children", "College Graduate", "First Time Buyer", "No Driving Experience")

write.table(china_binary, here::here("demo_summary", "china_binary_demographics_weighted_cars.csv"), sep=",", row.names=T, col.names=T)
write.table(usa_binary, here::here("demo_summary", "usa_binary_demographics_weighted_cars.csv"), sep=",", row.names=T, col.names=T)
