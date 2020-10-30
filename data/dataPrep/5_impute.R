# Author: John P. Helveston
# First Coded: February 19, 2013
# Last Updated: October 29, 2020

# Description: This code imports the combined choice data from China, MTurk, and the Pittsburgh international auto show and imputes means for missing values

library(tidyverse)
options(dplyr.width = Inf)

# Import all data:
choice_data <- read_csv(here::here("3fixed", "choice_data_clean_fixed.csv"))
demo_data <- read_csv(here::here("3fixed", "demo_data_clean_fixed.csv"))
c.demo <- demo_data[which(demo_data$usa==0),]
u.demo <- demo_data[which(demo_data$usa==1),]

mean_imput <- function(data) {
	data[is.na(data)] <- mean(na.omit(data))
	return(data)
}

# Replace missing values with means
c.demo$income <- mean_imput(c.demo$income)
u.demo$income <- mean_imput(u.demo$income)
c.demo$age <- mean_imput(c.demo$age)
u.demo$age <- mean_imput(u.demo$age)
c.demo$female <- mean_imput(c.demo$female)
u.demo$female <- mean_imput(u.demo$female)
c.demo$marriage <- mean_imput(c.demo$marriage)
u.demo$marriage <- mean_imput(u.demo$marriage)
c.demo$education <- mean_imput(c.demo$education)
u.demo$education <- mean_imput(u.demo$education)
c.demo$householdsize <- mean_imput(c.demo$householdsize)
u.demo$householdsize <- mean_imput(u.demo$householdsize)
c.demo$num_vehicles <- mean_imput(c.demo$num_vehicles)
u.demo$num_vehicles <- mean_imput(u.demo$num_vehicles)
c.demo$num_children <- mean_imput(c.demo$num_children)
u.demo$num_children <- mean_imput(u.demo$num_children)
c.demo$dailyvmt <- mean_imput(c.demo$dailyvmt)
u.demo$dailyvmt <- mean_imput(u.demo$dailyvmt)
c.demo$annualvmt <- mean_imput(c.demo$annualvmt)
u.demo$annualvmt <- mean_imput(u.demo$annualvmt)
c.demo$householdsize <- mean_imput(c.demo$householdsize)
u.demo$householdsize <- mean_imput(u.demo$householdsize)
demo_data <- rbind(c.demo, u.demo)

#####################################################
# swap fast charging coding
choice_data$`Fast Charge` <- choice_data$`Fast Charge` + 1
choice_data$`Fast Charge`[which(choice_data$`Fast Charge`==3)] <- 1

#####################################################
# Rename "RespNum"
names(demo_data)[1] <- "ID"

#####################################################
# ZIP CODE ANALYSIS - add state, population, population density, lat, long, urban, suburban, rural
zip_data <- read.csv(here::here("1raw", "zip", "zip.csv"), header=T)

start = which(demo_data$usa==1)[1]
state <- matrix(data=0, ncol=1, nrow=nrow(demo_data))
pop <- state
popdens <- state
lat <- state
long <- state
urban <- state
suburban <- state
rural <- state
for (i in start:nrow(demo_data)) {
	id <- which(zip_data$ZipCode == demo_data$zipcode[i])
	if (length(id)==0) {
		state[i,] <- NA
		pop[i,] <- NA
		popdens[i,] <- NA
		lat[i,] <- NA
		long[i,] <- NA
	} else {
		state[i,] <- as.character(zip_data[id,]$State)
		pop[i,] <- zip_data[id,]$Population
		popdens[i,] <- zip_data[id,]$PopDens
		lat[i,] <- zip_data[id,]$Latitude
		long[i,] <- zip_data[id,]$Longitude
		if (popdens[i,] > 3000) {
			urban[i,] <- 1
		}
		if (popdens[i,] <= 3000 & popdens[i,] > 1000) {
			suburban[i,] <- 1
		}
		if (popdens[i,] < 1000) {
			rural[i,] <- 1
		}
	}
}

urban[1:(start-1)] <- 1

Pacific <- c("WA", "OR", "CA", "AK", "HI")
Mountain <- c("MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM")
Midwest <- c("ND", "SD", "MN", "IA", "NE", "KS", "MO", "WI", "IL", "MI", "IN", "OH")
South <- c("TX", "OK", "AR", "LA", "KY", "TN", "MS", "AL", "WV", "VA", "DE", "MD", "DC", "NC", "SC", "GA", "FL")
Northeast <- c("ME", "NH", "VT", "MA", "RI", "CI", "NY", "NJ","PA")

region = matrix(data=0, ncol=1, nrow=nrow(demo_data))
for (i in start:nrow(demo_data)) {
	if (is.element(state[i], Pacific) == TRUE) region[i] = 1
	else if (is.element(state[i], Mountain) == TRUE) region[i] = 2
	else if (is.element(state[i], Midwest) == TRUE) region[i] = 3
	else if (is.element(state[i], South) == TRUE) region[i] = 4
	else region[i] = 5
}

demo_data <- cbind(demo_data, state, pop, lat, long, urban, suburban, rural, region)

### Add dummies for urban and suburban to choice data:
urban <- rep(urban, each=45)
suburban <- rep(suburban, each=45)
choice_data <- cbind(choice_data, urban, suburban)

#####################################################
# Export choice data
write_csv(choice_data, here::here("5imputed", "choice_data_clean_fixed_imputed.csv"))

# Export  demo data
write_csv(demo_data, here::here("5imputed", "demo_data_clean_fixed_imputed.csv"))
write_csv(demo_data, here::here("6coded", "demo_data_coded.csv"))
