# Author: John P. Helveston
# First Coded: April 8, 2012
# Last Updated: October 29, 2020

# Description: This code imports all of the cleaned China choice experiment data from each city and codes the data into a coding of the researcher's choice (dummy, effects, or thermometer). It also separates the PHEV & BEV types with their AERs, and whether they had the ability to fast charge or not.

# Required libraries:
source(here::here("code", "code_it.R"))

##### Import choice data #####
choice_data <- read_csv(here::here("5imputed", "choice_data_clean_fixed_imputed.csv"))

# Introduce AER variables for PHEV and EV
PHEV_aer = rep(0,nrow(choice_data))
BEV_aer = rep(0,nrow(choice_data))
choice_data = cbind(choice_data[,1:6], PHEV_aer, BEV_aer, choice_data[,7:ncol(choice_data)])

# Re-code data to separate-out AER from PHEV and EV vehicle types
choice_data[which(choice_data$Type==3),]$PHEV_aer <- 1
choice_data[which(choice_data$Type==4),]$PHEV_aer <- 2
choice_data[which(choice_data$Type==5),]$PHEV_aer <- 3
choice_data[which(choice_data$Type==6),]$BEV_aer <- 1
choice_data[which(choice_data$Type==7),]$BEV_aer <- 2
choice_data[which(choice_data$Type==8),]$BEV_aer <- 3
choice_data[which(choice_data$Type==4),]$Type <- 3
choice_data[which(choice_data$Type==5),]$Type <- 3
choice_data[which(choice_data$Type==6),]$Type <- 4
choice_data[which(choice_data$Type==7),]$Type <- 4
choice_data[which(choice_data$Type==8),]$Type <- 4

c_choice <- choice_data[which(choice_data$usa==0),1:13]
u_choice <- choice_data[which(choice_data$usa==1),1:13]

# INDICATORS: the next few objects describe the nature of the attributes and their levels
c=6 # the column number of the first attribute in the data file

################################################################################
####### CODE CHINA DATA:
###############################################################################
index = c(4,3,3,10,5,2,8,4) # a vector containing the number of levels of each attribute
type =  c(1,3,3,3,1,1,3,3) # the type of each attribute: 0 = effects, 1 = dummy, 2 = thermometer, 3 = continuous
data_header <- c("ID", "Answer", "Choice", "Task", "Alt", "HEV", "PHEV", "BEV", "PHEV_aer", "BEV_aer", "Price", "American", "Japanese", "Chinese", "SKorean", "FastCharge", "OpCost", "Acceleration")
levels <- c(1,1,1,1, 0, 15, 50, 0, 40, 120, 60, 90, 130, 170, 250, 75, 130, 200, 330, 500, 1, 1, 1, 1, 1, 1, 1, 34, 42, 49, 61, 46, 57, 68, 80, 9, 11, 13, 15)

# code clean data into a coding of the researcher's choice (dummy, effects, thermometer, or actual values).
X <- code_it(c_choice, index, c, levels, type, 0)
Y <- as.vector(c_choice[,1:5])
c_coded_data <- cbind(Y,X)
names(c_coded_data) <- data_header # rename coded data header

###############################################################################
####### CODE USA DATA:
###############################################################################
index = c(4,3,3,10,5,2,8,4) # a vector containing the number of levels of each attribute
type =  c(1,3,3,3 ,1,1,3,3) # the type of each attribute: 0 = effects, 1 = dummy, 2 = thermometer, 3 = continuous
data_header <- c("ID", "Answer", "Choice", "Task", "Alt", "HEV", "PHEV", "BEV", "PHEV_aer", "BEV_aer", "Price", "American", "Japanese", "Chinese", "SKorean", "FastCharge", "OpCost", "Acceleration")
levels <- c(1,1,1,1, 0, 10, 30, 0, 25, 75, 15,18,23,32,50,20,25,30,37,50, 1, 1, 1, 1, 1, 1, 1, 6,9,12,19,9,13,19,23, 5.5,7,8.5,10,7,8,9,10)

# code clean data into a coding of the researcher's choice (dummy, effects, thermometer, or actual values).
X <- code_it(u_choice, index, c, levels, type, 0)
Y <- as.vector(u_choice[,1:5])
u_coded_data <- cbind(Y,X)
names(u_coded_data) <- data_header # rename coded data header
##########################################################################################

# Convert Chinese to US Units
# conversions:
rmb = 6.37 # $1 USD in RMB
USgas_usd = 3.7 # US gas price in USD
Chinagas_rmb = 7.61 # China gas price in RMB
USelec_usd = 0.12 # US electricity price in USD
Chinaelec_rmb = 0.48 # China electricity price in RMB
gal_gas_elec_equiv = 33.7 # kWh equivalent of 1 gallon of gasoline
lit_gas_elec_equiv = 33.7 # kWh equivalent of 1 litre of gasoline
gal_lit = 3.7 # litres in a gallon
lbs_kg = 2.205 # lbs in a kg

# Convert aer from km to miles
c_coded_data$PHEV_aer = round(c_coded_data$PHEV_aer/1.6, 3)
c_coded_data$BEV_aer = round(c_coded_data$BEV_aer/1.6, 3)

# Convert price & income from RMB to USD
c_coded_data$Price = round(c_coded_data$Price/rmb, 3)

# Convert op cost from RMB/km to USD/mile
c_coded_data$OpCost = round(c_coded_data$OpCost/rmb*1.6, 2)

##########################################################################################
# Combine China and USA choice data:
demos <- choice_data[14:21]
coded_data <- cbind(rbind(c_coded_data, u_coded_data), demos)

### Create and add interactions with fast charge & AER:
PHEV_PHEVaer <- coded_data$PHEV * coded_data$PHEV_aer
BEV_BEVaer <- coded_data$BEV * coded_data$BEV_aer
PHEV_FastCharge <- coded_data$PHEV * coded_data$FastCharge
BEV_FastCharge <- coded_data$BEV * coded_data$FastCharge

coded_data <- cbind(coded_data[,1:8], PHEV_PHEVaer, BEV_BEVaer, PHEV_FastCharge, BEV_FastCharge, coded_data[,9:26])

# Rearrage and remove old variables:
coded_data <- cbind(coded_data[,1:15], coded_data[,21:22],coded_data[,16:ncol(coded_data)])
coded_data <- coded_data[,-c(13,14,22,23,24)]

# Export choice data
write_csv(coded_data, here::here("6coded", "choice_data_coded.csv"))
