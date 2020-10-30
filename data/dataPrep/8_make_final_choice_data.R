library(tidyverse)

# China data ------------------------------------------------

# Read in data
choice_data <- read_csv(here::here("6coded", "choice_data_coded.csv"))
weights <- read_csv(here::here("4weights", "china_car_weights_0.2.csv"))

# Screen out U.S. Data:
choice_data <- choice_data[-which(choice_data$usa==1),]

# Screen out Beijing Data:
choice_data <- choice_data[-which(choice_data$shanghai==0 & choice_data$shenzhen==0 & choice_data$chengdu==0),]

# Separate cars and SUVs
choice_data_suv <- choice_data[which(choice_data$SUVbuyer==1),]
choice_data_car <- choice_data[which(choice_data$SUVbuyer==0),]

# Renumber IDs
choice_data_car$ID <- rep(seq(1,length(unique(choice_data_car$ID))), each=45)
choice_data_suv$ID <- rep(seq(1,length(unique(choice_data_suv$ID))), each=45)

# create a vector to ID each unique choice observation:
number <- length(unique(choice_data_car$Task))*length(unique(choice_data_car$ID))
obsnum <- rep(seq(1,number), each=3)
choice_data_car <- cbind(choice_data_car, obsnum)
number <- length(unique(choice_data_suv$Task))*length(unique(choice_data_suv$ID))
obsnum <- rep(seq(1,number), each=3)
choice_data_suv <- cbind(choice_data_suv, obsnum)

# Data to export
choice_data_car <- subset(choice_data_car, select = c("ID", "obsnum", "Choice", "HEV", "PHEV", "BEV", "PHEV_PHEVaer", "BEV_BEVaer", "PHEV_FastCharge", "BEV_FastCharge", "Price", "OpCost", "Acceleration", "American", "Japanese", "Chinese", "SKorean"))
choice_data_suv <- subset(choice_data_suv, select = c("ID", "obsnum", "Choice", "HEV", "PHEV", "BEV", "PHEV_PHEVaer", "BEV_BEVaer", "PHEV_FastCharge", "BEV_FastCharge", "Price", "OpCost", "Acceleration", "American", "Japanese", "Chinese", "SKorean"))

# Add weights column for car data
choice_data_car$weights <- rep(weights$V1, each = 45)

# Save data
write_csv(choice_data_car, here::here("7final", "china_cars.csv"))
write_csv(choice_data_suv, here::here("7final", "china_suvs.csv"))






# USA data ------------------------------------------------

# Read in data
choice_data <- read_csv(here::here("6coded", "choice_data_coded.csv"))
weights <- read_csv(here::here("4weights", "us_car_weights_0.2.csv"))

pit <- rep(0, nrow(choice_data))
pit[which(choice_data$mturk==0 & choice_data$usa==1)] <- 1
choice_data <- cbind(choice_data, pit)
rur <- rep(0, nrow(choice_data))
rur[which(choice_data$urban==0 & choice_data$usa==1 & choice_data$suburban==0)] <- 1
choice_data <- cbind(choice_data, rur)

# Screen out China Data:
choice_data <- choice_data[-which(choice_data$usa==0),]
demo_data <- demo_data[-which(demo_data$usa==0),]

# Separate cars and SUVs
choice_data_suv <- choice_data[which(choice_data$SUVbuyer==1),]
choice_data_car <- choice_data[which(choice_data$SUVbuyer==0),]

# Renumber IDs
choice_data_car$ID <- rep(seq(1,length(unique(choice_data_car$ID))), each=45)
choice_data_suv$ID <- rep(seq(1,length(unique(choice_data_suv$ID))), each=45)

# create a vector to ID each unique choice observation:
number <- length(unique(choice_data_car$Task))*length(unique(choice_data_car$ID))
obsnum <- rep(seq(1,number), each=3)
choice_data_car <- cbind(choice_data_car, obsnum)
number <- length(unique(choice_data_suv$Task))*length(unique(choice_data_suv$ID))
obsnum <- rep(seq(1,number), each=3)
choice_data_suv <- cbind(choice_data_suv, obsnum)

# Data to export
choice_data_car <- subset(choice_data_car, select = c("ID", "obsnum", "Choice", "HEV", "PHEV", "BEV", "PHEV_PHEVaer", "BEV_BEVaer", "PHEV_FastCharge", "BEV_FastCharge", "Price", "OpCost", "Acceleration", "American", "Japanese", "Chinese", "SKorean"))
choice_data_suv <- subset(choice_data_suv, select = c("ID", "obsnum", "Choice", "HEV", "PHEV", "BEV", "PHEV_PHEVaer", "BEV_BEVaer", "PHEV_FastCharge", "BEV_FastCharge", "Price", "OpCost", "Acceleration", "American", "Japanese", "Chinese", "SKorean"))

# Add weights column for car data
choice_data_car$weights <- rep(weights$V1, each = 45)

# Save data
write_csv(choice_data_car, here::here("7final", "us_cars.csv"))
write_csv(choice_data_suv, here::here("7final", "us_suvs.csv"))
