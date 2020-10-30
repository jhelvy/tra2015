# Author: John P. Helveston
# First Coded: February 19, 2013
# Last Updated: October 29, 2020

# Description: This code imports all of the choice data from China and the U.S. (both mturk and auto show data) and cleans it by removing bad responses as well as those who completed the survey in less than 6 minutes or did not choose #2 in the example question.

library(tidyverse)

# CHINA DATA ------------------------------------------------------------------

# Import all demo data:
BJ_C_demo_data <- read_csv(here::here("1raw", "china", "BJ_C_demo_data.csv"))
BJ_S_demo_data <- read_csv(here::here("1raw", "china", "BJ_S_demo_data.csv"))
SH_C_demo_data <- read_csv(here::here("1raw", "china", "SH_C_demo_data.csv"))
SH_S_demo_data <- read_csv(here::here("1raw", "china", "SH_S_demo_data.csv"))
SZ_C_demo_data <- read_csv(here::here("1raw", "china", "SZ_C_demo_data.csv"))
SZ_S_demo_data <- read_csv(here::here("1raw", "china", "SZ_S_demo_data.csv"))
CD_C_demo_data <- read_csv(here::here("1raw", "china", "CD_C_demo_data.csv"))
CD_S_demo_data <- read_csv(here::here("1raw", "china", "CD_S_demo_data.csv"))
names(BJ_C_demo_data)[1] <- "RespNum"
names(BJ_S_demo_data)[1] <- "RespNum"
names(SH_C_demo_data)[1] <- "RespNum"
names(SH_S_demo_data)[1] <- "RespNum"
names(SZ_C_demo_data)[1] <- "RespNum"
names(SZ_S_demo_data)[1] <- "RespNum"
names(CD_C_demo_data)[1] <- "RespNum"
names(CD_S_demo_data)[1] <- "RespNum"

# Import all choice data:
BJ_C_choice <- read_csv(here::here("1raw", "china", "BJ_C_choice_data.csv"))
BJ_S_choice <- read_csv(here::here("1raw", "china", "BJ_S_choice_data.csv"))
SH_C_choice <- read_csv(here::here("1raw", "china", "SH_C_choice_data.csv"))
SH_S_choice <- read_csv(here::here("1raw", "china", "SH_S_choice_data.csv"))
SZ_C_choice <- read_csv(here::here("1raw", "china", "SZ_C_choice_data.csv"))
SZ_S_choice <- read_csv(here::here("1raw", "china", "SZ_S_choice_data.csv"))
CD_C_choice <- read_csv(here::here("1raw", "china", "CD_C_choice_data.csv"))
CD_S_choice <- read_csv(here::here("1raw", "china", "CD_S_choice_data.csv"))

# Delete demo data rows that don't have choice data:
BJ_C_demo_data <- subset(BJ_C_demo_data, subset = RespNum %in% unique(BJ_C_choice$RespNum))
BJ_S_demo_data <- subset(BJ_S_demo_data, subset = RespNum %in% unique(BJ_S_choice$RespNum))
SH_C_demo_data <- subset(SH_C_demo_data, subset = RespNum %in% unique(SH_C_choice$RespNum))
SH_S_demo_data <- subset(SH_S_demo_data, subset = RespNum %in% unique(SH_S_choice$RespNum))
SZ_C_demo_data <- subset(SZ_C_demo_data, subset = RespNum %in% unique(SZ_C_choice$RespNum))
SZ_S_demo_data <- subset(SZ_S_demo_data, subset = RespNum %in% unique(SZ_S_choice$RespNum))
CD_C_demo_data <- subset(CD_C_demo_data, subset = RespNum %in% unique(CD_C_choice$RespNum))
CD_S_demo_data <- subset(CD_S_demo_data, subset = RespNum %in% unique(CD_S_choice$RespNum))

# Combine C&S data and add dummy for "SUVbuyer" in each city:
BJ_S_choice$RespNum <- BJ_S_choice$RespNum + max(BJ_C_choice$RespNum)
SH_S_choice$RespNum <- SH_S_choice$RespNum + max(SH_C_choice$RespNum)
SZ_S_choice$RespNum <- SZ_S_choice$RespNum + max(SZ_C_choice$RespNum)
CD_S_choice$RespNum <- CD_S_choice$RespNum + max(CD_C_choice$RespNum)
BJ_S_demo_data$RespNum <- BJ_S_demo_data$RespNum + max(BJ_C_demo_data$RespNum)
SH_S_demo_data$RespNum <- SH_S_demo_data$RespNum + max(SH_C_demo_data$RespNum)
SZ_S_demo_data$RespNum <- SZ_S_demo_data$RespNum + max(SZ_C_demo_data$RespNum)
CD_S_demo_data$RespNum <- CD_S_demo_data$RespNum + max(CD_C_demo_data$RespNum)

SUVbuyer = c(rep(0,nrow(BJ_C_demo_data)),rep(1,nrow(BJ_S_demo_data)))
BJ_demo_data = cbind(rbind(BJ_C_demo_data, BJ_S_demo_data), SUVbuyer)
SUVbuyer = c(rep(0,nrow(BJ_C_choice)),rep(1,nrow(BJ_S_choice)))
BJ_choice = cbind(rbind(BJ_C_choice, BJ_S_choice), SUVbuyer)

SUVbuyer = c(rep(0,nrow(SH_C_demo_data)),rep(1,nrow(SH_S_demo_data)))
SH_demo_data = cbind(rbind(SH_C_demo_data, SH_S_demo_data), SUVbuyer)
SUVbuyer = c(rep(0,nrow(SH_C_choice)),rep(1,nrow(SH_S_choice)))
SH_choice = cbind(rbind(SH_C_choice, SH_S_choice), SUVbuyer)

SUVbuyer = c(rep(0,nrow(SZ_C_demo_data)),rep(1,nrow(SZ_S_demo_data)))
SZ_demo_data = cbind(rbind(SZ_C_demo_data, SZ_S_demo_data), SUVbuyer)
SUVbuyer = c(rep(0,nrow(SZ_C_choice)),rep(1,nrow(SZ_S_choice)))
SZ_choice = cbind(rbind(SZ_C_choice, SZ_S_choice), SUVbuyer)

SUVbuyer = c(rep(0,nrow(CD_C_demo_data)),rep(1,nrow(CD_S_demo_data)))
CD_demo_data = cbind(rbind(CD_C_demo_data, CD_S_demo_data), SUVbuyer)
SUVbuyer = c(rep(0,nrow(CD_C_choice)),rep(1,nrow(CD_S_choice)))
CD_choice = cbind(rbind(CD_C_choice, CD_S_choice), SUVbuyer)

# Combine all cities and add dummies for "shanghai", "shenzhen", and "chengdu"
SH_choice$RespNum <- SH_choice$RespNum + max(BJ_choice$RespNum)
SZ_choice$RespNum <- SZ_choice$RespNum + max(SH_choice$RespNum)
CD_choice$RespNum <- CD_choice$RespNum + max(SZ_choice$RespNum)
SH_demo_data$RespNum <- SH_demo_data$RespNum + max(BJ_demo_data$RespNum)
SZ_demo_data$RespNum <- SZ_demo_data$RespNum + max(SH_demo_data$RespNum)
CD_demo_data$RespNum <- CD_demo_data$RespNum + max(SZ_demo_data$RespNum)

shanghai = c(rep(0,nrow(BJ_demo_data)), rep(1,nrow(SH_demo_data)), rep(0,nrow(SZ_demo_data)), rep(0,nrow(CD_demo_data)))
shenzhen = c(rep(0,nrow(BJ_demo_data)), rep(0,nrow(SH_demo_data)), rep(1,nrow(SZ_demo_data)), rep(0,nrow(CD_demo_data)))
chengdu = c(rep(0,nrow(BJ_demo_data)), rep(0,nrow(SH_demo_data)), rep(0,nrow(SZ_demo_data)), rep(1,nrow(CD_demo_data)))
demo_data = cbind(rbind(BJ_demo_data, SH_demo_data, SZ_demo_data, CD_demo_data), shanghai, shenzhen, chengdu)
shanghai = c(rep(0,nrow(BJ_choice)), rep(1,nrow(SH_choice)), rep(0,nrow(SZ_choice)), rep(0,nrow(CD_choice)))
shenzhen = c(rep(0,nrow(BJ_choice)), rep(0,nrow(SH_choice)), rep(1,nrow(SZ_choice)), rep(0,nrow(CD_choice)))
chengdu = c(rep(0,nrow(BJ_choice)), rep(0,nrow(SH_choice)), rep(0,nrow(SZ_choice)), rep(1,nrow(CD_choice)))
choice_data = cbind(rbind(BJ_choice, SH_choice, SZ_choice, CD_choice), shanghai, shenzhen, chengdu)

# Delete responses where practice question answer is not "2"
demo_data <- demo_data[which(demo_data$CBCFIX1==2),]

# Delete responses where time is less than 6 minutes
demo_data <- demo_data[which((demo_data$sys_ElapsedTime/60)>6),]

######### Delete definite "bad eggs" from data sets:
badIDs = c(695495, 772042, 1275261, 880878, 1155631, 1211753, 1146184, 774535, 695182, 1330113, 910220, 1559717, 865460, 1630295, 1518577, 723736, 968807, 1396857, 957436, 604685, 732117, 1141037, 550468, 760055, 1646986, 1071381, 1233643, 766933, 1625607, 539045, 933081, 1068826, 1443509, 501072, 1466940, 730249, 1677909, 1054057, 902693, 1291590, 1486942, 1146260, 809778, 869394, 1193291)

for (i in 1:length(badIDs)) {
    demo_data = demo_data[!demo_data$sys_InternalRespNum == badIDs[i],]
}

# Create final "clean" choice data set:
choice_data <- subset(choice_data, subset = RespNum %in% demo_data$RespNum)

# Rename IDs:
choice_data$RespNum <- as.vector(rep(1:(nrow(choice_data)/45), each=45))
demo_data$RespNum <- as.vector(seq(1,nrow(demo_data)))

# Export data
write_csv(choice_data, here::here("2clean", "china_choice_data_clean.csv"))
write_csv(demo_data, here::here("2clean", "china_demo_data_clean.csv"))
china_choice <- choice_data
china_demo <- demo_data


# USA MTURK DATA --------------------------------------------------------------

# Import all data:
C_demo_data <- read_csv(here::here("1raw", "mturk", "C_demo_data.csv"))
S_demo_data <- read_csv(here::here("1raw", "mturk", "S_demo_data.csv"))
C_choice <- read_csv(here::here("1raw", "mturk", "C_choice_data.csv"))
S_choice <- read_csv(here::here("1raw", "mturk", "S_choice_data.csv"))
names(C_demo_data)[1] <- "RespNum"
names(S_demo_data)[1] <- "RespNum"
names(C_choice)[c(9, 10)] <- names(S_choice)[c(9, 10)]

# Delete demo data rows that don't have choice data
C_demo_data <- subset(C_demo_data, subset = RespNum %in% unique(C_choice$RespNum))
S_demo_data <- subset(S_demo_data, subset = RespNum %in% unique(S_choice$RespNum))

# Combine C&S data and add dummy for "SUVbuyer"
S_choice$RespNum <- S_choice$RespNum + max(C_choice$RespNum)
S_demo_data$RespNum <- S_demo_data$RespNum + max(C_demo_data$RespNum)
SUVbuyer = c(rep(0,nrow(C_demo_data)),rep(1,nrow(S_demo_data)))
demo_data = cbind(rbind(C_demo_data, S_demo_data), SUVbuyer)
SUVbuyer = c(rep(0,nrow(C_choice)),rep(1,nrow(S_choice)))
choice_data = cbind(rbind(C_choice, S_choice), SUVbuyer)

# Delete responses where practice question answer is not "2"
demo_data <- demo_data[which(demo_data$CBCFIX1==2),]

# Delete responses where time is less than 6 minutes
demo_data <- demo_data[which((demo_data$sys_ElapsedTime/60)>6),]

# Create final "clean" choice data set
choice_data <- subset(choice_data, subset = RespNum %in% demo_data$RespNum)

# Rename IDs:
choice_data$RespNum <- as.vector(rep(1:(nrow(choice_data)/45), each=45))
demo_data$RespNum <- as.vector(seq(1,nrow(demo_data)))

# Export data
write_csv(choice_data, here::here("2clean", "mturk_choice_data_clean.csv"))
write_csv(demo_data, here::here("2clean", "mturk_demo_data_clean.csv"))
mturk_choice <- choice_data
mturk_demo <- demo_data

# USA PIT DATA --------------------------------------------------------------
# Data from the Pittsburgh International Auto Show

# Import all data
C_demo_data <- read_csv(here::here("1raw", "pit", "C_demo_data.csv"))
S_demo_data <- read_csv(here::here("1raw", "pit", "S_demo_data.csv"))
C_choice <- read_csv(here::here("1raw", "pit", "C_choice_data.csv"))
S_choice <- read_csv(here::here("1raw", "pit", "S_choice_data.csv"))
names(C_demo_data)[1] <- "RespNum"
names(S_demo_data)[1] <- "RespNum"

# Delete demo data rows that don't have choice data
C_demo_data <- subset(C_demo_data, subset = RespNum %in% unique(C_choice$RespNum))
S_demo_data <- subset(S_demo_data, subset = RespNum %in% unique(S_choice$RespNum))

# Combine C&S data and add dummy for "SUVbuyer"
S_choice$RespNum <- S_choice$RespNum + max(C_choice$RespNum)
S_demo_data$RespNum <- S_demo_data$RespNum + max(C_demo_data$RespNum)
SUVbuyer = c(rep(0,nrow(C_demo_data)),rep(1,nrow(S_demo_data)))
demo_data = cbind(rbind(C_demo_data, S_demo_data), SUVbuyer)
SUVbuyer = c(rep(0,nrow(C_choice)),rep(1,nrow(S_choice)))
choice_data = cbind(rbind(C_choice, S_choice), SUVbuyer)

# # Delete responses where practice question answer is not "2"
# demo_data <- demo_data[which(demo_data$CBCFIX1==2),]

# Delete responses where time is less than 6 minutes
demo_data <- demo_data[which((demo_data$sys_ElapsedTime/60)>6),]

# Create final "clean" choice data set
choice_data <- subset(choice_data, subset = RespNum %in% demo_data$RespNum)

# Rename IDs
choice_data$RespNum <- as.vector(rep(1:(nrow(choice_data)/45), each=45))
demo_data$RespNum <- as.vector(seq(1,nrow(demo_data)))

# Export data
write_csv(choice_data, here::here("2clean", "pit_choice_data_clean.csv"))
write_csv(demo_data, here::here("2clean", "pit_demo_data_clean.csv"))
pit_choice <- choice_data
pit_demo <- demo_data

# ALL DATA ------------------------------------------------------------------

# Re-number RespNums all three data sets:
mturk_choice$RespNum <- mturk_choice$RespNum + max(china_choice$RespNum)
mturk_demo$RespNum <- mturk_demo$RespNum + max(china_demo$RespNum)
pit_choice$RespNum <- pit_choice$RespNum + max(mturk_choice$RespNum)
pit_demo$RespNum <- pit_demo$RespNum + max(mturk_demo$RespNum)

# Combine all demo data, creating dummies for "usa" and "mturk"
pit_demo <- subset(pit_demo,select=c("RespNum","sys_StartTime","sys_EndTime","sys_ElapsedTime","sys_InternalRespNum","sys_CBCVersion_CBC","pastbuyer","futurebuyer","primarybuyer","quota","CarMake","CarModel","VehicleOwnership","dailyvmt","annualvmt","carsizeselect","carimagesmall","carimagemid","carimagelarge","CBCFIX1","productuseratings_r1","productuseratings_r2","productuseratings_r3","productuseratings_r4","productuseratings_r5","productuseratings_r6","EnvLifeRatings_r1","EnvLifeRatings_r2","EnvLifeRatings_r3","EnvLifeRatings_r4","DrivingTypeExper_1","DrivingTypeExper_2","DrivingTypeExper_3","DrivingTypeExper_4","DrivingTypeExper_5","consideration_1","consideration_2","consideration_3","consideration_4","consideration_5","parkingHomeGarage","parkingHomeStructure","parkingHomeDriveway","parkingHomeStreet","parkingHomeOther","slowchargingaccess_1","slowchargingaccess_2","slowchargingaccess_3","slowchargingaccess_4","slowchargingaccess_5","slowchargingaccess_6","slowchargingaccess_7","slowchargingaccess_8","slowchargingaccess_9","fastchargingaccess","brandrank1","brankrank2","brandrank3","maintenance","income","sex","householdsize","YOB","zipcode","education","children","marriage","enthusiast","SUVbuyer"))

fill1 <- matrix(data=0,nrow=nrow(pit_demo), ncol=3)
colnames(fill1) <- c("shanghai", "shenzhen", "chengdu")

pit_demo <- cbind(pit_demo, fill1)

china_demo1 <- subset(china_demo,select=c("RespNum","sys_StartTime","sys_EndTime","sys_ElapsedTime","sys_InternalRespNum","sys_CBCVersion_CBC","pastbuyer","futurebuyer","primarybuyer","quota","CarMake","CarModel","VehicleOwnership","dailyvmt","annualvmt","carsizeselect","carimagesmall","carimagemid","carimagelarge","CBCFIX1","productuseratings_r1","productuseratings_r2","productuseratings_r3","productuseratings_r4","productuseratings_r5","productuseratings_r6","EnvLifeRatings_r1","EnvLifeRatings_r2","EnvLifeRatings_r3","EnvLifeRatings_r4","DrivingTypeExper_1","DrivingTypeExper_2","DrivingTypeExper_3","DrivingTypeExper_4","DrivingTypeExper_5","consideration_1","consideration_2","consideration_3","consideration_4","consideration_5"))

fill1 <- matrix(data=0,nrow=nrow(china_demo), ncol=5)
colnames(fill1) <- c("parkingHomeGarage","parkingHomeStructure","parkingHomeDriveway","parkingHomeStreet","parkingHomeOther")

fill2 <- subset(china_demo, select=c("slowchargingaccess_1","slowchargingaccess_2","slowchargingaccess_3","slowchargingaccess_4","slowchargingaccess_5"))

fill3 <- matrix(data=0,nrow=nrow(china_demo), ncol=4)
colnames(fill3) <- c("slowchargingaccess_6","slowchargingaccess_7","slowchargingaccess_8","slowchargingaccess_9")

fill4 <- subset(china_demo, select=c("fastchargingaccess","brandrank1","brankrank2","brandrank3","maintenance","income","sex","householdsize","YOB","zipcode","education","children","marriage"))

fill5 <- matrix(data=0,nrow=nrow(china_demo), ncol=1)
colnames(fill5) <- c("enthusiast")

fill6 <- subset(china_demo, select=c("SUVbuyer", "shanghai", "shenzhen", "chengdu"))

china_demo <- cbind(china_demo1, fill1, fill2, fill3, fill4, fill5, fill6)

mturk_demo1 <- subset(mturk_demo,select=c("RespNum","sys_StartTime","sys_EndTime","sys_ElapsedTime","sys_InternalRespNum","sys_CBCVersion_CBC","pastbuyer","futurebuyer","primarybuyer","quota","CarMake","CarModel","VehicleOwnership","dailyvmt","annualvmt","carsizeselect","carimagesmall","carimagemid","carimagelarge","CBCFIX1","productuseratings_r1","productuseratings_r2","productuseratings_r3","productuseratings_r4","productuseratings_r5","productuseratings_r6","EnvLifeRatings_r1","EnvLifeRatings_r2","EnvLifeRatings_r3","EnvLifeRatings_r4","DrivingTypeExper_1","DrivingTypeExper_2","DrivingTypeExper_3","DrivingTypeExper_4","DrivingTypeExper_5","consideration_1","consideration_2","consideration_3","consideration_4","consideration_5","parkingHomeGarage","parkingHomeStructure","parkingHomeDriveway","parkingHomeStreet","parkingHomeOther","slowchargingaccess_1","slowchargingaccess_2","slowchargingaccess_3","slowchargingaccess_4","slowchargingaccess_5","slowchargingaccess_6","slowchargingaccess_7","slowchargingaccess_8","slowchargingaccess_9","fastchargingaccess","brandrank1","brankrank2","brandrank3","maintenance","income","sex","householdsize","YOB","zipcode","education","children","marriage"))

fill1 <- matrix(data=0,nrow=nrow(mturk_demo), ncol=1)
colnames(fill1) <- c("enthusiast")

fill2 <- subset(mturk_demo, select=c("SUVbuyer"))

fill3 <- matrix(data=0,nrow=nrow(mturk_demo), ncol=3)
colnames(fill3) <- c("shanghai", "shenzhen", "chengdu")

mturk_demo <- cbind(mturk_demo1, fill1, fill2, fill3)

usa <- c(rep(0,nrow(china_demo)),rep(1,(nrow(mturk_demo)+nrow(pit_demo))))
mturk <- c(rep(0,nrow(china_demo)),rep(1,nrow(mturk_demo)),rep(0,nrow(pit_demo)))
demo_data <- cbind(rbind(china_demo, mturk_demo, pit_demo), usa, mturk)

# Combine ALL THE CHOICE DATA, creating dummies for "usa" and "mturk"
names(china_choice)[10] <- "OpCost"
names(mturk_choice)[10] <- "OpCost"
names(pit_choice)[10] <- "OpCost"

fill1 <- matrix(data=0,nrow=nrow(pit_choice), ncol=3)
colnames(fill1) <- c("shanghai", "shenzhen", "chengdu")
pit_choice <- cbind(pit_choice, fill1)

fill1 <- matrix(data=0,nrow=nrow(mturk_choice), ncol=3)
colnames(fill1) <- c("shanghai", "shenzhen", "chengdu")
mturk_choice <- cbind(mturk_choice, fill1)

usa <- c(rep(0,nrow(china_choice)),rep(1,(nrow(mturk_choice)+nrow(pit_choice))))
mturk <- c(rep(0,nrow(china_choice)),rep(1,nrow(mturk_choice)),rep(0,nrow(pit_choice)))
choice_data <- cbind(rbind(china_choice, mturk_choice, pit_choice), usa, mturk)

# Export data
write_csv(choice_data, here::here("2clean", "all_choice_data_clean.csv"))
write_csv(demo_data, here::here("2clean", "all_demo_data_clean.csv"))
