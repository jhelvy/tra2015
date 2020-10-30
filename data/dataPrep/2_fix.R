# Author: John P. Helveston
# First Coded: February 19, 2013
# Last Updated: October 29, 2020

# Description: This code imports the combined choice data from China, MTurk, and the Pittsburgh international auto show and re-codes the demographic data for uniformity

library(tidyverse)

# Import all demo data:
choice_data <- read_csv(here::here("2clean", "all_choice_data_clean.csv"))
demo_data <- read_csv(here::here("2clean", "all_demo_data_clean.csv"))
nChina <- nrow(demo_data[which(demo_data$usa==0),])
nMturk <- nrow(demo_data[which(demo_data$mturk==1),])
nPit <- nrow(demo_data[which(demo_data$usa==1 & demo_data$mturk==0),])

# Fix "brankrank2" typo:
names(demo_data)[57] <- "brandrank2"

# Adjust continuous levels according to suv or car:
choice_data[which(choice_data$SUVbuyer==1 & choice_data$Price==1),]$Price <- 6
choice_data[which(choice_data$SUVbuyer==1 & choice_data$Price==2),]$Price <- 7
choice_data[which(choice_data$SUVbuyer==1 & choice_data$Price==3),]$Price <- 8
choice_data[which(choice_data$SUVbuyer==1 & choice_data$Price==4),]$Price <- 9
choice_data[which(choice_data$SUVbuyer==1 & choice_data$Price==5),]$Price <- 10
choice_data[which(choice_data$SUVbuyer==1 & choice_data$OpCost==1),]$OpCost <- 5
choice_data[which(choice_data$SUVbuyer==1 & choice_data$OpCost==2),]$OpCost <- 6
choice_data[which(choice_data$SUVbuyer==1 & choice_data$OpCost==3),]$OpCost <- 7
choice_data[which(choice_data$SUVbuyer==1 & choice_data$OpCost==4),]$OpCost <- 8


#Create new columns in demo data for country-specific car make:

# China:
german_make = c(2,3,13,15,18,21,22)
american_make = c(4,5,6,7,8,9)
japanese_make = c(10,14,16,17,19,20)
chinese_make = seq(23,66)
korean_make = c(11,12)
other_make = c(67)
none_make = c(1)

c_country_make = rep(0, nrow(demo_data))
c_country_make[subset(demo_data, subset = CarMake %in% german_make)$RespNum] <- 1
c_country_make[subset(demo_data, subset = CarMake %in% american_make)$RespNum] <- 2
c_country_make[subset(demo_data, subset = CarMake %in% japanese_make)$RespNum] <- 3
c_country_make[subset(demo_data, subset = CarMake %in% chinese_make)$RespNum] <- 4
c_country_make[subset(demo_data, subset = CarMake %in% korean_make)$RespNum] <- 5
c_country_make[subset(demo_data, subset = CarMake %in% other_make)$RespNum] <- 6
c_country_make[subset(demo_data, subset = CarMake %in% none_make)$RespNum] <- 7
c_country_make[is.na(demo_data$CarMake)] <- 7
c_country_make <- c_country_make[1:nChina]

# Mturk
german_make = c(2,3,13,15,18,22,23)
american_make = c(4,5,6,7,8,9)
japanese_make = c(10,14,16,17,19,20,21)
chinese_make = seq(0)
korean_make = c(11,12)
other_make = c(24)
none_make = c(1)
m_country_make = rep(0, nrow(demo_data))
m_country_make[subset(demo_data, subset = CarMake %in% german_make)$RespNum] <- 1
m_country_make[subset(demo_data, subset = CarMake %in% american_make)$RespNum] <- 2
m_country_make[subset(demo_data, subset = CarMake %in% japanese_make)$RespNum] <- 3
m_country_make[subset(demo_data, subset = CarMake %in% chinese_make)$RespNum] <- 4
m_country_make[subset(demo_data, subset = CarMake %in% korean_make)$RespNum] <- 5
m_country_make[subset(demo_data, subset = CarMake %in% other_make)$RespNum] <- 6
m_country_make[subset(demo_data, subset = CarMake %in% none_make)$RespNum] <- 7
m_country_make[is.na(demo_data$CarMake)] <- 7
m_country_make <- m_country_make[(nChina+1):(nChina+nMturk)]

# PIT
german_make = c(2,3,13,15,18,22,23)+1
american_make = c(4,5,6,7,8,9)+1
japanese_make = c(1,10,14,16,17,19,20,21)+1
chinese_make = seq(0)
korean_make = c(11,12)+1
other_make = c(24)+1
none_make = c(1)
p_country_make = rep(0, nrow(demo_data))
p_country_make[subset(demo_data, subset = CarMake %in% german_make)$RespNum] <- 1
p_country_make[subset(demo_data, subset = CarMake %in% american_make)$RespNum] <- 2
p_country_make[subset(demo_data, subset = CarMake %in% japanese_make)$RespNum] <- 3
p_country_make[subset(demo_data, subset = CarMake %in% chinese_make)$RespNum] <- 4
p_country_make[subset(demo_data, subset = CarMake %in% korean_make)$RespNum] <- 5
p_country_make[subset(demo_data, subset = CarMake %in% other_make)$RespNum] <- 6
p_country_make[subset(demo_data, subset = CarMake %in% none_make)$RespNum] <- 7
p_country_make[is.na(demo_data$CarMake)] <- 7
p_country_make <- p_country_make[(nChina+nMturk+1):nrow(demo_data)]
country_make = c(c_country_make, m_country_make, p_country_make)

# Create new columns in demo data for country-specific brand pref:

# China:
german_brand = c(1,2,12,14,17,20,21)
american_brand = c(3,4,5,6,7,8)
japanese_brand = c(9,13,15,16,18,19)
chinese_brand = seq(22,65)
korean_brand = c(10,11)
other_brand = c(66)
no_pref = c(67)

c_country_brand1 = rep(0, length(demo_data$brandrank1))
c_country_brand1[subset(demo_data, subset = brandrank1 %in% german_brand)$RespNum] <- 1
c_country_brand1[subset(demo_data, subset = brandrank1 %in% american_brand)$RespNum] <- 2
c_country_brand1[subset(demo_data, subset = brandrank1 %in% japanese_brand)$RespNum] <- 3
c_country_brand1[subset(demo_data, subset = brandrank1 %in% chinese_brand)$RespNum] <- 4
c_country_brand1[subset(demo_data, subset = brandrank1 %in% korean_brand)$RespNum] <- 5
c_country_brand1[subset(demo_data, subset = brandrank1 %in% other_brand)$RespNum] <- 6
c_country_brand1[subset(demo_data, subset = brandrank1 %in% no_pref)$RespNum] <- 7
c_country_brand1[is.na(demo_data$brandrank1)] <- 7
c_country_brand1 <- c_country_brand1[1:nChina]

c_country_brand2 = rep(0, length(demo_data$brandrank2))
c_country_brand2[subset(demo_data, subset = brandrank2 %in% german_brand)$RespNum] <- 1
c_country_brand2[subset(demo_data, subset = brandrank2 %in% american_brand)$RespNum] <- 2
c_country_brand2[subset(demo_data, subset = brandrank2 %in% japanese_brand)$RespNum] <- 3
c_country_brand2[subset(demo_data, subset = brandrank2 %in% chinese_brand)$RespNum] <- 4
c_country_brand2[subset(demo_data, subset = brandrank2 %in% korean_brand)$RespNum] <- 5
c_country_brand2[subset(demo_data, subset = brandrank2 %in% other_brand)$RespNum] <- 6
c_country_brand2[subset(demo_data, subset = brandrank2 %in% no_pref)$RespNum] <- 7
c_country_brand2[is.na(demo_data$brandrank2)] <- 7
c_country_brand2 <- c_country_brand2[1:nChina]

c_country_brand3 = rep(0, length(demo_data$brandrank3))
c_country_brand3[subset(demo_data, subset = brandrank3 %in% german_brand)$RespNum] <- 1
c_country_brand3[subset(demo_data, subset = brandrank3 %in% american_brand)$RespNum] <- 2
c_country_brand3[subset(demo_data, subset = brandrank3 %in% japanese_brand)$RespNum] <- 3
c_country_brand3[subset(demo_data, subset = brandrank3 %in% chinese_brand)$RespNum] <- 4
c_country_brand3[subset(demo_data, subset = brandrank3 %in% korean_brand)$RespNum] <- 5
c_country_brand3[subset(demo_data, subset = brandrank3 %in% other_brand)$RespNum] <- 6
c_country_brand3[subset(demo_data, subset = brandrank3 %in% no_pref)$RespNum] <- 7
c_country_brand3[is.na(demo_data$brandrank3)] <- 7
c_country_brand3 <- c_country_brand3[1:nChina]

# Mturk
german_brand = c(2,3,13,15,18,22,23)
american_brand = c(4,5,6,7,8,9)
japanese_brand = c(10,14,16,17,19,20,21)
chinese_brand = seq(0)
korean_brand = c(11,12)
other_brand = c(24)
no_pref = c(1)

m_country_brand1 = rep(0, length(demo_data$brandrank1))
m_country_brand1[subset(demo_data, subset = brandrank1 %in% german_brand)$RespNum] <- 1
m_country_brand1[subset(demo_data, subset = brandrank1 %in% american_brand)$RespNum] <- 2
m_country_brand1[subset(demo_data, subset = brandrank1 %in% japanese_brand)$RespNum] <- 3
m_country_brand1[subset(demo_data, subset = brandrank1 %in% chinese_brand)$RespNum] <- 4
m_country_brand1[subset(demo_data, subset = brandrank1 %in% korean_brand)$RespNum] <- 5
m_country_brand1[subset(demo_data, subset = brandrank1 %in% other_brand)$RespNum] <- 6
m_country_brand1[subset(demo_data, subset = brandrank1 %in% no_pref)$RespNum] <- 7
m_country_brand1[is.na(demo_data$brandrank1)] <- 7
m_country_brand1 <- m_country_brand1[(nChina+1):(nChina+nMturk)]

m_country_brand2 = rep(0, length(demo_data$brandrank2))
m_country_brand2[subset(demo_data, subset = brandrank2 %in% german_brand)$RespNum] <- 1
m_country_brand2[subset(demo_data, subset = brandrank2 %in% american_brand)$RespNum] <- 2
m_country_brand2[subset(demo_data, subset = brandrank2 %in% japanese_brand)$RespNum] <- 3
m_country_brand2[subset(demo_data, subset = brandrank2 %in% chinese_brand)$RespNum] <- 4
m_country_brand2[subset(demo_data, subset = brandrank2 %in% korean_brand)$RespNum] <- 5
m_country_brand2[subset(demo_data, subset = brandrank2 %in% other_brand)$RespNum] <- 6
m_country_brand2[subset(demo_data, subset = brandrank2 %in% no_pref)$RespNum] <- 7
m_country_brand2[is.na(demo_data$brandrank2)] <- 7
m_country_brand2 <- m_country_brand2[(nChina+1):(nChina+nMturk)]

m_country_brand3 = rep(0, length(demo_data$brandrank3))
m_country_brand3[subset(demo_data, subset = brandrank3 %in% german_brand)$RespNum] <- 1
m_country_brand3[subset(demo_data, subset = brandrank3 %in% american_brand)$RespNum] <- 2
m_country_brand3[subset(demo_data, subset = brandrank3 %in% japanese_brand)$RespNum] <- 3
m_country_brand3[subset(demo_data, subset = brandrank3 %in% chinese_brand)$RespNum] <- 4
m_country_brand3[subset(demo_data, subset = brandrank3 %in% korean_brand)$RespNum] <- 5
m_country_brand3[subset(demo_data, subset = brandrank3 %in% other_brand)$RespNum] <- 6
m_country_brand3[subset(demo_data, subset = brandrank3 %in% no_pref)$RespNum] <- 7
m_country_brand3[is.na(demo_data$brandrank3)] <- 7
m_country_brand3 <- m_country_brand3[(nChina+1):(nChina+nMturk)]

# Pit
german_brand = c(2,3,13,15,18,22,23)+1
american_brand = c(4,5,6,7,8,9)+1
japanese_brand = c(1,10,14,16,17,19,20,21)+1
chinese_brand = seq(0)
korean_brand = c(11,12)+1
other_brand = c(24)+1
no_pref = c(1)

p_country_brand1 = rep(0, length(demo_data$brandrank1))
p_country_brand1[subset(demo_data, subset = brandrank1 %in% german_brand)$RespNum] <- 1
p_country_brand1[subset(demo_data, subset = brandrank1 %in% american_brand)$RespNum] <- 2
p_country_brand1[subset(demo_data, subset = brandrank1 %in% japanese_brand)$RespNum] <- 3
p_country_brand1[subset(demo_data, subset = brandrank1 %in% chinese_brand)$RespNum] <- 4
p_country_brand1[subset(demo_data, subset = brandrank1 %in% korean_brand)$RespNum] <- 5
p_country_brand1[subset(demo_data, subset = brandrank1 %in% other_brand)$RespNum] <- 6
p_country_brand1[subset(demo_data, subset = brandrank1 %in% no_pref)$RespNum] <- 7
p_country_brand1[is.na(demo_data$brandrank1)] <- 7
p_country_brand1 <- p_country_brand1[(nChina+nMturk+1):nrow(demo_data)]

p_country_brand2 = rep(0, length(demo_data$brandrank2))
p_country_brand2[subset(demo_data, subset = brandrank2 %in% german_brand)$RespNum] <- 1
p_country_brand2[subset(demo_data, subset = brandrank2 %in% american_brand)$RespNum] <- 2
p_country_brand2[subset(demo_data, subset = brandrank2 %in% japanese_brand)$RespNum] <- 3
p_country_brand2[subset(demo_data, subset = brandrank2 %in% chinese_brand)$RespNum] <- 4
p_country_brand2[subset(demo_data, subset = brandrank2 %in% korean_brand)$RespNum] <- 5
p_country_brand2[subset(demo_data, subset = brandrank2 %in% other_brand)$RespNum] <- 6
p_country_brand2[subset(demo_data, subset = brandrank2 %in% no_pref)$RespNum] <- 7
p_country_brand2[is.na(demo_data$brandrank2)] <- 7
p_country_brand2 <- p_country_brand2[(nChina+nMturk+1):nrow(demo_data)]

p_country_brand3 = rep(0, length(demo_data$brandrank3))
p_country_brand3[subset(demo_data, subset = brandrank3 %in% german_brand)$RespNum] <- 1
p_country_brand3[subset(demo_data, subset = brandrank3 %in% american_brand)$RespNum] <- 2
p_country_brand3[subset(demo_data, subset = brandrank3 %in% japanese_brand)$RespNum] <- 3
p_country_brand3[subset(demo_data, subset = brandrank3 %in% chinese_brand)$RespNum] <- 4
p_country_brand3[subset(demo_data, subset = brandrank3 %in% korean_brand)$RespNum] <- 5
p_country_brand3[subset(demo_data, subset = brandrank3 %in% other_brand)$RespNum] <- 6
p_country_brand3[subset(demo_data, subset = brandrank3 %in% no_pref)$RespNum] <- 7
p_country_brand3[is.na(demo_data$brandrank3)] <- 7
p_country_brand3 <- p_country_brand3[(nChina+nMturk+1):nrow(demo_data)]

country_brand1 = c(c_country_brand1, m_country_brand1, p_country_brand1)
country_brand2 = c(c_country_brand2, m_country_brand2, p_country_brand2)
country_brand3 = c(c_country_brand3, m_country_brand3, p_country_brand3)

# add car make and brand pref by country data into demo data:
demo_data = cbind(demo_data, country_make, country_brand1, country_brand2, country_brand3)

#####################################################
# Prep demo data with actual values &
# create bins for binary variables based on medians:
#####################################################

#####################################################
# INCOME
c_income_levels = c(20, 25, 35, 45, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240, 275, 350, 450, 500,0)/6.3
u_income_levels = c(12.5,16.25,22.5,27.5,33.75,43.5,56.25,68.75,81.25,93.75,100,0)
for (i in 1:length(c_income_levels)) {
	demo_data[which(demo_data$income == i & demo_data$usa==0),]$income <- c_income_levels[i]
}
for (i in 1:length(u_income_levels)) {
	demo_data[which(demo_data$income == i & demo_data$usa==1),]$income <- u_income_levels[i]
}
demo_data[which(demo_data$income == 0),]$income <- NA

c <- demo_data[which(demo_data$usa==0),]$income
c.med <- median(na.omit(c))
c.HIGH <- rep(0, length(c))
c.NA <- rep(0, length(c))
c.HIGH[which(c >= c.med)] <- 1
c.NA[which(is.na(c)==T)] <- 1

u <- demo_data[which(demo_data$usa==1),]$income
u.med <- median(na.omit(u))
u.HIGH <- rep(0, length(u))
u.NA <- rep(0, length(u))
u.HIGH[which(u >= u.med)] <- 1
u.NA[which(is.na(u)==T)] <- 1

income_HIGH <- c(c.HIGH, u.HIGH)
income_NA <- c(c.NA, u.NA)

demo_data = cbind(demo_data, income_HIGH, income_NA)

#####################################################
# AGE
age = 2013 - (demo_data$YOB+1941)
demo_data = cbind(demo_data, age)

c <- demo_data[which(demo_data$usa==0),]$age
c.med <- median(na.omit(c))
c.HIGH <- rep(0, length(c))
c.NA <- rep(0, length(c))
c.HIGH[which(c >= c.med)] <- 1
c.NA[which(is.na(c)==T)] <- 1

u <- demo_data[which(demo_data$usa==1),]$age
u.med <- median(na.omit(u))
u.HIGH <- rep(0, length(u))
u.NA <- rep(0, length(u))
u.HIGH[which(u >= u.med)] <- 1
u.NA[which(is.na(u)==T)] <- 1

age_HIGH <- c(c.HIGH, u.HIGH)
age_NA <- c(c.NA, u.NA)

demo_data = cbind(demo_data, age_HIGH, age_NA)

#####################################################
# FEMALE
demo_data$sex = demo_data$sex - 1
names(demo_data)[61] <- "female"
demo_data[which(demo_data$female==2),]$female <- NA

female_NA <- rep(0, length(demo_data$female))
female_NA[which(is.na(demo_data$female)==T)] <- 1

demo_data <- cbind(demo_data, female_NA)

#####################################################
# CHILDREN
demo_data[which(demo_data$children == 6 & demo_data$usa==1),]$children <- NA
demo_data[which(demo_data$children == 7 & demo_data$usa==0),]$children <- NA
demo_data$children = demo_data$children - 1
names(demo_data)[66] <- "num_children"

have_child = rep(0, nrow(demo_data))
have_child_NA = rep(0, nrow(demo_data))
have_child[which(demo_data$num_children>=1)] <- 1
have_child_NA[which(is.na(demo_data$num_children)==T)] <- 1
demo_data = cbind(demo_data, have_child, have_child_NA)

#####################################################
# HOUSEHOLD SIZE
demo_data[which(demo_data$householdsize == 8),]$householdsize <- NA

c <- demo_data[which(demo_data$usa==0),]$householdsize
c.med <- median(na.omit(c))
c.HIGH <- rep(0, length(c))
c.NA <- rep(0, length(c))
c.HIGH[which(c >= c.med)] <- 1
c.NA[which(is.na(c)==T)] <- 1

u <- demo_data[which(demo_data$usa==1),]$householdsize
u.med <- median(na.omit(u))
u.HIGH <- rep(0, length(u))
u.NA <- rep(0, length(u))
u.HIGH[which(u >= u.med)] <- 1
u.NA[which(is.na(u)==T)] <- 1

householdsize_HIGH <- c(c.HIGH, u.HIGH)
householdsize_NA <- c(c.NA, u.NA)

demo_data = cbind(demo_data, householdsize_HIGH, householdsize_NA)

#####################################################
# MARRIAGE
demo_data[which(demo_data$marriage == 6),]$marriage <- NA
demo_data[which(demo_data$marriage == 1 | demo_data$marriage == 4 | demo_data$marriage == 5),]$marriage <- 0
demo_data[which(demo_data$marriage == 2 | demo_data$marriage == 3),]$marriage <- 1

marriage_NA <- rep(0, nrow(demo_data))
marriage_NA[which(is.na(demo_data$marriage)==T)] <- 1
demo_data = cbind(demo_data, marriage_NA)

#####################################################
# EDUCATION
demo_data[which(demo_data$education == 8),]$education <- NA
demo_data[which(demo_data$education == 7),]$education <- 12
demo_data[which(demo_data$education == 6),]$education <- 10
demo_data[which(demo_data$education == 5),]$education <- 8
demo_data[which(demo_data$education == 4),]$education <- 6
demo_data[which(demo_data$education == 3),]$education <- 6
demo_data[which(demo_data$education == 2),]$education <- 4
demo_data[which(demo_data$education == 1),]$education <- 2

# Create new variables for whether has college degree or not:

college_grad = rep(0, nrow(demo_data))
college_grad_NA = rep(0, nrow(demo_data))
college_grad[which(demo_data$education>6)] <- 1
college_grad_NA[which(is.na(demo_data$education)==T)] <- 1

demo_data = cbind(demo_data, college_grad, college_grad_NA)

#####################################################
# DAILY VMT

# China (km)
# 1 = 8
# 2 = 12
# 3 = 20
# 4 = 28
# 5 = 36
# 6 = 44
# 7 = 60
# 8 = 64
# 9 = I don't know
# 10 = I don't drive

# U.S. (miles)
# 1 = 5
# 2 = 7.5
# 3 = 12.5
# 4 = 17.5
# 5 = 22.5
# 6 = 27.5
# 7 = 32.5
# 8 = 37.5
# 9 = 40
# 10 = I don't know
# 11 = I don't drive

c <- demo_data[which(demo_data$usa==0),]$dailyvmt
c.no_drive_expr <- rep(0, length(c))
c.no_drive_expr[which(c==10)] <- 1

u <- demo_data[which(demo_data$usa==1),]$dailyvmt
u.no_drive_expr <- rep(0, length(u))
u.no_drive_expr[which(u==10)] <- 1

no_drive_expr <- c(c.no_drive_expr, u.no_drive_expr)
demo_data = cbind(demo_data, no_drive_expr)

c_daily_vmt = c(8,12,20,28,36,44,60,64,0,0)/1.6
u_daily_vmt = c(5,7.5,12.5,17.5,22.5,27.5,32.5,37.5,40,0,0)
for (i in 1:length(c_daily_vmt)) {
	demo_data[which(demo_data$dailyvmt == i & demo_data$usa==0),]$dailyvmt <- c_daily_vmt[i]
}
for (i in 1:length(u_daily_vmt)) {
	demo_data[which(demo_data$dailyvmt == i & demo_data$usa==1),]$dailyvmt <- u_daily_vmt[i]
}
demo_data[which(demo_data$dailyvmt == 0),]$dailyvmt <- NA

c <- demo_data[which(demo_data$usa==0),]$dailyvmt
c.med <- median(na.omit(c))
c.HIGH <- rep(0, length(c))
c.NA <- rep(0, length(c))
c.HIGH[which(c >= c.med)] <- 1
c.NA[which(is.na(c)==T)] <- 1

u <- demo_data[which(demo_data$usa==1),]$dailyvmt
u.med <- median(na.omit(u))
u.HIGH <- rep(0, length(u))
u.NA <- rep(0, length(u))
u.HIGH[which(u >= u.med)] <- 1
u.NA[which(is.na(u)==T)] <- 1

dailyvmt_HIGH <- c(c.HIGH, u.HIGH)
dailyvmt_NA <- c(c.NA, u.NA)

demo_data = cbind(demo_data, dailyvmt_HIGH, dailyvmt_NA)

#####################################################
# ANNUAL VMT

# China (km):
# 1 = 2500
# 2 = 3750
# 3 = 6000
# 4 = 8000
# 5 = 10000
# 6 = 12000
# 7 = 14000
# 8 = 16000
# 9 = 18500
# 10 = 22500
# 11 = 27500
# 12 = 35000
# 13 = 45000
# 14 = 50000
# 15 = I don't know
# 16 = I don't drive

# US (miles):
# China:
# 1 = 5000
# 2 = 6000
# 3 = 8000
# 4 = 10000
# 5 = 12000
# 6 = 14000
# 7 = 16000
# 8 = 18000
# 9 = 19000
# 10 = I don't know
# 11 = I don't drive

c_annualvmt_levels = c(2500, 3750,6000,8000,10000,12000,14000,16000,18500,22500, 27500,35000,45000,50000,0,0)
u_annualvmt_levels = c(5000,6000,8000,10000,12000,14000,16000,18000,19000,0,0)
for (i in 1:length(c_annualvmt_levels)) {
	demo_data[which(demo_data$annualvmt==i & demo_data$usa==0),]$annualvmt <- c_annualvmt_levels[i]
}
for (i in 1:length(u_annualvmt_levels)) {
	demo_data[which(demo_data$annualvmt==i & demo_data$usa==1),]$annualvmt <- u_annualvmt_levels[i]
}
demo_data[which(demo_data$annualvmt == 0),]$annualvmt <- NA

c <- demo_data[which(demo_data$usa==0),]$annualvmt
c.med <- median(na.omit(c))
c.HIGH <- rep(0, length(c))
c.NA <- rep(0, length(c))
c.HIGH[which(c >= c.med)] <- 1
c.NA[which(is.na(c)==T)] <- 1

u <- demo_data[which(demo_data$usa==1),]$annualvmt
u.med <- median(na.omit(u))
u.HIGH <- rep(0, length(u))
u.NA <- rep(0, length(u))
u.HIGH[which(u >= u.med)] <- 1
u.NA[which(is.na(u)==T)] <- 1

annualvmt_HIGH <- c(c.HIGH, u.HIGH)
annualvmt_NA <- c(c.NA, u.NA)

demo_data = cbind(demo_data, annualvmt_HIGH, annualvmt_NA)

#####################################################
# RECENT PAST BUYER

demo_data[which(demo_data$pastbuyer == 1),]$pastbuyer <- 0
demo_data[which(demo_data$pastbuyer == 2),]$pastbuyer <- 1
demo_data[which(demo_data$pastbuyer == 3),]$pastbuyer <- 1
names(demo_data)[7] <- "recent_past_buyer"

#####################################################
# VEHICLE OWNERSHIP
demo_data$VehicleOwnership = demo_data$VehicleOwnership - 1
names(demo_data)[13] <- "num_vehicles"

vehicle_ownership <- rep(0, nrow(demo_data))
vehicle_ownership_NA <- rep(0, nrow(demo_data))
first_time_buyer <- rep(0, nrow(demo_data))
vehicle_ownership[which(demo_data$num_vehicles>=1)] <- 1
vehicle_ownership_NA[which(is.na(demo_data$num_vehicles)==T)] <- 1
first_time_buyer[which(demo_data$num_vehicles==0)] <- 1

demo_data = cbind(demo_data, vehicle_ownership, vehicle_ownership_NA, first_time_buyer)

#####################################################
# HOME SLOW CHARGING

# China
# 1=home, 2=work, 3=parking garage, 4=other, 5=don't know
c_home_charge <- rep(0,nrow(demo_data))
c_home_charge[which(demo_data$slowchargingaccess_1==1)] <- 1
c_home_charge[which(demo_data$slowchargingaccess_3==3)] <- 1
c_home_charge[which(demo_data$slowchargingaccess_4==4)] <- 1
c_home_charge <- c_home_charge[1:nChina]

# Mturk & Pit
# 1=home garage, 2=home public garage, 3=home driveway, 4=home street, 5=home other
m_home_charge <- rep(0,nrow(demo_data))
m_home_charge[which(demo_data$slowchargingaccess_1==1)] <- 1
m_home_charge[which(demo_data$slowchargingaccess_2==2)] <- 1
m_home_charge[which(demo_data$slowchargingaccess_3==3)] <- 1
m_home_charge[which(demo_data$slowchargingaccess_4==4)] <- 1
m_home_charge[which(demo_data$slowchargingaccess_5==5)] <- 1
m_home_charge <- m_home_charge[(nChina+1):nrow(demo_data)]

home_charge <- c(c_home_charge, m_home_charge)
demo_data <- cbind(demo_data, home_charge)

#####################################################
# Create variable for WORK slow charging availability and insert into dataframes:

# China
# 1=home, 2=work, 3=parking garage, 4=other, 5=don't know
c_work_charge <- rep(0,nrow(demo_data))
c_work_charge[which(demo_data$slowchargingaccess_2==2)] <- 1
c_work_charge <- c_work_charge[1:nChina]

# Mturk & Pit
# 6=work parking garage, 7=work parking lot, 8=work street
m_work_charge <- rep(0,nrow(demo_data))
m_work_charge[which(demo_data$slowchargingaccess_6==6)] <- 1
m_work_charge[which(demo_data$slowchargingaccess_7==7)] <- 1
m_work_charge[which(demo_data$slowchargingaccess_8==8)] <- 1
m_work_charge <- m_work_charge[(nChina+1):nrow(demo_data)]

work_charge <- c(c_work_charge, m_work_charge)
demo_data <- cbind(demo_data, work_charge)

#####################################################
# Fix fast charge coding:
demo_data$fastchargingaccess[which(demo_data$fastchargingaccess==2 | demo_data$fastchargingaccess==3)] <- 0

#####################################################
# Fix number of parking spaces coding:
demo_data$parkingHomeGarage[which(demo_data$parkingHomeGarage==7)] <- NA
demo_data$parkingHomeStructure[which(demo_data$parkingHomeStructure==7)] <- NA
demo_data$parkingHomeDriveway[which(demo_data$parkingHomeDriveway==7)] <- NA
demo_data$parkingHomeStreet[which(demo_data$parkingHomeStreet==7)] <- NA
demo_data$parkingHomeOther[which(demo_data$parkingHomeOther==7)] <- NA

demo_data[which(demo_data$usa==1),]$parkingHomeGarage <- demo_data[which(demo_data$usa==1),]$parkingHomeGarage - 1
demo_data[which(demo_data$usa==1),]$parkingHomeStructure <- demo_data[which(demo_data$usa==1),]$parkingHomeStructure - 1
demo_data[which(demo_data$usa==1),]$parkingHomeDriveway <- demo_data[which(demo_data$usa==1),]$parkingHomeDriveway - 1
demo_data[which(demo_data$usa==1),]$parkingHomeStreet <- demo_data[which(demo_data$usa==1),]$parkingHomeStreet - 1
demo_data[which(demo_data$usa==1),]$parkingHomeOther <- demo_data[which(demo_data$usa==1),]$parkingHomeOther - 1

demo_data[which(is.na(demo_data$parkingHomeGarage)==T),]$parkingHomeGarage <- 0
demo_data[which(is.na(demo_data$parkingHomeStructure)==T),]$parkingHomeStructure <- 0
demo_data[which(is.na(demo_data$parkingHomeDriveway)==T),]$parkingHomeGarage <- 0
demo_data[which(is.na(demo_data$parkingHomeStreet)==T),]$parkingHomeStreet <- 0
demo_data[which(is.na(demo_data$parkingHomeOther)==T),]$parkingHomeOther <- 0

#####################################################
# Create variable for whether their environmental view is important:
env_appearance = rep(0, length(demo_data$EnvLifeRatings_r2))
env_appearance[which(demo_data$EnvLifeRatings_r2==4 | demo_data$EnvLifeRatings_r2==5)] <- 1
demo_data <- cbind(demo_data, env_appearance)

#####################################################
# Create variable for wether the appearance of their car is an important status symbol:
status_symbol = rep(0, length(demo_data$EnvLifeRatings_r1))
status_symbol[which(demo_data$EnvLifeRatings_r1==4 | demo_data$EnvLifeRatings_r1==5)] <- 1
demo_data <- cbind(demo_data, status_symbol)

#####################################################
# Export data
write_csv(choice_data, here::here("3fixed", "choice_data_clean_fixed.csv"))
write_csv(demo_data, here::here("3fixed", "demo_data_clean_fixed.csv"))
