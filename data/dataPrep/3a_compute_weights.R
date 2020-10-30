library(tidyverse)

# Optimization constraints:
l <- 0.2 # lower constraint value
u <- 5 # upper constraint value

# Plot CDFs Function:
cdf_plot = function(cdf,Y) {
	plot(cdf, type="l", lwd=3, axes=F, ann=F, ylim=c(0,1), xlim=c(0,max(cdf$x)), xaxs="i", yaxs="i"); axis(1, cex.axis=1.5, col="gray", col.axis="grey42"); axis(2, las=2, cex.axis=1.5, col="gray", col.axis="grey42")
	#find median
	med=max(which(cdf$y<=0.50))
	sample_median = round(cdf$x[med], 0)
	abline(v=sample_median, col="black", lty=5)

	lines(cdf$x, Y, col="red", lwd=3)
	#find median
	med=max(which(Y<=0.50))
	sample_median = round(cdf$x[med], 0)
	abline(v=sample_median, col="red", lty=5)
}

##### Objective function for finding optimal weights
ob_func = function(W, age_X, inc_X, y1_p, y2_p) {

	# Weighted CDF is X*W/sum(W):
	y1_s <- age_X%*%W/sum(W)
	y2_s <- inc_X%*%W/sum(W)

	# calculate error
	y1_err = sum((y1_p - y1_s)^2)
	y2_err = sum((y2_p - y2_s)^2)
	err = y1_err + y2_err

	return(err)
}

##########################
#	CHINA OPTIMIZATION   #
##########################

# READ IN DATA
age_p <- as.matrix(read.csv(here::here("4weights", "data", "china_cars", "age.csv"), header=F))
inc_p <- as.matrix(read.csv(here::here("4weights", "data", "china", "income.csv"), header=F))
data <- read_csv(here::here("3fixed", "demo_data_clean_fixed.csv"))

# Screen out Beijing Data & SUV buyers - China only:
data <- data[-which(data$usa==0 & data$shanghai==0 & data$shenzhen==0 & data$chengdu==0),]
data <- data[which(data$SUVbuyer==0),]
data <- data[which(data$usa==0),]

# Run the optimization
source('3b_run_weights_optim.R')

# SAVE CHINA RESULTS
c_age_p_cdf <- age_p_cdf
c_inc_p_cdf <- inc_p_cdf
c_age_s_y <- age_s_y
c_inc_s_y <- inc_s_y
c_age_s_y_W <- age_s_y_W
c_inc_s_y_W <- inc_s_y_W
c_W_hat <- W_hat
c_err_hat <- err_hat

# Save results
write_csv(c_W_hat, here::here("4weights", "china_car_weights_0.2.csv"))

########################
#	USA OPTIMIZATION   #
########################

# READ IN DATA
age_p <- as.matrix(read.csv(here::here("4weights", "data", "us", "age.csv"), header=F))
inc_p <- as.matrix(read.csv(here::here("4weights", "data", "us", "income_trunk.csv"), header=F))
data <- read_csv(here::here("3fixed", "demo_data_clean_fixed.csv"))

# Screen out SUV buyers - USA only:
data <- data[which(data$SUVbuyer==0),]
data <- data[which(data$usa==1),]

# Run the optimization
source('3b_run_weights_optim.R')

# Save results
write_csv(W_hat, here::here("4weights", "us_car_weights_0.2.csv"))

# PLOT POPULATION & SAMPLE CDFS
pdf(here::here("4weights", "weights.pdf"), width=14, height=7)
par(mfrow=c(2,4))
margins = c(2,3,2,1)
par(mar=margins)
cdf_plot(c_age_p_cdf, c_age_s_y)
par(mar=margins)
cdf_plot(c_inc_p_cdf, c_inc_s_y)
par(mar=margins)
cdf_plot(age_p_cdf, age_s_y)
par(mar=margins)
cdf_plot(inc_p_cdf, inc_s_y)
par(mar=margins)
cdf_plot(c_age_p_cdf, c_age_s_y_W)
par(mar=margins)
cdf_plot(c_inc_p_cdf, c_inc_s_y_W)
par(mar=margins)
cdf_plot(age_p_cdf, age_s_y_W)
par(mar=margins)
cdf_plot(inc_p_cdf, inc_s_y_W)
dev.off()

# Compute weights for other sub-groups

# MTurk Only:
data <- read_csv(here::here("3fixed", "demo_data_clean_fixed.csv"))
data <- data[which(data$SUVbuyer==0),]
data <- data[which(data$usa==1),]
data <- data[which(data$mturk==1),]
source('3b_run_weights_optim.R')
write.table(W_hat, here::here("4weights", "other", "mturk", "us_car_weights_0.2.txt"), sep=" ", row.names=F, col.names=F)

# PIT Only:
data <- read_csv(here::here("3fixed", "demo_data_clean_fixed.csv"))
data <- data[which(data$SUVbuyer==0),]
data <- data[which(data$usa==1),]
data <- data[which(data$mturk==0),]
source('3b_run_weights_optim.R')
write.table(W_hat, here::here("4weights", "other", "pit", "us_car_weights_0.2.txt"), sep=" ", row.names=F, col.names=F)

# URBAN Only:
data <- read_csv(here::here("3fixed", "demo_data_clean_fixed.csv"))
data <- data[which(data$SUVbuyer==0),]
data <- data[which(data$usa==1),]
data <- data[which(data$urban==1),]
source('3b_run_weights_optim.R')
write.table(W_hat, here::here("4weights", "other", "us urban", "us_car_weights_0.2.txt"), sep=" ", row.names=F, col.names=F)

# SUBURBAN Only:
data <- read_csv(here::here("3fixed", "demo_data_clean_fixed.csv"))
data <- data[which(data$SUVbuyer==0),]
data <- data[which(data$usa==1),]
data <- data[which(data$suburban==1),]
source('3b_run_weights_optim.R')
write.table(W_hat, here::here("4weights", "other", "us suburban", "us_car_weights_0.2.txt"), sep=" ", row.names=F, col.names=F)

# RURAL Only:
data <- read_csv(here::here("3fixed", "demo_data_clean_fixed.csv"))
data <- data[which(data$SUVbuyer==0),]
data <- data[which(data$usa==1),]
data <- data[which(data$urban==0 & data$suburban==0),]
source('3b_run_weights_optim.R')
write.table(W_hat, here::here("4weights", "other", "us rural", "us_car_weights_0.2.txt"), sep=" ", row.names=F, col.names=F)
