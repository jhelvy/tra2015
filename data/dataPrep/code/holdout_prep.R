# create vector of random numbers from 1 to 15 to choose which questions to hold out:
nresp = nrow(outdata)/45
hold_IDs <- sample(1:15,nresp,replace=T)
ref <- rep(seq(1:15), each=3)

resp_data <- outdata[which(outdata[,1]==1),]
holddata_check <- resp_data[which(ref==hold_IDs[1]),]
holddata_est <- resp_data[which(ref!=hold_IDs[1]),]
for (i in 2:nresp) {
	resp_data <- outdata[which(outdata[,1]==i),]
	check <- resp_data[which(ref==hold_IDs[i]),]
	est <- resp_data[which(ref!=hold_IDs[i]),]
	holddata_check <- rbind(holddata_check, check)
	holddata_est <- rbind(holddata_est, est)
}

# Re-name the obsnum variable to be a continuous vector of observation IDs:
obsnum <- rep(seq(1,nrow(holddata_est)/3),each=3)
holddata_est[,2] <- obsnum
