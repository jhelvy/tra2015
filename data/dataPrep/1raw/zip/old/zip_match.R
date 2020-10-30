##### Import data #####
demo_data <- read.csv("./coded/demo_data_coded.csv", header=T)
zip_data <- read.csv("./raw/zip/free-zipcode-database.csv", header=T)
ruca <- read.csv("./raw/zip/ruca2006.csv", header=T)

urban <- matrix(data=0, ncol=1, nrow=nrow(ruca))
suburban <- urban
rural <- urban
urban[which(ruca$RUCA2.0==1.0 | ruca$RUCA2.0==1.1 | ruca$RUCA2.0==2.0 | ruca$RUCA2.0==2.1 | ruca$RUCA2.0==3.0)] <- 1
rural[which(ruca$RUCA2.0==10.0 | ruca$RUCA2.0==10.1 | ruca$RUCA2.0==10.2 | ruca$RUCA2.0==10.3 | ruca$RUCA2.0==10.4 | ruca$RUCA2.0==10.5 | ruca$RUCA2.0==10.6)] <- 1
suburban[which(urban==0 & rural==0)] <- 1
ruca <- cbind(ruca, urban, suburban, rural)

start = which(demo_data$usa==1)[1]

state <- matrix(data=0, ncol=1, nrow=nrow(demo_data))
pop <- state
lat <- state
long <- state
urban <- state
suburban <- state
rural <- state
for (i in start:nrow(demo_data)) {
	zipid <- which(ruca$ZIPA == demo_data$zipcode[i])
	if (length(zipid)!=0) {
		urban[i,] <- ruca[zipid,]$urban
		suburban[i,] <- ruca[zipid,]$suburban
		rural[i,] <- ruca[zipid,]$rural		
	}
	id <- which(zip_data$Zipcode==demo_data$zipcode[i])[1]
	state[i,] <- as.character(zip_data[id,]$State)
	pop[i,] <- zip_data[id,]$EstimatedPopulation
	lat[i,] <- zip_data[id,]$Lat
	long[i,] <- zip_data[id,]$Long
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

out <- data.frame(zipcode = demo_data$zipcode, state=state, population=pop, lat=lat, long=long, urban=urban, suburban=suburban, rural=rural, mturk=demo_data$mturk, suv=demo_data$SUVbuyer, region=region)
out <- out[start:nrow(out),]
write.table(out,file="./zip analysis/usa_loc_demo.csv",sep=",", row.names=F, col.names=T)

full <- cbind(demo_data, state, pop, lat, long, urban, suburban, rural, region)
write.table(full,file="./coded/full_demo.csv",sep=",", row.names=F, col.names=T)