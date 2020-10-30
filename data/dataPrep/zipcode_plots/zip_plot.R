library(maps)
library(readr)

data <- read_csv(here::here("6coded", "demo_data_coded.csv"))

data <- data[which(data$SUVbuyer==0),]
mturk <- data[which(data$mturk==1),]
pit <- data[which(data$mturk==0),]

pointshape = 20
pointsize = 0.6

# Full Map
dev.new()
urb <- data[which(data$urban==1),]
sub <- data[which(data$suburban==1),]
rur <- data[which(data$rural==1),]
urbperc <- round((nrow(urb)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
subperc <- round((nrow(sub)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
rurperc <- round((nrow(rur)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
urbperc <- sub("^", urbperc, "%")
subperc <- sub("^", subperc, "%")
rurperc <- sub("^", rurperc, "%")
map("state", interior = FALSE)
map("state", boundary = FALSE, col="gray", add = TRUE)
points(x=rur$long, y=rur$lat, cex=pointsize, col="forestgreen", pch=pointshape)
points(x=sub$long, y=sub$lat, cex=pointsize, col="red", pch=pointshape)
points(x=urb$long, y=urb$lat, cex=pointsize, col="blue", pch=pointshape)
title(main="U.S. Sample: 384 Car Respondents")
legend(-121, 31, col=c("blue", "red", "forestgreen"), c("Urban", "Suburban    ", "Rural"), pch=pointshape, cex=1, bty="n")
legend(-113, 31, c(urbperc, subperc, rurperc), cex=1, bty="n")


# Mturk Map
dev.new()
urb <- mturk[which(mturk$urban==1),]
sub <- mturk[which(mturk$suburban==1),]
rur <- mturk[which(mturk$rural==1),]
urbperc <- round((nrow(urb)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
subperc <- round((nrow(sub)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
rurperc <- round((nrow(rur)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
urbperc <- sub("^", urbperc, "%")
subperc <- sub("^", subperc, "%")
rurperc <- sub("^", rurperc, "%")
map("state", interior = FALSE)
map("state", boundary = FALSE, col="gray", add = TRUE)
points(x=rur$long, y=rur$lat, cex=pointsize, col="forestgreen", pch=pointshape)
points(x=sub$long, y=sub$lat, cex=pointsize, col="red", pch=pointshape)
points(x=urb$long, y=urb$lat, cex=pointsize, col="blue", pch=pointshape)
title(main="U.S. Sample: 283 MTurk Car Respondents")
legend(-121, 31, col=c("blue", "red", "forestgreen"), c("Urban", "Suburban    ", "Rural"), pch=pointshape, cex=1, bty="n")
legend(-113, 31, c(urbperc, subperc, rurperc), cex=1, bty="n")


# Pit Map:
dev.new()
urb <- pit[which(pit$urban==1),]
sub <- pit[which(pit$suburban==1),]
rur <- pit[which(pit$rural==1),]
urbperc <- round((nrow(urb)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
subperc <- round((nrow(sub)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
rurperc <- round((nrow(rur)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
urbperc <- sub("^", urbperc, "%")
subperc <- sub("^", subperc, "%")
rurperc <- sub("^", rurperc, "%")
map("state", interior = FALSE)
map("state", boundary = FALSE, col="gray", add = TRUE)
points(x=rur$long, y=rur$lat, cex=pointsize, col="forestgreen", pch=pointshape)
points(x=sub$long, y=sub$lat, cex=pointsize, col="red", pch=pointshape)
points(x=urb$long, y=urb$lat, cex=pointsize, col="blue", pch=pointshape)
title(main="U.S. Sample: 101 Auto Show Car Respondents")
legend(-121, 31, col=c("blue", "red", "forestgreen"), c("Urban", "Suburban    ", "Rural"), pch=pointshape, cex=1, bty="n")
legend(-113, 31, c(urbperc, subperc, rurperc), cex=1, bty="n")


# Pit Map - ZOOMED:
dev.new()
urb <- pit[which(pit$urban==1),]
sub <- pit[which(pit$suburban==1),]
rur <- pit[which(pit$rural==1),]
urbperc <- round((nrow(urb)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
subperc <- round((nrow(sub)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
rurperc <- round((nrow(rur)/(nrow(urb) + nrow(sub) + nrow(rur)))*100, 1)
urbperc <- sub("^", urbperc, "%")
subperc <- sub("^", subperc, "%")
rurperc <- sub("^", rurperc, "%")
map(database="state", region=c("ohio", "pennsylvania", "west virginia"))
points(x=rur$long, y=rur$lat, cex=pointsize, col="forestgreen", pch=pointshape)
points(x=sub$long, y=sub$lat, cex=pointsize, col="red", pch=pointshape)
points(x=urb$long, y=urb$lat, cex=pointsize, col="blue", pch=pointshape)
title(main="U.S. Sample: 101 Auto Show Car Respondents")
legend(-78.6, 39, col=c("blue", "red", "forestgreen"), c("Urban", "Suburban    ", "Rural"), pch=pointshape, cex=1, bty="n")
legend(-77.3, 39, c(urbperc, subperc, rurperc), cex=1, bty="n")


##### Barplot of Auto Show respondents' home state:
PA = length(which(pit$state=="PA"))
WV = length(which(pit$state=="WV"))
TN = length(which(pit$state=="TN"))
MI = length(which(pit$state=="MI"))
NY = length(which(pit$state=="NY"))
MD = length(which(pit$state=="MD"))
OH = length(which(pit$state=="OH"))
states = c(PA, WV, TN, MI, NY, MD, OH)
names(states) = c("PA", "WV", "TN", "MI", "NY", "MD", "OH")
dev.new()
barplot(states, ylab="Number of Respondents", xlab="State", main="Home state of auto show respondents")
