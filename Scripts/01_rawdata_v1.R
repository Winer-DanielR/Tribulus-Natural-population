#################################################################
################Tribulus Natural Population Dataset##############
#################################################################
### The main goal is to show the morphological variation of Tribulus cistoides mericarps
### Record of various populations of Tribulus in four islands in Galapagos from 2015 to 2019
### Record of predation of birds and insects (in 2019 only)
### By Daniel Reyes Corral

### Raw data
#Tribulus1 has replaces all spine distances of 0 into NAs##########

tribulus <- read.csv("~/R/Tribulus/Tribulus Natural populations/Tribulus-Natural-population/Data/Raw/All time Tribulus1.csv")
head(natpop)
View(tribulus)
summary(natpop)

#Extracting data from 2017 - 2019
year <- filter(tribulus, year >= 2017)
View(year)
clean_l <- filter(year, !is.na(length))
clean_d <- filter(clean_l, !is.na(depth))
clean_s <- filter(clean_d, !is.na(longest_spine))


################spine tip distance measurements were ommited in 2019. This is a PCA of morphology without spine tip distance from 2017 - 2019############
tribulus <- clean_s
Pr <- prcomp(tribulus[,8:11], scale. = TRUE)
Pr
summary(Pr)
plot(Pr, type = "l")
biplot(Pr, scale = 0)


#Extracting PC scores
str(Pr)
Pr$x
#Adding PC scores to dataset
tribulusPC <- cbind(tribulus, Pr$x[,1:3])
head(tribulusPC)
#plot with ggplot
ggplot(tribulusPC, aes(PC1, PC2, col = island)) + stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + geom_point(shape = 21, col = "black")
custom_pca()

#### I removed four outliers that were from 2018 in Santa Cruz that showed larger sizes overall. 
#### I also corrected four outliers that showed mistakes in the measurements. 




###############Including spine distance BUT ommiting 2019 dataaset############################################
clean_sd <- filter(clean_s, !is.na(spine_tip_distance))
PrSd <- prcomp(clean_sd[,8:12], scale. = TRUE)
PrSd
summary(PrSd)
plot(PrSd, type = "l")
biplot(PrSd, scale = 0)

#Extracting PC scores
str(PrSd)
PrSd$x
#Adding PC scores to dataset
tribulusSD <- cbind(clean_sd, PrSd$x[,1:2])
head(tribulusSD)
#plot with ggplot
ggplot(tribulusSD, aes(PC1, PC2, col = island)) + stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + geom_point(shape = 21, col = "black")

#############################PCA with only length width and depth#############################################
tribulusdepth <- clean_d

PrD <- prcomp(tribulusdepth[,8:10], scale. = TRUE)
PrD
summary(PrD)
plot(PrD, type = "l")
biplot(PrD, scale = 0)
#Extracting PC scores
str(PrD)
PrD$x
#Adding PC scores to dataset
View(tribulusdepth)
depthPC <- cbind(tribulusdepth, PrD$x[,1:2])
head(depthPC)

#plot with ggplot
ggplot(depthPC, aes(PC1, PC2, col = island)) + stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + geom_point(shape = 21, col = "black")

#plots in multiple views
par(mfrow=c(2,2))
