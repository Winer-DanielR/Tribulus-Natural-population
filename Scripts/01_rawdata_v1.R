#################################################################
################Tribulus Natural Population Dataset##############
#################################################################
### The main goal is to show the morphological variation of Tribulus cistoides mericarps
### Record of various populations of Tribulus in four islands in Galapagos from 2015 to 2019
### Record of predation of birds and insects (in 2019 only)
### By Daniel Reyes Corral
### Raw data

tribulus <- read.csv("~/R/Tribulus/Tribulus Natural populations/Tribulus-Natural-population/Data/Raw/All time Tribulus1.csv")
head(natpop)
summary(natpop)

#Extracting data from 2017 - 2019
year <- filter(tribulus, year >= 2017)
View(year)



#Select mophological variables: length, width, depth, spine distance, spine length
morph <- prcomp(natpop[,])

