##### Using the dataset of Tribulus PCs to calculate PC means per population ######
#Export this dataset into a new CSV for records
tribulusPC2 <- write.csv(tribulus.PCA, "C:/Users/Daniel/Documents/R/Tribulus/Tribulus Natural populations/Tribulus-Natural-population/Data/Processed/Tribulus_PC2.csv")
?write.csv()
tribulusPC <- read.csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus Natural populations/Tribulus-Natural-population/Data/Processed/Tribulus_PC.csv")
#View(tribulusPC)

#Means by year and population
PC_summary1 <- group_by(tribulus.PCA, year, island, year.island, population, year_pop)
PCA_year.pop <- PC_summary1 %>% summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), PC1:PC6)
#Calculating ellipses
ggplot(PCA_year.pop, aes(PC1_mean, PC2_mean, color = island)) + geom_point(size = 3) + stat_ellipse(type = "t", linetype = 2) #+ stat_ellipse(type = "t")



#Means by year.island
PC_summary2 <- group_by(tribulus.PCA, year, island, year.island)
PCA_island <- PC_summary2 %>% summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), PC1:PC6)
ggplot(PC_means_year_island, aes(PC1_mean, PC2_mean)) + geom_point(aes(shape = island, size = 5, color = year))

#Means by population
PC_summary3 <- group_by(tribulusPC, island, population)
PC_means_population <- PC_summary3 %>% summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), PC1:PC3)
ggplot(PC_means_population, aes(PC1_mean, PC2_mean)) + geom_point(aes(color = population, size = 5))

#Means by island
PC_summary4 <- group_by(tribulusPC, island)
PC_means_island <- PC_summary4 %>% summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), PC1:PC3)
#View(PC_means_island)

#Means by year
PC_summary5 <- group_by(tribulusPC, year)
PC_means_year <- PC_summary5 %>% summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), PC1:PC3)
#View(PC_means_year)
ggplot(PC_means_year, aes(PC1_mean, PC2_mean)) + geom_point(aes(color = year, size = 5))

#plots in multiple views
par(mfrow=c(2,2))
