#################################################################
################Tribulus Natural Population Dataset##############
#################################################################
### The main goal is to show the morphological variation of Tribulus cistoides mericarps
### Record of various populations of Tribulus in four islands in Galapagos from 2015 to 2019
### Record of predation of birds and insects (in 2019 only)
### By Daniel Reyes Corral

### Raw data
#Tribulus1 has replaces all spine distances of 0 into NAs##########

tribulus <- read.csv("~/R/Tribulus/Tribulus Natural populations/Tribulus-Natural-population/Data/Raw/All time Tribulus.csv")
head(tribulus)
str(tribulus)

#Standarize morphological variables. Create a dataframe just with morphological data.
trib.stan <- scale(tribulus [ , c(8:14)])
trib.stan <- as.data.frame(trib.stan)
trib.var <- select(tribulus, year, island, year.island, population, year_pop, npop)
trib.stan <- bind_cols(trib.var, trib.stan)
str(trib.stan)


############################################### Generate PCAs #####################################################


#### PCAs wont work if there is NAs on the columns, so I omited the NAs on the variables. 

#### However I ended up losing data:
#### If I try to include most years I can only work with length, witdth and lower spines.
#### 10813 mericarps
trib.inclusive <- na.omit(trib.stan[,c(1,2,6,8:15)])
Pr3 <- prcomp(trib.inclusive[,c(1:3)], center = T, scale. = T)
summary(Pr3)
plot(Pr3, type = "l")
biplot(Pr3, scale = 0)
#Extracting PC scores
str(Pr3)
#Adding PC scores to main dataset
tribulus.PCA.years <- cbind(trib.inclusive, Pr3$x[,1:3])

####################### If I try to include all variables, I omit most of the data. #############################
#### 3393 mericarps
trib.na.all <- na.omit(trib.stan)
Pr2 <- prcomp(trib.na.all[,c(7:13)], center = T, scale. = T)
summary(Pr2)
plot(Pr2, type = "l")
biplot(Pr2, scale = 0)
#Extracting PC scores
str(Pr2)
#Adding PC scores to trib.na.all
tribulus.PCA.all.variables <- cbind(trib.na.all, Pr2$x[,1:7])

Pc.all <- PCA(trib.na.all[,c(7:13)], graph = FALSE)
trib.na.all <- drop.levels(trib.na.all) 
str(trib.na)

# Visualize
# Use habillage to specify groups for coloring
fviz_pca_ind(Pc.all,
             label = "none", # hide individual labels
             habillage = trib.na.all$island, # color by groups
             palette = c("#126837",  "#f77e23", "#e22126", "#181b24", "#878ed4"),
             addEllipses = TRUE # Concentration ellipses
             
)

#Variables explained by PCs
fviz_pca_var(Pc.all,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Contributions of variables to PC1
fviz_contrib(Pc.all, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(Pc, choice = "var", axes = 2, top = 10)

# Contributions of variables to PC2
fviz_contrib(Pc, choice = "var", axes = 3, top = 10)




#### I can omit spine tip distance from the analysis is similar to longest spine.
#### Omit NAs. 5552 obs from 2017 to 2019. Includes all variables but tip distance
trib.na <- na.omit(trib.stan[,c(1:10,12,13)])
Pr <- prcomp(trib.na[,c(7:12)], center = T, scale. = T)
fviz_eig(Pr)
biplot(Pr2, scale = 0)
#Extracting PC scores
str(Pr)
#Adding PC scores to trib.na.all
tribulus.PCA <- cbind(trib.na, Pr$x[,1:6])
str(tribulus.PCA)

# Compute PCA
# before PCA analysis
Pc <- PCA(trib.na[,c(7:12)], graph = FALSE)
trib.na <- drop.levels(trib.na) 
str(trib.na)

# Visualize
# Use habillage to specify groups for coloring
fviz_pca_ind(Pc,
             label = "none", # hide individual labels
             habillage = trib.na$island, # color by groups
             palette = c("#126837",  "#f77e23", "#e22126", "#181b24", "#878ed4"),
             addEllipses = TRUE # Concentration ellipses
          
)

#Variables explained by PCs
fviz_pca_var(Pc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )

# Contributions of variables to PC1
fviz_contrib(Pc, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(Pc, choice = "var", axes = 2, top = 10)

# Contributions of variables to PC2
fviz_contrib(Pc, choice = "var", axes = 3, top = 10)







#PCA 3D plot 
pc <- princomp(trib.na[,c(1:6)], cor = T, scores = T)
summary(pc)
plot(pc, type = "lines")
biplot(pc)

plot3d(pc$scores[,1:3], col = as.factor(trib.na$island.num), xlab = "PC1.size", ylab = "PC2.spines", zlab = "PC3.spine_angle")

text3d(pc$scores[,1:3], texts = rownames(trib.na))
text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pc$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pc$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=4)

#Extracting PC scores
str(Pc)

#Adding PC scores to trib.na
tribulus.PCA <- cbind(trib.na, Pr$x[,1:6])
