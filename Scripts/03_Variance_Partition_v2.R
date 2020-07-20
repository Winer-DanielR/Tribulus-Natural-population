#################################################################
################Tribulus Natural Population Dataset##############
#################################################################
### The main goal is to show the morphological variation of Tribulus cistoides mericarps
### Record of various populations of Tribulus in four islands in Galapagos from 2015 to 2019
### Record of predation of birds and insects (in 2019 only)
### By Daniel Reyes Corral #####

###############################################################################################

### Version 2. Added interactions of year:population and year:island ###

###############################################################################################

#####*****************************######
##### Variance partition analysis ######
#### Based on Sofia's code ####
# Estimating variation in nature (poulation/plant/schizocarp) per trait.
# Lower spine 1 is present 0 is absent

tribulusPC <- read.csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus Natural populations/Tribulus-Natural-population/Data/Processed/Tribulus_PC.csv")
tribulusPC #database from 2017 - 2019 with PC scores of length, width, depth and spine legth
           #includes spine distance 2017 - 2018 and lower spines
head(tribulusPC)
str(tribulusPC)

#set variables as factor
tribulusPC <- within(tribulusPC, {npop <- factor(npop) 
  mericarp <- factor(mericarp)
  lower_spine <- factor(lower_spine)
  spine_position <- factor(spine_position)
  eaten <- factor(eaten)
  eaten_insects <- factor(eaten_insects)
  seed_position_1 <- factor(seed_position_1)
  seed_position_2 <- factor(seed_position_2)
  seed_position_3 <- factor(seed_position_3)
  seed_position_4 <- factor(seed_position_4)
  seed_position_5 <- factor(seed_position_5)
  seed_position_6 <- factor(seed_position_6)
  germinated <- factor(germinated)
  germinated_position_1 <- factor(germinated_position_1)
  germinated_position_2 <- factor(germinated_position_2)
  germinated_position_3 <- factor(germinated_position_3)
  germinated_position_4 <- factor(germinated_position_4)
  germinated_position_5 <- factor(germinated_position_5)
  germinated_position_6 <- factor(germinated_position_6)
  year <- factor(year)})

str(tribulusPC)

#Descriptive data of all variables overall
pairs(tribulusPC[,8:14])
descrip.data <- as.data.frame(t(sapply(tribulusPC[,c(8:12)], 
                                       function(x) list(means=mean(x,na.rm = TRUE),sd=sd(x,na.rm = TRUE),min=min(x,na.rm = TRUE), max=max(x,nar.rm= TRUE)))))
View(descrip.data)

#####*********************************************************************#######
### In order to perform the variance partition we used linear mixed effect models
### as a response variable we used the mericarp traits and the random factors were:
### island/population
### year is considered a fixed effect ### ??? 
### Multiple sites within an island are considered random and nested within island


### Libraries ###
library(lme4)
library(lmerTest)

###----------- Length -------------- ### I could consider all the other interactions

lm.length <- lmer(length ~ 1 + (1|year) + (1|island) + (1|year:island) + (1|year:population) + (1|island:population), data = tribulusPC, REML = T)

### Checking assumptions
plot(lm.length)
opar <- par(mfrow=c(2,2))
hist(tribulusPC$length)
plot(fitted(lm.length),resid(lm.length)) 
abline(h=0,lty=2,col="red")
qqnorm(resid(lm.length))
qqline(resid(lm.length), col="red")
hist(resid(lm.length)) 
par(opar)

### Summary variance data
### Sofia's note: Use the "Variance" values of this table to estimate the % of variance explained
### Sofia's note: p-values were divided by 2 check paper methods explanation
summary(lm.length)

### testing significance of each random effects
ranova(lm.length)

###----------- Width -------------- ### I could consider all the other interactions
lm.width <- lmer(width ~ 1 + (1|year) + (1|island) + (1|year:island) + (1|year:population) + (1|island:population), data = tribulusPC, REML = T)

### Checking assumptions
plot(lm.width)
opar <- par(mfrow=c(2,2))
hist(tribulusPC$width)
plot(fitted(lm.width),resid(lm.width)) 
abline(h=0,lty=2,col="red")
qqnorm(resid(lm.width))
qqline(resid(lm.width), col="red")
hist(resid(lm.width)) 
par(opar)

### Summary variance data
summary(lm.width)


### testing significance of each random effects
ranova(lm.width)

###----------- Depth -------------- ### I could consider all the other interactions
lm.depth <- lmer(depth ~ 1 + (1|year) + (1|island) + (1|year:island) + (1|year:population) + (1|island:population), data = tribulusPC, REML = T)

### Checking assumptions
plot(lm.depth)
opar <- par(mfrow=c(2,2))
hist(tribulusPC$depth)
plot(fitted(lm.depth),resid(lm.depth)) 
abline(h=0,lty=2,col="red")
qqnorm(resid(lm.depth))
qqline(resid(lm.depth), col="red")
hist(resid(lm.depth)) 
par(opar)

### Summary variance data
summary(lm.depth)

### testing significance of each random effects
ranova(lm.depth)

###----------- Longest Spine -------------- ### I could consider all the other interactions
lm.longspine<- lmer(longest_spine ~ 1 + (1|year) + (1|island) + (1|year:island) + (1|year:population) + (1|island:population), data = tribulusPC, REML = T)

### Checking assumptions
plot(lm.longspine)
opar <- par(mfrow=c(2,2))
hist(tribulusPC$longest_spine)
plot(fitted(lm.longspine),resid(lm.longspine)) 
abline(h=0,lty=2,col="red")
qqnorm(resid(lm.longspine))
qqline(resid(lm.longspine), col="red")
hist(resid(lm.longspine)) 
par(opar)

### Summary variance data
summary(lm.longspine)

### testing significance of each random effects
ranova(lm.longspine)

###----------- Tip distance -------------- ### I could consider all the other interactions
lm.tipdist<- lmer(spine_tip_distance ~ 1 + (1|year) + (1|island) + (1|year:island) + (1|year:population) + (1|island:population), data = tribulusPC, REML = T)

### Checking assumptions
plot(lm.tipdist)
opar <- par(mfrow=c(2,2))
hist(tribulusPC$spine_tip_distance)
plot(fitted(lm.tipdist),resid(lm.tipdist)) 
abline(h=0,lty=2,col="red")
qqnorm(resid(lm.tipdist))
qqline(resid(lm.tipdist), col="red")
hist(resid(lm.tipdist)) 
par(opar)

### Summary variance data
summary(lm.tipdist) #Tip distance was not measured in 2019. This measurement was too much guessing the distance

### testing significance of each random effects
ranova(lm.tipdist)

### Sofia's note: For lower spines she used a glmer model because is a binary response
### ranova function does not work with glmer models so test the significance of random effects manually
###----------- Lower Spines -------------- ###

lm.lowspines <- glmer(lower_spine ~ 1 + (1|year) + (1|island) + + (1|year:island) + (1|year:population) +
                       (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)
summary(lm.lowspines) # no residual variation is estimetated in glmer
plot(lm.lowspines)

### Testing significance of each random effects
mfull <- glmer(lower_spine ~ 1 + (1|year) + (1|island) + (1|year:island) + (1|year:population) + (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)
summary(mfull)
### Island:Population removed
mred <- glmer(lower_spine ~ 1 + (1|year) + (1|island) + (1|year:island) + (1|year:population), 
              data = tribulusPC, family = binomial, na.action = na.omit, 
              control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mred)
anova(mfull, mred) #p values / by 2 (Population)

### Island removed
mred2 <- glmer(lower_spine ~ 1 + (1|year) + (1|year:island) + (1|year:population) + (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)

summary(mred2)
anova(mfull, mred2) #p values / by 2 (Island)

### Year removed
mred3 <- glmer(lower_spine ~ 1 + (1|island) + (1|year:island) + (1|year:population) + (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)

summary(mred3)
anova(mfull, mred3)

### Year:island removed
mred4 <- glmer(lower_spine ~ 1 + (1|year) + (1|island) + (1|year:population) + (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)
summary(mred4)
anova(mfull,mred4)

### Year:population removed
mred5 <- glmer(lower_spine ~ 1 + (1|year) + (1|island) + (1|year:island) + (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)

summary(mred5)
anova(mfull, mred5)
?anova



############***************************************************************###
############******** Variance partition per island *****************###
############***************************************************************###

########## Isabela ########

Trib_Isabela <- subset(tribulusPC, island == "Isabela")

### Droping unused factor levels aka other islands and categories

Trib_Isabela <- drop.levels(Trib_Isabela) 
str(Trib_Isabela1)

###-------------------Length----------------###

lm.isa.length <- lmer(length ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Isabela, REML = T)
plot(lm.isa.length)
summary(lm.isa.length)
ranova(lm.isa.length)

###-------------------Width----------------###

lm.isa.width <- lmer(width ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Isabela, REML = T)
plot(lm.isa.width)
summary(lm.isa.width)
ranova(lm.isa.width)

###-------------------Depth----------------###

lm.isa.depth <- lmer(depth ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Isabela, REML = T)
plot(lm.isa.depth)
summary(lm.isa.depth)
ranova(lm.isa.depth)

###-------------------Spine Length----------------###

lm.isa.spine <- lmer(longest_spine ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Isabela, REML = T)
plot(lm.isa.spine)
summary(lm.isa.spine)
ranova(lm.isa.spine)


###-------------------Tip distance----------------###

lm.isa.tipdist <- lmer(spine_tip_distance ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Isabela, REML = T)
plot(lm.isa.tipdist)
summary(lm.isa.tipdist)
ranova(lm.isa.tipdist)

###-------------------Lower Spines----------------###

lm.isa.lower <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population) + (1|population), data = Trib_Isabela, family = binomial)
summary(lm.isa.lower)
plot(lm.isa.lower)

#testing significance of each random effect

mfull.isa <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population) + (1|population), data =Trib_Isabela, family = binomial)

mred.isa <- glmer(lower_spine ~ 1  + (1|year:population) + (1|population), data =Trib_Isabela, family = binomial) 
anova(mfull.isa, mred.isa) #p values / by 2

mred.isa2 <- glmer(lower_spine ~ 1  + (1|year) + (1|population), data =Trib_Isabela, family = binomial)
anova(mfull.isa, mred.isa2) #p values / by 2

mred.isa3 <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population), data =Trib_Isabela, family = binomial)
anova(mfull.isa, mred.isa3) #p values / by 2

########## Santa Cruz ########

Trib_Santa.Cruz <- subset(tribulusPC, island == "Santa.Cruz")

### Droping unused factor levels aka other islands and categories

Trib_Santa.Cruz <- drop.levels(Trib_Santa.Cruz) 


###-------------------Length----------------###

lm.cruz.length <- lmer(length ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Santa.Cruz, REML = T)
plot(lm.cruz.length)
summary(lm.cruz.length)
ranova(lm.cruz.length)

###-------------------Width----------------###

lm.cruz.width <- lmer(width ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Santa.Cruz, REML = T)
plot(lm.cruz.width)
summary(lm.cruz.width)
ranova(lm.cruz.width)

###-------------------Depth----------------###

lm.cruz.depth <- lmer(depth ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Santa.Cruz, REML = T)
plot(lm.cruz.depth)
summary(lm.cruz.depth)
ranova(lm.cruz.depth)

###-------------------Spine Length----------------###

lm.cruz.spine <- lmer(longest_spine ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Santa.Cruz, REML = T)
plot(lm.cruz.spine)
summary(lm.cruz.spine)
ranova(lm.cruz.spine)


###-------------------Tip distance----------------###

lm.cruz.tipdist <- lmer(spine_tip_distance ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Santa.Cruz, REML = T)
plot(lm.cruz.tipdist)
summary(lm.cruz.tipdist)
ranova(lm.cruz.tipdist)

###-------------------Lower Spines----------------###

lm.cruz.lower <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population) + (1|population), data = Trib_Santa.Cruz, family = binomial)
summary(lm.cruz.lower)
plot(lm.cruz.lower)

#testing significance of each random effect

mfull.cruz <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population) + (1|population), data =Trib_Santa.Cruz, family = binomial)

mred.cruz <- glmer(lower_spine ~ 1  + (1|year:population) + (1|population), data =Trib_Santa.Cruz, family = binomial) 
anova(mfull.cruz, mred.cruz) #p values / by 2

mred.cruz2 <- glmer(lower_spine ~ 1  + (1|year) + (1|population), data =Trib_Santa.Cruz, family = binomial)
anova(mfull.cruz, mred.cruz2) #p values / by 2

mred.cruz3 <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population), data =Trib_Santa.Cruz, family = binomial)
anova(mfull.cruz, mred.cruz3) #p values / by 2


########## San Cristobal ########

Trib_San.Cristobal <- subset(tribulusPC, island == "San.Cristobal")

### Droping unused factor levels aka other islands and categories

Trib_San.Cristobal <- drop.levels(Trib_San.Cristobal) 

###-------------------Length----------------###

lm.crist.length <- lmer(length ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_San.Cristobal, REML = T)
plot(lm.crist.length)
summary(lm.crist.length)
ranova(lm.crist.length)

###-------------------Width----------------###

lm.crist.width <- lmer(width ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_San.Cristobal, REML = T)
plot(lm.crist.width)
summary(lm.crist.width)
ranova(lm.crist.width)

###-------------------Depth----------------###

lm.crist.depth <- lmer(depth ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_San.Cristobal, REML = T)
plot(lm.crist.depth)
summary(lm.crist.depth)
ranova(lm.crist.depth)

###-------------------Spine Length----------------###

lm.crist.spine <- lmer(longest_spine ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_San.Cristobal, REML = T)
plot(lm.crist.spine)
summary(lm.crist.spine)
ranova(lm.crist.spine)


###-------------------Tip distance----------------###

lm.crist.tipdist <- lmer(spine_tip_distance ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_San.Cristobal, REML = T)
plot(lm.crist.tipdist)
summary(lm.crist.tipdist)
ranova(lm.crist.tipdist)

###-------------------Lower Spines----------------###

lm.crist.lower <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population) + (1|population), data = Trib_San.Cristobal, family = binomial)
summary(lm.crist.lower)
plot(lm.crist.lower)

#testing significance of each random effect

mfull.crist <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population) + (1|population), data =Trib_San.Cristobal, family = binomial)

mred.crist <- glmer(lower_spine ~ 1  + (1|year:population) + (1|population), data =Trib_San.Cristobal, family = binomial) 
anova(mfull.crist, mred.crist) #p values / by 2

mred.crist2 <- glmer(lower_spine ~ 1  + (1|year) + (1|population), data =Trib_San.Cristobal, family = binomial)
anova(mfull.crist, mred.crist2) #p values / by 2

mred.crist3 <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population), data =Trib_San.Cristobal, family = binomial)
anova(mfull.crist, mred.crist3) #p values / by 2


########## Floreana ########

Trib_Floreana <- subset(tribulusPC, island == "Floreana")

### Droping unused factor levels aka other islands and categories

Trib_Floreana <- drop.levels(Trib_Floreana) 

###-------------------Length----------------###

lm.flor.length <- lmer(length ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Floreana, REML = T)
plot(lm.flor.length)
summary(lm.flor.length)
ranova(lm.flor.length)

###-------------------Width----------------###

lm.flor.width <- lmer(width ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Floreana, REML = T)
plot(lm.flor.width)
summary(lm.flor.width)
ranova(lm.flor.width)

###-------------------Depth----------------###

lm.flor.depth <- lmer(depth ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Floreana, REML = T)
plot(lm.flor.depth)
summary(lm.flor.depth)
ranova(lm.flor.depth)

###-------------------Spine Length----------------###

lm.flor.spine <- lmer(longest_spine ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Floreana, REML = T)
plot(lm.flor.spine)
summary(lm.flor.spine)
ranova(lm.flor.spine)


###-------------------Tip distance----------------###

lm.flor.tipdist <- lmer(spine_tip_distance ~ 1 + (1|year) + (1|year:population) + (1|population), data = Trib_Floreana, REML = T)
plot(lm.flor.tipdist)
summary(lm.flor.tipdist)
ranova(lm.flor.tipdist)

###-------------------Lower Spines----------------###

lm.flor.lower <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population) + (1|population), data = Trib_Floreana, family = binomial)
summary(lm.flor.lower)
plot(lm.flor.lower)

#testing significance of each random effect

mfull.flor <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population) + (1|population), data =Trib_Floreana, family = binomial)

mred.flor <- glmer(lower_spine ~ 1  + (1|year:population) + (1|population), data =Trib_Floreana, family = binomial) 
anova(mfull.flor, mred.flor) #p values / by 2

mred.flor2 <- glmer(lower_spine ~ 1  + (1|year) + (1|population), data =Trib_Floreana, family = binomial)
anova(mfull.flor, mred.flor2) #p values / by 2

mred.flor3 <- glmer(lower_spine ~ 1  + (1|year) + (1|year:population), data =Trib_Floreana, family = binomial)
anova(mfull.flor, mred.flor3) #p values / by 2
