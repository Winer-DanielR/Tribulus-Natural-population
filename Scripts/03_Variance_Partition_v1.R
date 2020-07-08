#################################################################
################Tribulus Natural Population Dataset##############
#################################################################
### The main goal is to show the morphological variation of Tribulus cistoides mericarps
### Record of various populations of Tribulus in four islands in Galapagos from 2015 to 2019
### Record of predation of birds and insects (in 2019 only)
### By Daniel Reyes Corral #####

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
#View(descrip.data)

#####*********************************************************************#######
### In order to perform the variance partition we used linear mixed effect models
### as a response variable we used the mericarp traits and the random factors were:
### island/population
### year is considered a fixed effect ### ??? 
### Multiple sites within an island are considered random and nested within island


###----------- Length -------------- ### I could consider all the other interactions
lm.length <- lmer(length ~ 1 + (1|year) + (1|island) + (1|island:population), data = tribulusPC, REML = T)

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
lm.width <- lmer(width ~ 1 + (1|year) + (1|island) + (1|island:population), data = tribulusPC, REML = T)

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
lm.depth <- lmer(depth ~ 1 + (1|year) + (1|island) + (1|island:population), data = tribulusPC, REML = T)

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
lm.longspine<- lmer(longest_spine ~ 1  + (1|year) + (1|island) + 
                     (1|island:population), data = tribulusPC, na.action = na.omit, REML = T)

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
lm.tipdist<- lmer(spine_tip_distance ~ 1  + (1|year) + (1|island) + 
                      (1|island:population), data = tribulusPC, na.action = na.omit, REML = T)

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

lm.lowspines <- glmer(lower_spine ~ 1 + (1|year) + (1|island) +
                       (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)
summary(lm.lowspines) # no residual variation is estimetated in glmer
plot(lm.lowspines)

### Testing significance of each random effects
mfull <- glmer(lower_spine ~ 1 + (1|year) + (1|island) + (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)

### Island:Population removed
mred <- glmer(lower_spine ~ 1 + (1|year) + (1|island), 
              data = tribulusPC, family = binomial, na.action = na.omit, 
              control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mred)
anova(mfull, mred) #p values / by 2 (Population)

### Island removed
mred2 <- glmer(lower_spine ~ 1 + (1|year) + (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)

summary(mred2)
anova(mfull, mred2) #p values / by 2 (Island)

### Year removed
mred3 <- glmer(lower_spine ~ 1 + (1|island) + (1|island:population), data = tribulusPC, na.action = na.omit, family = binomial)

summary(mred3)
anova(mfull, mred3)


###***************************************************************###
###******** I could do these analysis per island *****************###