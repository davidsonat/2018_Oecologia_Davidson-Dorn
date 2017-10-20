#Analysis for Davidson and Dorn (in review). Manipulations included nutrient enrichment 
#(increased productivity; low and high) and predator (crayfish) presence/absence. 
#The data consists of 4 data sets. The first data set contains data about the survivorship 
#and final biomass of P. paludosa and P. maculata prey as a function of crayfish presence and
#productivity level. The second and third data sets contain data on the growth of apple snails 
#and crayfish in the experiment at varying productivity. The final data set describes how predator
#effect sizes varied with productivity in the experiment.

#All ANOVAs include Levene's test and the Shapiro-Wilk test to check that model assumptions
#are met.

#Load in first data set and load necessary packages. 
library(MASS)
library(car)
library(multcomp)
library(outliers)
library(xlsx)

#The first set of analyses assess the effects of crayfish and increased productivity
#on the survivorship and assemblage structure of a mixed community of P. paludosa and 
#P. maculata apple snails. A brief analysis checking for crayfish and productivity 
#effects on the survivorship of ramshorn snails incidentally present in the mesocosms is 
#also included.
survivorshipdata <- read.xlsx("C:/Users/adavi/Dropbox/FAU/Data/Mesocosms/FinalData/DavidsonDorn_OecologiaDataSet.xlsx", sheetName = "Survivorship_Biomass") 

#MANOVA assessing the effects of crayfish and productivity on the survivorship of both
#P. maculata and P. paludosa together.
survivalboth <- manova(cbind(logitpaludosa_surv,logitmaculata_surv)~productivity*crayfish, data=survivorshipdata)
summary(survivalboth)

#ANOVA assessing the effects of crayfish and productivity on the survivorship of 
#P. paludosa.
survivalnative <- aov(logitpaludosa_surv~productivity*crayfish, data=survivorshipdata)
summary(survivalnative)
plot(survivalnative, 1)
plot(survivalnative, 2)
leveneTest(logitpaludosa_surv~productivity*crayfish,data=survivorshipdata)
shapiro.test(residuals(survivalnative))

#ANOVA assessing the effects of crayfish and productivity on the survivorship of
#P. maculata. Because there is an interaction term here, planned contrasts were run.
survivalexotic <- aov(logitmaculata_surv~productivity*crayfish, data=survivorshipdata)
summary(survivalexotic)
TukeyHSD(survivalexotic)
plot(survivalexotic, 1)
plot(survivalexotic, 2)
leveneTest(logitmaculata_surv~productivity*crayfish,data=survivorshipdata)
shapiro.test(residuals(survivalexotic))

#Setting up the planned contrasts.
survivorshipdata$ab <- interaction(survivorshipdata$productivity, survivorshipdata$crayfish)
levels(survivorshipdata$ab)

aovRes <- aov(logitprop_paludosa~ab, data=survivorshipdata)

cntrMat <- rbind("contr 01"=c(0, 1,  0,  -1), #Crayfish effects at low productivity. 
                 "contr 02"=c(-1,  1,  0, 0),  #Productivity effects where crayfish are absent.
                 "contr 03"=c(0, 0, -1,  1), #Productivity effects where crayfish are present.
                 "contr 04"=c(1, 0, -1, 0)) #Crayfish effects at high productivity.

aovRes2 <- aov(logitmaculata_surv~ab, data=survivorshipdata)

summary(glht(aovRes2, linfct=mcp(ab=cntrMat), alternative="two.sided"),test=adjusted("none"))


#ANOVA assessing the effects of crayfish and productivity on total apple snail biomass.
snailbiomass <- aov(totalbiomass ~ productivity * crayfish, data=survivorshipdata)
summary(snailbiomass)
plot(snailbiomass, 1)
plot(snailbiomass, 2)
leveneTest(totalbiomass ~ productivity * crayfish, data=survivorshipdata)
shapiro.test(residuals(snailbiomass))


#ANOVA assessing the effects of crayfish and increased productivity on the relative abundance
#of P. maculata as compared to P. paludosa (i.e., the assemblage structure), including planned 
#contrasts of the four treatments.
assemblage <- aov(logitprop_paludosa~productivity*crayfish, data=survivorshipdata)
summary(assemblage)
plot(assemblage, 1)
plot(assemblage, 2)
leveneTest(logitprop_paludosa~productivity*crayfish,data=survivorshipdata)
shapiro.test(residuals(assemblage))

summary(glht(aovRes, linfct=mcp(ab=cntrMat), alternative="two.sided"),test=adjusted("none"))


#ANOVA assessing the effects of crayfish presence and nutrient availability on the 
#density of ramshorn snails in the mesocosms.
ramsurvivorship <- aov(ramshorn~productivity*crayfish, data=survivorshipdata)
summary(ramsurvivorship)
plot(ramsurvivorship, 1)
plot(ramsurvivorship, 2)
leveneTest(ramshorn~productivity*crayfish,data=survivorshipdata)
shapiro.test(residuals(ramsurvivorship))





#The second set of analyses report the effects of productivity level on the growth
#rates of the apple snails P. paludosa and P. maculata. Because growth is often
#density dependent, we did not consider the effects of crayfish on growth here
#and instead compared average growth rates across low and high productivity mesocosms
#where crayfish were absent.
growthrates <- read.xlsx("C:/Users/adavi/Dropbox/FAU/Data/Mesocosms/FinalData/DavidsonDorn_OecologiaDataSet.xlsx", sheetName = "Snail_Growth")

#MANOVA for both P. maculata and P. paludosa growth rates as a function of nutrient enrichment.
growthboth <- manova(cbind(maculata_growth,paludosa_growth)~productivity, data=growthrates)
summary(growthboth)

#ANOVA of Average P. maculata growth rate as a function of nutrient enrichment.
growthexotic <- aov(maculata_growth ~ productivity, data=growthrates)
summary(growthexotic)
plot(growthexotic, 1)
plot(growthexotic, 2)
leveneTest(maculata_growth~productivity,data=growthrates)
shapiro.test(residuals(growthexotic))

#ANOVA of Average P. paludosa growth rate as a function of nutrient enrichment.
growthnative <- aov(paludosa_growth ~ productivity, data=growthrates)
summary(growthnative)
plot(growthnative, 1)
plot(growthnative, 2)
leveneTest(paludosa_growth~productivity,data=growthrates)
shapiro.test(residuals(growthnative))





#Small data set describing how productivity influenced crayfish size.

crayfishgrowth <- read.xlsx("C:/Users/adavi/Dropbox/FAU/Data/Mesocosms/FinalData/DavidsonDorn_OecologiaDataSet.xlsx", sheetName = "Crayfish_Size")

#ANOVA describing how crayfish growth rates vary with nutrient enrichment.
growthcrayfish <- aov(avg_size ~ productivity, data=crayfishgrowth)
summary(growthcrayfish)
plot(growthcrayfish, 1)
plot(growthcrayfish, 2)
leveneTest(avg_size ~ productivity, data=crayfishgrowth)
shapiro.test(residuals(growthcrayfish))





#The final data set describes how predator effect sizes on the survivorship of apple snails
#vary with productivity.
effectsizes <- read.xlsx("C:/Users/adavi/Dropbox/FAU/Data/Mesocosms/FinalData/DavidsonDorn_OecologiaDataSet.xlsx", sheetName = "EffectSizes")

#ANOVA describing how crayfish effect sizes on total apple snail biomass vary with productivity.
totaleffectsize <- aov(total_effectsize ~ productivity, data=effectsizes)
summary(totaleffectsize)
plot(totaleffectsize, 1)
plot(totaleffectsize, 2)
leveneTest(total_effectsize ~ productivity, data=totaleffectsize)
shapiro.test(residuals(effectsize))