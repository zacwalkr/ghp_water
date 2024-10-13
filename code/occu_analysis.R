########################
# 
# Occupancy of four gravelly herbfield pavement species 
#   in relation to microsite wetness 
#
#########################

#load packages
library(dplyr)
library(ggeffects)
library(ggpubr)
library(lme4)

#clear envrionment
rm(list = ls())

#read in data
wetland_data <- read.csv("~/data/wetland_data.csv")

#######
wet.df <- wetland_data

#create one dataframe for each species
mmdf <- wet.df %>% dplyr::select(Site, Transect, Wetness, Ci) #Psychrophila introloba
mmdf1 <- wet.df %>% dplyr::select(Site,Transect, Wetness, Op) #Oreobolus pumilio
mmdf2 <- wet.df %>% dplyr::select(Site,Transect, Wetness, Mp) #Myriophyllum pedunculatum
mmdf3 <- wet.df %>% dplyr::select(Site,Transect, Wetness, Da) #Drosera arcturi

#add a column to each to convert cover to presence-absence data
mmdf$Present <- ifelse(mmdf$Ci > 0, 1, 0)
mmdf1$Present <- ifelse(mmdf1$Op > 0, 1, 0)
mmdf2$Present <- ifelse(mmdf2$Mp > 0, 1, 0)
mmdf3$Present <- ifelse(mmdf3$Da > 0, 1, 0)

#binomial glmm with site and transect as random effects
mmbmod.c <- glmer(Present ~ (Wetness) + (1|Site/Transect), data=mmdf, family = binomial)
mmbmod.o <- glmer(Present ~ (Wetness) + (1|Site/Transect), data=mmdf1, family = binomial)
mmbmod.m <- glmer(Present  ~ (Wetness) + (1|Site/Transect), data=mmdf2, family = binomial)
mmbmod.d <- glmer(Present ~ (Wetness) + (1|Site/Transect), data=mmdf3, family = binomial)

#check model summarys
summary(mmbmod.c)
summary(mmbmod.o)
summary(mmbmod.m)
summary(mmbmod.d)

#test significance of wetness variable on presence-absence with Type II Wald chisquare tests
car::Anova(mmbmod.c) #***
car::Anova(mmbmod.o) #***
car::Anova(mmbmod.m) #n.s.
car::Anova(mmbmod.d) #n.s.

#create dataframe that holds the significance to add this to plotted effects
blab <- data.frame(x = c(22, 22, 22, 22),
                   y = c(0.9, 0.9, 0.9, 0.9),
                   lab = c("***", "***", "n.s.", "n.s."))

#extract predicted probabilities from models
c3 <- ggpredict(mmbmod.c)
o3 <- ggpredict(mmbmod.o)
m3 <- ggpredict(mmbmod.m)
d3 <- ggpredict(mmbmod.d)

#plot each predicted probability
mm.a <- ggplot(c3$Wetness, aes(x = x, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25) +
  geom_line(aes(),size=1)+
  theme_classic(base_size = 15)+
  geom_jitter(data = mmdf, aes(x = Wetness, y = Present), colour = "blue", alpha = 0.15, width = 10, height = 0.02)+
  xlim(16,100)+
  ylim(0,1)+
  xlab(" ")+
  ylab("Occupancy index")+
  ggtitle(bquote(italic("P. introloba")))+
  geom_text(data = blab[1,], aes(x = x, y = y, label = lab), size = 12)

mm.b <- ggplot(o3$Wetness, aes(x = x, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25) +
  geom_line(aes(),size=1)+
  theme_classic(base_size = 15)+
  geom_jitter(data = mmdf1, aes(x = Wetness, y = Present), colour = "blue", alpha = 0.15, width = 10, height = 0.02)+
  xlim(16,100)+
  ylim(0,1)+
  xlab(" ")+
  ylab(" ")+
  ggtitle(bquote(italic("O. pumilio")))+
  geom_text(data = blab[2,], aes(x = x, y = y, label = lab), size = 12)

mm.c <- ggplot(m3$Wetness, aes(x = x, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25) +
  geom_line(aes(),size=1)+
  theme_classic(base_size = 15)+
  geom_jitter(data = mmdf2, aes(x = Wetness, y = Present), colour = "blue", alpha = 0.15, width = 10, height = 0.02)+
  xlim(16,100)+
  ylim(0,1)+
  xlab("Wetness score (%)")+
  ylab("Occupancy index")+
  ggtitle(bquote(italic("M. pedunculatum")))+
  geom_text(data = blab[3,], aes(x = x, y = y, label = lab), size = 7)

mm.d <- ggplot(d3$Wetness, aes(x = x, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25) +
  geom_line(aes(),size=1)+
  theme_classic(base_size = 15)+
  geom_jitter(data = mmdf3, aes(x = Wetness, y = Present), colour = "blue", alpha = 0.15, width = 10, height = 0.02)+
  xlim(16,100)+
  ylim(0,1)+
  xlab("Wetness score (%)")+
  ylab(" ")+
  ggtitle(bquote(italic("D. arcturi")))+
  geom_text(data = blab[4,], aes(x = x, y = y, label = lab), size = 7)

#combine into figure
fig2 <- ggarrange(mm.a, mm.b, mm.c, mm.d)
fig2

#save figure
ggsave("fig_2.png", plot = fig2, width = 3000, height = 2200, units = "px", bg = "white")


#### checking model fit - overall fine
library(DHARMa)

simulationOutput <- simulateResiduals(fittedModel = mmbmod.c)
plot(simulationOutput, asFactor = F) # minor deviations but overall fine and within reason
testDispersion(simulationOutput) # no dispersion
testOutliers(simulationOutput, type = "binomial") #no outliers

simulationOutput <- simulateResiduals(fittedModel = mmbmod.o)
plot(simulationOutput, asFactor = F) # minor deviations but overall fine and within reason
testDispersion(simulationOutput) # no dispersion
testOutliers(simulationOutput, type = "binomial") #no outliers

simulationOutput <- simulateResiduals(fittedModel = mmbmod.m)
plot(simulationOutput, asFactor = F) #minor deviations but overall fine and within reason
testDispersion(simulationOutput) # no dispersion
testOutliers(simulationOutput, type = "binomial") #no outliers


simulationOutput <- simulateResiduals(fittedModel = mmbmod.d)
plot(simulationOutput, asFactor = F) #minor deviations but overall fine and within reason
testDispersion(simulationOutput) # no dispersion
testOutliers(simulationOutput, type = "binomial") #no outliers
