rm(list=ls()); graphics.off()
install.packages("dplyr")
install.packages("colorspace")
install.packages("ggplot2")
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(tidyverse)


setwd("G:\\Shared drives\\USDA_SCRI\\PSI_field_trial_results\\Calcmet_processed\\VOC\\outputfiles")

data <- read.csv("mean.all.data.csv")
str(data)

data = transform(data, 
                 Methacrylic.acid_mean = as.numeric(Methacrylic.acid_mean),
                 Isopentyl.acetate_mean = as.numeric(Isopentyl.acetate_mean),
                 Ethyl.benzene_mean = as.numeric(Ethyl.benzene_mean),
                 Butane_mean = as.numeric(Butane_mean),
                 Isobutanol_mean = as.numeric(Isobutanol_mean),
                 Hexane_mean = as.numeric(Hexane_mean),
                 Ethylcyclohexane_mean = as.numeric(Ethylcyclohexane_mean),
                 Nonane_mean = as.numeric(Nonane_mean),
                 Ethanolamine_mean = as.numeric(Ethanolamine_mean))
str(data)
summary(data)


#data$Date = as.Date(data$Date, format = "%m/%d/%Y")

data$c <- ifelse(data$Plot == "ASD01", 'ASD',
                 ifelse(data$Plot == "ASD06", 'ASD',
                        ifelse(data$Plot == "ASD09", 'ASD',
                               ifelse(data$Plot == "ASD10", 'ASD','UTC')))) #I changed "<=" to "==", so I could run the ifelse as it declared "<=" was not a valid operator.

colnames(data)[colnames(data)=="Treatment"] <- "Plot"
colnames(data)[colnames(data)=="c"] <- "Treatment"

str(data)

library(tidyr)

data.subset.CO2 = subset(data, select = c("Treatment", "Plot", "Date", "Carbon.dioxide_mean"))
str(data.subset.CO2)
mean.voc <-subset(data, select = -c(X, Water.vapor_mean, Carbon.dioxide_mean, Methane_mean, Nitrous.oxide_mean))
str(mean.voc)
str(data.subset.CO2)


long.mean.voc <- mean.voc %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Date, Plot, Treatment))
str(long.mean.voc)

has_rownames(long.mean.voc)
long.mean.voc <- long.mean.voc %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
#long.mean.voc <- na.omit(long.mean.voc)
output_path = (paste("G:\\Shared drives\\USDA_SCRI\\PSI_field_trial_results\\Calcmet_processed\\VOC\\outputfiles\\", sep = ""))
write.csv(long.mean.voc,
          paste(output_path, "long.mean.voc", ".csv", sep="" ))

list(long.mean.voc$Date)

voc_split <- split(long.mean.voc, long.mean.voc$Gas_Species)
voc_split
new_names <- c("data.Ammonia", "data.Butane","data.Butylamine..1.butanamine.", "data.Cyclohexane", "data.Cyclopentane", "data.Cyclopentanone",
               "data.Dimethyl.sulfide", "data.Dodecane", "data.Ethane", "data.Ethanol", "data.Ethanolamine", "data.Ethyl.benzene", "data.Ethylcyclohexane",
               "data.Ethylene.glycol","data.Hexane","data.Hexanoic.acid", "data.Hexene","data.Isobutanol","data.Isobutene..2.methylpropene.",
               "data.Isopentyl.acetate","data.Methacrylic.acid","data.Methyl.isocyanate","data.Nonane","data.Pentane","data.Propane","data.Propyl.acetate",
               "data.Propylamine","data.Propylene.oxide","data.t.Butanol","data.Undecane","data.X1.2.4.Trimethylbenzene",      
               "data.1.3.5.Trimethylbenzene","data.X1.Butanethiol","data.X1.Butene","data.X1.Heptene", "data.X1.Hexanol",              
               "data.X1.Methylimidazol","data.X1.Pentene","data.X2.3.Dimethylpyrazine","data.X2.3.Heptanedione",
               "data.X2.3.Hexanedione","data.X2.Ethylhexanol","data.X2.Methylpyrazine","data.X3.Ethyltoluene","data.X4.Ethyltoluene")
for (i in 1:length(voc_split)) {
  assign(new_names[i], voc_split[[i]])
}

str(data.Ammonia)
has_rownames(data.Ammonia)


############################################################################################
############################################################################################
if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rcompanion)){install.packages("rcompanion")}
require(multcomp)
#Only one date so do a one way anova to look at between plots

data.Ammonia <- tibble::rowid_to_column(data.Ammonia, var = "rowid")
data.Butylamine..1.butanamine. <- tibble::rowid_to_column(data.Butylamine..1.butanamine., var = "rowid")
data.Cyclohexane <- tibble::rowid_to_column(data.Cyclohexane, var = "rowid")
data.Cyclopentanone <- tibble::rowid_to_column(data.Cyclopentanone, var = "rowid")


##################AMMONIA##################################
###  Order factors by the order in data frame
###  Otherwise, R will alphabetize them

data.Ammonia$Treatment = factor(data.Ammonia$Treatment,
                                levels=unique(data.Ammonia$Treatment))


###  Check the data frame

library(psych)
library(rcompanion)
library(nlme)
library(car)
library(emmeans)
library(multcompView)
library(lsmeans)
library(rstatix)

headTail(data.Ammonia)

str(data.Ammonia)

summary(data.Ammonia)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Ammonia.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Ammonia)
summary(Ammonia.aov)
#Basic ANOVA with summary table


#LME model
data.Ammonia$TreatmentDate <- interaction(data.Ammonia$Treatment, data.Ammonia$Date)
data.Ammonia$TreatmentDate <- factor(data.Ammonia$TreatmentDate) #Converted TreatmentDate into factor so I could use glht function.
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Ammonia)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type="bonferroni"))

Ammonia.model.null = lme(Gas_Concentration_PPM ~ 1,
                         random = ~1|Plot,
                         data = data.Ammonia)

nagelkerke(model,
           Ammonia.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Ammonia,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")
  

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

########################################################


headTail(data.Butane)

str(data.Butane)

summary(data.Butane)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Butane.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Butane)
summary(Butane.aov)
#Basic ANOVA with summary table


#LME model
data.Butane$TreatmentDate <- interaction(data.Butane$Treatment, data.Butane$Date)
data.Butane$TreatmentDate <- factor(data.Butane$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Butane)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Butane.model.null = lme(Gas_Concentration_PPM ~ 1,
                         random = ~1|Plot,
                         data = data.Butane)

nagelkerke(model,
           Butane.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Butane,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))


###################################################################################



headTail(data.Butylamine..1.butanamine.)

str(data.Butylamine..1.butanamine.)

summary(data.Butylamine..1.butanamine.)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Butylamine..1.butanamine..aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Butylamine..1.butanamine.)
summary(Butylamine..1.butanamine..aov)
#Basic ANOVA with summary table


#LME model
data.Butylamine..1.butanamine.$TreatmentDate <- interaction(data.Butylamine..1.butanamine.$Treatment, data.Butylamine..1.butanamine.$Date)
data.Butylamine..1.butanamine.$TreatmentDate <- factor(data.Butylamine..1.butanamine.$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Butylamine..1.butanamine.)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Butylamine..1.butanamine..model.null = lme(Gas_Concentration_PPM ~ 1,
                        random = ~1|Plot,
                        data = data.Butylamine..1.butanamine.)

nagelkerke(model,
           Butylamine..1.butanamine..model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Butylamine..1.butanamine.,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))


###################################################################################


headTail(data.Cyclohexane)

str(data.Cyclohexane)

summary(data.Cyclohexane)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Cyclohexane.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Cyclohexane)
summary(Cyclohexane.aov)
#Basic ANOVA with summary table


#LME model
data.Cyclohexane$TreatmentDate <- interaction(data.Cyclohexane$Treatment, data.Cyclohexane$Date)
data.Cyclohexane$TreatmentDate <- factor(data.Cyclohexane$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Cyclohexane)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Cyclohexane.model.null = lme(Gas_Concentration_PPM ~ 1,
                                           random = ~1|Plot,
                                           data = data.Cyclohexane)

nagelkerke(model,
           Cyclohexane.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Cyclohexane,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))


###################################################################################

headTail(data.Cyclopentane)

str(data.Cyclopentane)

summary(data.Cyclopentane)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Cyclopentane.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Cyclopentane)
summary(Cyclopentane.aov)
#Basic ANOVA with summary table


#LME model
data.Cyclopentane$TreatmentDate <- interaction(data.Cyclopentane$Treatment, data.Cyclopentane$Date)
data.Cyclopentane$TreatmentDate <- factor(data.Cyclopentane$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Cyclopentane)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Cyclopentane.model.null = lme(Gas_Concentration_PPM ~ 1,
                             random = ~1|Plot,
                             data = data.Cyclopentane)

nagelkerke(model,
           Cyclopentane.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Cyclopentane,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

##########################################################
headTail(data.Cyclopentanone)

str(data.Cyclopentanone)

summary(data.Cyclopentanone)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Cyclopentanone.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Cyclopentanone)
summary(Cyclopentanone.aov)
#Basic ANOVA with summary table


#LME model
data.Cyclopentanone$TreatmentDate <- interaction(data.Cyclopentanone$Treatment, data.Cyclopentanone$Date)
data.Cyclopentanone$TreatmentDate <- factor(data.Cyclopentanone$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Cyclopentanone)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Cyclopentanone.model.null = lme(Gas_Concentration_PPM ~ 1,
                              random = ~1|Plot,
                              data = data.Cyclopentanone)

nagelkerke(model,
           Cyclopentanone.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Cyclopentanone,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

#########################################################################
headTail(data.Dimethyl.sulfide)

str(data.Dimethyl.sulfide)

summary(data.Dimethyl.sulfide)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Dimethyl.sulfide.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Dimethyl.sulfide)
summary(Dimethyl.sulfide.aov)
#Basic ANOVA with summary table


#LME model
data.Dimethyl.sulfide$TreatmentDate <- interaction(data.Dimethyl.sulfide$Treatment, data.Dimethyl.sulfide$Date)
data.Dimethyl.sulfide$TreatmentDate <- factor(data.Dimethyl.sulfide$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Dimethyl.sulfide)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Dimethyl.sulfide.model.null = lme(Gas_Concentration_PPM ~ 1,
                                random = ~1|Plot,
                                data = data.Dimethyl.sulfide)

nagelkerke(model,
           Dimethyl.sulfide.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Dimethyl.sulfide,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

##############################################################

headTail(data.Dodecane)

str(data.Dodecane)

summary(data.Dodecane)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Dodecane.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Dodecane)
summary(Dodecane.aov)
#Basic ANOVA with summary table


#LME model
data.Dodecane$TreatmentDate <- interaction(data.Dodecane$Treatment, data.Dodecane$Date)
data.Dodecane$TreatmentDate <- factor(data.Dodecane$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Dodecane)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Dodecane.model.null = lme(Gas_Concentration_PPM ~ 1,
                                  random = ~1|Plot,
                                  data = data.Dodecane)

nagelkerke(model,
           Dodecane.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Dodecane,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

###########################################################

headTail(data.Ethane)

str(data.Ethane)

summary(data.Ethane)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Ethane.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Ethane)
summary(Ethane.aov)
#Basic ANOVA with summary table


#LME model
data.Ethane$TreatmentDate <- interaction(data.Ethane$Treatment, data.Ethane$Date)
data.Ethane$TreatmentDate <- factor(data.Ethane$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Ethane)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Ethane.model.null = lme(Gas_Concentration_PPM ~ 1,
                          random = ~1|Plot,
                          data = data.Ethane)

nagelkerke(model,
           Ethane.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Ethane,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

#############################################################

headTail(data.Ethanol)

str(data.Ethanol)

summary(data.Ethanol)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Ethanol.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Ethanol)
summary(Ethanol.aov)
#Basic ANOVA with summary table


#LME model
data.Ethanol$TreatmentDate <- interaction(data.Ethanol$Treatment, data.Ethanol$Date)
data.Ethanol$TreatmentDate <- factor(data.Ethanol$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Ethanol)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Ethanol.model.null = lme(Gas_Concentration_PPM ~ 1,
                        random = ~1|Plot,
                        data = data.Ethanol)

nagelkerke(model,
           Ethanol.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Ethanol,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

#############################################################

headTail(data.Ethylene.glycol)

str(data.Ethylene.glycol)

summary(data.Ethylene.glycol)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Ethylene.glycol.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Ethylene.glycol)
summary(Ethylene.glycol.aov)
#Basic ANOVA with summary table


#LME model
data.Ethylene.glycol$TreatmentDate <- interaction(data.Ethylene.glycol$Treatment, data.Ethylene.glycol$Date)
data.Ethylene.glycol$TreatmentDate <- factor(data.Ethylene.glycol$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Ethylene.glycol)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Ethylene.glycol.model.null = lme(Gas_Concentration_PPM ~ 1,
                         random = ~1|Plot,
                         data = data.Ethylene.glycol)

nagelkerke(model,
           Ethylene.glycol.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Ethylene.glycol,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

#######################################################

headTail(data.Hexanoic.acid)

str(data.Hexanoic.acid)

summary(data.Hexanoic.acid)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Hexanoic.acid.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Hexanoic.acid)
summary(Hexanoic.acid.aov)
#Basic ANOVA with summary table


#LME model
data.Hexanoic.acid$TreatmentDate <- interaction(data.Hexanoic.acid$Treatment, data.Hexanoic.acid$Date)
data.Hexanoic.acid$TreatmentDate <- factor(data.Hexanoic.acid$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Hexanoic.acid)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Hexanoic.acid.model.null = lme(Gas_Concentration_PPM ~ 1,
                                 random = ~1|Plot,
                                 data = data.Hexanoic.acid)

nagelkerke(model,
           Hexanoic.acid.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Hexanoic.acid,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

################################################
headTail(data.Hexene)

str(data.Hexene)

summary(data.Hexene)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Hexene.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Hexene)
summary(Hexene.aov)
#Basic ANOVA with summary table


#LME model
data.Hexene$TreatmentDate <- interaction(data.Hexene$Treatment, data.Hexene$Date)
data.Hexene$TreatmentDate <- factor(data.Hexene$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Hexene)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Hexene.model.null = lme(Gas_Concentration_PPM ~ 1,
                               random = ~1|Plot,
                               data = data.Hexene)

nagelkerke(model,
           Hexene.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Hexene,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

###########################################
headTail(data.Isobutene..2.methylpropene.)

str(data.Isobutene..2.methylpropene.)

summary(data.Isobutene..2.methylpropene.)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Isobutene..2.methylpropene..aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Isobutene..2.methylpropene.)
summary(Isobutene..2.methylpropene..aov)
#Basic ANOVA with summary table


#LME model
data.Isobutene..2.methylpropene.$TreatmentDate <- interaction(data.Isobutene..2.methylpropene.$Treatment, data.Isobutene..2.methylpropene.$Date)
data.Isobutene..2.methylpropene.$TreatmentDate <- factor(data.Isobutene..2.methylpropene.$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Isobutene..2.methylpropene.)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Isobutene..2.methylpropene..model.null = lme(Gas_Concentration_PPM ~ 1,
                        random = ~1|Plot,
                        data = data.Isobutene..2.methylpropene.)

nagelkerke(model,
           Isobutene..2.methylpropene..model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Isobutene..2.methylpropene.,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

##############################################################
headTail(data.Methyl.isocyanate)

str(data.Methyl.isocyanate)

summary(data.Methyl.isocyanate)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Methyl.isocyanate.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Methyl.isocyanate)
summary(Methyl.isocyanate.aov)
#Basic ANOVA with summary table


#LME model
data.Methyl.isocyanate$TreatmentDate <- interaction(data.Methyl.isocyanate$Treatment, data.Methyl.isocyanate$Date)
data.Methyl.isocyanate$TreatmentDate <- factor(data.Methyl.isocyanate$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Methyl.isocyanate)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Methyl.isocyanate.model.null = lme(Gas_Concentration_PPM ~ 1,
                                             random = ~1|Plot,
                                             data = data.Methyl.isocyanate)

nagelkerke(model,
           Methyl.isocyanate.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Methyl.isocyanate,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))


#######################################################

headTail(data.Pentane)

str(data.Pentane)

summary(data.Pentane)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Pentane.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Pentane)
summary(Pentane.aov)
#Basic ANOVA with summary table


#LME model
data.Pentane$TreatmentDate <- interaction(data.Pentane$Treatment, data.Pentane$Date)
data.Pentane$TreatmentDate <- factor(data.Pentane$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Pentane)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Pentane.model.null = lme(Gas_Concentration_PPM ~ 1,
                                   random = ~1|Plot,
                                   data = data.Pentane)

nagelkerke(model,
           Pentane.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Pentane,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

########################################
headTail(data.Propane)

str(data.Propane)

summary(data.Propane)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Propane.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Propane)
summary(Propane.aov)
#Basic ANOVA with summary table


#LME model
data.Propane$TreatmentDate <- interaction(data.Propane$Treatment, data.Propane$Date)
data.Propane$TreatmentDate <- factor(data.Propane$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Propane)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Propane.model.null = lme(Gas_Concentration_PPM ~ 1,
                         random = ~1|Plot,
                         data = data.Propane)

nagelkerke(model,
           Propane.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Propane,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

##############################################################

headTail(data.Propyl.acetate)

str(data.Propyl.acetate)

summary(data.Propyl.acetate)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Propyl.acetate.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Propyl.acetate)
summary(Propyl.acetate.aov)
#Basic ANOVA with summary table


#LME model
data.Propyl.acetate$TreatmentDate <- interaction(data.Propyl.acetate$Treatment, data.Propyl.acetate$Date)
data.Propyl.acetate$TreatmentDate <- factor(data.Propyl.acetate$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Propyl.acetate)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Propyl.acetate.model.null = lme(Gas_Concentration_PPM ~ 1,
                         random = ~1|Plot,
                         data = data.Propyl.acetate)

nagelkerke(model,
           Propyl.acetate.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Propyl.acetate,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))


####################################################

headTail(data.Propylamine)

str(data.Propylamine)

summary(data.Propylamine)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Propylamine.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Propylamine)
summary(Propylamine.aov)
#Basic ANOVA with summary table


#LME model
data.Propylamine$TreatmentDate <- interaction(data.Propylamine$Treatment, data.Propylamine$Date)
data.Propylamine$TreatmentDate <- factor(data.Propylamine$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Propylamine)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Propylamine.model.null = lme(Gas_Concentration_PPM ~ 1,
                                random = ~1|Plot,
                                data = data.Propylamine)

nagelkerke(model,
           Propylamine.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Propylamine,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

###########################################################

headTail(data.Propylene.oxide)

str(data.Propylene.oxide)

summary(data.Propylene.oxide)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Propylene.oxide.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Propylene.oxide)
summary(Propylene.oxide.aov)
#Basic ANOVA with summary table


#LME model
data.Propylene.oxide$TreatmentDate <- interaction(data.Propylene.oxide$Treatment, data.Propylene.oxide$Date)
data.Propylene.oxide$TreatmentDate <- factor(data.Propylene.oxide$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Propylene.oxide)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Propylene.oxide.model.null = lme(Gas_Concentration_PPM ~ 1,
                             random = ~1|Plot,
                             data = data.Propylene.oxide)

nagelkerke(model,
           Propylene.oxide.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Propylene.oxide,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))


###############################################

headTail(data.t.Butanol)

str(data.t.Butanol)

summary(data.t.Butanol)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

t.Butanol.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.t.Butanol)
summary(t.Butanol.aov)
#Basic ANOVA with summary table


#LME model
data.t.Butanol$TreatmentDate <- interaction(data.t.Butanol$Treatment, data.t.Butanol$Date)
data.t.Butanol$TreatmentDate <- factor(data.t.Butanol$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.t.Butanol)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

t.Butanol.model.null = lme(Gas_Concentration_PPM ~ 1,
                                 random = ~1|Plot,
                                 data = data.t.Butanol)

nagelkerke(model,
           t.Butanol.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.t.Butanol,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

###############################################

headTail(data.Undecane)

str(data.Undecane)

summary(data.Undecane)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

Undecane.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Undecane)
summary(Undecane.aov)
#Basic ANOVA with summary table


#LME model
data.Undecane$TreatmentDate <- interaction(data.Undecane$Treatment, data.Undecane$Date)
data.Undecane$TreatmentDate <- factor(data.Undecane$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Undecane)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

Undecane.model.null = lme(Gas_Concentration_PPM ~ 1,
                           random = ~1|Plot,
                           data = data.Undecane)

nagelkerke(model,
           Undecane.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.Undecane,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))


#####################################

headTail(data.X1.2.4.Trimethylbenzene)

str(data.X1.2.4.Trimethylbenzene)

summary(data.X1.2.4.Trimethylbenzene)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X1.2.4.Trimethylbenzene.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X1.2.4.Trimethylbenzene)
summary(X1.2.4.Trimethylbenzene.aov)
#Basic ANOVA with summary table


#LME model
data.X1.2.4.Trimethylbenzene$TreatmentDate <- interaction(data.X1.2.4.Trimethylbenzene$Treatment, data.X1.2.4.Trimethylbenzene$Date)
data.X1.2.4.Trimethylbenzene$TreatmentDate <- factor(data.X1.2.4.Trimethylbenzene$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X1.2.4.Trimethylbenzene)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

X1.2.4.Trimethylbenzene.model.null = lme(Gas_Concentration_PPM ~ 1,
                          random = ~1|Plot,
                          data = data.X1.2.4.Trimethylbenzene)

nagelkerke(model,
           X1.2.4.Trimethylbenzene.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X1.2.4.Trimethylbenzene,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

####################################################################

headTail(data.1.3.5.Trimethylbenzene)

str(data.1.3.5.Trimethylbenzene)

summary(data.1.3.5.Trimethylbenzene)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

data.1.3.5.Trimethylbenzene.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.1.3.5.Trimethylbenzene)
summary(data.1.3.5.Trimethylbenzene.aov)
#Basic ANOVA with summary table


#LME model
data.1.3.5.Trimethylbenzene$TreatmentDate <- interaction(data.1.3.5.Trimethylbenzene$Treatment, data.1.3.5.Trimethylbenzene$Date)
data.1.3.5.Trimethylbenzene$TreatmentDate <- factor(data.1.3.5.Trimethylbenzene$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.1.3.5.Trimethylbenzene)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.1.3.5.Trimethylbenzene.model.null = lme(Gas_Concentration_PPM ~ 1,
                                         random = ~1|Plot,
                                         data = data.1.3.5.Trimethylbenzene)

nagelkerke(model,
           data.1.3.5.Trimethylbenzene.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.1.3.5.Trimethylbenzene,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

###########################################

headTail(data.X1.Butanethiol)

str(data.X1.Butanethiol)

summary(data.X1.Butanethiol)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X1.Butanethiol.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X1.Butanethiol)
summary(X1.Butanethiol.aov)
#Basic ANOVA with summary table


#LME model
data.X1.Butanethiol$TreatmentDate <- interaction(data.X1.Butanethiol$Treatment, data.X1.Butanethiol$Date)
data.X1.Butanethiol$TreatmentDate <- factor(data.X1.Butanethiol$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X1.Butanethiol)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X1.Butanethiol.model.null = lme(Gas_Concentration_PPM ~ 1,
                                             random = ~1|Plot,
                                             data = data.X1.Butanethiol)

nagelkerke(model,
           data.X1.Butanethiol.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X1.Butanethiol,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

######################################
headTail(data.X1.Butene)

str(data.X1.Butene)

summary(data.X1.Butene)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X1.Butene.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X1.Butene)
summary(X1.Butene.aov)
#Basic ANOVA with summary table


#LME model
data.X1.Butene$TreatmentDate <- interaction(data.X1.Butene$Treatment, data.X1.Butene$Date)
data.X1.Butene$TreatmentDate <- factor(data.X1.Butene$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X1.Butene)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X1.Butene.model.null = lme(Gas_Concentration_PPM ~ 1,
                                     random = ~1|Plot,
                                     data = data.X1.Butene)

nagelkerke(model,
           data.X1.Butene.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X1.Butene,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

########################################################

headTail(data.X1.Heptene)

str(data.X1.Heptene)

summary(data.X1.Heptene)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X1.Heptene.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X1.Heptene)
summary(X1.Heptene.aov)
#Basic ANOVA with summary table


#LME model
data.X1.Heptene$TreatmentDate <- interaction(data.X1.Heptene$Treatment, data.X1.Heptene$Date)
data.X1.Heptene$TreatmentDate <- factor(data.X1.Heptene$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X1.Heptene)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X1.Heptene.model.null = lme(Gas_Concentration_PPM ~ 1,
                                random = ~1|Plot,
                                data = data.X1.Heptene)

nagelkerke(model,
           data.X1.Heptene.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X1.Heptene,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

################################################################

headTail(data.X1.Hexanol)

str(data.X1.Hexanol)

summary(data.X1.Hexanol)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X1.Hexanol.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X1.Hexanol)
summary(X1.Hexanol.aov)
#Basic ANOVA with summary table


#LME model
data.X1.Hexanol$TreatmentDate <- interaction(data.X1.Hexanol$Treatment, data.X1.Hexanol$Date)
data.X1.Hexanol$TreatmentDate <- factor(data.X1.Hexanol$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X1.Hexanol)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X1.Hexanol.model.null = lme(Gas_Concentration_PPM ~ 1,
                                 random = ~1|Plot,
                                 data = data.X1.Hexanol)

nagelkerke(model,
           data.X1.Hexanol.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X1.Hexanol,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

###############################################################

headTail(data.X1.Methylimidazol)

str(data.X1.Methylimidazol)

summary(data.X1.Methylimidazol)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X1.Methylimidazol.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X1.Methylimidazol)
summary(X1.Methylimidazol.aov)
#Basic ANOVA with summary table


#LME model
data.X1.Methylimidazol$TreatmentDate <- interaction(data.X1.Methylimidazol$Treatment, data.X1.Methylimidazol$Date)
data.X1.Methylimidazol$TreatmentDate <- factor(data.X1.Methylimidazol$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X1.Methylimidazol)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X1.Methylimidazol.model.null = lme(Gas_Concentration_PPM ~ 1,
                                 random = ~1|Plot,
                                 data = data.X1.Methylimidazol)

nagelkerke(model,
           data.X1.Methylimidazol.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X1.Methylimidazol,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

#####################################################

headTail(data.X1.Pentene)

str(data.X1.Pentene)

summary(data.X1.Pentene)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X1.Pentene.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X1.Pentene)
summary(X1.Pentene.aov)
#Basic ANOVA with summary table


#LME model
data.X1.Pentene$TreatmentDate <- interaction(data.X1.Pentene$Treatment, data.X1.Pentene$Date)
data.X1.Pentene$TreatmentDate <- factor(data.X1.Pentene$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X1.Pentene)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X1.Pentene.model.null = lme(Gas_Concentration_PPM ~ 1,
                                        random = ~1|Plot,
                                        data = data.X1.Pentene)

nagelkerke(model,
           data.X1.Pentene.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X1.Pentene,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

#################################################################

headTail(data.X2.3.Dimethylpyrazine)

str(data.X2.3.Dimethylpyrazine)

summary(data.X2.3.Dimethylpyrazine)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X2.3.Dimethylpyrazine.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X2.3.Dimethylpyrazine)
summary(X2.3.Dimethylpyrazine.aov)
#Basic ANOVA with summary table


#LME model
data.X2.3.Dimethylpyrazine$TreatmentDate <- interaction(data.X2.3.Dimethylpyrazine$Treatment, data.X2.3.Dimethylpyrazine$Date)
data.X2.3.Dimethylpyrazine$TreatmentDate <- factor(data.X2.3.Dimethylpyrazine$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X2.3.Dimethylpyrazine)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X2.3.Dimethylpyrazine.model.null = lme(Gas_Concentration_PPM ~ 1,
                                 random = ~1|Plot,
                                 data = data.X2.3.Dimethylpyrazine)

nagelkerke(model,
           data.X2.3.Dimethylpyrazine.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X2.3.Dimethylpyrazine,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

##############################################################

headTail(data.X2.3.Heptanedione)

str(data.X2.3.Heptanedione)

summary(data.X2.3.Heptanedione)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X2.3.Heptanedione.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X2.3.Heptanedione)
summary(X2.3.Heptanedione.aov)
#Basic ANOVA with summary table


#LME model
data.X2.3.Heptanedione$TreatmentDate <- interaction(data.X2.3.Heptanedione$Treatment, data.X2.3.Heptanedione$Date)
data.X2.3.Heptanedione$TreatmentDate <- factor(data.X2.3.Heptanedione$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X2.3.Heptanedione)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X2.3.Heptanedione.model.null = lme(Gas_Concentration_PPM ~ 1,
                                            random = ~1|Plot,
                                            data = data.X2.3.Heptanedione)

nagelkerke(model,
           data.X2.3.Heptanedione.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X2.3.Heptanedione,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

######################################################

headTail(data.X2.3.Hexanedione)

str(data.X2.3.Hexanedione)

summary(data.X2.3.Hexanedione)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X2.3.Hexanedione.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X2.3.Hexanedione)
summary(X2.3.Hexanedione.aov)
#Basic ANOVA with summary table


#LME model
data.X2.3.Hexanedione$TreatmentDate <- interaction(data.X2.3.Hexanedione$Treatment, data.X2.3.Hexanedione$Date)
data.X2.3.Hexanedione$TreatmentDate <- factor(data.X2.3.Hexanedione$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X2.3.Hexanedione)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X2.3.Hexanedione.model.null = lme(Gas_Concentration_PPM ~ 1,
                                        random = ~1|Plot,
                                        data = data.X2.3.Hexanedione)

nagelkerke(model,
           data.X2.3.Hexanedione.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X2.3.Hexanedione,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

######################################################

headTail(data.X2.Ethylhexanol)

str(data.X2.Ethylhexanol)

summary(data.X2.Ethylhexanol)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X2.Ethylhexanol.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X2.Ethylhexanol)
summary(X2.Ethylhexanol.aov)
#Basic ANOVA with summary table


#LME model
data.X2.Ethylhexanol$TreatmentDate <- interaction(data.X2.Ethylhexanol$Treatment, data.X2.Ethylhexanol$Date)
data.X2.Ethylhexanol$TreatmentDate <- factor(data.X2.Ethylhexanol$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X2.Ethylhexanol)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X2.Ethylhexanol.model.null = lme(Gas_Concentration_PPM ~ 1,
                                       random = ~1|Plot,
                                       data = data.X2.Ethylhexanol)

nagelkerke(model,
           data.X2.Ethylhexanol.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X2.Ethylhexanol,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

#############################################################

headTail(data.X2.Methylpyrazine)

str(data.X2.Methylpyrazine)

summary(data.X2.Methylpyrazine)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X2.Methylpyrazine.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X2.Methylpyrazine)
summary(X2.Methylpyrazine.aov)
#Basic ANOVA with summary table


#LME model
data.X2.Methylpyrazine$TreatmentDate <- interaction(data.X2.Methylpyrazine$Treatment, data.X2.Methylpyrazine$Date)
data.X2.Methylpyrazine$TreatmentDate <- factor(data.X2.Methylpyrazine$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X2.Methylpyrazine)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X2.Methylpyrazine.model.null = lme(Gas_Concentration_PPM ~ 1,
                                      random = ~1|Plot,
                                      data = data.X2.Methylpyrazine)

nagelkerke(model,
           data.X2.Methylpyrazine.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X2.Methylpyrazine,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

####################################################


headTail(data.X3.Ethyltoluene)

str(data.X3.Ethyltoluene)

summary(data.X3.Ethyltoluene)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X3.Ethyltoluene.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X3.Ethyltoluene)
summary(X3.Ethyltoluene.aov)
#Basic ANOVA with summary table


#LME model
data.X3.Ethyltoluene$TreatmentDate <- interaction(data.X3.Ethyltoluene$Treatment, data.X3.Ethyltoluene$Date)
data.X3.Ethyltoluene$TreatmentDate <- factor(data.X3.Ethyltoluene$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X3.Ethyltoluene)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X3.Ethyltoluene.model.null = lme(Gas_Concentration_PPM ~ 1,
                                        random = ~1|Plot,
                                        data = data.X3.Ethyltoluene)

nagelkerke(model,
           data.X3.Ethyltoluene.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X3.Ethyltoluene,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))

#################################################################

headTail(data.X4.Ethyltoluene)

str(data.X4.Ethyltoluene)

summary(data.X4.Ethyltoluene)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

X4.Ethyltoluene.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.X4.Ethyltoluene)
summary(X4.Ethyltoluene.aov)
#Basic ANOVA with summary table


#LME model
data.X4.Ethyltoluene$TreatmentDate <- interaction(data.X4.Ethyltoluene$Treatment, data.X4.Ethyltoluene$Date)
data.X4.Ethyltoluene$TreatmentDate <- factor(data.X4.Ethyltoluene$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.X4.Ethyltoluene)
summary(model)

require(multcomp)
summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))

data.X4.Ethyltoluene.model.null = lme(Gas_Concentration_PPM ~ 1,
                                      random = ~1|Plot,
                                      data = data.X4.Ethyltoluene)

nagelkerke(model,
           data.X4.Ethyltoluene.model.null)

Sum = groupwiseMean(Gas_Concentration_PPM ~ Treatment + Date,
                    data   = data.X4.Ethyltoluene,
                    conf   = 0.99999999,
                    digits = 10,
                    traditional = FALSE,
                    percentile  = TRUE)

Sum

pd = position_dodge(.2)

ggplot(Sum, aes(x =    Date,
                y =    Mean,
                color = Treatment)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean ppm per day")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))
