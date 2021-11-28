#####Run lines 1-114 at the start of each R session

rm(list=ls()); graphics.off()
install.packages("dplyr")
install.packages("colorspace")
install.packages("ggplot2")
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(tidyverse)


setwd("G:\\Shared drives\\USDA_SCRI\\UCSC_field_trial_results\\Calcmet_processed_data\\UCSC_duringASD\\VOC_processed_with_KPlibraries\\R_Processing_Folder\\outputfiles")

data <- read.csv("mean.all.data.csv")
str(data)

data = transform(data,Plot = as.character(Plot), 
                 Chloroform_mean = as.numeric(Chloroform_mean),
                 Ethanolamine_mean = as.numeric(Ethanolamine_mean),
                 Isobutanol_mean = as.numeric(Isobutanol_mean),
                 Tetrahydrothiopene_mean = as.numeric(Tetrahydrothiopene_mean),
                 t.Butanol_mean = as.numeric(t.Butanol_mean),
                 X2.Ethylhexanol_mean = as.numeric(X2.Ethylhexanol_mean),
                 Hexane_mean = as.numeric(Hexane_mean), 
                 Isopentane_mean = as.numeric(Isopentane_mean), 
                 Nonane_mean = as.numeric(Nonane_mean),
                 Decane_mean = as.numeric(Decane_mean), 
                 Cyclohexane_mean = as.numeric(Cyclohexane_mean), 
                 X1.Butanethiol_mean = as.numeric(X1.Butanethiol_mean), 
                 Isobutyl.formate_mean = as.numeric(Isobutyl.formate_mean), 
                 X1.Butanethiol_mean = as.numeric(X1.Butanethiol_mean),
                 XX1.2.4.Trimethylbenzene_mean = as.numeric(X1.2.4.Trimethylbenzene_mean), 
                 X1.Butanol_mean = as.numeric(X1.Butanol_mean),
                 X3.Ethyltoluene_mean = as.numeric(X3.Ethyltoluene_mean))
str(data)
#setwd("C:\\CalcmetResults\\Results_Location_UTC")

#filenames.UTC = list.files(pattern="*.csv")
#filenames.UTC

####If I want to import csvs as separate objects/df in R for manipulation
#for(i in filenames){
#assign(i, read.csv(paste(i, ".csv", sep="")))}

#combines all listed files into one data frame
#myfiles.UTC = do.call(rbind, lapply(filenames.UTC, function(x) read.csv(x, stringsAsFactors = FALSE)))
#myfiles.UTC

#data <- do.call("rbind", list(myfiles.ASD, myfiles.UTC))
#str(data)



data$Date = as.Date(data$Date, format = "%m/%d/%Y")

data$c <- ifelse(data$Plot <= "1A01", 'ASD',
                        ifelse(data$Plot <= "1A27", 'ASD',
                               ifelse(data$Plot <= "1A35", 'ASD',
                                      ifelse(data$Plot <= "1A60", 'ASD','UTC'))))
str(data)
colnames(data)[colnames(data)=="Treatment"] <- "Plot"
colnames(data)[colnames(data)=="c"] <- "Treatment"









str(data)

library(tidyr)

data.subset.CO2 = subset(data, select = c("Treatment", "Plot", "Date", "Carbon.dioxide_mean"))
str(data.subset.CO2)
mean.voc <-subset(data, select = -c(X, Water.vapor_mean, Carbon.dioxide_mean, Methane_mean, Nitrous.oxide_mean))
str(mean.voc)
data.subset.CO = subset(data, select = c("Treatment", "Plot", "Date", "Carbon.monoxide_mean"))
str(data.subset.CO2)

#list_df <- split(iris, iris$Species) #split the dataset into a list of datasets based on the value of iris$Species
#list2env(list_DF, envir= .GlobalEnv) #split the list into separate datasets

long.mean.voc <- mean.voc %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Date, Plot, Treatment))
str(long.mean.voc)

has_rownames(long.mean.voc)
long.mean.voc <- long.mean.voc %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
long.mean.voc <- subset(long.mean.voc, Gas_Species!="Plot.1") ### For some odd reason, it was creating a "Plot.1" row in the long.mean.voc dataframe. This code is meant to completely remove it.
unique(long.mean.voc$Gas_Species)

output_path = (paste("G:\\Shared drives\\USDA_SCRI\\UCSC_field_trial_results\\Calcmet_processed_data\\UCSC_duringASD\\VOC_processed_with_KPlibraries\\R_Processing_Folder\\outputfiles\\", sep = ""))
write.csv(long.mean.voc,
          paste(output_path, "long.mean.voc", ".csv", sep="" ))


voc_split <- split(long.mean.voc, long.mean.voc$Gas_Species)
voc_split
new_names <- c("data.Allylcyanide..3.butenenitrile.", "data.Ammonia", "data.Butylamine..1.butanamine.","data.Carbon.monoxide", "data.Chloroform", 
               "data.Cyclohexane", "data.Cyclopentanone", "data.Decane", "data.Dimethyl.disulfide", "data.Dimethyl.sulfide", "data.Dodecane",
               "data.Ethanol", "data.Ethanolamine", "data.Ethyl.fluoride","data.Eucalyptol","data.Heptane","data.Hexane","data.Hexanoic.acid",
               "data.Hexene","data.Hexylamine","data.Isobutanol","data.Isobutene..2.methylpropene.", "data.Isobutyl.formate","data.Isohexane",
               "data.Isopentane","data.Isopentyl.acetate","data.Isopropanol","data.Methyl.chloride","data.Methyl.isocyanate","data.Nitrogen.dioxide",   
               "data.Nonane","data.Pentane","data.Propene","data.Propyl.acetate","data.Propylamine","data.Propylene.oxide",            
               "data.t.Butanol","data.Tetrahydrothiopene","data.Trans.1.2.Dichloroethene","data.Undecane","data.X1.2.3.Trimethylbenzene","data.X1.2.4.Trimethylbenzene",      
               "data.X1.3.5.Trimethylbenzene","data.X1.Butanethiol","data.X1.Butanol","data.X1.Butene","data.X1.Heptene", "data.X1.Hexanol",              
               "data.X1.Methylimidazol","data.X1.Pentanol","data.X1.Pentene","data.X1.Propanol","data.X2.3.Dimethylpyrazine",
               "data.X2.3.Heptanedione","data.X2.3.Hexanedione","data.X2.Ethylhexanol","data.X3.Chloro.2.methyl.1.propene","data.X3.Ethyltoluene", 
               "data.X4.Methyl.3.penten.2.one")
for (i in 1:length(voc_split)) {
  assign(new_names[i], voc_split[[i]])
}



options(max.print=10000)
############################################################################################
###Run 121-129
if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rcompanion)){install.packages("rcompanion")}
require(multcomp)


#Only one date so do a one way anova to look at between plots
data.Allylcyanide..3.butenenitrile. <- tibble::rowid_to_column(data.Allylcyanide..3.butenenitrile., var = "rowid")
data.Ammonia <- tibble::rowid_to_column(data.Ammonia, var = "rowid")
data.Butylamine..1.butanamine. <- tibble::rowid_to_column(data.Butylamine..1.butanamine., var = "rowid")
data.Carbon.monoxide <- tibble::rowid_to_column(data.Carbon.monoxide, var = "rowid")
data.Chloroform <- tibble::rowid_to_column(data.Chloroform, var = "rowid")
data.Cyclohexane <- tibble::rowid_to_column(data.Cyclohexane, var = "rowid")
data.Cyclopentanone <- tibble::rowid_to_column(data.Cyclopentanone, var = "rowid")
data.Decane <- tibble::rowid_to_column(data.Decane, var = "rowid")


################data.Ethanol#############
###  Order factors by the order in data frame
###  Otherwise, R will alphabetize them
#######Copy and paste 146-240 to the end of the code/r sheet and replace "Ethanol" with the name of the VOC of interest
data.Ethanol$Treatment = factor(data.Ethanol$Treatment,
                                           levels=unique(data.Ethanol$Treatment))
data.Ethanol$Date = factor(data.Ethanol$Date,
                           levels=unique(data.Ethanol$Date))
data.Ethanol$Plot = factor(data.Ethanol$Plot,
                           levels=unique(data.Ethanol$Plot))


data.Ethanol$Date = as.Date(data$Date, format = "%m/%d/%Y")
###  Check the data frame

library(psych)

headTail(data.Ethanol)

str(data.Ethanol)

summary(data.Ethanol)

require(nlme)
# Below call does not work for glht, thus we created the interaction term in the data frame
#model <- lme(response ~ group*time, random = ~ 1 | subject / time, dat)

res.aov3 <- aov(Gas_Concentration_PPM ~ Treatment * Plot, data = data.Ethanol)
res.aov3 <- aov(Gas_Concentration_PPM ~ Treatment + Date + Treatment:Date, data = data.Ethanol)
summary(res.aov3)

Ethanol.aov <- aov(Gas_Concentration_PPM ~ Treatment * Date + Error(Plot), data = data.Ethanol)
summary(Ethanol.aov)

data.Ethanol = transform(data.Ethanol,Treatment = as.character(Treatment),
                                 Gas_Concentration_PPM = as.numeric(Gas_Concentration_PPM))


#Basic ANOVA with summary table

cor(data.Ethanol)

#LME model
data.Ethanol$TreatmentDate <- interaction(data.Ethanol$Treatment, data.Ethanol$Date)
data.Ethanol$TreatmentDate <- factor(data.Ethanol$TreatmentDate)
model <- lme(Gas_Concentration_PPM ~ TreatmentDate, random = ~ 1 | Plot / Date, data.Ethanol)
summary(model)





require(multcomp)

summary(glht(model, linfct=mcp(TreatmentDate="Tukey")), test = adjusted(type = "bonferroni"))


getOption("max.print")


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

#####################################################################################

voc.sig <- read.csv(file = 'G:\\Shared drives\\USDA_SCRI\\UCSC_field_trial_results\\Calcmet_processed_data\\UCSC_duringASD\\VOC_processed_with_KPlibraries\\R_Processing_Folder\\outputfiles\\voc.sig.stat.csv')
head(voc.sig)


voc.sig$Gas_Species <- factor(voc.sig$Gas_Species)
voc.sig$Plot <- factor(voc.sig$Plot)

library(reshape2)

voc.sig.wide <- spread(voc.sig, Date ~ Gas_Species, value.var="Gas_Concentration_PPM")
head(voc.sig.wide)

voc.sig.wide <- voc.sig.wide %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
head(voc.sig.wide)

output_path = (paste("G:\\Shared drives\\USDA_SCRI\\UCSC_field_trial_results\\Calcmet_processed_data\\UCSC_duringASD\\VOC_processed_with_KPlibraries\\R_Processing_Folder\\outputfiles\\", sep = ""))
write.csv(voc.sig.wide,
          paste(output_path, "voc.sig.wide", ".csv", sep="" ))