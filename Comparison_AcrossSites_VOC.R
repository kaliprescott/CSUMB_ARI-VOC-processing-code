rm(list=ls()); graphics.off()
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plyr)
install.packages("ggpubr")
library(ggpubr)
if(!require(car)){install.packages("car")}
if(!require(tidyverse)){install.packages("tidyverse")}
#if(!require(multcomp)){install.packages("multcomp")} do not install if using dplyr
if(!require(xtable)){install.packages("xtable")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(stringr)){install.packages("stringr")}
install.packages("imputeTS")
library(imputeTS)
setwd("G:\\Shared drives\\USDA_SCRI\\UCSC_field_trial_results\\Calcmet_processed_data\\UCSC_duringASD\\VOC_processed_with_KPlibraries\\R_Processing_Folder\\outputfiles")
all_csv_UCSC = read.csv("all_csv_temp.csv")

#strip the minutes and seconds off to get the start hour, keep the date or 
#you won't be able it easily link the temperature data to the start time for each sample period
all_csv_UCSC$Date = as.Date(all_csv_UCSC$Date, format="%m/%d/%Y")
all_csv_UCSC$DateTime <- as.POSIXct(paste(all_csv_UCSC$Date, all_csv_UCSC$Time), format="%Y-%m-%d %H:%M")
all_csv_UCSC$HourTime <- format(strptime(all_csv_UCSC$DateTime,"%Y-%m-%d %H:%M"),'%Y-%m-%d %H')

## Use round.Date to round, then format to format
all_csv_UCSC$HourTime <- format(round(all_csv_UCSC$HourTime, units="hours"), format="%m/%d/%y %H")

all_csv_UCSC$Treatment <- ifelse(all_csv_UCSC$Plot == "1A01", 'ASD',
                            ifelse(all_csv_UCSC$Plot == "1A27", 'ASD',
                                   ifelse(all_csv_UCSC$Plot == "1A35", 'ASD',
                                          ifelse(all_csv_UCSC$Plot == "1A60", 'ASD','UTC')))) #I changed "<=" to "==", so I could run the ifelse as it declared "<=" was not a valid operator.



#Output again just in case it gets messed up and you need to reupload
output_path = (paste("..\\outputfiles\\", sep = ""))
write.csv(all_csv_UCSC,
          paste( output_path, "all_csv_UCSC_final", ".csv", sep="" ))

#format Datetime as Time m/d/y h:m in 24 hour format
all_csv_UCSC <- all_csv_UCSC %>% select(-c("X"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("X.1"))

all_UCSC_MSM <-subset(all_csv_UCSC, Treatment == "UTC")
all_csv_UCSC <-subset(all_csv_UCSC, Treatment == "ASD")

eh.ucsc <- read.csv("G:\\Shared drives\\USDA_SCRI\\Meta\\UCSC_field_metadata\\ASD_metadata\\SoilEh\\eh.6.reup.csv")
eh.ucsc <- eh.ucsc %>% select(-c("X"))
eh.ucsc$Date = as.Date(eh.ucsc$Date, format="%Y-%m-%d")
eh.ucsc$DateTime <- as.POSIXct(paste(eh.ucsc$Date, eh.ucsc$Time), format="%Y-%m-%d %H:%M")
eh.ucsc$HourTime <- format(strptime(eh.ucsc$DateTime,"%Y-%m-%d %H:%M"),'%Y-%m-%d %H')
#merge by hourtime and plot
#This drops 10/10-10/14 which are huge portion of the data but don't have Eh data
all_csv_UCSC = merge(all_csv_UCSC, eh.ucsc, by=c("HourTime","Plot"), all=FALSE)

all_csv_UCSC <- all_csv_UCSC %>% select(-c("Time.y"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("Date.y"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("DateTime.y"))
  all_csv_UCSC <- all_csv_UCSC %>% select(-c("StartTime"))
  all_csv_UCSC <- all_csv_UCSC %>% select(-c("Water.vapor"))
all_csv_UCSC <- ungroup(all_csv_UCSC) %>% select(-c("X.1"))
all_csv_UCSC <- all_csv_UCSC %>% ungroup() %>% select(-contains("LibraryFile"))
all_csv_UCSC$Site <- "UCSC"
all_csv_UCSC$Treatment<- "fASD"
all_csv_UCSC$Time = all_csv_UCSC$Time.x
all_csv_UCSC$Date = all_csv_UCSC$Date.x
all_csv_UCSC$DateTime = all_csv_UCSC$DateTime.x
all_csv_UCSC <- all_csv_UCSC %>% select(-c("Time.x"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("Date.x"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("DateTime.x"))

all_csv_UCSC$Date = as.Date(all_csv_UCSC$Date, format = "%m/%d/%Y")
all_csv_UCSC$DateTime <- as.POSIXct(paste(all_csv_UCSC$Date, all_csv_UCSC$Time), format="%Y-%m-%d %H:%M:%S")



duration(24, "hours")
dhours(x = 1)
detach("package:MASS")
if(!require(dplyr)){install.packages("dplyr")}
day_ucsc = all_csv_UCSC %>%
  #  select(everything()) %>%
  arrange(all_csv_UCSC$DateTime) %>%
  mutate(Days_diff = DateTime - min(DateTime)) %>%
  mutate(hour24 = Days_diff / dhours())

day_ucsc$Days.dec = day_ucsc$Days_diff / 86400
print(colnames(day_ucsc)) 
tail(day_ucsc$Days.dec)
tail(day_ucsc$Days_diff)
day_ucsc$Days = floor(day_ucsc$Days.dec)
day_ucsc$Days.dec = gsub(" secs", " ", day_ucsc$Days.dec)
all_csv_UCSC = day_ucsc
rm(day_ucsc)
unique(all_csv_UCSC$Days)
all_csv_UCSC <- all_csv_UCSC %>% select(-c( "Days_diff"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("hour24"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("Date"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("Time"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("DateTime"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("HourTime"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("TreatmentCode"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("Depth"))
all_csv_UCSC <- all_csv_UCSC %>% select(-c("DayCount"))
all_csv_UCSC$Days.dec=as.numeric(all_csv_UCSC$Days.dec)

all_csv_UCSC$EhPhase <- ifelse(all_csv_UCSC$SoilEh >= 200, 'Aerobic', 'Anaerobic')
############################################# MSM data processing ##########################

all_UCSC_MSM <- all_UCSC_MSM %>% select(-c("StartTime"))
all_UCSC_MSM <- all_UCSC_MSM %>% select(-c("Water.vapor"))
all_UCSC_MSM <- ungroup(all_UCSC_MSM) %>% select(-c("X.1"))
all_UCSC_MSM <- all_UCSC_MSM %>% ungroup() %>% select(-contains("LibraryFile"))
all_UCSC_MSM$Site <- "UCSC"

all_UCSC_MSM$Date = as.Date(all_UCSC_MSM$Date, format = "%m/%d/%Y")
all_UCSC_MSM$DateTime <- as.POSIXct(paste(all_UCSC_MSM$Date, all_UCSC_MSM$Time), format="%Y-%m-%d %H:%M:%S")

duration(24, "hours")
dhours(x = 1)
detach("package:MASS")
if(!require(dplyr)){install.packages("dplyr")}
day_ucsc = all_UCSC_MSM %>%
  #  select(everything()) %>%
  arrange(all_UCSC_MSM$DateTime) %>%
  mutate(Days_diff = DateTime - min(DateTime)) %>%
  mutate(hour24 = Days_diff / dhours())

day_ucsc$Days.dec = day_ucsc$Days_diff / 86400
print(colnames(day_ucsc)) 
tail(day_ucsc$Days.dec)
tail(day_ucsc$Days_diff)
day_ucsc$Days = floor(day_ucsc$Days.dec)
day_ucsc$Days.dec = gsub(" secs", " ", day_ucsc$Days.dec)
all_UCSC_MSM = day_ucsc
rm(day_ucsc)
unique(all_UCSC_MSM$Days)
all_UCSC_MSM <- all_UCSC_MSM %>% select(-c( "Days_diff"))
all_UCSC_MSM <- all_UCSC_MSM %>% select(-c("hour24"))
all_UCSC_MSM <- all_UCSC_MSM %>% select(-c("Date"))
all_UCSC_MSM <- all_UCSC_MSM %>% select(-c("Time"))
all_UCSC_MSM <- all_UCSC_MSM %>% select(-c("DateTime"))
all_UCSC_MSM <- all_UCSC_MSM %>% select(-c("HourTime"))
all_UCSC_MSM$Days.dec=as.numeric(all_UCSC_MSM$Days.dec)

all_na <- function(x) any(!is.na(x))
all_UCSC_MSM <- all_UCSC_MSM %>% select_if(all_na)

all_UCSC_MSM <- na.replace(all_UCSC_MSM, 0)
max.MSM = all_UCSC_MSM %>%
    group_by(Site, Treatment, Plot, Days) %>%
    summarise_at(.vars = vars(Carbon.dioxide, Carbon.monoxide,Nitrous.oxide,             
                              Methane, Ammonia, X1.Pentene,                
                              Butylamine..1.butanamine., Propylene.oxide, X2.3.Heptanedione,         
                              Ethanol, Heptane, X1.Pentanol,               
                              X1.Butene, t.Butanol, X1.Hexanol,                
                              X1.Heptene, Dodecane, X1.Methylimidazol,         
                              X2.3.Hexanedione, Hexene, Hexane,                    
                              Hexanoic.acid, Undecane, Isopentane,                
                              Nonane, Hexylamine, Pentane,                   
                              Dimethyl.sulfide, Propylamine, Decane,                    
                              Cyclopentanone, Cyclohexane, X1.3.5.Trimethylbenzene,   
                              X1.2.3.Trimethylbenzene, Propene, Isobutyl.formate),
               .funs = c(max="max"))

long.MSM <- max.MSM %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Site, Treatment, Plot, Days))
long.MSM$Gas_Concentration_PPM=as.numeric(long.MSM$Gas_Concentration_PPM)

library(data.table)
long.MSM <- setDT(long.MSM)[!(Gas_Concentration_PPM %between% c(0, 1))]
unique(long.MSM$Gas_Species)
output_path = (paste("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\FieldTrialVOCStatsReference\\", sep = ""))
write.csv(max.day.MSM,
paste( output_path, "max.day.MSM", ".csv", sep="" ))
max.day.MSM<-spread(long.MSM, key=Gas_Species, value=Gas_Concentration_PPM)
###########################################Venn UCSC vs MSM ########################################
library(ggvenn)
library(RColorBrewer)
ucsc.ana<- read.csv("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\FieldTrialVOCStatsReference\\ucsc.ana.csv", header=FALSE)
ucsc.ana
ucsc.ana <- c("Carbon dioxide","Dimethyl disulfide","Methane","Hexylamine","Dodecane",
              "1-Hexanol","Ethanol","1-Butene","Carbon monoxide","1-Heptene","Nitrous oxide",
              "Dimethyl sulfide","Hexene","1-Pentene","Propylene oxide",
              "Propylamine","Butylamine-1-butanamine","2-Ethylhexanol","Hexanoic acid","Isohexane",
              "Tetrahydrothiopene","Isobutanol","Heptane","t-Butanol","Pentane","1-Pentanol",
              "Methyl chloride","2,3-Heptanedione","Ethyl fluoride","1-Methylimidazol","Nitrogen dioxide",
              "Ethanolamine","1,2,3-Trimethylbenzene","2,3-Hexanedione")

ucsc.a<- read.csv("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\FieldTrialVOCStatsReference\\ucsc.a.csv", header=FALSE)
ucsc.a
ucsc.a<-c("Carbon dioxide","Dimethyl disulfide","Methane","Hexylamine","Dodecane",
          "1-Hexanol","Ethanol","1-Butene","Carbon monoxide","1-Heptene","Nitrous oxide",
          "Dimethyl sulfide","Hexene","1-Pentene","Propylene oxide","4-Methyl-3-penten-2-one",
          "2,3-Dimethylpyrazine","Allylcyanide-3-butenenitrile","Propene","3-Chloro-2-methyl-1-propene")  

msm<-read.csv("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\FieldTrialVOCStatsReference\\MSM.csv", header=FALSE)
msm
msm <- c("Carbon dioxide","Methane","1-Pentene","Butylamine-1-butanamine","Propylene oxide",        
         "Heptane","1-Butene","1-Hexanol","Hexene","Hexane",                 
         "Undecane","Isopentane","Nitrous oxide","Nonane","Hexylamine",             
         "Pentane","Cyclohexane","1,3,5-Trimethylbenzene","Propene")

x=list(UCSC_Aerobic=ucsc.a,UCSC_Anaerobic=ucsc.ana,MSM=msm)

venn=ggvenn(x, 
            show_elements = T, 
            set_name_size = 4, 
            text_size = 2.5,label_sep = "\n", 
            fill_color = brewer.pal(name="Set2",n=3))


InW=10; InH=10 #inches high=, inches wide=  this sets the width and height of the graph in inches. The image can still be adjusted and maintain proportions.
windows(wid=InW, hei=InH)  #this uses above to establish window size parameters and opens the external window to be used
par(mai=c(0.80, 0.85, 0.5, 0.15) ,mgp = c(2.5,1,0)) #this sets the graphing window parameters. You can play around with the numbers to achieve the settings you want.
venn
#calls plot to put in the external window
setwd("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\Figures\\")  #sets working directory for where plot will be saved
ggsave(paste("vennfASD_MSM_r.png", sep=''),  #save the plot and name it as you please
       width=25.4, height=25.4, units='cm')  #same width and height parameters previously established but in cm units. Don't ask me why, just do it. :laughing:


################################################################# PSI processing #######################
setwd("G:\\Shared drives\\USDA_SCRI\\PSI_field_trial_results\\Calcmet_processed\\VOC\\PSI_2019-2020\\outputfiles\\")

all_csv_PSI19 <- read.csv("all_csv_temp.csv")

all_csv_PSI19$Date = as.Date(all_csv_PSI19$Date, format="%m/%d/%Y")
all_csv_PSI19$DateTime <- as.POSIXct(paste(all_csv_PSI19$Date, all_csv_PSI19$Time), format="%Y-%m-%d %H:%M")
all_csv_PSI19$HourTime <- format(strptime(all_csv_PSI19$DateTime,"%Y-%m-%d %H:%M"),'%Y-%m-%d %H')

## Use round.Date to round, then format to format
all_csv_PSI19$HourTime <- format(round(all_csv_PSI19$HourTime, units="hours"), format="%m/%d/%y %H")


#format Datetime as Time m/d/y h:m in 24 hour format
all_csv_PSI19 <- all_csv_PSI19 %>% select(-c("X"))
all_csv_PSI19 <-subset(all_csv_PSI19, Treatment == "ASD")
all_csv_PSI19$Treatment = "bASD"
setwd("G:\\Shared drives\\USDA_SCRI\\Meta\\PSI_field_metadata\\During_ASD\\SoilEh\\PSI2019_2020")
eh.psi = read.csv("PSI2019_ASD_Eh_KP.csv")

eh.psi$Date = as.Date(eh.psi$Date, format="%Y-%m-%d")
eh.psi$DateTime <- as.POSIXct(paste(eh.psi$Date, eh.psi$Time), format="%Y-%m-%d %H:%M")
eh.psi$HourTime <- format(strptime(eh.psi$DateTime,"%Y-%m-%d %H:%M"),'%Y-%m-%d %H')

all_csv_PSI19 = merge(all_csv_PSI19, eh.psi, by=c("HourTime","Plot"), all=FALSE)

colnames(all_csv_PSI19)
library(dplyr)
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Unit"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Compensation"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Residual"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Status"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Line"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("SpectrumFile"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-c("X"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Unnamed.."))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-c("Water.vapor"))
#all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Time"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Gasmet"))
all_csv_PSI19 <- all_csv_PSI19 %>% ungroup() %>% select(-contains("LibraryFile"))
all_csv_PSI19$Site <- "PSI19"
all_csv_PSI19 <- all_csv_PSI19 %>% select(-c("Time.y"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-c("Date.y"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-c("DateTime.y"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-c("Treatment.y"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-c("day"))
colnames(all_csv_PSI19)
detach("package:plyr")
all_csv_PSI19 <- all_csv_PSI19 %>% 
  rename(
    Time = Time.x,
    Date = Date.x,
    DateTime = DateTime.x,
    Treatment = Treatment.x,
    SoilEh = Eh
  )


all_csv_PSI19$Date = as.Date(all_csv_PSI19$Date, format = "%m/%d/%Y")
all_csv_PSI19$DateTime <- as.POSIXct(paste(all_csv_PSI19$Date, all_csv_PSI19$Time), format="%Y-%m-%d %H:%M:%S")



duration(24, "hours")
dhours(x = 1)
detach("package:MASS")
if(!require(dplyr)){install.packages("dplyr")}
day_psi19 = all_csv_PSI19 %>%
  #  select(everything()) %>%
  arrange(all_csv_PSI19$DateTime) %>%
  mutate(Days_diff = DateTime - min(DateTime)) %>%
  mutate(hour24 = Days_diff / dhours())

day_psi19$Days.dec = day_psi19$Days_diff / 86400
print(colnames(day_psi19)) 
tail(day_psi19$Days.dec)
tail(day_psi19$Days_diff)
day_psi19$Days = floor(day_psi19$Days.dec)
day_psi19$Days.dec = gsub("secs", " ", day_psi19$Days.dec)
all_csv_PSI19 = day_psi19
rm(day_psi19)

unique(all_csv_PSI19$Days)
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Days_diff"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("hour24"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Date"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("Time"))
all_csv_PSI19 <- all_csv_PSI19 %>% select(-contains("DateTime"))

all_csv_PSI19$Days.dec=as.numeric(all_csv_PSI19$Days.dec)

all_csv_PSI19$EhPhase <- ifelse(all_csv_PSI19$SoilEh >= 200, 'Aerobic', 'Anaerobic')

#output_path = (paste("..\\outputfiles\\", sep = ""))
#write.csv(all_csv_PSI19,
          #paste( output_path, "all_csv_PSI19_final", ".csv", sep="" ))
############################################################### Venn Diagram #########################################
library(ggvenn)
library(RColorBrewer)
ucsc.ana<- read.csv("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\FieldTrialVOCStatsReference\\ucsc.ana.csv", header=FALSE)
ucsc.ana
ucsc.ana <- c("Butylamine-1-butanamine","Carbon dioxide","Carbon monoxide","Dimethyl disulfide",
              "Dimethyl sulfide","Dodecane","Ethanol","Ethanolamine","Ethyl fluoride",
              "Heptane","Hexanoic acid","Hexene","Hexylamine","Isobutanol","Isohexane",
              "Methane","Methyl chloride","Nitrogen dioxide","Nitrous oxide","Pentane",
              "Propylamine","Propylene oxide","t-Butanol","Tetrahydrothiopene","1,2,3-Trimethylbenzene",
              "1-Butene","1-Heptene","1-Hexanol","1-Methylimidazol","1-Pentanol","1-Pentene","2,3-Heptanedione",
              "2,3-Hexanedione", "2-Ethylhexanol"
              )

ucsc.a<- read.csv("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\FieldTrialVOCStatsReference\\ucsc.a.csv", header=FALSE)
ucsc.a
ucsc.a<-c("Allylcyanide-3-butenenitrile","Carbon dioxide","Carbon monoxide","Dimethyl disulfide",
          "Dimethyl sulfide", "Dodecane","Ethanol","Hexene","Hexylamine","Methane","Nitrous oxide",
          "Propene","Propylene oxide","1-Butene","1-Heptene","1-Hexanol","1-Pentene","2,3-Dimethylpyrazine",
          "3-Chloro-2-methyl-1-propene","4-Methyl-3-penten-2-one")  

psi.ana<- read.csv("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\FieldTrialVOCStatsReference\\psi.ana.csv", header=FALSE)
psi.ana
psi.ana<-c("Carbon dioxide","Cis-2-Butene","Cis-2-pentene","Hexene","Methane","Nitrous oxide","Propylene oxide", "1-Butanethiol",
          "1-Pentene","2,3-Dimethylpyrazine","2,3-Heptanedione","2-Methylpyrazine")

psi.a<- read.csv("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\FieldTrialVOCStatsReference\\psi.a.csv", header=FALSE)
psi.a
psi.a <-c( "Butylamine-1-butanamine","Carbon dioxide","Methane", "Nitrous oxide", "Propylamine", "Propylene oxide", "Undecane",
         "1-Butanethiol","1-Butene","1-Heptene", "2,3-Dimethylpyrazine","2-Methylpyrazine")
x=list(UCSC_Anaerobic=ucsc.ana,UCSC_Aerobic=ucsc.a
       #PSI_Aerobic=psi.a, PSI_Anaerobic=psi.ana
       )

venn=ggvenn(x, show_elements = T, 
            set_name_size = 4, 
            text_size = 2.5,label_sep = "\n", 
            fill_color = brewer.pal(name="Set2",n=4))


InW=10; InH=10 #inches high=, inches wide=  this sets the width and height of the graph in inches. The image can still be adjusted and maintain proportions.
windows(wid=InW, hei=InH)  #this uses above to establish window size parameters and opens the external window to be used
par(mai=c(0.80, 0.85, 0.5, 0.15) ,mgp = c(2.5,1,0)) #this sets the graphing window parameters. You can play around with the numbers to achieve the settings you want.
venn
#calls plot to put in the external window
setwd("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\Figures\\")  #sets working directory for where plot will be saved
ggsave(paste("vennUCSConly.png", sep=''),  #save the plot and name it as you please
       width=25.4, height=25.4, units='cm')  #same width and height parameters previously established but in cm units. Don't ask me why, just do it. :laughing:


###############################################convert to long###########################################################################


long.UCSC.eh <- all_csv_UCSC %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Days.dec, Days, Site, Plot, Treatment, EhPhase, SoilEh))
long.PSI19.eh <- all_csv_PSI19 %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Days.dec, Days, Site, Plot, Treatment, EhPhase, SoilEh))
long.UCSC.eh$Gas_Concentration_PPM=as.numeric(long.UCSC.eh$Gas_Concentration_PPM)
#long.UCSC.eh$Gas_Concentration_PPM[long.UCSC.eh$Gas_Concentration_PPM < 1] <- NA
#drop zeros
library(data.table)
long.UCSC.eh <- setDT(long.UCSC.eh)[!(Gas_Concentration_PPM %between% c(0.0000001, 1))]
long.PSI19.eh <- setDT(long.PSI19.eh)[!(Gas_Concentration_PPM %between% c(0.0000001, 1))]
#long.UCSC.eh$Gas_Concentration_PPM <- na_replace(long.UCSC.eh$Gas_Concentration_PPM, 0)

long.PSI19.eh$Gas_Concentration_PPM=as.numeric(long.PSI19.eh$Gas_Concentration_PPM)
#long.PSI19.eh$Gas_Concentration_PPM[long.PSI19.eh$Gas_Concentration_PPM < 1] <- NA
#long.PSI19.eh$Gas_Concentration_PPM <- na_replace(long.PSI19.eh$Gas_Concentration_PPM, 0)
long.eh=rbind(long.UCSC.eh,long.PSI19.eh)
######################################producing means and maxes##########################################################
all_csv_UCSC <- all_csv_UCSC %>% select(-contains(" Days"))

all_csv_UCSC$Days = as.numeric(all_csv_UCSC$Days)
colnames(all_csv_UCSC)

all_csv_UCSC[all_csv_UCSC < 1] <- NA
all_csv_UCSC <- na_replace(all_csv_UCSC, 0)

max.UCSC = all_csv_UCSC %>%
  group_by(Site, Days) %>%
  summarise_at(.vars = vars( Carbon.dioxide,Carbon.monoxide,Nitrous.oxide,Methane, Ammonia,                        
                            Chloroform,X1.Pentene,Butylamine..1.butanamine.,Ethanolamine,Propylene.oxide,
                            X2.3.Heptanedione,Ethanol,Heptane,Isobutanol,X1.Pentanol,X1.Butene,t.Butanol,                      
                            X1.Hexanol,X1.Heptene,Dodecane,Isopentyl.acetate,X1.Methylimidazol, X2.3.Hexanedione,X2.Ethylhexanol,
                            Hexene,Tetrahydrothiopene,Hexane,Hexanoic.acid,Undecane,                      
                            Isopentane, Nonane, Hexylamine,Pentane, Isobutene..2.methylpropene.,X3.Chloro.2.methyl.1.propene,   
                            Isohexane,Dimethyl.sulfide,Propylamine,Decane, Dimethyl.disulfide, Cyclopentanone,                 
                            Cyclohexane,X1.3.5.Trimethylbenzene,Nitrogen.dioxide,Ethyl.fluoride,X4.Methyl.3.penten.2.one,         
                            Eucalyptol,X1.2.3.Trimethylbenzene,Propene,Methyl.chloride,X2.3.Dimethylpyrazine,Isobutyl.formate,
                            Trans.1.2.Dichloroethene,Isopropanol,X1.Propanol,Allylcyanide..3.butenenitrile., 
                            Methyl.isocyanate,Propyl.acetate,X1.Butanethiol,X1.2.4.Trimethylbenzene,X1.Butanol,X3.Ethyltoluene),
               .funs = c(max="max"))

setwd("G:\\Shared drives\\USDA_SCRI\\UCSC_field_trial_results\\Calcmet_processed_data\\UCSC_duringASD\\VOC_processed_with_KPlibraries\\R_Processing_Folder\\outputfiles")
output_path = (paste("..\\outputfiles\\", sep = ""))
write.csv(max.UCSC,
paste( output_path, "max.UCSC", ".csv", sep="" ))


#colnames(PSI19.eh)
#PSI19.eh <- PSI19.eh %>% select(c(Days.dec, Days, Site, Plot, EhPhase, Carbon.dioxide, Nitrous.oxide, Methane,X1.Pentene,Propylene.oxide,
                                  #X2.3.Heptanedione,X1.Butanethiol, X2.Methylpyrazine, X2.3.Dimethylpyrazine, X1.Heptene, Propylamine, 
                                  #Undecane, Hexene, X1.Butene, Butylamine..1.butanamine., Cis.2.Butene, Cis.2.pentene))
#PSI19.eh <- PSI19.eh %>% select(-contains("Days"))
#PSI19.eh[PSI19.eh < 1] <- NA
#PSI19.eh <- na_replace(PSI19.eh, 0)
#max.PSI19 = PSI19.eh %>%
#  group_by(Site, Treatment, Plot, EhPhase) %>%
#  summarise_at(.vars = vars( Carbon.dioxide,                Nitrous.oxide,                 Methane,                       Ammonia,                       Propane,                     
#                            Methacrylic.acid,              X1.Hexanol,                    X1.Pentene,                    Isopentyl.acetate,             Propyl.acetate,              
#                            X2.3.Hexanedione,              Cyclopentanone,                Propylene.oxide,               Ethanol,                       Pentane,                     
#                            X1.2.4.Trimethylbenzene,       t.Butanol,                     Methyl.isocyanate,             X1.Butanethiol,                Ethylene.glycol,             
#                            X3.Ethyltoluene,               X2.3.Heptanedione,             X2.Methylpyrazine,             Ethyl.benzene,                 Isobutanol,                  
#                            Isobutene..2.methylpropene.,   Butane,                        X4.Ethyltoluene,               Cyclopentane,                  X2.3.Dimethylpyrazine,       
#                            X2.Ethylhexanol,               Ethane,                        Cyclohexane,                   X1.Heptene,                    Hexanoic.acid,               
#                            Propylamine,                   X1.Methylimidazol,             Hexane,                        Undecane,                      X1.3.5.Trimethylbenzene,     
#                            Dimethyl.sulfide,              Hexene,                        X1.Butene,                     X1.2.3.Trimethylbenzene,       Ethylcyclohexane,            
#                            Butylamine..1.butanamine.,     Cis.2.Butene,                  Cis.2.pentene,                 Nonane,                        Dodecane,                    
#                            Ethanolamine ),
#               .funs = c(max="max"))

#setwd("G:\\Shared drives\\USDA_SCRI\\PSI_field_trial_results\\Calcmet_processed\\VOC\\PSI_2019-2020\\outputfiles\\")
#output_path = (paste("..\\outputfiles\\", sep = ""))
#write.csv(max.PSI19,
          #paste( output_path, "max.psi19.EhPhase", ".csv", sep="" ))
####################################long means #####################################################################################################
long.eh = na.omit(long.eh)
#long.eh <- long.eh %>% select(-c("EhPhase"))
long.eh <- long.eh %>% select(-c("Days.dec"))

aerobic.anaerobic.max = long.eh %>%
    group_by(Site, EhPhase, Gas_Species) %>%
    summarise_at(.vars = vars(Gas_Concentration_PPM),
               .funs = c(max="max"))
setwd("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\FieldTrialVOCStatsReference\\")
output_path = (paste("..\\FieldTrialVOCStatsReference\\", sep = ""))
write.csv(aerobic.anaerobic.max,
paste( output_path, "aerobic.anaerobic.max", ".csv", sep="" ))
########################################################Stats for between Eh Phases#######################################################################
if(!require(multcomp)){install.packages("multcomp")}
if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(ARTool)){install.packages("ARTool")}

install.packages("RTools")

require(multcomp)
###Compare within site aerobic to anaerobic
###Use list already generated

### UCSC both phases
#Carbon.dioxide
#Nitrous.oxide
#Methane
#X1.Pentene
#Butylamine..1.butanamine.
#Ethanolamine
#Propylene.oxide
#X1.Butene
#X1.Hexanol
#X1.Heptene
#Dodecane
#X2.3.Hexanedione
#X2.Ethylhexanol
#Hexylamine
#Pentane
#Dimethyl.sulfide
#Propylamine
#Dimethyl.disulfide
#X1.2.3.Trimethylbenzene

#PSI both phases
#Carbon.dioxide#
#Nitrous.oxide#
#Methane#
#Propylene.oxide
#X1.Butanethiol

#X2.3.Dimethylpyrazine
long.eh$Days = as.factor(long.eh$Days)
unique(long.eh$Days)
u.cbPalette <- c("#999999", "#E69F00")
DMS<- ggplot(subset(long.eh, Gas_Species == "Dimethyl.sulfide")) + 
  (mapping = aes(x = Days, y = Gas_Concentration_PPM, color=Site)) + 
  geom_boxplot() + 
  scale_color_manual(values=u.cbPalette)+
  scale_x_discrete(name = "Sample Day", limits = c("0", "1", "2", "3", "4", "6", "7", "9",
                                                   "10", "13", "14", "16", "17", "18", "19", "20",
                                                   "21", "25", "26", "27", "28", "29", "30", "34",
                                                   "37", "38", "39", "46", "60")) +
  #geom_line(size=1) + 
  #scale_x_continuous(name = "Day", breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)) +
  scale_y_continuous(name = "Concentration (PPM)") +
  #ylim(0,25) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("Dimethyl sulfide")
DMS

DMS <- ggplot(subset(long.eh, Gas_Species == "Dimethyl.sulfide")) + 
  (mapping = aes(x = Days, y = Gas_Concentration_PPM, color=Treatment)) +
  geom_boxplot() + 
  scale_color_manual(values=u.cbPalette)+
  theme_classic() +
 #geom_line(size=1) + 
  scale_x_discrete(name = "Sample Day", limits = c("0", "1", "2", "3", "4", "6", "7", "9",
                                                   "10", "13", "14", "16", "17", "18", "19", "20",
                                                   "21", "25", "26", "27", "28", "29", "30", "34",
                                                   "37", "38", "39", "46", "60")) +
  scale_y_continuous(name = "Concentration (PPM)") +
  #ylim(0,25) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("A. DMS")
DMS
unique(long.eh$Gas_Species)
u.cbPalette <- c("#E69F00")
DMDS<- ggplot(subset(long.eh, Gas_Species == "Dimethyl.disulfide")) + 
  (mapping = aes(x = Days, y = Gas_Concentration_PPM, color=Treatment)) +
  geom_boxplot() + 
  scale_color_manual(values=u.cbPalette)+
  theme_classic() +
  #geom_line(size=1) + 
  scale_x_discrete(name = "Sample Day", limits = c("0", "1", "2", "3", "4", "6", "7", "9",
                                                   "10", "13", "14", "16", "17", "18", "19", "20",
                                                   "21", "25", "26", "27", "28", "29", "30", "34",
                                                   "37", "38", "39", "46", "60")) +
  scale_y_continuous(name = "Concentration (PPM)") +
  #ylim(0,25) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("B. DMDS")
DMDS

InW=5; InH=5 #inches high=, inches wide=  this sets the width and height of the graph in inches. The image can still be adjusted and maintain proportions.
windows(wid=InW, hei=InH)  #this uses above to establish window size parameters and opens the external window to be used
par(mai=c(0.80, 0.85, 0.5, 0.15) ,mgp = c(2.5,1,0)) #this sets the graphing window parameters. You can play around with the numbers to achieve the settings you want.
ggarrange(DMS, DMDS, ncol = 1, nrow = 2)
#calls plot to put in the external window
setwd("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\Figures\\")  #sets working directory for where plot will be saved
ggsave(paste("DMS_DMDS.png", sep=''),  #save the plot and name it as you please
       width=12.7, height=12.7, units='cm')  #same width and height parameters previously established but in cm units. Don't ask me why, just do it. :laughing:

ggarrange(DMS, DMDS, ncol = 1, nrow = 2)



#Both Sites
###########################################################Carbon.dioxide##################################
Split_Species<-split(m.long.eh,f=m.long.eh$Gas_Species)

str(Split_Species$Carbon.dioxide)

Split_Species$Carbon.dioxide$Days = as.numeric(Split_Species$Carbon.dioxide$Days)
Split_Species$Carbon.dioxide$Gas_Concentration_PPM = as.numeric(Split_Species$Carbon.dioxide$Gas_Concentration_PPM)


Split_Species$Carbon.dioxide <- Split_Species$Carbon.dioxide %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Carbon.dioxide)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Carbon.dioxide <- Split_Species$Carbon.dioxide[complete.cases(Split_Species$Carbon.dioxide[ ,8]),]
b=ggplot(Split_Species$Carbon.dioxide, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Carbon.dioxide)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

m1 = aov(Gas_Concentration_PPM ~ Site+EhPhase, data = Split_Species$Carbon.dioxide)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$Carbon.dioxide, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
Anova(r1, type="III")

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Site*Days, data = Split_Species$Carbon.dioxide) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$Carbon.dioxide, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Site+Days, random = ~1|Plot, data = Split_Species$Carbon.dioxide, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
install.packages("mmrm")
library(mmrm)
Split_Species$Carbon.dioxide$Days = as.factor(Split_Species$Carbon.dioxide$Days)
Split_Species$Carbon.dioxide$Plot = as.factor(Split_Species$Carbon.dioxide$Plot)
Split_Species$Carbon.dioxide$Site = as.factor(Split_Species$Carbon.dioxide$Site)
mmrm2 <- mmrm(mean~Days+Site*Days+us(Days|Plot), data=Split_Species$Carbon.dioxide)
summary(mmrm2)
#####################################################Nitrous.oxide######################################

Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Nitrous.oxide)

Split_Species$Nitrous.oxide$Days = as.numeric(Split_Species$Nitrous.oxide$Days)
Split_Species$Nitrous.oxide$Gas_Concentration_PPM = as.numeric(Split_Species$Nitrous.oxide$Gas_Concentration_PPM)


Split_Species$Nitrous.oxide <- Split_Species$Nitrous.oxide %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Nitrous.oxide)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Nitrous.oxide <- Split_Species$Nitrous.oxide[complete.cases(Split_Species$Nitrous.oxide[ ,8]),]
#b=ggplot(Split_Species$Nitrous.oxide, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()

#c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Nitrous.oxide)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

#ggarrange(a,b,c, 
#          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
#          ncol = 2, nrow = 2)

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Nitrous.oxide)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)
## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$Nitrous.oxide, method = "REML") 
anova(r1)
#the fixed effects are EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
Anova(r1, type="III")

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Site+Days, data = Split_Species$Nitrous.oxide) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Site+Days, random = ~1|Plot, data = Split_Species$Nitrous.oxide, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Site+Days, random = ~1|Plot, data = Split_Species$Nitrous.oxide, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))

##################################################### non-normal Methane################################################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Methane)

Split_Species$Methane$Days = as.numeric(Split_Species$Methane$Days)
Split_Species$Methane$Gas_Concentration_PPM = as.numeric(Split_Species$Methane$Gas_Concentration_PPM)


Split_Species$Methane <- Split_Species$Methane %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Methane)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Methane <- Split_Species$Methane[complete.cases(Split_Species$Methane[ ,8]),]
b=ggplot(Split_Species$Methane, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Methane)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Methane)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ Site*Days*EhPhase, random = ~1|Plot, data = Split_Species$Methane, method = "REML") 
anova(r1)
Anova(r1, type="III")
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lm1.1=lm(Gas_Concentration_PPM ~ Site*Days*EhPhase, random = ~1|Plot, data = Split_Species$Methane)
summary(lm1.1)

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase+Site*Days, data = Split_Species$Methane) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase+Site*Days, random = ~1|Plot, data = Split_Species$Methane, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Site+Days, random = ~1|Plot, data = Split_Species$Methane, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
shapiro.test(r1$residuals)
############################################# Ethanol ##########################################

Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Ethanol)

Split_Species$Ethanol$Days = as.numeric(Split_Species$Ethanol$Days)
Split_Species$Ethanol$Gas_Concentration_PPM = as.numeric(Split_Species$Ethanol$Gas_Concentration_PPM)


Split_Species$Ethanol <- Split_Species$Ethanol %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Ethanol)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a

lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Days + (1|Plot), data=Split_Species$Ethanol)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

###################################################### zero inflated X1.Pentene######################################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$X1.Pentene)

Split_Species$X1.Pentene$Days = as.numeric(Split_Species$X1.Pentene$Days)
Split_Species$X1.Pentene$Gas_Concentration_PPM = as.numeric(Split_Species$X1.Pentene$Gas_Concentration_PPM)


#Split_Species$X1.Pentene <- Split_Species$X1.Pentene %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X1.Pentene)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$X1.Pentene <- Split_Species$X1.Pentene[complete.cases(Split_Species$X1.Pentene[ ,8]),]
b=ggplot(Split_Species$X1.Pentene, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X1.Pentene)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X1.Pentene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)


## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$X1.Pentene, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$X1.Pentene)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)
### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$X1.Pentene) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X1.Pentene, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$X1.Pentene, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB)
Split_Species$X1.Pentene$Days = as.factor(Split_Species$X1.Pentene$Days)
Split_Species$X1.Pentene$EhPhase = as.factor(Split_Species$X1.Pentene$EhPhase)
Split_Species$X1.Pentene$Plot = as.factor(Split_Species$X1.Pentene$Plot)
Split_Species$X1.Pentene$Site = as.factor(Split_Species$X1.Pentene$Site)
Split_Species$X1.Pentene$Gas_Concentration_PPM = as.numeric(Split_Species$X1.Pentene$Gas_Concentration_PPM)

hurP1 <- glmmadmb(Gas_Concentration_PPM ~ EhPhase * Site * Days + (1 | Plot), 
                  data = Split_Species$X1.Pentene, family = "binomial")
#Got this warning because Gas_concentration is not discrete because the data has decimal places and hurdle wants 1s and 0s
#In glmmadmb(Gas_Concentration_PPM ~ EhPhase * Site * Days + (1 |  :
#non-integer response values in discrete family
###NEED POST-HOC##########################################################zero inflated Butylamine..1.butanamine.###########################

Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Butylamine..1.butanamine.)

Split_Species$Butylamine..1.butanamine.$Days = as.numeric(Split_Species$Butylamine..1.butanamine.$Days)
Split_Species$Butylamine..1.butanamine.$Gas_Concentration_PPM = as.numeric(Split_Species$Butylamine..1.butanamine.$Gas_Concentration_PPM)


Split_Species$Butylamine..1.butanamine. <- Split_Species$Butylamine..1.butanamine. %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Butylamine..1.butanamine.)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Butylamine..1.butanamine. <- Split_Species$Butylamine..1.butanamine.[complete.cases(Split_Species$Butylamine..1.butanamine.[ ,8]),]
b=ggplot(Split_Species$Butylamine..1.butanamine., aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Butylamine..1.butanamine.)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Butylamine..1.butanamine.)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Butylamine..1.butanamine., method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Days + (1|Plot), data=Split_Species$Butylamine..1.butanamine.)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)
### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Butylamine..1.butanamine.) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Butylamine..1.butanamine., method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Butylamine..1.butanamine., method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
#########################################################zero inflated Propylene.oxide###############################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Propylene.oxide)

Split_Species$Propylene.oxide$Days = as.numeric(Split_Species$Propylene.oxide$Days)
Split_Species$Propylene.oxide$Gas_Concentration_PPM = as.numeric(Split_Species$Propylene.oxide$Gas_Concentration_PPM)


Split_Species$Propylene.oxide <- Split_Species$Propylene.oxide %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Propylene.oxide)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Propylene.oxide <- Split_Species$Propylene.oxide[complete.cases(Split_Species$Propylene.oxide[ ,8]),]
b=ggplot(Split_Species$Propylene.oxide, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Propylene.oxide)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
hist(Split_Species$Propylene.oxide$Gas_Concentration_PPM)
ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Propylene.oxide)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)


## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Propylene.oxide, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$Propylene.oxide)
anova(lmeModel)
Anova(lmeModel, test="F")

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Propylene.oxide) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Propylene.oxide, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Propylene.oxide, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))

######################################################### not enough data Propylamine########################################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)
str(Split_Species$Propylamine)

Split_Species$Propylamine$Days = as.numeric(Split_Species$Propylamine$Days)
Split_Species$Propylamine$Gas_Concentration_PPM = as.numeric(Split_Species$Propylamine$Gas_Concentration_PPM)
hist(Split_Species$Propylamine$Gas_Concentration_PPM)

Split_Species$Propylamine <- Split_Species$Propylamine %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Propylamine)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Propylamine <- Split_Species$Propylamine[complete.cases(Split_Species$Propylamine[ ,8]),]
b=ggplot(Split_Species$Propylamine, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Propylamine)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$Propylamine$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Propylamine)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Propylamine, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.


### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Propylamine) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Propylamine, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Propylamine, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB)
Split_Species$Propylamine$Days = as.factor(Split_Species$Propylamine$Days)
Split_Species$Propylamine$EhPhase = as.factor(Split_Species$Propylamine$EhPhase)
Split_Species$Propylamine$Plot = as.factor(Split_Species$Propylamine$Plot)

hurP1 <- glmmadmb(Gas_Concentration_PPM ~ EhPhase * Days + (1 | Plot), 
                  data = Split_Species$Propylamine, family = "binomial")

#########################################################not normal 1-butene###############################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$X1.Butene)

Split_Species$X1.Butene$Days = as.numeric(Split_Species$X1.Butene$Days)
Split_Species$X1.Butene$Gas_Concentration_PPM = as.numeric(Split_Species$X1.Butene$Gas_Concentration_PPM)


Split_Species$X1.Butene <- Split_Species$X1.Butene %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X1.Butene)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
hist((Split_Species$X1.Butene$Gas_Concentration_PPM))
#exclude all rows with NAs
Split_Species$X1.Butene <- Split_Species$X1.Butene[complete.cases(Split_Species$X1.Butene[ ,8]),]
b=ggplot(Split_Species$X1.Butene, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X1.Butene)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X1.Butene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X1.Butene, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$X1.Butene)
anova(lmeModel)
Anova(lmeModel, test="F")
### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$X1.Butene) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X1.Butene, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$X1.Butene, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))

##########################################################not enough data 1-heptene##############################

Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$X1.Heptene)

Split_Species$X1.Heptene$Days = as.numeric(Split_Species$X1.Heptene$Days)
Split_Species$X1.Heptene$Gas_Concentration_PPM = as.numeric(Split_Species$X1.Heptene$Gas_Concentration_PPM)
hist(Split_Species$X1.Heptene$Gas_Concentration_PPM)

Split_Species$X1.Heptene <- Split_Species$X1.Heptene %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X1.Heptene)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$X1.Heptene <- Split_Species$X1.Heptene[complete.cases(Split_Species$X1.Heptene[ ,8]),]
b=ggplot(Split_Species$X1.Heptene, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X1.Heptene)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X1.Heptene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X1.Heptene, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$Hexene)
anova(lmeModel)
Anova(lmeModel, test="F")

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$X1.Heptene) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X1.Heptene, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$X1.Heptene, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
###NEED POST-HOC########################################################hexene#########################

Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Hexene)

Split_Species$Hexene$Days = as.numeric(Split_Species$Hexene$Days)
Split_Species$Hexene$Gas_Concentration_PPM = as.numeric(Split_Species$Hexene$Gas_Concentration_PPM)
hist(Split_Species$Hexene$Gas_Concentration_PPM)

Split_Species$Hexene <- Split_Species$Hexene %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Hexene)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Hexene <- Split_Species$Hexene[complete.cases(Split_Species$Hexene[ ,8]),]
b=ggplot(Split_Species$Hexene, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Hexene)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Hexene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$Hexene, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$Hexene)
anova(lmeModel)
Anova(lmeModel, test="F")

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Hexene) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Hexene, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Hexene, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))

###########################################################################UCSC only#################
######################################################## Ethanolamine################################


Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Ethanolamine)

Split_Species$Ethanolamine$Days = as.numeric(Split_Species$Ethanolamine$Days)
Split_Species$Ethanolamine$Gas_Concentration_PPM = as.numeric(Split_Species$Ethanolamine$Gas_Concentration_PPM)


Split_Species$Ethanolamine <- Split_Species$Ethanolamine %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Ethanolamine)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Ethanolamine <- Split_Species$Ethanolamine[complete.cases(Split_Species$Ethanolamine[ ,8]),]
b=ggplot(Split_Species$Ethanolamine, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Ethanolamine)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

hist((Split_Species$Ethanolamine$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Ethanolamine)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)


## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Ethanolamine, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$Ethanolamine)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

Split_Species<-split(long.eh,f=long.eh$Gas_Species)
m1 = lm(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Ethanolamine)
summary(m1)
plot(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Ethanolamine)
#only measured at UCSC during anaerobic

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Ethanolamine) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Ethanolamine, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Ethanolamine, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))


##########################################################not normal 1-hexanol##############################
Split_Species<-split(long.UCSC.eh,f=long.UCSC.eh$Gas_Species)

str(Split_Species$X1.Hexanol)

Split_Species$X1.Hexanol$Days = as.numeric(Split_Species$X1.Hexanol$Days)
Split_Species$X1.Hexanol$Gas_Concentration_PPM = as.numeric(Split_Species$X1.Hexanol$Gas_Concentration_PPM)


Split_Species$X1.Hexanol <- Split_Species$X1.Hexanol %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=EhPhase), data = Split_Species$X1.Hexanol)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()

#exclude all rows with NAs
Split_Species$X1.Hexanol <- Split_Species$X1.Hexanol[complete.cases(Split_Species$X1.Hexanol[ ,8]),]
b=ggplot(Split_Species$X1.Hexanol, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X1.Hexanol)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X1.Hexanol)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhases where the gas was detected


r1 = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X1.Hexanol, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.


### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$X1.Hexanol) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X1.Hexanol, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$X1.Hexanol, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
##########################################################not normal Dodecane#############################

Split_Species<-split(long.UCSC.eh,f=long.UCSC.eh$Gas_Species)

str(Split_Species$Dodecane)

Split_Species$Dodecane$Days = as.numeric(Split_Species$Dodecane$Days)
Split_Species$Dodecane$Gas_Concentration_PPM = as.numeric(Split_Species$Dodecane$Gas_Concentration_PPM)


Split_Species$Dodecane <- Split_Species$Dodecane %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Dodecane)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Dodecane <- Split_Species$Dodecane[complete.cases(Split_Species$Dodecane[ ,8]),]
b=ggplot(Split_Species$Dodecane, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Dodecane)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$Dodecane$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Dodecane)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Dodecane, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Days + (1|Plot), data=Split_Species$Dodecane)
anova(lmeModel)
Anova(lmeModel, test="F")

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Dodecane) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Dodecane, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Dodecane, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
##########################################################X2.3.Hexanedione#####################

Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$X2.3.Hexanedione)

Split_Species$X2.3.Hexanedione$Days = as.numeric(Split_Species$X2.3.Hexanedione$Days)
Split_Species$X2.3.Hexanedione$Gas_Concentration_PPM = as.numeric(Split_Species$X2.3.Hexanedione$Gas_Concentration_PPM)


Split_Species$X2.3.Hexanedione <- Split_Species$X2.3.Hexanedione %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X2.3.Hexanedione)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$X2.3.Hexanedione <- Split_Species$X2.3.Hexanedione[complete.cases(Split_Species$X2.3.Hexanedione[ ,8]),]
b=ggplot(Split_Species$X2.3.Hexanedione, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X2.3.Hexanedione)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$X2.3.Hexanedione$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X2.3.Hexanedione)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$X2.3.Hexanedione, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase*Site*Days + (1|Plot), data=Split_Species$X2.3.Hexanedione)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$X2.3.Hexanedione) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X2.3.Hexanedione, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$X2.3.Hexanedione, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB)
Split_Species$X2.3.Hexanedione$Days = as.factor(Split_Species$X2.3.Hexanedione$Days)
Split_Species$X2.3.Hexanedione$EhPhase = as.factor(Split_Species$X2.3.Hexanedione$EhPhase)
Split_Species$X2.3.Hexanedione$Plot = as.factor(Split_Species$X2.3.Hexanedione$Plot)

hurP1 <- glmmadmb(Gas_Concentration_PPM ~ EhPhase * Days + (1 | Plot), 
                  data = Split_Species$X2.3.Hexanedione, family = "binomial")

##########################################################X2.Ethylhexanol##############################

Split_Species<-split(long.UCSC.eh,f=long.UCSC.eh$Gas_Species)

str(Split_Species$X2.Ethylhexanol)

Split_Species$X2.Ethylhexanol$Days = as.numeric(Split_Species$X2.Ethylhexanol$Days)
Split_Species$X2.Ethylhexanol$Gas_Concentration_PPM = as.numeric(Split_Species$X2.Ethylhexanol$Gas_Concentration_PPM)


Split_Species$X2.Ethylhexanol <- Split_Species$X2.Ethylhexanol %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=EhPhase), data = Split_Species$X2.Ethylhexanol)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()

#exclude all rows with NAs
Split_Species$X2.Ethylhexanol <- Split_Species$X2.Ethylhexanol[complete.cases(Split_Species$X2.Ethylhexanol[ ,8]),]
b=ggplot(Split_Species$X2.Ethylhexanol, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X2.Ethylhexanol)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$X2.Ethylhexanol$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X2.Ethylhexanol)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X2.Ethylhexanol, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.


### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$X2.Ethylhexanol) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X2.Ethylhexanol, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$X2.Ethylhexanol, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB)
Split_Species$X2.Ethylhexanol$Days = as.factor(Split_Species$X2.Ethylhexanol$Days)
Split_Species$X2.Ethylhexanol$EhPhase = as.factor(Split_Species$X2.Ethylhexanol$EhPhase)
Split_Species$X2.Ethylhexanol$Plot = as.factor(Split_Species$X2.Ethylhexanol$Plot)

hurP1 <- glmmadmb(Gas_Concentration_PPM ~ EhPhase * Days + (1 | Plot), 
                  data = Split_Species$X2.Ethylhexanol, family = "binomial")


##greater in aerobic#################################################################Hexylamine###############################

Split_Species<-split(long.UCSC.eh,f=long.UCSC.eh$Gas_Species)

str(Split_Species$Hexylamine)

Split_Species$Hexylamine$Days = as.numeric(Split_Species$Hexylamine$Days)
Split_Species$Hexylamine$Gas_Concentration_PPM = as.numeric(Split_Species$Hexylamine$Gas_Concentration_PPM)


Split_Species$Hexylamine <- Split_Species$Hexylamine %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Hexylamine)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Hexylamine <- Split_Species$Hexylamine[complete.cases(Split_Species$Hexylamine[ ,8]),]
b=ggplot(Split_Species$Hexylamine, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Hexylamine)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$Hexylamine$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Hexylamine)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Hexylamine, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase*Site*Days + (1|Plot), data=Split_Species$Hexylamine)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Hexylamine) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Hexylamine, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Hexylamine, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB)
Split_Species$Hexylamine$Days = as.factor(Split_Species$Hexylamine$Days)
Split_Species$Hexylamine$EhPhase = as.factor(Split_Species$Hexylamine$EhPhase)
Split_Species$Hexylamine$Plot = as.factor(Split_Species$Hexylamine$Plot)

hurP1 <- glmmadmb(Gas_Concentration_PPM ~ EhPhase * Days + (1 | Plot), 
                  data = Split_Species$Hexylamine, family = "binomial")

###################################################################Pentane#################################
Split_Species<-split(long.UCSC.eh,f=long.UCSC.eh$Gas_Species)

str(Split_Species$Pentane)

Split_Species$Pentane$Days = as.numeric(Split_Species$Pentane$Days)
Split_Species$Pentane$Gas_Concentration_PPM = as.numeric(Split_Species$Pentane$Gas_Concentration_PPM)


Split_Species$Pentane <- Split_Species$Pentane %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=EhPhase), data = Split_Species$Pentane)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()

#exclude all rows with NAs
Split_Species$Pentane <- Split_Species$Pentane[complete.cases(Split_Species$Pentane[ ,8]),]
b=ggplot(Split_Species$Pentane, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Pentane)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$Pentane$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Pentane)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Pentane, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.


### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Pentane) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Pentane, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Pentane, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB)
Split_Species$Pentane$Days = as.factor(Split_Species$Pentane$Days)
Split_Species$Pentane$EhPhase = as.factor(Split_Species$Pentane$EhPhase)
Split_Species$Pentane$Plot = as.factor(Split_Species$Pentane$Plot)

hurP1 <- glmmadmb(Gas_Concentration_PPM ~ EhPhase * Days + (1 | Plot), 
                  data = Split_Species$Pentane, family = "binomial")
##NEED POST-HOC##################################################################Dimethyl.sulfide##################################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Dimethyl.sulfide)

Split_Species$Dimethyl.sulfide$Days = as.numeric(Split_Species$Dimethyl.sulfide$Days)
Split_Species$Dimethyl.sulfide$Gas_Concentration_PPM = as.numeric(Split_Species$Dimethyl.sulfide$Gas_Concentration_PPM)


Split_Species$Dimethyl.sulfide <- Split_Species$Dimethyl.sulfide %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Dimethyl.sulfide)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Dimethyl.sulfide <- Split_Species$Dimethyl.sulfide[complete.cases(Split_Species$Dimethyl.sulfide[ ,8]),]
b=ggplot(Split_Species$Dimethyl.sulfide, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Dimethyl.sulfide)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$Dimethyl.sulfide$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Dimethyl.sulfide)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$Dimethyl.sulfide, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase*Days + (1|Plot), data=Split_Species$Dimethyl.disulfide)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Dimethyl.sulfide) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Dimethyl.sulfide, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Dimethyl.sulfide, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB)
Split_Species$Dimethyl.sulfide$Days = as.factor(Split_Species$Dimethyl.sulfide$Days)
Split_Species$Dimethyl.sulfide$EhPhase = as.factor(Split_Species$Dimethyl.sulfide$EhPhase)
Split_Species$Dimethyl.sulfide$Plot = as.factor(Split_Species$Dimethyl.sulfide$Plot)

hurP1 <- glmmadmb(Gas_Concentration_PPM ~ EhPhase * Days + (1 | Plot), 
                  data = Split_Species$Dimethyl.sulfide, family = "binomial")
###Need post hoc seems greater in anaerobic####################################################################Dimethyl.disulfide##################################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)
str(Split_Species$Dimethyl.disulfide)

Split_Species$Dimethyl.disulfide$Days = as.numeric(Split_Species$Dimethyl.disulfide$Days)
Split_Species$Dimethyl.disulfide$Gas_Concentration_PPM = as.numeric(Split_Species$Dimethyl.disulfide$Gas_Concentration_PPM)


Split_Species$Dimethyl.disulfide <- Split_Species$Dimethyl.disulfide %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Dimethyl.disulfide)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Dimethyl.disulfide <- Split_Species$Dimethyl.disulfide[complete.cases(Split_Species$Dimethyl.disulfide[ ,8]),]
b=ggplot(Split_Species$Dimethyl.disulfide, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Dimethyl.disulfide)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$Dimethyl.disulfide$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Dimethyl.disulfide)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Dimethyl.disulfide, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.

library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase*Days + (1|Plot), data=Split_Species$Dimethyl.disulfide)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$Dimethyl.disulfide) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$Dimethyl.disulfide, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$Dimethyl.disulfide, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
########################################################################X1.2.3.Trimethylbenzene#####################
Split_Species<-split(long.UCSC.eh,f=long.UCSC.eh$Gas_Species)
str(Split_Species$X1.2.3.Trimethylbenzene)

Split_Species$X1.2.3.Trimethylbenzene$Days = as.numeric(Split_Species$X1.2.3.Trimethylbenzene$Days)
Split_Species$X1.2.3.Trimethylbenzene$Gas_Concentration_PPM = as.numeric(Split_Species$X1.2.3.Trimethylbenzene$Gas_Concentration_PPM)


Split_Species$X1.2.3.Trimethylbenzene <- Split_Species$X1.2.3.Trimethylbenzene %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X1.2.3.Trimethylbenzene)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$X1.2.3.Trimethylbenzene <- Split_Species$X1.2.3.Trimethylbenzene[complete.cases(Split_Species$X1.2.3.Trimethylbenzene[ ,8]),]
b=ggplot(Split_Species$X1.2.3.Trimethylbenzene, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X1.2.3.Trimethylbenzene)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$X1.2.3.Trimethylbenzene$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X1.2.3.Trimethylbenzene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$X1.2.3.Trimethylbenzene, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase*Site*Days + (1|Plot), data=Split_Species$X1.2.3.Trimethylbenzene)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

m1 = lm(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$X1.2.3.Trimethylbenzene)
summary(m1)
plot(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$X1.2.3.Trimethylbenzene)
### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase*Days, data = Split_Species$X1.2.3.Trimethylbenzene) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X1.2.3.Trimethylbenzene, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Days, random = ~1|Plot, data = Split_Species$X1.2.3.Trimethylbenzene, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))


##NEED POST-HOC########################################################X2.3.Heptanedione#####################

Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$X2.3.Heptanedione)

Split_Species$X2.3.Heptanedione$Days = as.numeric(Split_Species$X2.3.Heptanedione$Days)
Split_Species$X2.3.Heptanedione$Gas_Concentration_PPM = as.numeric(Split_Species$X2.3.Heptanedione$Gas_Concentration_PPM)


Split_Species$X2.3.Heptanedione <- Split_Species$X2.3.Heptanedione %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X2.3.Heptanedione)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$X2.3.Heptanedione <- Split_Species$X2.3.Heptanedione[complete.cases(Split_Species$X2.3.Heptanedione[ ,8]),]
b=ggplot(Split_Species$X2.3.Heptanedione, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X2.3.Heptanedione)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$X2.3.Heptanedione$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X2.3.Heptanedione)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$X2.3.Heptanedione, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site*Days + (1|Plot), data=Split_Species$X2.3.Heptanedione)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

################################################################### hexanoic acid ##############################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Hexanoic.acid)

Split_Species$Hexanoic.acid$Days = as.numeric(Split_Species$Hexanoic.acid$Days)
Split_Species$Hexanoic.acid$Gas_Concentration_PPM = as.numeric(Split_Species$Hexanoic.acid$Gas_Concentration_PPM)


Split_Species$Hexanoic.acid <- Split_Species$Hexanoic.acid %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Hexanoic.acid)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Hexanoic.acid <- Split_Species$Hexanoic.acid[complete.cases(Split_Species$Hexanoic.acid[ ,8]),]
b=ggplot(Split_Species$Hexanoic.acid, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Hexanoic.acid)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$Hexanoic.acid$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Hexanoic.acid)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$Hexanoic.acid, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$Hexanoic.acid)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

################################################# Tetrahydrothiopene #################################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Tetrahydrothiopene)

Split_Species$Tetrahydrothiopene$Days = as.numeric(Split_Species$Tetrahydrothiopene$Days)
Split_Species$Tetrahydrothiopene$Gas_Concentration_PPM = as.numeric(Split_Species$Tetrahydrothiopene$Gas_Concentration_PPM)


Split_Species$Tetrahydrothiopene <- Split_Species$Tetrahydrothiopene %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Tetrahydrothiopene)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Tetrahydrothiopene <- Split_Species$Tetrahydrothiopene[complete.cases(Split_Species$Tetrahydrothiopene[ ,8]),]
b=ggplot(Split_Species$Tetrahydrothiopene, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Tetrahydrothiopene)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$Tetrahydrothiopene$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Tetrahydrothiopene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$Tetrahydrothiopene, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase*Site*Days + (1|Plot), data=Split_Species$Tetrahydrothiopene)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

Split_Species<-split(long.eh,f=long.eh$Gas_Species)
m1 = lm(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Tetrahydrothiopene)
summary(m1)
plot(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Tetrahydrothiopene)
#only one measured at UCSC

Split_Species<-split(long.eh,f=long.eh$Gas_Species)
m1 = lm((Gas_Concentration_PPM) ~ Days.dec, data=Split_Species$Tetrahydrothiopene)
summary(m1)
plot((Gas_Concentration_PPM) ~ Days.dec, data=Split_Species$Tetrahydrothiopene)

######################################################## 1-Methylimidazol ##########################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$X1.Methylimidazol)

Split_Species$X1.Methylimidazol$Days = as.numeric(Split_Species$X1.Methylimidazol$Days)
Split_Species$X1.Methylimidazol$Gas_Concentration_PPM = as.numeric(Split_Species$X1.Methylimidazol$Gas_Concentration_PPM)


Split_Species$X1.Methylimidazol <- Split_Species$X1.Methylimidazol %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X1.Methylimidazol)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$X1.Methylimidazol <- Split_Species$X1.Methylimidazol[complete.cases(Split_Species$X1.Methylimidazol[ ,8]),]
b=ggplot(Split_Species$X1.Methylimidazol, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X1.Methylimidazol)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$X1.Methylimidazol$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X1.Methylimidazol)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$X1.Methylimidazol, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$X1.Methylimidazol)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

################################################################ Carbon Monoxide ##########################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$Carbon.monoxide)

Split_Species$Carbon.monoxide$Days = as.numeric(Split_Species$Carbon.monoxide$Days)
Split_Species$Carbon.monoxide$Gas_Concentration_PPM = as.numeric(Split_Species$Carbon.monoxide$Gas_Concentration_PPM)


Split_Species$Carbon.monoxide <- Split_Species$Carbon.monoxide %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$Carbon.monoxide)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$Carbon.monoxide <- Split_Species$Carbon.monoxide[complete.cases(Split_Species$Carbon.monoxide[ ,8]),]
b=ggplot(Split_Species$Carbon.monoxide, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$Carbon.monoxide)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$Carbon.monoxide$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$Carbon.monoxide)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$Carbon.monoxide, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$Carbon.monoxide)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)
########################################################################## 2,3-Dimethylpyrazine ###############################
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$X2.3.Dimethylpyrazine)

Split_Species$X2.3.Dimethylpyrazine$Days = as.numeric(Split_Species$X2.3.Dimethylpyrazine$Days)
Split_Species$X2.3.Dimethylpyrazine$Gas_Concentration_PPM = as.numeric(Split_Species$X2.3.Dimethylpyrazine$Gas_Concentration_PPM)


Split_Species$X2.3.Dimethylpyrazine <- Split_Species$X2.3.Dimethylpyrazine %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X2.3.Dimethylpyrazine)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$X2.3.Dimethylpyrazine <- Split_Species$X2.3.Dimethylpyrazine[complete.cases(Split_Species$X2.3.Dimethylpyrazine[ ,8]),]
b=ggplot(Split_Species$X2.3.Dimethylpyrazine, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X2.3.Dimethylpyrazine)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$X2.3.Dimethylpyrazine$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X2.3.Dimethylpyrazine)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$X2.3.Dimethylpyrazine, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase+Site+Days + (1|Plot), data=Split_Species$X2.3.Dimethylpyrazine)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

Anova(r1, type="III")
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lm1.1=lm(Gas_Concentration_PPM ~ Site*Days*EhPhase, random = ~1|Plot, data = Split_Species$X2.3.Dimethylpyrazine)
summary(lm1.1)

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase+Site*Days, data = Split_Species$X2.3.Dimethylpyrazine) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase+Site*Days, random = ~1|Plot, data = Split_Species$X2.3.Dimethylpyrazine, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Site+Days, random = ~1|Plot, data = Split_Species$X2.3.Dimethylpyrazine, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
shapiro.test(r1$residuals)
########################################################################### 2-methylpyrazine ##############
Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$X2.Methylpyrazine)

Split_Species$X2.Methylpyrazine$Days = as.numeric(Split_Species$X2.Methylpyrazine$Days)
Split_Species$X2.Methylpyrazine$Gas_Concentration_PPM = as.numeric(Split_Species$X2.Methylpyrazine$Gas_Concentration_PPM)


Split_Species$X2.Methylpyrazine <- Split_Species$X2.Methylpyrazine %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X2.Methylpyrazine)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$X2.Methylpyrazine <- Split_Species$X2.Methylpyrazine[complete.cases(Split_Species$X2.Methylpyrazine[ ,8]),]
b=ggplot(Split_Species$X2.Methylpyrazine, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X2.Methylpyrazine)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$X2.Methylpyrazine$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ Site*EhPhase, data = Split_Species$X2.Methylpyrazine)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$X2.Methylpyrazine, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase*Days + (1|Plot), data=Split_Species$X2.Methylpyrazine)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

Anova(r1, type="III")
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lm1.1=lm(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X2.Methylpyrazine)
summary(lm1.1)

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase+Site*Days, data = Split_Species$X2.Methylpyrazine) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase+Site*Days, random = ~1|Plot, data = Split_Species$X2.Methylpyrazine, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Site+Days, random = ~1|Plot, data = Split_Species$X2.Methylpyrazine, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
shapiro.test(r1$residuals)
################################################## 1-butanethiol #########################

Split_Species<-split(long.eh,f=long.eh$Gas_Species)

str(Split_Species$X1.Butanethiol)

Split_Species$X1.Butanethiol$Days = as.numeric(Split_Species$X1.Butanethiol$Days)
Split_Species$X1.Butanethiol$Gas_Concentration_PPM = as.numeric(Split_Species$X1.Butanethiol$Gas_Concentration_PPM)


Split_Species$X1.Butanethiol <- Split_Species$X1.Butanethiol %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Gas_Concentration_PPM, x = EhPhase,fill=Site), data = Split_Species$X1.Butanethiol)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="EhPhase",y="Gas Concentration (PPM)")+scale_y_sqrt()
a
#exclude all rows with NAs
Split_Species$X1.Butanethiol <- Split_Species$X1.Butanethiol[complete.cases(Split_Species$X1.Butanethiol[ ,8]),]
b=ggplot(Split_Species$X1.Butanethiol, aes(x=as.factor(Days), y=Gas_Concentration_PPM, fill=EhPhase)) + geom_boxplot()+theme_classic()+labs(x="Day of Pot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Gas_Concentration_PPM, x = Days, color=EhPhase), data = Split_Species$X1.Butanethiol)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Pot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)
hist((Split_Species$X1.Butanethiol$Gas_Concentration_PPM))

m1 = aov(Gas_Concentration_PPM ~ EhPhase, data = Split_Species$X1.Butanethiol)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test EhPhase where the gas was detected


r1 = lme(sqrt(Gas_Concentration_PPM) ~ EhPhase*Site*Days, random = ~1|Plot, data = Split_Species$X1.Butanethiol, method = "REML") 
anova(r1)
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
#library(lme4)
lmeModel = lmer(Gas_Concentration_PPM ~ EhPhase*Days + (1|Plot), data=Split_Species$X1.Butanethiol)
anova(lmeModel)
Anova(lmeModel, test="F")
qqnorm(residuals(lmeModel))
plot(lmeModel)

Anova(r1, type="III")
#the fixed effects are nutrient EhPhase (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.
lm1.1=lm(Gas_Concentration_PPM ~ EhPhase*Days, random = ~1|Plot, data = Split_Species$X1.Butanethiol)
summary(lm1.1)

### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Gas_Concentration_PPM ~ EhPhase+Site*Days, data = Split_Species$X1.Butanethiol) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Gas_Concentration_PPM ~ EhPhase+Site*Days, random = ~1|Plot, data = Split_Species$X1.Butanethiol, method = "ML") #full model using ML
r2 = lme(Gas_Concentration_PPM ~ EhPhase+Site+Days, random = ~1|Plot, data = Split_Species$X1.Butanethiol, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))
shapiro.test(r1$residuals)
###########################################Stats for comparing different timeframes#######################################################################################################
all_csv_PSI19_v2 = all_csv_PSI19[, intersect(colnames(all_csv_UCSC), colnames(all_csv_PSI19))]
all_csv_UCSC_v2 = all_csv_UCSC[, intersect(colnames(all_csv_PSI19), colnames(all_csv_UCSC))]

PSI19.lm <- all_csv_PSI19_v2 %>% select(c(Days.dec, Site, X1.Heptene, Hexene, Propylamine))
PSI19.lm$Days.dec <- as.numeric(PSI19.lm$Days.dec)

long.PSI19.lm <- PSI19.lm %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Days.dec, Site))


UCSC.lm <- all_csv_UCSC_v2 %>% select(c(Days.dec, Site, X1.Heptene, Hexene, Propylamine))
UCSC.lm$Days.dec <- as.numeric(UCSC.lm$Days.dec)

long.UCSC.lm <- UCSC.lm %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Days.dec, Site))



######################### Linear Regression ###################################

#Hexene
Split_Species<-split(long.PSI19.lm,f=long.PSI19.lm$Gas_Species)
m1 = lm(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Hexene)
summary(m1)
plot(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Hexene)
#only one measurement at PSI

Split_Species<-split(long.UCSC.lm,f=long.UCSC.lm$Gas_Species)
m1 = lm(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Hexene)
summary(m1)
plot(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Hexene)

#1-Heptene

Split_Species<-split(long.PSI19.lm,f=long.PSI19.lm$Gas_Species)
m1 = lm(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$X1.Heptene)
summary(m1)
plot(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$X1.Heptene)
#only one measurement at PSI

Split_Species<-split(long.UCSC.lm,f=long.UCSC.lm$Gas_Species)
m1 = lm((Gas_Concentration_PPM) ~ Days.dec, data=Split_Species$X1.Heptene)
summary(m1)
plot((Gas_Concentration_PPM) ~ Days.dec, data=Split_Species$X1.Heptene)

#Propylamine

Split_Species<-split(long.PSI19.lm,f=long.PSI19.lm$Gas_Species)
m1 = lm(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Propylamine)
summary(m1)
plot(Gas_Concentration_PPM ~ Days.dec, data=Split_Species$Propylamine)
#only one measurement at PSI

Split_Species<-split(long.UCSC.lm,f=long.UCSC.lm$Gas_Species)
m1 = lm((Gas_Concentration_PPM) ~ Days.dec, data=Split_Species$Propylamine)
summary(m1)
plot((Gas_Concentration_PPM) ~ Days.dec, data=Split_Species$Propylamine)


###################################################### End Linear Regression ########################
all_csv=rbind(all_csv_PSI19_v2, all_csv_UCSC_v2)
install.packages("imputeTS")
library(imputeTS)

unique(all_csv$Days)
colnames(all_csv)
unique(all_csv$Treatment)

all_csv <- all_csv %>% select(c(Site, Timeframe, Carbon.dioxide,Nitrous.oxide,Methane,                  
                                  X1.Pentene, Butylamine..1.butanamine., Propylene.oxide,           
                                  X2.3.Heptanedione, X1.Butene, X1.Heptene,               
                                  Hexene, Propylamine, X2.3.Dimethylpyrazine,    
                                  X1.Butanethiol))

all_csv[all_csv < 1] <- NA
all_csv <- na_replace(all_csv, 0)

mean = all_csv %>%
  group_by(Site, Timeframe) %>%
  summarise_at(.vars = vars(Carbon.dioxide,Nitrous.oxide,Methane,                  
                            X1.Pentene, Butylamine..1.butanamine., Propylene.oxide,           
                            X2.3.Heptanedione, X1.Butene, X1.Heptene,               
                            Hexene, Propylamine, X2.3.Dimethylpyrazine,    
                            X1.Butanethiol),
               .funs = c(mean="mean"))

long <- mean %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Site,
                                                                  Timeframe))
long$Site <- as.factor(long$Site)
long$Timeframe <- as.factor(long$Timeframe)

gas<-unique(long$Gas_Species)
gas<-sort(gas)
gas
voc_split <- split(long, long$Gas_Species)
voc_split
new_names <- c("Butylamine..1.butanamine.", "Carbon.dioxide", "Hexene",                   
              "Methane", "Nitrous.oxide", "Propylamine",              
               "Propylene.oxide", "X1.Butanethiol", "X1.Butene",                
               "X1.Heptene", "X1.Pentene", "X2.3.Dimethylpyrazine",    
                "X2.3.Heptanedione")
for (i in 1:length(voc_split)) {
  assign(new_names[i], voc_split[[i]])
}


############################################ need nonparametric, Friedman ##################################################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$Butylamine..1.butanamine.)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$Butylamine..1.butanamine.)

m1 = aov(Gas_Concentration_PPM ~ Site*Timeframe, data = Split_Species$Butylamine..1.butanamine.)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= Butylamine..1.butanamine.)

#Friedman rank sum test
pairwise.wilcox.test(Butylamine..1.butanamine.$Gas_Concentration_PPM, g = Butylamine..1.butanamine.$Site)


############################################ need nonparametric, friedman ##################################################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$Carbon.dioxide)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$Carbon.dioxide)

m1 = aov(Gas_Concentration_PPM ~ Site*Timeframe, data = Split_Species$Carbon.dioxide)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= Carbon.dioxide)

#Friedman rank sum test
pairwise.wilcox.test(Carbon.dioxide$Gas_Concentration_PPM, g = Carbon.dioxide$Site)


########################################### need nonparametric ######################################################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$Methane)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$Methane)

m1 = aov(Gas_Concentration_PPM ~ Site*Timeframe, data = Split_Species$Methane)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= Methane)

#Friedman rank sum test
pairwise.wilcox.test(Methane$Gas_Concentration_PPM, g = Methane$Site)


########################################## need nonparametric ###############################################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$Nitrous.oxide)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$Nitrous.oxide)

m1 = aov(Gas_Concentration_PPM ~ Site*Timeframe, data = Split_Species$Nitrous.oxide)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= Nitrous.oxide)

#Friedman rank sum test
pairwise.wilcox.test(Nitrous.oxide$Gas_Concentration_PPM, g = Nitrous.oxide$Site)

############################################### parametric good #####################################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$X1.Pentene)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$X1.Pentene)

m1 = aov(Gas_Concentration_PPM ~ Site*Timeframe, data = Split_Species$X1.Pentene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

##################################### need nonparametric #############################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$Propylene.oxide)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$Propylene.oxide)

m1 = aov(Gas_Concentration_PPM ~ Site*Timeframe, data = Split_Species$Propylene.oxide)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= Propylene.oxide)

#Friedman rank sum test
pairwise.wilcox.test(Propylene.oxide$Gas_Concentration_PPM, g = Propylene.oxide$Site)

########################################### need non parametric ##########################################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$X1.Butene)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$X1.Butene)

m1 = aov(Gas_Concentration_PPM ~ Site*Timeframe, data = Split_Species$X1.Butene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
leveneTest(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= X1.Butene)

#Friedman rank sum test
pairwise.wilcox.test(X1.Butene$Gas_Concentration_PPM, g = X1.Butene$Site)
########################################## parametric ##################################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$X1.Heptene)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$X1.Heptene)

m1 = aov(Gas_Concentration_PPM ~ Site+Timeframe, data = Split_Species$X1.Heptene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= X1.Heptene)

#Friedman rank sum test
pairwise.wilcox.test(X1.Heptene$Gas_Concentration_PPM, g = X1.Heptene$Site)
######################################### parametric ##############################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$Hexene)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$Hexene)

m1 = aov(Gas_Concentration_PPM ~ Site+Timeframe, data = Split_Species$Hexene)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= Hexene)

#Friedman rank sum test
pairwise.wilcox.test(Hexene$Gas_Concentration_PPM, g = Hexene$Site)
######################################## parametric ####################################### 
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$Propylamine)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$Propylamine)

m1 = aov(Gas_Concentration_PPM ~ Site+Timeframe, data = Split_Species$Propylamine)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= Propylamine)

#Friedman rank sum test
pairwise.wilcox.test(Propylamine$Gas_Concentration_PPM, g = Propylamine$Site)

####################################### parametric? #################################################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$X2.3.Dimethylpyrazine)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$X2.3.Dimethylpyrazine)

m1 = aov(Gas_Concentration_PPM ~ Site+Timeframe, data = Split_Species$X2.3.Dimethylpyrazine)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= X2.3.Dimethylpyrazine)

#Friedman rank sum test
pairwise.wilcox.test(X2.3.Dimethylpyrazine$Gas_Concentration_PPM, g = X2.3.Dimethylpyrazine$Site)

################################################# need nonparametric??? ###################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$X1.Butanethiol)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$X1.Butanethiol)

m1 = aov(Gas_Concentration_PPM ~ Site*Timeframe, data = Split_Species$X1.Butanethiol)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= X1.Butanethiol)

#Friedman rank sum test
pairwise.wilcox.test(X1.Butanethiol$Gas_Concentration_PPM, g = X1.Butanethiol$Site)

################################################# need nonparametric??? ###################
bartlett.test(Gas_Concentration_PPM ~ Site, data=Split_Species$X2.3.Heptanedione)
leveneTest(Gas_Concentration_PPM ~ Site, data=Split_Species$X2.3.Heptanedione)

m1 = aov(Gas_Concentration_PPM ~ Site+Timeframe, data = Split_Species$X2.3.Heptanedione)
summary(m1)
TukeyHSD(m1)
shapiro.test(m1$residuals)
plot(m1$residuals)
library(ggpubr)
ggqqplot(m1$residuals)

friedman.test(Gas_Concentration_PPM ~ Site | Timeframe, data= X2.3.Heptanedione)

#Friedman rank sum test
pairwise.wilcox.test(X2.3.Heptanedione$Gas_Concentration_PPM, g = X2.3.Heptanedione$Site)


