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
library(data.table)
library("lubridate")
####################################################################################UCSC

setwd("G:\\Shared drives\\USDA_SCRI\\UCSC_field_trial_results\\Calcmet_processed_data\\UCSC_duringASD\\VOC_processed_with_KPlibraries\\R_Processing_Folder\\outputfiles")
all_csv = read.csv("all_csv_temp.csv")

#strip the minutes and seconds off to get the start hour, keep the date or 
#you won't be able it easily link the temperature data to the start time for each sample period
all_csv$Date = as.Date(all_csv$Date, format="%m/%d/%Y")
all_csv$DateTime <- as.POSIXct(paste(all_csv$Date, all_csv$Time), format="%Y-%m-%d %H:%M")
all_csv$HourTime <- format(strptime(all_csv$DateTime,"%Y-%m-%d %H:%M"),'%Y-%m-%d %H')

## Use round.Date to round, then format to format
all_csv$HourTime <- format(round(all_csv$HourTime, units="hours"), format="%m/%d/%y %H")

all_csv$Treatment <- ifelse(all_csv$Plot == "1A01", 'ASD',
                            ifelse(all_csv$Plot == "1A27", 'ASD',
                                   ifelse(all_csv$Plot == "1A35", 'ASD',
                                          ifelse(all_csv$Plot == "1A60", 'ASD','UTC')))) #I changed "<=" to "==", so I could run the ifelse as it declared "<=" was not a valid operator.


#Output again just in case it gets messed up and you need to reupload
output_path = (paste("..\\outputfiles\\", sep = ""))
write.csv(all_csv,
          paste( output_path, "all_csv_final", ".csv", sep="" ))

#format Datetime as Time m/d/y h:m in 24 hour format
all_csv <- all_csv %>% select(-c("X"))
all_csv <- all_csv %>% select(-c("X.1"))
eh = read.csv("G:\\Shared drives\\USDA_SCRI\\Meta\\UCSC_field_metadata\\ASD_metadata\\SoilEh\\UCSC_SoilEh.csv")

eh$Date = as.Date(eh$Date, format="%Y-%m-%d")
eh$DateTime <- as.POSIXct(paste(eh$Date, eh$Time), format="%Y-%m-%d %H:%M")
eh$HourTime <- format(strptime(eh$DateTime,"%Y-%m-%d %H:%M"),'%Y-%m-%d %H')
#merge by hourtime and plot

all_eh_csv = merge(all_csv, eh, by=c("HourTime","Plot"), all=FALSE)
all_eh_csv <- subset(all_eh_csv, select = -c(Water.vapor, Carbon.dioxide, Methane,Date.x, Time.x,Date.y, Time.y,LibraryFile,DateTime.y,TreatmentCode,Treatment,StartTime))
#all_eh_csv.T <- subset(all_eh_csv, select = -c(Plot))
#all_eh_csv.P <- subset(all_eh_csv, select = -c(Treatment))
long.eh.voc <- all_eh_csv %>% gather(Gas_Species, Gas_Concentration_PPM, -c(DateTime.x, HourTime, Plot, Depth, SoilEh))
library(data.table)
long.eh.voc$Gas_Concentration_PPM[is.na(long.eh.voc$Gas_Concentration_PPM)] <- 0
eh.ppm1 <- setDT(long.eh.voc)[!(Gas_Concentration_PPM %between% c(0.0000001, 1))]
eh.ppm1 <- subset(long.eh.voc, Gas_Concentration_PPM > 0)


max.all.data.UCSC = all_eh_csv %>%
  group_by(DateTime.x, HourTime,Plot,Depth) %>%
  summarise_at(.vars = vars(Carbon.monoxide,Nitrous.oxide,
                            Ammonia,Chloroform
                            ,X1.Pentene,Butylamine..1.butanamine.,
                            Ethanolamine,Propylene.oxide,
                            X2.3.Heptanedione,Ethanol,
                            Heptane,Isobutanol
                            ,X1.Pentanol,X1.Butene,
                            t.Butanol,X1.Hexanol,
                            X1.Heptene,Dodecane,
                            Isopentyl.acetate,X1.Methylimidazol,
                            X2.3.Hexanedione,X2.Ethylhexanol,
                            Hexene,Tetrahydrothiopene,
                            Hexane,Hexanoic.acid,
                            Undecane,Isopentane,
                            Nonane,Hexylamine,
                            Pentane,Isobutene..2.methylpropene.,
                            X3.Chloro.2.methyl.1.propene,Isohexane,
                            Dimethyl.sulfide,Propylamine,
                            Decane,Dimethyl.disulfide,
                            Cyclopentanone,Cyclohexane,
                            X1.3.5.Trimethylbenzene,Nitrogen.dioxide,
                            Ethyl.fluoride,X4.Methyl.3.penten.2.one,
                            Eucalyptol,X1.2.3.Trimethylbenzene,
                            Propene,Methyl.chloride,
                            X2.3.Dimethylpyrazine,Isobutyl.formate,
                            Trans.1.2.Dichloroethene,Isopropanol,
                            X1.Propanol,Allylcyanide..3.butenenitrile.,
                            Methyl.isocyanate,Propyl.acetate,
                            X1.Butanethiol,X1.2.4.Trimethylbenzene,
                            X1.Butanol,X3.Ethyltoluene,
                            SoilEh),
               .funs = c(max="max"))
max.all.data.UCSC$Depth = as.numeric(max.all.data.UCSC$Depth)
max.6 <- subset(max.all.data.UCSC, Depth < 12)
max.12 <- subset(max.all.data.UCSC, Depth = 12)
max.18 <- subset(max.all.data.UCSC, Depth > 12)

long.max6.voc <- max.6 %>% gather(Gas_Species, Gas_Concentration_PPM, -c(DateTime.x, HourTime, Plot, Depth, SoilEh_max))
long.max6.voc <- subset(long.max6.voc, Gas_Concentration_PPM > 0)
gas<-unique(long.max6.voc$Gas_Species)
sort(gas)



voc_split <- split(long.max6.voc, long.max6.voc$Gas_Species)
voc_split
new_names <- c("Allylcyanide..3.butenenitrile._max", "Butylamine..1.butanamine._max",      "Carbon.monoxide_max",               
               "Cyclohexane_max",                    "Dimethyl.disulfide_max",             "Dimethyl.sulfide_max",              
               "Dodecane_max",                       "Ethanol_max",                        "Ethyl.fluoride_max",                
               "Heptane_max",                        "Hexane_max",                         "Hexanoic.acid_max",                 
               "Hexene_max",                         "Hexylamine_max",                     "Isobutanol_max",                    
               "Isohexane_max",                      "Isopentane_max",                     "Methyl.chloride_max",               
               "Nitrogen.dioxide_max",               "Nitrous.oxide_max",                  "Nonane_max",                        
               "Pentane_max",                        "Propene_max",                        "Propylamine_max",                   
               "Propylene.oxide_max",                "t.Butanol_max",                      "Tetrahydrothiopene_max",            
               "Undecane_max",                       "X1.2.3.Trimethylbenzene_max",        "X1.3.5.Trimethylbenzene_max",       
               "X1.Butene_max",                      "X1.Heptene_max",                     "X1.Hexanol_max",                    
               "X1.Methylimidazol_max",              "X1.Pentanol_max",                    "X1.Pentene_max",                    
               "X2.3.Dimethylpyrazine_max",          "X2.3.Heptanedione_max",              "X2.3.Hexanedione_max",              
               "X2.Ethylhexanol_max",                "X3.Chloro.2.methyl.1.propene_max",   "X4.Methyl.3.penten.2.one_max") 
for (i in 1:length(voc_split)) {
  assign(new_names[i], voc_split[[i]])
}


######################################################################################
#"Allylcyanide..3.butenenitrile._max" 
#"Butylamine..1.butanamine._max"      
#"Carbon.monoxide_max"               
#"Cyclohexane_max"                   
#"Dimethyl.disulfide_max"  = "#CC9933", #gold             
#"Dimethyl.sulfide_max" = "#0000FF", #brightblue             
#"Dodecane_max"                      
#"Ethanol_max"                        
#"Ethyl.fluoride_max"                
#"Heptane_max"                       
#"Hexane_max"                         
#"Hexanoic.acid_max"                 
#"Hexene_max"  = "#CC99FF", #lavender                         
#"Hexylamine_max"                     
#"Isobutanol_max"                    
#"Isohexane_max"                      
#"Isopentane_max"                     
#"Methyl.chloride_max"               
#"Nitrogen.dioxide_max"               
#"Nitrous.oxide_max" = "#FF9900", #orange                 
#"Nonane_max"                        
#"Pentane_max"                        
#"Propene_max"                        
#"Propylamine_max"                   
#"Propylene.oxide_max" = "#56B4E9", #light blue               
#"t.Butanol_max" = "#FF3333", #brightred                      
#"Tetrahydrothiopene_max"            
#"Undecane_max"                       
#"X1.2.3.Trimethylbenzene_max" = "#F0E442",  #yellow       
#"X1.3.5.Trimethylbenzene_max" = "#9933CC", #purple     
#"X1.Butene_max"  = "#009999",   #sage green                      
#"X1.Heptene_max"  = "#990066", #maroon                     
#"X1.Hexanol_max"  = "#0072B2", #dark blue                   
#"X1.Methylimidazol_max"              
#"X1.Pentanol_max"                    
#"X1.Pentene_max" = "#000000",  #black                    
#"X2.3.Dimethylpyrazine_max"  = "#006600", #forest green         
#"X2.3.Heptanedione_max" = "#FF3300", #neon orange              
#"X2.3.Hexanedione_max" = "#FFFF00", #neon yellow             
#"X2.Ethylhexanol_max"                
#"X3.Chloro.2.methyl.1.propene_max" = "#9900FF", #violet 
#"X4.Methyl.3.penten.2.one_max" 

library(RColorBrewer)
n <- 50
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

gas<-unique(long.max6.voc$Gas_Species)
sort(gas)

unique(long.max6.voc$Gas_Species)

date = unique(all_csv$Date)
sort(date)

long.max6.voc$HourTime = as.POSIXct(long.max6.voc$HourTime,"%Y-%m-%d %H")
Plot1A01<-long.max6.voc %>% filter(Plot == "1A01")
Plot1A27<-long.max6.voc %>% filter(Plot == "1A27")
Plot1A35<-long.max6.voc %>% filter(Plot == "1A35")
Plot1A60<-long.max6.voc %>% filter(Plot == "1A60")

########################################## VOC and Eh Graphs #####################


voc.ASD <- ggplot(data = long.max6.voc, aes(x=DateTime.x, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Gas_Species),              
             size=3) +
  #scale_color_brewer(palette = "Paired") +
  scale_colour_manual(values=col_vector)+
  scale_x_datetime(name = "Date", breaks = as.POSIXct(c("2018-08-29", "2018-08-31", 
                                                     "2018-09-01", "2018-09-05",
                                                     "2018-09-08", "2018-09-12",
                                                     "2018-09-17", "2018-09-19",
                                                     "2018-09-24", "2018-09-26",
                                                     "2018-10-02", "2018-10-07",
                                                     "2018-10-10"),format="%Y-%m-%d")) +
  scale_y_continuous(name = "Concentration (ppm)") +
  ylim(0,75) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("UCSC Max Concentration of VOC During ASD")
voc.ASD

voc.1A01 <- ggplot(data = Plot1A01, aes(x=DateTime.x, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Gas_Species),              
             size=3) +
  #scale_color_brewer(palette = "Paired") +
  scale_colour_manual(values=col_vector)+
  scale_x_datetime(name = "Date", breaks = as.POSIXct(c("2018-08-29", "2018-08-31", 
                                                        "2018-09-01", "2018-09-05",
                                                        "2018-09-08", "2018-09-12",
                                                        "2018-09-17", "2018-09-19",
                                                        "2018-09-24", "2018-09-26",
                                                        "2018-10-02", "2018-10-07",
                                                        "2018-10-10"),format="%Y-%m-%d")) +
  scale_y_continuous(name = "Concentration (ppm)") +
  ylim(0,75) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("UCSC-1A01 Max Concentration of VOC During ASD")
voc.1A01


voc.1A27 <- ggplot(data = Plot1A27, aes(x=DateTime.x, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Gas_Species),              
             size=3) +
  #scale_color_brewer(palette = "Paired") +
  scale_colour_manual(values=col_vector)+
  scale_x_datetime(name = "Date", breaks = as.POSIXct(c("2018-08-29", "2018-08-31", 
                                                        "2018-09-01", "2018-09-05",
                                                        "2018-09-08", "2018-09-12",
                                                        "2018-09-17", "2018-09-19",
                                                        "2018-09-24", "2018-09-26",
                                                        "2018-10-02", "2018-10-07",
                                                        "2018-10-10"),format="%Y-%m-%d")) +
  scale_y_continuous(name = "Concentration (ppm)") +
  ylim(0,75) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("UCSC-1A27 Max Concentration of VOC During ASD")
voc.1A27

voc.1A35 <- ggplot(data = Plot1A35, aes(x=DateTime.x, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Gas_Species),              
             size=3) +
  #scale_color_brewer(palette = "Paired") +
  scale_colour_manual(values=col_vector)+
  scale_x_datetime(name = "Date", breaks = as.POSIXct(c("2018-08-29", "2018-08-31", 
                                                        "2018-09-01", "2018-09-05",
                                                        "2018-09-08", "2018-09-12",
                                                        "2018-09-17", "2018-09-19",
                                                        "2018-09-24", "2018-09-26",
                                                        "2018-10-02", "2018-10-07",
                                                        "2018-10-10"),format="%Y-%m-%d")) +
  scale_y_continuous(name = "Concentration (ppm)") +
  ylim(0,75) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("UCSC-1A35 Max Concentration of VOC During ASD")
voc.1A35


voc.1A60 <- ggplot(data = Plot1A35, aes(x=DateTime.x, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Gas_Species),              
             size=3) +
  #scale_color_brewer(palette = "Paired") +
  scale_colour_manual(values=col_vector)+
  scale_x_datetime(name = "Date", breaks = as.POSIXct(c("2018-08-29", "2018-08-31", 
                                                        "2018-09-01", "2018-09-05",
                                                        "2018-09-08", "2018-09-12",
                                                        "2018-09-17", "2018-09-19",
                                                        "2018-09-24", "2018-09-26",
                                                        "2018-10-02", "2018-10-07",
                                                        "2018-10-10"),format="%Y-%m-%d")) +
  scale_y_continuous(name = "Concentration (ppm)") +
  ylim(0,75) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("UCSC-1A60 Max Concentration of VOC During ASD")
voc.1A60

eh.6 <- subset(eh, Depth < 12)
#eh.6$DateTime <-  as.POSIXct(eh.6$DateTime,"%m-%d-%Y %H:%M")

output_path = (paste("G:\\Shared drives\\USDA_SCRI\\Meta\\UCSC_field_metadata\\ASD_metadata\\SoilEh\\", sep = ""))
write.csv(eh.6,
          paste( output_path, "eh.6", ".csv", sep="" ))

library("lubridate")
eh.6 <- read.csv("G:\\Shared drives\\USDA_SCRI\\Meta\\UCSC_field_metadata\\ASD_metadata\\SoilEh\\eh.6.reup.csv")
eh.6$Date = as.Date(eh.6$Date, format="%m/%d/%Y")

eh.6$DateTime <- as.POSIXct(paste(eh.6$Date, eh.6$Time), format="%Y-%m-%d %H:%M")

duration(24, "hours")
#dhours(x = 1)
detach("package:MASS")
if(!require(dplyr)){install.packages("dplyr")}
day_eh = eh.6 %>%
  #  select(everything()) %>%
  arrange(eh.6$Plot, eh.6$DateTime) %>%
  group_by(Plot) %>%
  mutate(Days_diff = DateTime - min(DateTime)) %>%
  mutate(hour24 = Days_diff / dhours())




day_eh$Plot[day_eh$Plot=="1A01"] <- "R1"
day_eh$Plot[day_eh$Plot=="1A27"] <- "R2"
day_eh$Plot[day_eh$Plot=="1A35"] <- "R3"
day_eh$Plot[day_eh$Plot=="1A60"] <- "R4"
unique(day_eh$Plot)


Eh.UC <- ggplot(data = day_eh, aes(x=DayCount, y=SoilEh, group=Plot)) + 
  #geom_smooth(aes(color=Plot)) +
  geom_line(aes(color=Plot)) +
  theme_classic() +
  scale_x_continuous(name = "Day of Trial", 
                     breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45))) +
  ylab("Eh (mV)") +
  ylim(-400,400)+
  geom_hline(yintercept = 200, colour = "red")+
  theme(axis.title.y = element_text(color = "red", size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=10)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("A. fASD: Eh")+
  theme(plot.title = element_text(size = 12))
Eh.UC

aerobicR1 = subset(day_eh, Plot == "R1" & SoilEh >= 200)
aerobicR2 = subset(day_eh, Plot == "R2" & SoilEh >= 200)
aerobicR3 = subset(day_eh, Plot == "R3" & SoilEh >= 200)
aerobicR4 = subset(day_eh, Plot == "R4" & SoilEh >= 200)
anaerobicR1 = subset(day_eh, Plot == "R1" & SoilEh < 200)
anaerobicR2 = subset(day_eh, Plot == "R2" & SoilEh < 200)
anaerobicR3 = subset(day_eh, Plot == "R3" & SoilEh < 200)
anaerobicR4 = subset(day_eh, Plot == "R4" & SoilEh < 200)

InW=6.5; InH=9 #inches high=, inches wide=  this sets the width and height of the graph in inches. The image can still be adjusted and maintain proportions.
windows(wid=InW, hei=InH)  #this uses above to establish window size parameters and opens the external window to be used
par(mai=c(0.80, 0.85, 0.5, 0.15) , mgp = c(2.5,1,0)) #this sets the graphing window parameters. You can play around with the numbers to achieve the settings you want.
ggarrange(Eh.UC, Eh.19, Eh.21, ncol = 1, nrow = 3)
#calls plot to put in the external window
setwd("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\Figures\\")  #sets working directory for where plot will be saved
ggsave(paste("EhFigure.png", sep=''),  #save the plot and name it as you please
       width=16.51, height=22.86, units='cm')  #same width and height parameters previously established but in cm units. Don't ask me why, just do it. :laughing:


########################### UCSC Soil Temps ####################################

setwd("G:\\Shared drives\\USDA_SCRI\\Meta\\UCSC_field_metadata")
ucsc.temp = read.csv("UCSC_ASD_5TE_TempData_KP.csv")

ucsc.temp$Date = as.Date(ucsc.temp$Date, format = "%m/%d/%Y")
ucsc.temp$DateTime <- as.POSIXct(paste(ucsc.temp$Date, ucsc.temp$TempTime), format="%Y-%m-%d %H:%M")
ucsc.temp$HourTime <- strptime(ucsc.temp$DateTime, format="%Y-%m-%d %H")
#merge by hourtime and plot
#all_meta_csv = merge(all_csv, meta, by=c("HourTime","Plot","Treatment"), all=FALSE)

duration(24, "hours")
#dhours(x = 1)
detach("package:MASS")
if(!require(dplyr)){install.packages("dplyr")}
day_ucsc.temp = ucsc.temp %>%
  #  select(everything()) %>%
  arrange(ucsc.temp$Plot, ucsc.temp$DateTime) %>%
  group_by(Plot) %>%
  mutate(Days_diff = DateTime - min(DateTime)) %>%
  mutate(hour24 = Days_diff / dhours())

day_ucsc.temp$Days.dec = day_ucsc.temp$Days_diff / 86400
print(colnames(day_ucsc.temp)) 
tail(day_ucsc.temp$Days.dec)
tail(day_ucsc.temp$Days_diff)
day_ucsc.temp$Days = str_sub(day_ucsc.temp$Days.dec, 1, 1)




day_ucsc.temp$Treatment <- ifelse(day_ucsc.temp$Plot == "1A01", 'fASD',
                            ifelse(day_ucsc.temp$Plot == "1A27", 'fASD',
                                   ifelse(day_ucsc.temp$Plot == "1A35", 'fASD',
                                          ifelse(day_ucsc.temp$Plot == "1A60", 'fASD','MSM'))))

UCSC.temp <- ggplot(data = day_ucsc.temp, aes(x=Days.dec, y=Temp_8in, group=Plot)) + 
  #geom_smooth(aes(color=Plot)) +
  geom_line(aes(color=Plot)) +
  theme_classic() +
  scale_x_continuous(name = "Day of Trial", 
                     breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))) +
  ylab("Soil Temperature (C)") +
  #ylim(-400,400)+
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("UCSC-CfA Soil Temperature at 20cm")
UCSC.temp

u.cbPalette <- c("#E69F00", "#009E73")
UCSC.temp.20 <- ggplot(data = day_ucsc.temp, aes(x=Days.dec, y=Temp_8in, group=Treatment,color=Treatment)) + 
  geom_smooth(
    method = "auto",
    se = FALSE,
    fullrange=TRUE)  + 
  #geom_line(aes(color=Treatment)) +
  scale_color_manual(name = "Treatment",
                     breaks = c("fASD", "MSM"),
                     values = u.cbPalette)+
  theme_classic() +
  scale_x_continuous(name = "Day of Trial", 
                     breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))) +
  ylab("Soil Temperature (C)") +
  ylim(20,35)+
  theme(axis.title.y = element_text(color = "red", size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=10)) +
  #theme(legend.position="bottom") +
  #theme(legend.title = element_blank()) +
  ggtitle("A. fASD and MSM Soil Temperature at 20 cm")+
  theme(plot.title = element_text(size = 12))
UCSC.temp.20

MSM<-day_ucsc.temp %>% filter(Treatment == "MSM")
MSM$Temp_8in = as.numeric(MSM$Temp_8in)
MSM<-MSM[complete.cases(MSM$Temp_8in),]
mean(MSM$Temp_8in)
sd(MSM$Temp_8in)
hist(MSM$Temp_8in)
qqnorm(MSM$Temp_8in)

ASD<-day_ucsc.temp %>% filter(Treatment == "fASD")
ASD<-ASD[complete.cases(ASD$Temp_8in),]
mean(ASD$Temp_8in)
sd(ASD$Temp_8in)
hist(ASD$Temp_8in)
qqnorm(ASD$Temp_8in)

UCSC.t <- ASD %>% ungroup %>% select(Temp_8in)
MSM.t <- MSM %>% ungroup %>% select(Temp_8in)

t.test(MSM.t, UCSC.t, var.equal = TRUE)

UTC<-day_ucsc.temp %>% filter(Treatment == "UTC")
UTC$Temp_12in = as.numeric(UTC$Temp_12in)
UTC<-UTC[complete.cases(UTC$Temp_12in),]
mean(UTC$Temp_12in)
sd(UTC$Temp_12in)
hist(UTC$Temp_12in)
qqnorm(UTC$Temp_12in)

ASD<-day_ucsc.temp %>% filter(Treatment == "ASD")
ASD<-ASD[complete.cases(ASD$Temp_12in),]
mean(ASD$Temp_12in)
sd(ASD$Temp_12in)
hist(ASD$Temp_12in)
qqnorm(ASD$Temp_12in)
#> mean(ASD$Temp_12in)
#[1] 29.0135
#> sd(ASD$Temp_12in)
#[1] 1.940419

ASD$Trial = "UCSC"
ASD$Days = as.character(ASD$Days)




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

#day_ucsc.temp <- day_ucsc.temp %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Temp_8in, x = Treatment,fill=Treatment), data = day_ucsc.temp)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="Treatment",y="Gas Concentration (PPM)")+scale_y_sqrt()

#exclude all rows with NAs
day_ucsc.temp<-day_ucsc.temp[complete.cases(day_ucsc.temp$Temp_8in),]
b=ggplot(day_ucsc.temp, aes(x=as.factor(Days), y=Temp_8in, fill=Treatment)) + geom_boxplot()+theme_classic()+labs(x="Day of Plot Trial",y="Gas Concentration (PPM)")+scale_y_sqrt()
c=ggplot(aes(y = Temp_8in, x = Days, color=Treatment), data = day_ucsc.temp)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Plot Trial",y="Average Gas Concentration (PPM)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)



## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test treatments where the gas was detected


r1 = lme(Temp_8in ~ Treatment*Days, random = ~1|Plot, data = day_ucsc.temp, method = "REML") 
anova(r1)
#the fixed effects are nutrient treatment (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.


### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Temp_8in ~ Treatment*Days, data = day_ucsc.temp) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Temp_8in ~ Treatment*Days, random = ~1|Plot, data = day_ucsc.temp, method = "ML") #full model using ML
r2 = lme(Temp_8in ~ Treatment+Days, random = ~1|Plot, data = day_ucsc.temp, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))



##################################### PSI 2019 ##############


setwd("G:\\Shared drives\\USDA_SCRI\\Meta\\PSI_field_metadata\\During_ASD\\SoilEh\\PSI2019_2020")
eh = read.csv("PSI2019_ASD_Eh_KP.csv")
eh$Date = as.Date(eh$Date, format = "%m/%d/%Y")
eh$DateTime <- as.POSIXct(paste(eh$Date, eh$Time), format="%Y-%m-%d %H:%M")
eh$HourTime <- strptime(eh$DateTime, format="%Y-%m-%d %H")
#merge by hourtime and plot
#all_meta_csv = merge(all_csv, meta, by=c("HourTime","Plot","Treatment"), all=FALSE)

duration(24, "hours")
#dhours(x = 1)
detach("package:MASS")
if(!require(dplyr)){install.packages("dplyr")}
day_eh.19 = eh %>%
  #  select(everything()) %>%
  arrange(eh$Plot, eh$DateTime) %>%
  group_by(Plot) %>%
  mutate(Days_diff = DateTime - min(DateTime)) %>%
  mutate(hour24 = Days_diff / dhours())

day_eh.19$Plot[day_eh.19$Plot=="ASD01"] <- "R1"
day_eh.19$Plot[day_eh.19$Plot=="ASD06"] <- "R2"
day_eh.19$Plot[day_eh.19$Plot=="ASD09"] <- "R3"
day_eh.19$Plot[day_eh.19$Plot=="ASD10"] <- "R4"
unique(day_eh.19$Plot)

Eh.19 <- ggplot(data = day_eh.19, aes(x=day, y=Eh, group=Plot)) + 
  #geom_smooth(aes(color=Plot)) +
  geom_line(aes(color=Plot)) +
  theme_classic() +
  scale_x_continuous(name = "Day of Trial", 
                     breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))) +
  ylab("Eh (mV)") +
  ylim(-400,400)+
  geom_hline(yintercept = 200, colour = "red")+
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("B. bASD: Eh")+
  theme(plot.title = element_text(size = 12))
Eh.19

aerobicR1 = subset(day_eh.19, Plot == "R1" & Eh >= 200)
aerobicR2 = subset(day_eh.19, Plot == "R2" & Eh >= 200)
aerobicR3 = subset(day_eh.19, Plot == "R3" & Eh >= 200)
aerobicR4 = subset(day_eh.19, Plot == "R4" & Eh >= 200)
anaerobicR1 = subset(day_eh.19, Plot == "R1" & Eh < 200)
anaerobicR2 = subset(day_eh.19, Plot == "R2" & Eh < 200)
anaerobicR3 = subset(day_eh.19, Plot == "R3" & Eh < 200)
anaerobicR4 = subset(day_eh.19, Plot == "R4" & Eh < 200)



########################### PSI 2019 Temps ####################################

setwd("G:\\Shared drives\\USDA_SCRI\\Meta\\PSI_field_metadata\\During_ASD")
psi19.temp = read.csv("PSI2019_ASD_5TE_TempData_KP.csv")

psi19.temp$Date = as.Date(psi19.temp$Date, format = "%m/%d/%Y")
psi19.temp$DateTime <- as.POSIXct(paste(psi19.temp$Date, psi19.temp$TempTime), format="%Y-%m-%d %H:%M")
psi19.temp$HourTime <- strptime(psi19.temp$DateTime, format="%Y-%m-%d %H")
#merge by hourtime and plot
#all_meta_csv = merge(all_csv, meta, by=c("HourTime","Plot","Treatment"), all=FALSE)

duration(24, "hours")
#dhours(x = 1)
detach("package:MASS")
if(!require(dplyr)){install.packages("dplyr")}
day_psi19.temp = psi19.temp %>%
  #  select(everything()) %>%
  arrange(psi19.temp$Plot, psi19.temp$DateTime) %>%
  group_by(Plot) %>%
  mutate(Days_diff = DateTime - min(DateTime)) %>%
  mutate(hour24 = Days_diff / dhours())

day_psi19.temp$Days.dec = day_psi19.temp$Days_diff / 86400
print(colnames(day_psi19.temp)) 
tail(day_psi19.temp$Days.dec)
tail(day_psi19.temp$Days_diff)
day_psi19.temp$Days = str_sub(day_psi19.temp$Days.dec, 1, 1)

#day_psi19.temp$Treatment <- ifelse(day_psi19.temp$Treatment == "non-bASD", "UTC", "bASD")
p19.cbPalette <- c("#999999", "#F0E442")
psi19.temp <- ggplot(data = day_psi19.temp, aes(x=Days.dec, y=Temp_8in, group=Treatment,color=Treatment)) + 
  geom_smooth(
    method = "auto",
    se = FALSE,
    fullrange=TRUE)  + 
  #geom_line(aes(color=Treatment)) +
  scale_color_manual(name = "Treatment",
                     breaks = c("bASD","UTC"),
                     values = p19.cbPalette)+
  theme_classic() +
  scale_x_continuous(name = "Day of Trial", 
                     breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))) +
  ylab("Soil Temperature (C)") +
  ylim(20,35)+
  theme(axis.title.y = element_text(color = "red", size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=10)) +
  #theme(legend.position="bottom") +
  #theme(legend.title = element_blank()) +
  ggtitle("B. bASD 2019 Trial and UTC Soil Temperature at 20 cm")+
  theme(plot.title = element_text(size = 12))
psi19.temp


UTC<-day_psi19.temp %>% filter(Treatment == "UTC")
UTC$Temp_8in = as.numeric(UTC$Temp_8in)
UTC<-UTC[complete.cases(UTC$Temp_8in),]
mean(UTC$Temp_8in)
sd(UTC$Temp_8in)
hist(UTC$Temp_8in)
qqnorm(UTC$Temp_8in)

ASD.19<-day_psi19.temp %>% filter(Treatment == "bASD")
ASD.19<-ASD.19[complete.cases(ASD.19$Temp_8in),]
mean(ASD.19$Temp_8in)
sd(ASD.19$Temp_8in)
hist(ASD.19$Temp_8in)
qqnorm(ASD.19$Temp_8in)
ASD.19$Plot = as.character(ASD.19$Plot)
ASD.19$Trial = "PSI19"
#> mean(ASD.19$Temp_8in)
#[1] 30.53506
#> sd(ASD.19$Temp_8in)
#[1] 4.655185

ASD.19.t <- ASD.19 %>% ungroup %>% select(Temp_8in)
UTC.t <- UTC %>% ungroup %>% select(Temp_8in)

t.test(UTC.t, ASD.19.t, var.equal = TRUE)


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

#day_psi19.temp <- day_psi19.temp %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Temp_8in, x = Treatment,fill=Treatment), data = day_psi19.temp)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="Treatment",y="Temp_8in")+scale_y_sqrt()
day_psi19.temp$Days = as.numeric(day_psi19.temp$Days)
#exclude all rows with NAs
day_psi19.temp<-day_psi19.temp[complete.cases(day_psi19.temp$Temp_8in),]
b=ggplot(day_psi19.temp, aes(x=as.factor(Days), y=Temp_8in, fill=Treatment)) + geom_boxplot()+theme_classic()+labs(x="Day of Plot Trial",y="Temp_8in")+scale_y_sqrt()
day_psi19.temp$Days = as.numeric(day_psi19.temp$Days)
c=ggplot(aes(y = Temp_8in, x = Days, color=Treatment), data = day_psi19.temp)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Plot Trial",y="Average Temp_8in")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test treatments where the gas was detected
day_psi19.temp$Days = as.character(day_psi19.temp$Days)

r1 = lme(Temp_8in ~ Treatment*Days, random = ~1|Plot, data = day_psi19.temp, method = "REML") 
anova(r1)
#the fixed effects are nutrient treatment (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.


### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Temp_8in ~ Treatment*Days, data = day_psi19.temp) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Temp_8in ~ Treatment*Days, random = ~1|Plot, data = day_psi19.temp, method = "ML") #full model using ML
r2 = lme(Temp_8in ~ Treatment+Days, random = ~1|Plot, data = day_psi19.temp, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output


anova(r1)


### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))



##################################### PSI 2021 #############

setwd("G:\\Shared drives\\USDA_SCRI\\Meta\\PSI_field_metadata\\During_ASD\\SoilEh\\PSI2021_2022")
eh.21 = read.csv("PSI2021_ASD_Eh_KP.csv")
eh.21$Date = as.Date(eh.21$Date, format = "%m/%d/%Y")
eh.21$DateTime <- as.POSIXct(paste(eh.21$Date, eh.21$Time), format="%Y-%m-%d %H:%M")
eh.21$HourTime <- strptime(eh.21$DateTime, format="%Y-%m-%d %H")

PlotASD01<-eh.21 %>% filter(Plot == "ASD01")
PlotASD02<-eh.21 %>% filter(Plot == "ASD02")
PlotASD03<-eh.21 %>% filter(Plot == "ASD03")
PlotASD04<-eh.21 %>% filter(Plot == "ASD04")
PlotASD<-subset(eh.21, Plot %in% c("ASD01", "ASD02", "ASD03", "ASD04"))

PlotASD$Plot[PlotASD$Plot=="ASD01"] <- "R1"
PlotASD$Plot[PlotASD$Plot=="ASD02"] <- "R2"
PlotASD$Plot[PlotASD$Plot=="ASD03"] <- "R3"
PlotASD$Plot[PlotASD$Plot=="ASD04"] <- "R4"
unique(PlotASD$Plot)

Eh.21 <- ggplot(data = PlotASD, aes(x=day, y=Eh, group=Plot)) + 
  #geom_smooth(aes(color=Plot)) +
  geom_line(aes(color=Plot)) +
  theme_classic() +
  scale_x_continuous(name = "Day of Trial", 
                     breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))) +
  ylab("Eh (mV)") +
  ylim(-400,400)+
  geom_hline(yintercept = 200, colour = "red")+
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("C. bASD 2021 Trial: Eh")+
  theme(plot.title = element_text(size = 12))
Eh.21



head(day_eh.19)
head(PlotASD)

############################################### PSI 2021 Temp ########################################
setwd("G:\\Shared drives\\USDA_SCRI\\Meta\\PSI_field_metadata\\During_ASD")
psi21.temp = read.csv("PSI2021_ASD_5TE_TempData_KP.csv")

psi21.temp$Date = as.Date(psi21.temp$Date, format = "%m/%d/%Y")
psi21.temp$DateTime <- as.POSIXct(paste(psi21.temp$Date, psi21.temp$TempTime), format="%Y-%m-%d %H:%M")
psi21.temp$HourTime <- strptime(psi21.temp$DateTime, format="%Y-%m-%d %H")
#merge by hourtime and plot
#all_meta_csv = merge(all_csv, meta, by=c("HourTime","Plot","Treatment"), all=FALSE)

duration(24, "hours")
#dhours(x = 1)
detach("package:MASS")
if(!require(dplyr)){install.packages("dplyr")}
day_psi21.temp = psi21.temp %>%
  #  select(everything()) %>%
  arrange(psi21.temp$Plot, psi21.temp$DateTime) %>%
  group_by(Plot) %>%
  mutate(Days_diff = DateTime - min(DateTime)) %>%
  mutate(hour24 = Days_diff / dhours())

day_psi21.temp$Days.dec = day_psi21.temp$Days_diff / 86400
print(colnames(day_psi21.temp)) 
tail(day_psi21.temp$Days.dec)
tail(day_psi21.temp$Days_diff)
day_psi21.temp$Days = str_sub(day_psi21.temp$Days.dec, 1, 1)


day_psi21.temp$Plot = as.character(day_psi21.temp$Plot)



p21.cbPalette <- c("#999999", "#56B4E9")
psi21.temp <- ggplot(data = day_psi21.temp, aes(x=Days.dec, y=Temp_8in, group=Treatment,color=Treatment)) + 
  geom_smooth(
    method = "auto",
    se = FALSE,
    fullrange=TRUE)  + 
  #geom_line(aes(color=Treatment)) +
  scale_color_manual(name = "Treatment",
                     breaks = c("bASD", "FUM"),
                     values = p21.cbPalette)+
  theme_classic() +
  scale_x_continuous(name = "Day of Trial", 
                     breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))) +
  ylab("Soil Temperature (C)") +
  ylim(20,35)+
  theme(axis.title.y = element_text(color = "red", size=10)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=10)) +
  #theme(legend.position="bottom") +
  #theme(legend.title = element_blank()) +
  ggtitle("C. bASD 2021 Trial and FUM Soil Temperature at 20 cm")+
  theme(plot.title = element_text(size = 12))
psi21.temp


InW=6.5; InH=9 #inches high=3, inches wide=2.5  this sets the width and height of the graph in inches. The image can still be adjusted and maintain porportions.
windows(wid=InW, hei=InH)  #this uses above to establish window size parameters and opens the external window to be used
par(mai=c(0.80, 0.85, 0.5, 0.15) , mgp = c(2.5,1,0)) #this sets the graphing window parameters. You can play around with the numbers to acheive the settings you want.
ggarrange(UCSC.temp.20, psi19.temp, psi21.temp, ncol = 1, nrow = 3)
#calls plot to put in the external window
setwd("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\Figures\\")  #sets working directory for where plot wil lbe saved
ggsave(paste("SoilTempFigure.png", sep=''),  #save the plot and name it as you please
       width=16.51, height=22.86, units='cm')  #same width and heigth parameters previously established but in cm units. Don't ask me why, just do it. :laughing:

ggarrange(UCSC.temp.20, psi19.temp, psi21.temp, ncol = 1, nrow = 3)

ASD.21<-day_psi21.temp %>% filter(Treatment == "bASD")
ASD.21<-ASD.21[complete.cases(ASD.21$Temp_8in),]
mean(ASD.21$Temp_8in)
sd(ASD.21$Temp_8in)
hist(ASD.21$Temp_8in)
qqnorm(ASD.21$Temp_8in)

FUM<-day_psi21.temp %>% filter(Treatment == "FUM")
FUM$Temp_8in = as.numeric(FUM$Temp_8in)
FUM<-FUM[complete.cases(FUM$Temp_8in),]
mean(FUM$Temp_8in)
sd(FUM$Temp_8in)
hist(FUM$Temp_8in)
qqnorm(FUM$Temp_8in)

ASD.21.t <- ASD.21 %>% ungroup %>% select(Temp_8in)
FUM.t <- FUM %>% ungroup %>% select(Temp_8in)

t.test(FUM.t, ASD.21.t, var.equal = TRUE)

ASD.21<-day_psi21.temp %>% filter(Treatment == "bASD")
ASD.21<-ASD.21[complete.cases(ASD.21$Temp_12in),]
mean(ASD.21$Temp_12in)
sd(ASD.21$Temp_12in)
hist(ASD.21$Temp_12in)
qqnorm(ASD.21$Temp_12in)

#> mean(ASD.21$Temp_8in)
#[1] 28.22909
#> sd(ASD.21$Temp_8in)
#[1] 2.679836

FUM<-day_psi21.temp %>% filter(Treatment == "FUM")
FUM$Temp_12in = as.numeric(FUM$Temp_12in)
FUM<-FUM[complete.cases(FUM$Temp_12in),]
mean(FUM$Temp_12in)
sd(FUM$Temp_12in)
hist(FUM$Temp_12in)
qqnorm(FUM$Temp_12in)




ASD.21<-day_psi21.temp %>% filter(Treatment == "ASD")
ASD.21<-ASD.21[complete.cases(ASD.21$Temp_12in),]
mean(ASD.21$Temp_12in)
sd(ASD.21$Temp_12in)
hist(ASD.21$Temp_12in)
qqnorm(ASD.21$Temp_12in)
#> mean(ASD.21$Temp_12in)
#[1] 27.24266
#> sd(ASD.21$Temp_12in)
#[1] 1.64035
ASD.19$Trial = "PSI19"
ASD.21$Trial = "PSI21"
ASD$Trial = "UCSC"
ASD.21 <- ASD.21 %>% ungroup %>% select(Treatment, Temp_8in, Trial)
ASD.19 <- ASD.19 %>% ungroup %>% select(Trial, Treatment, Temp_8in)
ASD <- ASD %>% ungroup %>% select(Trial, Treatment, Temp_8in)


ASD.Temp.all=rbind(ASD, 
                   ASD.19
                   )
ASD.Temp.all=rbind(ASD.Temp.all,ASD.21)


ASD.Temp.all$Treatment <- ifelse(ASD.Temp.all$Trial == "UCSC", 'ASD', 'ASD')

ASD.Temp.all$Trial <- as.factor(ASD.Temp.all$Trial)




mean(ASD$Temp_8in)
sd(ASD$Temp_8in)
mean(ASD.19$Temp_8in)
sd(ASD.19$Temp_8in)
mean(ASD.21$Temp_8in)
sd(ASD.21$Temp_8in)

#day_psi21.temp <- day_psi21.temp %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Temp_8in, x = Treatment,fill=Treatment), data = day_psi21.temp)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="Treatment",y="Temp_8in")+scale_y_sqrt()
day_psi21.temp$Days = as.numeric(day_psi21.temp$Days)
#exclude all rows with NAs
day_psi21.temp<-day_psi21.temp[complete.cases(day_psi21.temp$Temp_8in),]
b=ggplot(day_psi21.temp, aes(x=as.factor(Days), y=Temp_8in, fill=Treatment)) + geom_boxplot()+theme_classic()+labs(x="Day of Plot Trial",y="Temp_8in")+scale_y_sqrt()
day_psi21.temp$Days = as.numeric(day_psi21.temp$Days)
c=ggplot(aes(y = Temp_8in, x = Days, color=Treatment), data = day_psi21.temp)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Plot Trial",y="Average Temp_8in")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test treatments where the gas was detected
day_psi21.temp$Days = as.character(day_psi21.temp$Days)

r1 = lme(Temp_8in ~ Treatment*Days, random = ~1|Plot, data = day_psi21.temp, method = "REML") 
anova(r1)
#the fixed effects are nutrient treatment (a factor with two levels) and week. I used week at a continuous variable, but you could also convert it to a factor (as in the second method). They give similar results and in this example using it as a continuous variable made the most sense. The random effect is random intercept for plant to account for repeatedly sampling a plant across 5 weeks. You should use REML (the default), except when comparing models with different fixed effects.


### 2) Run a comparable model using gls without the random effect to determine if the random effect is important


r1a =gls(Temp_8in ~ Treatment*Days, data = day_psi21.temp) #this is how you run the a model with no random effect to compare with r1 (if you just delete the random term from the lme model you will get an error). 
anova(r1,r1a) # compare the two models run using REML. You will find that they are significantly different, and r1 has a lower AIC. So the model including plant, and thereby accounting for repeatedly sampling the same plant, is best.  Even if not significant - if data not independent, you still need to include the random effect, this just lets you see how important it likely is. 


### 3) Now we will modify the fixed effects to find the best model. To do this we need to run the model using ML

r1_ml = lme(Temp_8in ~ Treatment*Days, random = ~1|Plot, data = day_psi21.temp, method = "ML") #full model using ML
r2 = lme(Temp_8in ~ Treatment+Days, random = ~1|Plot, data = day_psi21.temp, method = "ML") #removed the interaction term
anova(r1_ml,r2) # compares the model. Which one is the best?
#r1 with interactions

### 4) Run the best model using REML and look at the ANOVA output

m1=aov(Temp_8in~Treatment, data = day_psi21.temp)
anova(m1)

TukeyHSD(m1)
### 5) Assess model fit by plotting the residuals


plot(resid(r1))
ggqqplot(resid(r1))

########################################## Temp comparison for ASD across Trials #############################


#ASD.Temp.all <- ASD.Temp.all %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
a = ggplot(aes(y = Temp_8in, x = Trial,fill=Trial), data = ASD.Temp.all)+ geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle=45,vjust=0.55))+labs(x="Trial",y="Temp_8in")+scale_y_sqrt()
ASD.Temp.all$Days = as.numeric(ASD.Temp.all$Days)
#exclude all rows with NAs
ASD.Temp.all<-ASD.Temp.all[complete.cases(ASD.Temp.all$Temp_8in),]
b=ggplot(ASD.Temp.all, aes(x=as.factor(Days), y=Temp_8in, fill=Trial)) + geom_boxplot()+theme_classic()+labs(x="Day of Plot Trial",y="Temp_8in")+scale_y_sqrt()
ASD.Temp.all$Days = as.numeric(ASD.Temp.all$Days)
c=ggplot(aes(y = Temp_8in, x = Days, color=Trial), data = ASD.Temp.all)+ geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=TRUE)+ theme_classic() + labs(x="Day of Plot Trial",y="Average Temp_8in")+scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

ggarrange(a,b,c, 
          labels = c("Conc. Dist. by Treat. (Sq. Rt.)", "Conc. Dist. by Day (Sq. Rt.)", "Treat. Daily Avg. (Spline)"),
          ncol = 2, nrow = 2)

## A.2  Perform a repeated measures anova using a mixed-effects model (lme). I will also demonstrate how you can select the best model. This model still works even if you are missing some data (for example if you did not get to measure some of the plants every week). As you imagine this happens frequently in ecology. 
### 1) Run the full model with the random effect
#### This only will test Trials where the gas was detected

day_psi19.temp$Trial = "PSI19"
day_psi21.temp$Trial = "PSI21"
day_ucsc.temp$Trial = "UCSC"

ASD.21.two.way <- day_psi21.temp %>% ungroup %>% select(Treatment, Temp_8in, Trial)
ASD.19.two.way <- day_psi19.temp %>% ungroup %>% select(Trial, Treatment, Temp_8in)
ASD.two.way <- day_ucsc.temp %>% ungroup %>% select(Trial, Treatment, Temp_8in)


ASD.Temp.two.way=rbind(ASD.two.way, 
                   ASD.19.two.way)
ASD.Temp.two.way=rbind(ASD.Temp.two.way,ASD.21.two.way)


m1 = aov(Temp_8in ~ Trial, data = ASD.Temp.all)
summary(m1) #small table with F statistic and P value
coef(m1) 

plot(m1)
shapiro.test(m1$residuals)
tt=TukeyHSD(m1)  
str(tt)
tt$Trial[,"p adj"]
print(tt,digits=30)


mean(ASD$Temp_8in)
sd(ASD$Temp_8in)
mean(ASD.19$Temp_8in)
sd(ASD.19$Temp_8in)
mean(ASD.21$Temp_8in)
sd(ASD.21$Temp_8in)

bartlett.test(Temp_8in ~ Trial, data = ASD.Temp.all)
leveneTest(Temp_8in ~ Trial, data = ASD.Temp.all)

#############################Soil Temp Between Sites#############################################################

ucsc.temp
psi19.temp
psi21.temp


















  

##########################   Yield Data Graphs    ##############################

setwd("G:\\Shared drives\\USDA_SCRI\\Meta\\Multiproject Documents\\YieldData_Joji\\KPEdittedYieldSheets")
yield = read.csv("R_Prescott_MasterCumYield_UCSC_PSI19_PSI21.csv")

yieldUCSC<-yield %>% filter(Site == "UCSC")
yieldPSI2019<-yield %>% filter(Site == "PSI2019")
yieldPSI2021<-yield %>% filter(Site == "PSI2021")

yieldUCSC$Treatment <- ifelse(yieldUCSC$Treatment == "Summer ASD", "fASD", 'MSM')

yieldPSI2019$Treatment <- ifelse(yieldPSI2019$Treatment == "Summer ASD", "bASD",
                                ifelse(yieldPSI2019$Treatment == "Fumigant", 'Fumigant', 'UTC'))

yieldPSI2021$Treatment <- ifelse(yieldPSI2021$Treatment == "Fumigant", 'Fumigant', 'UTC')


yield = rbind(yieldUCSC, yieldPSI2019)
yield = rbind(yield, yieldPSI2021)
yield$Site <- factor(yield$Site, levels=c("UCSC", "PSI2019", "PSI2021"))
yield$Site <- factor(yield$Site, levels=c("UCSC", "PSI2019"))

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggsignif)


#                 bASD,       fASD,     Fumigant, MSM,      UTC
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

y <- ggplot(data = yield, aes(x=Site, y=MrktTotal_lbsPerAcre, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Total Marketable Yield (lbs/acre)", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red", size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 10)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Total Marketable Yield per Treatment")+
  theme(plot.title = element_text(size = 12))
y


u.cbPalette <- c("#E69F00", "#009E73")

u <- ggplot(data = yieldUCSC, aes(x=Treatment, y=MrktTotal_lbsPerAcre, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=u.cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Total Marketable Yield (lbs/acre)", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("UCSC MT: Total Marketable Yield per Treatment")
u

p19.cbPalette <- c("#999999", "#56B4E9", "#F0E442")

p19 <- ggplot(data = yieldPSI2019, aes(x=Treatment, y=MrktTotal_lbsPerAcre, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=p19.cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Total Marketable Yield (lbs/acre)", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("PSI 2019: Total Marketable Yield per Treatment")
p19

bartlett.test(MrktTotal_lbsPerAcre ~ Treatment, data=yieldUCSC)
leveneTest(MrktTotal_lbsPerAcre ~ Treatment, data=yieldUCSC)

pop1 = subset(yieldUCSC, Treatment=="Summer ASD")
pop2 = subset(yieldUCSC, Treatment=="UTC")


grand.m = mean(yieldUCSC$MrktTotal_lbsPerAcre)
var.pool = var(yieldUCSC$MrktTotal_lbsPerAcre)

pop1.m = mean(pop1$MrktTotal_lbsPerAcre)
pop2.m = mean(pop2$MrktTotal_lbsPerAcre)


k = 2
nt = 8

SSEF = sum((pop1$MrktTotal_lbsPerAcre-pop1.m)^2, (pop2$MrktTotal_lbsPerAcre-pop2.m)^2)
SSEF
SN = SSEF/(nt-k)

SN

m1 = aov(MrktTotal_lbsPerAcre ~ Treatment, data=yieldUCSC)
summary(m1) #small table with F statistic and P value
coef(m1) 

plot(m1)
shapiro.test(m1$residuals)
TukeyHSD(m1)  


bartlett.test(MrktTotal_lbsPerAcre ~ Treatment, data=yieldPSI2019)
leveneTest(MrktTotal_lbsPerAcre ~ Treatment, data=yieldPSI2019)

pop1 = subset(yieldPSI2019, Treatment=="Summer ASD")
pop2 = subset(yieldPSI2019, Treatment=="UTC")
pop3 = subset(yieldPSI2019, Treatment=="Fumigant")

grand.m = mean(yieldPSI2019$MrktTotal_lbsPerAcre)
var.pool = var(yieldPSI2019$MrktTotal_lbsPerAcre)

pop1.m = mean(pop1$MrktTotal_lbsPerAcre)
pop2.m = mean(pop2$MrktTotal_lbsPerAcre)
pop3.m = mean(pop3$MrktTotal_lbsPerAcre)

k = 3
nt = 12

SSEF = sum((pop1$MrktTotal_lbsPerAcre-pop1.m)^2, (pop2$MrktTotal_lbsPerAcre-pop2.m)^2, (pop3$MrktTotal_lbsPerAcre-pop2.m)^3)
SSEF
SN = SSEF/(nt-k)

SN

m1 = aov(MrktTotal_lbsPerAcre ~ Treatment*Site, data=yield)
summary(m1) #small table with F statistic and P value
coef(m1) 

plot(m1)
shapiro.test(m1$residuals)
TukeyHSD(m1)  


##########################   percent mortality Graphs    ##############################

setwd("G:\\Shared drives\\USDA_SCRI\\Meta\\Multiproject Documents\\YieldData_Joji\\KPEdittedYieldSheets")
mort = read.csv("KP_PercentMortality_UCSC_PSI19_PSI21.csv")

library("dplyr")
install.packages(ggplot2)
library("ggplot2")



mortUCSC<-mort %>% filter(Site == "UCSC")
mortPSI2019<-mort %>% filter(Site == "PSI2019")
mortPSI2021<-mort %>% filter(Site == "PSI2021")

mortUCSC$Treatment <- ifelse(mortUCSC$Treatment == "Summer ASD", "fASD", 'MSM')

mortPSI2019$Treatment <- ifelse(mortPSI2019$Treatment == "Summer ASD", "bASD",
                             ifelse(mortPSI2019$Treatment == "Fumigant", 'Fumigant', 'UTC'))
mortPSI2021$Treatment <- ifelse(mortPSI2021$Treatment == "Fumigant", 'Fumigant', 'UTC')


mort = rbind(mortUCSC, mortPSI2019, mortPSI2021)
mort$Site <- factor(mort$Site, levels=c("UCSC", "PSI2019", "PSI2021"))

mean(mortUCSC$mortality)
sd(mortUCSC$mortality)
mean(mortUCSC$Wilt.score)
sd(mortUCSC$Wilt.score)

mean(mortPSI2019$mortality)
sd(mortPSI2019$mortality)
mean(mortPSI2019$Wilt.score)
sd(mortPSI2019$Wilt.score)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

m <- ggplot(data = mort, aes(x=Site, y=mortality, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Percent Mortality", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red", size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 10)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("A. Percent Mortality per Treatment")+
  theme(plot.title = element_text(size = 12))
m


u.cbPalette <- c("#E69F00", "#56B4E9")

u <- ggplot(data = mortUCSC, aes(x=Site, y=mortality, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=u.cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Percent Mortality)", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Percent Mortality per Treatment")
u

p19.cbPalette <- c("#999999", "#E69F00", "#56B4E9")

p19 <- ggplot(data = mortPSI2019, aes(x=Site, y=mortality, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=p19.cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Percent Mortality", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Percent Mortality per Treatment")
p19

bartlett.test(mortality ~ Treatment, data=mortUCSC)
leveneTest(mortality ~ Treatment, data=mortUCSC)

pop1 = subset(mortUCSC, Treatment=="Summer ASD")
pop2 = subset(mortUCSC, Treatment=="UTC")


grand.m = mean(mortUCSC$mortality)
var.pool = var(mortUCSC$mortality)

pop1.m = mean(pop1$mortality)
pop2.m = mean(pop2$mortality)


k = 2
nt = 8

SSEF = sum((pop1$mortality-pop1.m)^2, (pop2$mortality-pop2.m)^2)
SSEF
SN = SSEF/(nt-k)

SN

m1 = aov(mortality ~ Treatment, data=mortUCSC)
summary(m1) #small table with F statistic and P value
coef(m1) 

plot(m1)
shapiro.test(m1$residuals)
TukeyHSD(m1)  


bartlett.test(mortality ~ Treatment, data=mortPSI2019)
leveneTest(mortality ~ Treatment, data=mortPSI2019)

pop1 = subset(mortPSI2019, Treatment=="Summer ASD")
pop2 = subset(mortPSI2019, Treatment=="UTC")
pop3 = subset(mortPSI2019, Treatment=="Fumigant")

grand.m = mean(mortPSI2019$mortality)
var.pool = var(mortPSI2019$mortality)

pop1.m = mean(pop1$mortality)
pop2.m = mean(pop2$mortality)
pop3.m = mean(pop3$mortality)

k = 3
nt = 12

SSEF = sum((pop1$mortality-pop1.m)^2, (pop2$mortality-pop2.m)^2, (pop3$mortality-pop2.m)^3)
SSEF
SN = SSEF/(nt-k)

SN

m1 = aov(mortality ~ Treatment*Site, data=mort)
summary(m1) #small table with F statistic and P value
coef(m1) 

plot(m1)
shapiro.test(m1$residuals)
TukeyHSD(m1)  

######################################## wilt score ##########################################
wilt <-subset(mort, Site!="PSI2021")
wilt
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

w <- ggplot(data = wilt, aes(x=Site, y=Wilt.score, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Wilt Score", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red", size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=10)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("B. Percent Wilt Score per Treatment")+
  theme(plot.title = element_text(size = 12))
w

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

y <- ggplot(data = mort, aes(x=Site, y=Wilt.score, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Wilt.score", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Wilt.score per Treatment")
y


u.cbPalette <- c("#E69F00", "#56B4E9")

u <- ggplot(data = mortUCSC, aes(x=Site, y=Wilt.score, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=u.cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Wilt.score)", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Wilt.score per Treatment")
u

p19.cbPalette <- c("#999999", "#E69F00", "#56B4E9")

p19 <- ggplot(data = mortPSI2019, aes(x=Site, y=Wilt.score, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=p19.cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Wilt.score", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Wilt.score per Treatment")
p19

bartlett.test(Wilt.score ~ Treatment, data=mortUCSC)
leveneTest(Wilt.score ~ Treatment, data=mortUCSC)

pop1 = subset(mortUCSC, Treatment=="Summer ASD")
pop2 = subset(mortUCSC, Treatment=="UTC")


grand.m = mean(mortUCSC$Wilt.score)
var.pool = var(mortUCSC$Wilt.score)

pop1.m = mean(pop1$Wilt.score)
pop2.m = mean(pop2$Wilt.score)


k = 2
nt = 8

SSEF = sum((pop1$Wilt.score-pop1.m)^2, (pop2$Wilt.score-pop2.m)^2)
SSEF
SN = SSEF/(nt-k)

SN

m1 = aov(Wilt.score ~ Treatment, data=mortUCSC)
summary(m1) #small table with F statistic and P value
coef(m1) 

plot(m1)
shapiro.test(m1$residuals)
TukeyHSD(m1)  


bartlett.test(Wilt.score ~ Treatment, data=mortPSI2019)
leveneTest(Wilt.score ~ Treatment, data=mortPSI2019)

pop1 = subset(mortPSI2019, Treatment=="Summer ASD")
pop2 = subset(mortPSI2019, Treatment=="UTC")
pop3 = subset(mortPSI2019, Treatment=="Fumigant")

grand.m = mean(mortPSI2019$Wilt.score)
var.pool = var(mortPSI2019$Wilt.score)

pop1.m = mean(pop1$Wilt.score)
pop2.m = mean(pop2$Wilt.score)
pop3.m = mean(pop3$Wilt.score)

k = 3
nt = 12

SSEF = sum((pop1$Wilt.score-pop1.m)^2, (pop2$Wilt.score-pop2.m)^2, (pop3$Wilt.score-pop2.m)^3)
SSEF
SN = SSEF/(nt-k)

SN

m1 = aov(Wilt.score ~ Treatment+Site, data=mort)
summary(m1) #small table with F statistic and P value
coef(m1) 

plot(m1)
shapiro.test(m1$residuals)
TukeyHSD(m1)  

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

y <- ggplot(data = mort, aes(x=Site, y=Wilt.score, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Percent Wilt.score", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Percent Wilt.score per Treatment")
y

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

y <- ggplot(data = mort, aes(x=Site, y=Wilt.score, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Wilt.score)", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Wilt.score per Treatment")
y


u.cbPalette <- c("#E69F00", "#56B4E9")

u <- ggplot(data = mortUCSC, aes(x=Site, y=Wilt.score, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=u.cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Wilt.score)", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Wilt.score per Treatment")
u

p19.cbPalette <- c("#999999", "#E69F00", "#56B4E9")

p19 <- ggplot(data = mortPSI2019, aes(x=Site, y=Wilt.score, fill=Treatment)) + 
  geom_boxplot() +
  scale_fill_manual(values=p19.cbPalette) +
  theme_classic() +
  scale_y_continuous(name = "Wilt.score", 
                     #breaks = as.numeric(c(0,5,10,15,20,25,30,35,40,45,50,55,60))
  ) +
  #ylim(0,60000)+
  scale_x_discrete(name="Site") +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Wilt.score per Treatment")
p19

bartlett.test(Wilt.score ~ Treatment, data=mortUCSC)
leveneTest(Wilt.score ~ Treatment, data=mortUCSC)

pop1 = subset(mortUCSC, Treatment=="Summer ASD")
pop2 = subset(mortUCSC, Treatment=="UTC")


grand.m = mean(mortUCSC$Wilt.score)
var.pool = var(mortUCSC$Wilt.score)

pop1.m = mean(pop1$Wilt.score)
pop2.m = mean(pop2$Wilt.score)


k = 2
nt = 8

SSEF = sum((pop1$Wilt.score-pop1.m)^2, (pop2$Wilt.score-pop2.m)^2)
SSEF
SN = SSEF/(nt-k)

SN

m1 = aov(Wilt.score ~ Treatment, data=mortUCSC)
summary(m1) #small table with F statistic and P value
coef(m1) 

plot(m1)
shapiro.test(m1$residuals)
TukeyHSD(m1)  


bartlett.test(Wilt.score ~ Treatment, data=mortPSI2019)
leveneTest(Wilt.score ~ Treatment, data=mortPSI2019)

pop1 = subset(mortPSI2019, Treatment=="Summer ASD")
pop2 = subset(mortPSI2019, Treatment=="UTC")
pop3 = subset(mortPSI2019, Treatment=="Fumigant")

grand.m = mean(mortPSI2019$Wilt.score)
var.pool = var(mortPSI2019$Wilt.score)

pop1.m = mean(pop1$Wilt.score)
pop2.m = mean(pop2$Wilt.score)
pop3.m = mean(pop3$Wilt.score)

k = 3
nt = 12

SSEF = sum((pop1$Wilt.score-pop1.m)^2, (pop2$Wilt.score-pop2.m)^2, (pop3$Wilt.score-pop2.m)^3)
SSEF
SN = SSEF/(nt-k)

SN

m1 = aov(Wilt.score ~ Treatment+Site, data=mort)
summary(m1) #small table with F statistic and P value
coef(m1) 

plot(m1)
shapiro.test(m1$residuals)
TukeyHSD(m1)

InW=6.5; InH=9 #inches high=3, inches wide=2.5  this sets the width and height of the graph in inches. The image can still be adjusted and maintain porportions.
windows(wid=InW, hei=InH)  #this uses above to establish window size parameters and opens the external window to be used
par(mai=c(0.80, 0.85, 0.5, 0.15) , mgp = c(2.5,1,0)) #this sets the graphing window parameters. You can play around with the numbers to acheive the settings you want.
ggarrange(m, w, y, ncol = 1, nrow = 3)
#calls plot to put in the external window
setwd("G:\\Shared drives\\USDA_SCRI\\Publications_presentations\\VOCManuscript\\Figures\\")  #sets working directory for where plot wil lbe saved
ggsave(paste("mortwiltyield.png", sep=''),  #save the plot and name it as you please
       width=16.51, height=22.86, units='cm')  #same width and heigth parameters previously established but in cm units. Don't ask me why, just do it. :laughing:

