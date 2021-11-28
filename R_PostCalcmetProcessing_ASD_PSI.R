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

#create master by pulling multiple csv and join

setwd("G:\\Shared drives\\USDA_SCRI\\PSI_field_trial_results\\Calcmet_processed\\VOC")


#If the path is different than your working directory
# you'll need to set full.names = TRUE to get the full
# paths.
my_files <- list.files(pattern="*.csv")
all_csv = do.call(rbind.fill, lapply(my_files, function(x) read.csv(x, stringsAsFactors = FALSE)))
all_csv  

#Further arguments to read.csv can be passed in ...
#all_csv <- lapply(my_files,read.csv)

#Set the name of each list element to its
# respective file name. Note full.names = FALSE to
# get only the file names, not the full path.
#names(all_csv) <- gsub(".csv","",
                       #list.files(pattern="*.csv",full.names = FALSE),
                       #fixed = TRUE)
#names(all_csv)

#for(i in 1:length(all_csv)) {
  #all_csv[["i"]][-c(7:9,11:13,15:17,19:21,23:25,27:29,31:33,35:37,
     #39:41,43:45,47:49,51:53,55:57,59:61,63:65,67:69,
     #71:73,75:77,79:81,83:85,87:89)]
#}
#remove excess columns
#remove excess columns
all_csv <- all_csv %>% select(-contains("Unit"))
all_csv <- all_csv %>% select(-contains("Compensation"))
all_csv <- all_csv %>% select(-contains("Residual"))
all_csv <- all_csv %>% select(-contains("Status"))
all_csv <- all_csv %>% select(-contains("Line"))
all_csv <- all_csv %>% select(-contains("Time"))
all_csv <- all_csv %>% select(-contains("SpectrumFile"))
#all_csv <- all_csv[, -grep("Unit*", "Compensation*", "Residual*", colnames(all_csv))]
output_path = (paste("..\\VOC\\outputfiles\\", sep = ""))
write.csv(all_csv,
          paste( output_path, "all_csv", ".csv", sep="" ))

all_csv$Carbon.dioxide <- all_csv$Carbon.dioxide*10000
all_csv$Carbon.dioxide <- ifelse(is.na(all_csv$Carbon.dioxide), 
                                 all_csv$Carbon.dioxide.ppm, all_csv$Carbon.dioxide)
all_csv <- all_csv %>% select(-contains("Carbon.dioxide.ppm"))

all_csv$Plot = substr(all_csv$LibraryFile,11,15)                                         
#all_csv$Gasmet = substr(all_csv$LibraryFile,8,9)
output_path = (paste("..\\VOC\\outputfiles\\", sep = ""))
write.csv(all_csv,
          paste( output_path, "all_csv", ".csv", sep="" ))


mean.all.data = all_csv %>%
  group_by(Date, Plot) %>%
  summarise_at(.vars = vars(Water.vapor,Carbon.dioxide,Nitrous.oxide,Methane,Ammonia,Propane,Methacrylic.acid,
                            X1.Hexanol,X1.Pentene,Isopentyl.acetate,Propyl.acetate,X2.3.Hexanedione,Cyclopentanone,
                            Propylene.oxide,Ethanol,Pentane,X1.2.4.Trimethylbenzene,t.Butanol,Methyl.isocyanate,
                            X1.Butanethiol,Ethylene.glycol,X3.Ethyltoluene,X2.3.Heptanedione,X2.Methylpyrazine,
                            Ethyl.benzene,Isobutanol,Isobutene..2.methylpropene.,Butane,X4.Ethyltoluene,Cyclopentane,
                            X2.3.Dimethylpyrazine,X2.Ethylhexanol,Ethane,Cyclohexane,X1.Heptene,Hexanoic.acid,
                            Propylamine,X1.Methylimidazol,Hexane,Undecane,X1.3.5.Trimethylbenzene,Dimethyl.sulfide,
                            Hexene,X1.Butene,Ethylcyclohexane,Butylamine..1.butanamine.,Nonane,Dodecane,Ethanolamine),
               .funs = c(mean="mean"))

str(mean.all.data)
output_path = (paste("..\\VOC\\outputfiles\\", sep = ""))
write.csv(mean.all.data,
          paste( output_path, "mean.all.data", ".csv", sep="" ))
#mean.all.data <- mean.all.data[-c(1),] 

mean.all.data$Date <- as.Date(mean.all.data$Date, "%m/%d/%Y")
output_path = (paste("..\\VOC\\outputfiles\\", sep = ""))
write.csv(mean.all.data,
          paste( output_path, "mean.all.data", ".csv", sep="" ))
mean.voc <-subset(mean.all.data, select = -c(Water.vapor_mean, Carbon.dioxide_mean, Methane_mean))
mean.CO2 <-subset(mean.all.data, select = c("Date", "Plot", "Carbon.dioxide_mean"))
mean.CH4 <-subset(mean.all.data, select = c("Date", "Plot", "Methane_mean"))
str(mean.voc)
library(tidyr)
long.mean.voc <- mean.voc %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Date, Plot))
long.mean.voc <- subset(long.mean.voc, Gas_Concentration_PPM >= 1)
str(long.mean.voc)

long.mean.voc.ASD01 <- long.mean.voc[which(long.mean.voc$Plot =="ASD01"),]
long.mean.voc.ASD06 <- long.mean.voc[which(long.mean.voc$Plot =="ASD06"),]
long.mean.voc.ASD09 <- long.mean.voc[which(long.mean.voc$Plot =="ASD09"),]
long.mean.voc.ASD10 <- long.mean.voc[which(long.mean.voc$Plot =="ASD10"),]

long.mean.voc.ASD01$Gas_Species = substr(long.mean.voc.ASD01$Gas_Species,1,nchar(long.mean.voc.ASD01$Gas_Species)-5)
long.mean.voc.ASD06$Gas_Species = substr(long.mean.voc.ASD06$Gas_Species,1,nchar(long.mean.voc.ASD06$Gas_Species)-5)
long.mean.voc.ASD09$Gas_Species = substr(long.mean.voc.ASD09$Gas_Species,1,nchar(long.mean.voc.ASD09$Gas_Species)-5)
long.mean.voc.ASD10$Gas_Species = substr(long.mean.voc.ASD10$Gas_Species,1,nchar(long.mean.voc.ASD10$Gas_Species)-5)

long.mean.CO2 <- mean.CO2 %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Date, Plot))
long.mean.CO2
long.mean.CH4 <- mean.CH4 %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Date, Plot))
long.mean.CH4

long.mean.CO2.ASD01 <- long.mean.CO2[which(long.mean.CO2$Plot =="ASD01"),]
long.mean.CO2.ASD06 <- long.mean.CO2[which(long.mean.CO2$Plot =="ASD06"),]
long.mean.CO2.ASD09 <- long.mean.CO2[which(long.mean.CO2$Plot =="ASD09"),]
long.mean.CO2.ASD10 <- long.mean.CO2[which(long.mean.CO2$Plot =="ASD10"),]

long.mean.CH4.ASD01 <- long.mean.CH4[which(long.mean.CH4$Plot =="ASD01"),]
long.mean.CH4.ASD06 <- long.mean.CH4[which(long.mean.CH4$Plot =="ASD06"),]
long.mean.CH4.ASD09 <- long.mean.CH4[which(long.mean.CH4$Plot =="ASD09"),]
long.mean.CH4.ASD10 <- long.mean.CH4[which(long.mean.CH4$Plot =="ASD10"),]


output_path = (paste("..\\VOC\\outputfiles\\", sep = ""))
write.csv(long.mean.voc,
          paste( output_path, "long.mean.voc", ".csv", sep="" ))
output_path = (paste("..\\VOC\\outputfiles\\", sep = ""))
write.csv(long.mean.CH4,
          paste( output_path, "long.mean.CH4", ".csv", sep="" ))


#####################################################################################

Legend.voc=rep(paste0(long.mean.voc$Gas_Species), each=1)
Legend.voc

voc <- ggplot(data = long.mean.voc, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Legend.voc)) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,10,20,30,40,50,60,70,80,90,100,
                                                              110,120,130,140,150,160,170,180,190,200)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  ggtitle("Mean VOC Concentrations During ASD")
voc 

#####################################################################################

Legend.voc.ASD01=rep(paste0(long.mean.voc.ASD01$Gas_Species), each=1)
Legend.voc.ASD01

voc.ASD01 <- ggplot(data = long.mean.voc.ASD01, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Legend.voc.ASD01), size = 3.5) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,20,40,60,80,100,120,140,160,180,200)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust=1, size = 10)) +
  theme(plot.title = element_text(size=14)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10)) +
  ggtitle("Mean VOC Concentrations During ASD: Plot ASD01")
voc.ASD01 

ggarrange(voc.ASD01, 
          labels = c("A"),
          ncol = 1, nrow = 1)
####################################################################################################################

Legend.voc.ASD06=rep(paste0(long.mean.voc.ASD06$Gas_Species), each=1)
Legend.voc.ASD06
voc.ASD06 <- ggplot(data = long.mean.voc.ASD06, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Legend.voc.ASD06), size = 3.5) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,2,4,6,8,10)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust=1, size = 10)) +
  theme(plot.title = element_text(size=14)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10)) +
  ggtitle("Mean VOC Concentrations During ASD: Plot ASD06")
voc.ASD06 

ggarrange(voc.ASD06,
          labels = c("B"),
          ncol = 1, nrow = 1)
####################################################################################################################

Legend.voc.ASD09=rep(paste0(long.mean.voc.ASD09$Gas_Species), each=1)
Legend.voc.ASD09

voc.ASD09 <- ggplot(data = long.mean.voc.ASD09, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Legend.voc.ASD09), size = 3.5) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,5,10,15,20,25,30)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust=1, size = 10)) +
  theme(plot.title = element_text(size=14)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10)) +
  ggtitle("Mean VOC Concentrations During ASD: Plot ASD09")
voc.ASD09

ggarrange(voc.ASD09, 
          labels = c("C"),
          ncol = 1, nrow = 1)

####################################################################################################################
Legend.voc.ASD10=rep(paste0(long.mean.voc.ASD10$Gas_Species), each=1)
Legend.voc.ASD10

voc.ASD10 <- ggplot(data = long.mean.voc.ASD10, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_point(aes(color=Legend.voc.ASD10), size = 3.5) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,2,4,6,8,10,12,14,16,18,20)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust=1, size = 10)) +
  theme(plot.title = element_text(size=14)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=10)) +
  ggtitle("Mean VOC Concentrations During ASD: Plot ASD10")
voc.ASD10

ggarrange(voc.ASD10, 
          labels = c("D"),
          ncol = 1, nrow = 1)

ggarrange(voc.ASD01, voc.ASD06, voc.ASD09, voc.ASD10, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
####################################################################################################################

Legend.CO2=rep(paste0(long.mean.CO2$Plot), each=1)
Legend.CO2

CO2 <- ggplot(data = long.mean.CO2, aes(x=Date, y=Gas_Concentration_PPM, group=Plot)) + 
  geom_line(aes(color=Legend.CO2), size = 1.5) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,25000,50000,75000,100000,125000,
                                                              150000,175000,200000)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(plot.title = element_text(size=22)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  ggtitle("Mean CO2 Concentrations During ASD")
CO2

#####################################################################################
Legend.CH4=rep(paste0(long.mean.CH4$Plot), each=1)
Legend.CH4

CH4 <- ggplot(data = long.mean.CH4, aes(x=Date, y=Gas_Concentration_PPM, group=Plot)) + 
  geom_line(aes(color=Legend.CH4), size = 1.5) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,1,2,3,4,5)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  ggtitle("Mean CH4 Concentrations During ASD")
CH4
#####################################################################################################
Legend.CH4.ASD01=rep(paste0(long.mean.CH4.ASD01$Gas_Species), each=1)
Legend.CH4.ASD01

CH4.ASD01 <- ggplot(data = long.mean.CH4.ASD01, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_line(aes(color=Legend.CH4.ASD01)) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(00,1,2,3,4,5)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Mean CH4 Plot ASD01 During ASD")
CH4.ASD01
#####################################################################################################
Legend.CH4.ASD06=rep(paste0(long.mean.CH4.ASD06$Gas_Species), each=1)
Legend.CH4.ASD06

CH4.ASD06 <- ggplot(data = long.mean.CH4.ASD06, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_line(aes(color=Legend.CH4.ASD06)) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,1,2,3,4,5)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Mean CH4 Plot ASD06 During ASD")
CH4.ASD06
#####################################################################################################
Legend.CH4.ASD09=rep(paste0(long.mean.CH4.ASD09$Gas_Species), each=1)
Legend.CH4.ASD09

CH4.ASD09 <- ggplot(data = long.mean.CH4.ASD09, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_line(aes(color=Legend.CH4.ASD09)) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,1,2,3,4,5)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Mean CH4 Plot ASD09 During ASD")
CH4.ASD09
#####################################################################################################
Legend.CH4.ASD10=rep(paste0(long.mean.CH4.ASD10$Gas_Species), each=1)
Legend.CH4.ASD10

CH4.ASD10 <- ggplot(data = long.mean.CH4.ASD10, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_line(aes(color=Legend.CH4.ASD10)) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = c(0,1,2,3,4,5)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  ggtitle("Mean CH4 Plot ASD10 During ASD")
CH4.ASD10

ggarrange(CH4.ASD01, CH4.ASD06, CH4.ASD09, CH4.ASD10, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
#####################################################################################################
####
####STAT ANALYSIS




























##can select name of datafram based on letter number
#names <-substr(filenames,1,17)

#paste0("sheet_",1:length(filelist))
###automatically names a list of file names with an autonumber and set prefix
