rm(list=ls()); graphics.off()
install.packages("dplyr")
install.packages("colorspace")
install.packages("ggplot2")
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(tidyverse)

###Only ASD data
setwd("C:\\CalcmetResults\\Results_Location_ASD")

filenames = list.files(pattern="*.csv")
filenames

####If I want to import csvs as separate objects/df in R for manipulation
#for(i in filenames){
  #assign(i, read.csv(paste(i, ".csv", sep="")))}

#combines all listed files into one data frame
myfiles = do.call(rbind, lapply(filenames, function(x) read.csv(x, stringsAsFactors = FALSE)))
myfiles

##############################################################################################
### Need to rename, fill, and move some columns because stats are dumb
myfiles$c <- ifelse(myfiles$Treatment <="1A01", 'ASD',
                           ifelse(myfiles$Treatment <="1A27", 'ASD',
                                  ifelse(myfiles$Treatment <="1A35", 'ASD',
                                         ifelse(myfiles$Treatment <="1A60", 'ASD','UTC'))))

colnames(myfiles)[colnames(myfiles)=="Treatment"] <- "Plot"
colnames(myfiles)[colnames(myfiles)=="c"] <- "Treatment"
str(myfiles)

#subsets data frame
myfiles.subset = subset(myfiles, select = c("Treatment", "Plot", "Date", "Time", "Chamber_ID", "GasmetID", 
                       "Carbon.dioxide.CO2", "Methane.CH4", "Nitrous.oxide.N2O", 
                       "Ammonia.NH3", "Carbon.monoxide.CO", "Benzene", 
                       "Butyric.acid",  "Nitrogen.dioxide", "Hydrogen.fluoride", 
                       "Nitrogen.monoxide", "Ozone", "Valeric.acid", 
                       "Propionic.acid", "trans.2.Butene", "Sulfur.trioxide", 
                       "cis.2.pentene", "Acetaldehyde", "Ethylene", 
                       "Hydrogen.chloride", "Sulfuryl.fluoride", "Toluene", 
                       "Trimethylamine", "Cyclohexane", "Hydrogen.cyanide", 
                       "Dodecane", "Methyl.bromide", "Cyclohexanone", "Phosphine", 
                       "p.Xylene", "Isohexane", "Ethyl.benzene", "Morpholine", 
                       "Vinyl.chloride", "Hydrogen.bromide", "Chlorine.dioxide", 
                       "Pyridine", "alpha.Methylstyrene", "X1.3.Butadiene", "Dimethylvinylchlorosilane", 
                       "Acetic.acid"))
str(myfiles.subset)
#transforming misslabeled columns from char to num
myfiles.subset = transform(myfiles.subset, Butyric.acid = as.numeric(Butyric.acid),
                          Nitrogen.monoxide = as.numeric(Nitrogen.monoxide),
                          Ozone = as.numeric(Ozone), 
                          Valeric.acid = as.numeric(Valeric.acid), 
                          Propionic.acid = as.numeric(Propionic.acid),
                          Sulfur.trioxide = as.numeric(Sulfur.trioxide), 
                          Acetaldehyde = as.numeric(Acetaldehyde), 
                          Ethylene = as.numeric(Ethylene), 
                          Sulfuryl.fluoride = as.numeric(Sulfuryl.fluoride), 
                          Hydrogen.cyanide = as.numeric(Hydrogen.cyanide),
                          Methyl.bromide = as.numeric(Methyl.bromide), 
                          Cyclohexanone = as.numeric(Cyclohexanone),
                          Phosphine = as.numeric(Phosphine),
                          Vinyl.chloride = as.numeric(Vinyl.chloride),
                          Chlorine.dioxide = as.numeric(Chlorine.dioxide),
                          Pyridine = as.numeric(Pyridine),
                          alpha.Methylstyrene = as.numeric(alpha.Methylstyrene),
                          X1.3.Butadiene = as.numeric(X1.3.Butadiene),
                          Dimethylvinylchlorosilane = as.numeric(Dimethylvinylchlorosilane),
                          Acetic.acid = as.numeric(Acetic.acid))
str(myfiles.subset)


#Average concentrations by date and create line plot showing change over date. 
##Can group by other variables if need be
###i.e. by Gasmet or by chamber
mean.myfiles.subset = myfiles.subset %>%
  group_by(Date, Treatment, Plot) %>%
  summarise_at(.vars = vars(Carbon.dioxide.CO2, 
                            Methane.CH4, 
                            Nitrous.oxide.N2O, 
                            Ammonia.NH3, Carbon.monoxide.CO, 
                            Benzene, Butyric.acid, 
                            Nitrogen.dioxide, Hydrogen.fluoride, Nitrogen.monoxide, 
                            Ozone, Valeric.acid, Propionic.acid, trans.2.Butene, Sulfur.trioxide, 
                            cis.2.pentene, Acetaldehyde, Ethylene, Hydrogen.chloride, Sulfuryl.fluoride, 
                            Toluene, Trimethylamine, Cyclohexane, Hydrogen.cyanide, Dodecane, 
                            Methyl.bromide, Cyclohexanone, Phosphine, p.Xylene, Isohexane, 
                            Ethyl.benzene, Morpholine, Vinyl.chloride, Hydrogen.bromide, Chlorine.dioxide, 
                            Pyridine, alpha.Methylstyrene, X1.3.Butadiene, Dimethylvinylchlorosilane, Acetic.acid),
               .funs = c(mean="mean"))

str(mean.myfiles.subset)

#removed some data points that were skewing the data due to massive interference
###these lines are not vital but you have change things further down in the code without them
#removed.mean.myfiles.subset <- read.csv("C:\\CalcmetResults\\removed.mean.myfiles.subset.2.csv")
#head(removed.mean.myfiles.subset)


#Transform data from wide to long
library(tidyr)
long.mean.myfiles.subset <- mean.myfiles.subset %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Date, Treatment, Plot))
long.mean.myfiles.subset
output_path = (paste("C:\\CalcmetResults\\", sep = ""))
write.csv(long.mean.myfiles.subset,
          paste( output_path, "long.mean.myfiles.subset", ".csv", sep="" ))

##############################################################################################
#### Can also just subset by gas species and a leave data "wide"

mean.CO2.subset = myfiles.subset %>% 
  group_by(Date, Treatment, Plot) %>%
  summarise_at(.vars = vars(Carbon.dioxide.CO2), 
               .funs = c(mean="mean"))
colnames(mean.CO2.subset)[colnames(mean.CO2.subset)=="mean"] <- "Carbon.dioxide.CO2_mean"
str(mean.CO2.subset)

#plot: Multiple lines with color added using "long" data
Legend=rep(paste0(long.mean.myfiles.subset$Gas_Species), each=1)
Legend

long.mean.myfiles.subset$Date <- as.Date(long.mean.myfiles.subset$Date, format = "%m/%d/%Y")
str(long.mean.myfiles.subset)
mean.CO2.subset$Date <- as.Date(mean.CO2.subset$Date, format = "%m/%d/%Y")
str(mean.CO2.subset)

install.packages("ggpubr")

library(ggpubr)
####For reproducible examples in stat forums
dput(mean.CO2.subset)
dput(long.mean.myfiles.subset)
####This graph is not really working
voc <- ggplot(data = long.mean.myfiles.subset, aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_line(aes(color=Legend)) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = seq(0,1500,250)) +
                      
  theme(axis.title.y = element_text(color = "red")) +
  theme(legend.position="bottom") +
  ggtitle("Mean Gas Concentrations During ASD")
voc    
#####This graph is ok
CO2 <- ggplot() +
  geom_line(mapping = aes(x=mean.CO2.subset$Date, y=mean.CO2.subset$Carbon.dioxide.CO2_mean), color= "black") +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)") +
  theme(axis.title.y = element_text(color = "red")) +
  ggtitle("Mean CO2 Concentrations During ASD")
CO2
######THIS WILL BE SLOW GIVE IT TIME
####Can use this code to display multiple graphs if you have a large enough monitor
ggarrange(CO2, 
          labels = c("A"),
          ncol = 1, nrow = 1)



########################################################################
###Boxplot of ASD data

myfiles.subset.box = subset(myfiles, select = c("Treatment", "Date", "Plot",
                                            "Carbon.dioxide.CO2", "Methane.CH4", "Nitrous.oxide.N2O", 
                                            "Ammonia.NH3", "Carbon.monoxide.CO", "Benzene", 
                                            "Butyric.acid",  "Nitrogen.dioxide", "Hydrogen.fluoride", 
                                            "Nitrogen.monoxide", "Ozone", "Valeric.acid", 
                                            "Propionic.acid", "trans.2.Butene", "Sulfur.trioxide", 
                                            "cis.2.pentene", "Acetaldehyde", "Ethylene", 
                                            "Hydrogen.chloride", "Sulfuryl.fluoride", "Toluene", 
                                            "Trimethylamine", "Cyclohexane", "Hydrogen.cyanide", 
                                            "Dodecane", "Methyl.bromide", "Cyclohexanone", "Phosphine", 
                                            "p.Xylene", "Isohexane", "Ethyl.benzene", "Morpholine", 
                                            "Vinyl.chloride", "Hydrogen.bromide", "Chlorine.dioxide", 
                                            "Pyridine", "alpha.Methylstyrene", "X1.3.Butadiene", "Dimethylvinylchlorosilane", 
                                            "Acetic.acid"))
myfiles.subset.box = myfiles.subset.box %>%
  group_by(Date, Treatment, Gas_Species) 
head(myfiles.subset.box)

#install.packages("ggpubr")

#library(ggpubr)
library(tidyr)
long.myfiles.subset.box <- myfiles.subset.box %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Date, Treatment))
myfiles.subset.box
output_path = (paste("C:\\CalcmetResults\\", sep = ""))
write.csv(long.myfiles.subset.box, row.names=FALSE,
          paste( output_path, "long.myfiles.subset.box", ".csv", sep="" ))
str(long.myfiles.subset.box)

removed.long.myfiles.subset.box <- read.csv("C:\\CalcmetResults\\removed.long.myfiles.subset.box.csv")
head(removed.long.myfiles.subset.box)

removed.long.myfiles.subset.box $Date <- as.Date(removed.long.myfiles.subset.box $Date, format = "%m/%d/%Y")
str(removed.long.myfiles.subset.box)
Legend.box=rep(paste0(removed.long.myfiles.subset.box$Gas_Species), each=1)
Legend.box

voc.box <- ggplot(data = removed.long.myfiles.subset.box, aes(x=Gas_Species, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_boxplot(aes(color=Legend.box)) +
  scale_x_discrete(name = "Gas_Species") +
  #scale_y_discrete(name = "Concentration (ppm)", breaks = seq(0,200,50)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.title.x = element_text(color = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="bottom") +
  ggtitle("Gas Concentrations During ASD")
voc.box
ggarrange(voc.box, 
          labels = c("A"),
          ncol = 1, nrow = 1)








########################################################################################
##########Control/UTC data only



setwd("C:\\CalcmetResults\\Results_Location_UTC")

filenames.UTC = list.files(pattern="*.csv")
filenames.UTC

####If I want to import csvs as separate objects/df in R for manipulation
#for(i in filenames){
#assign(i, read.csv(paste(i, ".csv", sep="")))}

#combines all listed files into one data frame
myfiles.UTC = do.call(rbind, lapply(filenames.UTC, function(x) read.csv(x, stringsAsFactors = FALSE)))
myfiles.UTC

myfiles.UTC$c <- ifelse(myfiles.UTC$Treatment <="1A01", 'ASD',
                    ifelse(myfiles.UTC$Treatment <="1A27", 'ASD',
                           ifelse(myfiles.UTC$Treatment <="1A35", 'ASD',
                                  ifelse(myfiles.UTC$Treatment <="1A60", 'ASD','UTC'))))

colnames(myfiles.UTC)[colnames(myfiles.UTC)=="Treatment"] <- "Plot"
colnames(myfiles.UTC)[colnames(myfiles.UTC)=="c"] <- "Treatment"
str(myfiles.UTC)




#subsets data frame
myfiles.subset.UTC = subset(myfiles.UTC, select = c("Treatment", "Plot", "Date", "Time", "Chamber_ID", "GasmetID", 
                                            "Carbon.dioxide.CO2", "Methane.CH4", "Nitrous.oxide.N2O", 
                                            "Ammonia.NH3", "Carbon.monoxide.CO", "Benzene", 
                                            "Butyric.acid",  "Nitrogen.dioxide", "Hydrogen.fluoride", 
                                            "Nitrogen.monoxide", "Ozone", "Valeric.acid", 
                                            "Propionic.acid", "trans.2.Butene", "Sulfur.trioxide", 
                                            "cis.2.pentene", "Acetaldehyde", "Ethylene", 
                                            "Hydrogen.chloride", "Sulfuryl.fluoride", "Toluene", 
                                            "Trimethylamine", "Cyclohexane", "Hydrogen.cyanide", 
                                            "Dodecane", "Methyl.bromide", "Cyclohexanone", "Phosphine", 
                                            "p.Xylene", "Isohexane", "Ethyl.benzene", "Morpholine", 
                                            "Vinyl.chloride", "Hydrogen.bromide", "Chlorine.dioxide", 
                                            "Pyridine", "alpha.Methylstyrene", "X1.3.Butadiene", "Dimethylvinylchlorosilane", 
                                            "Acetic.acid"))
str(myfiles.subset.UTC)

myfiles.subset.UTC = transform(myfiles.subset.UTC, Carbon.monoxide.CO = as.numeric(Carbon.monoxide.CO), 
                               Hydrogen.fluoride = as.numeric(Hydrogen.fluoride), 
                               Ozone = as.numeric(Ozone))
                                
str(myfiles.subset.UTC)

#Average concentrations by date and create line plot showing change over date. 
##Can group by other variables if need by
###i.e. by Gasmet or by chamber
#Can COmment out the gases I'm not interested in
mean.myfiles.subset.UTC = myfiles.subset.UTC %>%
  group_by(Date, Treatment, Plot) %>%
  summarise_at(.vars = vars(#Carbon.dioxide.CO2, 
    Methane.CH4, 
    Nitrous.oxide.N2O, 
    Ammonia.NH3, Carbon.monoxide.CO, 
    Benzene, Butyric.acid, 
    Nitrogen.dioxide, Hydrogen.fluoride, Nitrogen.monoxide, 
    Ozone, Valeric.acid, Propionic.acid, trans.2.Butene, Sulfur.trioxide, 
    cis.2.pentene, Acetaldehyde, Ethylene, Hydrogen.chloride, Sulfuryl.fluoride, 
    Toluene, Trimethylamine, Cyclohexane, Hydrogen.cyanide, Dodecane, 
    Methyl.bromide, Cyclohexanone, Phosphine, p.Xylene, Isohexane, 
    Ethyl.benzene, Morpholine, Vinyl.chloride, Hydrogen.bromide, Chlorine.dioxide, 
    Pyridine, alpha.Methylstyrene, X1.3.Butadiene, Dimethylvinylchlorosilane, Acetic.acid),
    .funs = c(mean="mean"))

head(mean.myfiles.subset.UTC)
output_path = (paste("C:\\CalcmetResults\\", sep = ""))
write.csv(mean.myfiles.subset.UTC, row.names=FALSE,
          paste( output_path, "mean.myfiles.subset.UTC", ".csv", sep="" ))

#Transform data from wide to long
#library(tidyr)
long.mean.myfiles.subset.UTC <- mean.myfiles.subset.UTC %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Date, Treatment, Plot))
long.mean.myfiles.subset.UTC
output_path = (paste("C:\\CalcmetResults\\", sep = ""))
write.csv(long.mean.myfiles.subset.UTC,
          paste( output_path, "long.mean.myfiles.subset.UTC", ".csv", sep="" ))

Legend=rep(paste0(long.mean.myfiles.subset.UTC $Gas_Species), each=1)
Legend

long.mean.myfiles.subset.UTC $Date <- as.Date(long.mean.myfiles.subset.UTC $Date, format = "%m/%d/%Y")
str(long.mean.myfiles.subset.UTC )


#install.packages("ggpubr")

#library(ggpubr)

dput(long.mean.myfiles.subset.UTC )

utc <- ggplot(data = long.mean.myfiles.subset.UTC , aes(x=Date, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_line(aes(color=Legend)) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Concentration (ppm)", breaks = seq(0,10,1)) +
  
  theme(axis.title.y = element_text(color = "red")) +
  theme(legend.position="bottom") +
  ggtitle("Mean Gas Concentrations During Control")
utc

###########################################

myfiles.subset.UTC.box = subset(myfiles.UTC, select = c("Treatment", "Date", "Plot", 
                                                #"Carbon.dioxide.CO2", 
                                                "Methane.CH4", 
                                                "Nitrous.oxide.N2O", 
                                                "Ammonia.NH3", "Carbon.monoxide.CO", "Benzene", 
                                                "Butyric.acid",  "Nitrogen.dioxide", "Hydrogen.fluoride", 
                                                "Nitrogen.monoxide", "Ozone", "Valeric.acid", 
                                                "Propionic.acid", "trans.2.Butene", "Sulfur.trioxide", 
                                                "cis.2.pentene", "Acetaldehyde", "Ethylene", 
                                                "Hydrogen.chloride", "Sulfuryl.fluoride", "Toluene", 
                                                "Trimethylamine", "Cyclohexane", "Hydrogen.cyanide", 
                                                "Dodecane", "Methyl.bromide", "Cyclohexanone", "Phosphine", 
                                                "p.Xylene", "Isohexane", "Ethyl.benzene", "Morpholine", 
                                                "Vinyl.chloride", "Hydrogen.bromide", "Chlorine.dioxide", 
                                                "Pyridine", "alpha.Methylstyrene", "X1.3.Butadiene", "Dimethylvinylchlorosilane", 
                                                "Acetic.acid"))
myfiles.subset.UTC.box = myfiles.subset.UTC.box %>%
  group_by(Date, Treatment) 
str(myfiles.subset.UTC.box)

myfiles.subset.UTC.box$c <- ifelse(myfiles.subset.UTC.box$Treatment <="1A01", 'ASD',
                                ifelse(myfiles.subset.UTC.box$Treatment <="1A27", 'ASD',
                                    ifelse(myfiles.subset.UTC.box$Treatment <="1A35", 'ASD',
                                        ifelse(myfiles.subset.UTC.box$Treatment <="1A60", 'ASD','UTC'))))

colnames(myfiles.subset.UTC.box)[colnames(myfiles.subset.UTC.box)=="Treatment"] <- "Plot"
colnames(myfiles.subset.UTC.box)[colnames(myfiles.subset.UTC.box)=="c"] <- "Treatment"
str(myfiles.subset.UTC.box)


install.packages("ggpubr")

library(ggpubr)
library(tidyr)
long.myfiles.subset.UTC.box <- myfiles.subset.UTC.box %>% gather(Gas_Species, Gas_Concentration_PPM, -c(Date, Treatment, Plot))
long.myfiles.subset.UTC.box
output_path = (paste("C:\\CalcmetResults\\", sep = ""))
write.csv(long.myfiles.subset.UTC.box, row.names=FALSE,
          paste( output_path, "long.myfiles.subset.UTC.box", ".csv", sep="" ))
str(long.myfiles.subset.UTC.box)

#removed.long.myfiles.subset.box $Date <- as.Date(removed.long.myfiles.subset.box $Date, format = "%m/%d/%Y")
#str(removed.long.myfiles.subset.box)
Legend.box=rep(paste0(long.myfiles.subset.UTC.box$Gas_Species), each=1)
Legend.box

voc.box <- ggplot(data = long.myfiles.subset.UTC.box, aes(x=Gas_Species, y=Gas_Concentration_PPM, group=Gas_Species)) + 
  geom_boxplot(aes(color=Legend.box)) +
  scale_x_discrete(name = "Gas_Species") +
  #scale_y_discrete(name = "Concentration (ppm)", breaks = seq(0,200,50)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(color = "red")) +
  theme(axis.title.x = element_text(color = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="bottom") +
  ggtitle("Gas Concentrations During ASD")
voc.box
ggarrange(voc.box,
          labels = c("A"),
          ncol = 1, nrow = 1)



#Single gas plot using "wide" data
#ggplot(data=mean.myfiles.subset, aes(x=Date, y=Phosphine_mean, group=1)) +
  #geom_line()+
  #geom_point()

#Framework to create a function to process data.
processFile <- function(f) {
  df <- read.csv(f)
  # ...and do stuff...
  file.info(f)$size # dummy result
}

# Find all .csv files
files <- dir("C:/CalcmetResults", recursive=TRUE, full.names=FALSE, pattern="\\.csv$")

# Apply the function to all files.
result <- sapply(files, processFile)
result



