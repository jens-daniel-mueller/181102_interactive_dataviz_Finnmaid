#load packages

library(data.table)
library(ggplot2)
library(lubridate)


#read complete dataset


setwd("C:/Mueller_Jens_Data/Projects/181102_interactive_datavisualisation_Finnmaid/data")

df <- data.table(read.csv("Finnmaid_all_2003-2018.csv"))
df$date <- ymd_hms(df$date)


#subset data for latitude thresholds

df.sub <- df[Lat > 56 & Lat<56.3]


#calculate mean values for each transect (ID) of the ship

df.sub.mean <- df.sub[,.(
  date = mean(date),
  mean.Sal = mean(Sal, na.rm = TRUE),
  SD.Sal = sd(Sal, na.rm = TRUE),
  mean.Tem = mean(Tem, na.rm = TRUE),
  SD.Tem = sd(Tem, na.rm = TRUE),
  mean.pCO2 = mean(pCO2, na.rm = TRUE),
  SD.pCO2 = sd(pCO2, na.rm = TRUE),
  max.pCO2 = max(pCO2),
  min.pCO2 = min(pCO2)),
  by=.(ID)]



#produce pCO2 vs time plot for subsetted and averages dataset 

ggplot(df.sub.mean, aes(date, mean.pCO2, ymin=min.pCO2, ymax=max.pCO2))+
  geom_ribbon()+
  geom_path()

