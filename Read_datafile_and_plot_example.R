################################################################################
# R Script for interaktive Plots, Finnmaid Data                                #
# Autor: Jens D. Müller & Lara S. Burchardt                                    #
# Email:                                                                       #
################################################################################

# 00: load packages -- ---------------------------------------------------------
library(data.table)
library(ggplot2)
library(lubridate)
# 01: load data  ---------------------------------------------------------------


#setwd("C:/Mueller_Jens_Data/Projects/181102_interactive_datavisualisation_Finnmaid/data")
setwd("D:/181102_interactive_datavisualisation_Finnmaid")

df <- data.table(read.csv("Finnmaid_all_2003-2018.csv"))
df$date <- ymd_hms(df$date)

# 02: Datamanagment   -----------------------------------------------------------
#subset data for latitude thresholds

df.sub <- df[Lat > 56.2 & Lat<56.3]

#calculate mean values for each transect (ID) of the ship

df.sub.mean <- df.sub[,.(
  date = mean(as.numeric(date)),
  mean.Sal = mean(Sal, na.rm = TRUE),
  SD.Sal = sd(Sal, na.rm = TRUE),
  mean.Tem = mean(Tem, na.rm = TRUE),
  SD.Tem = sd(Tem, na.rm = TRUE),
  mean.pCO2 = mean(pCO2, na.rm = TRUE),
  SD.pCO2 = sd(pCO2, na.rm = TRUE),
  max.pCO2 = max(pCO2),
  min.pCO2 = min(pCO2)),
  by=.(ID)]


# 03: Plots   -------------------------------------------------------------------
#produce pCO2 vs time plot for subsetted and averages dataset 

ggplot(df.sub.mean, aes(date, mean.pCO2, ymin=min.pCO2, ymax=max.pCO2))+
  geom_point()
  #geom_ribbon()
  #geom_path()

