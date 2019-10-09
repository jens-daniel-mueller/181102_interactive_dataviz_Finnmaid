# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# This app shows the pCO2, Temperature, Salinity and cO2 measurments - 
# in a given time- and coordinate window,
# chosen by the user- over time. Databasis are continous pCO2 measurments from 
# the ferry Finmaid starting in June 2003 until today. 
#
##########################################################################

#global.R

# 00: load packages -- ---------------------------------------------------------
library(shiny)
library(data.table)
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(plotly)
library(viridis)
library(base)
library(geosphere)

# 01: load data -- -------------------------------------------------------------

df <- readRDS("df.rds")

# 02:attributes  -----------------------------------------------------------

trav<-c(10.8605315,53.9414096)

baltic.coastlines <- ggplot2::map_data('world')
land.colour   <- "grey75"
border.colour <- "grey10"
basemap= baltic.coastlines
xmin= 10
xmax= 31.5
ymin=53.5
ymax=61
# definition of routes to plot in map plot
#I chose one ID for each route (E,W,G,P) randomly and saved the corresponding data 
# as individual route subsets 
# This data is used in the depiction of routes in the map as an easy and fast(!) solution
routeE<-df %>% 
  filter(ID == "06-06-22")
routeW<-df %>% 
  filter(ID == "20190312")
routeG<-df %>% 
  filter(ID == "20120727")
routeP<-df %>% 
  filter(ID == "20160328")

# 03: define ui --------------------------------------------------------------

  #defined in ui.R

# 04: Server function --------------------------------------------------------

  #defined in server.R
