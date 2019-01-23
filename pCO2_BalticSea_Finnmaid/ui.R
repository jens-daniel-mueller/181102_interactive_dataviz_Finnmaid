# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# This app shows the pCO2 measurments - in a given time- and coordinate window,
# chosen by the user- over time. Databasis are continous pCO2 measurments from 
# the ferry Finmaid starting in June 2003 until today. 
#
##########################################################################
# 00: load packages -- ---------------------------------------------------------
library(shiny)
library(base)
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(maps)
library(plotly)

# 01: load data -- -------------------------------------------------------------

df <- data.table(read.csv("Finnmaid_all_2003-2018.csv"))
df$date<-as.Date(df$date)

# 02: map attributes  -----------------------------------------------------------

baltic.coastlines <- map_data('world', xlim = c(4, 29), ylim = c(50, 66))
land.colour   <- "grey75"
border.colour <- "grey10"
basemap= baltic.coastlines
xmin= 10
xmax= 31.5
ymin=53.5
ymax=61

# definition of routes to plot in map plot
#I chose one ID for each route (E,W,G,P,S) randomly and saved the corresponding data 
# as individual route subsets 
# This data is used in the depiction of routes in the map as an easy and fast(!) solution
routeE<-df %>% 
  filter(ID == "06-06-22")
routeW<-df %>% 
  filter(ID == "06-07-22")
routeG<-df %>% 
  filter(ID == "20090816")
routeP<-df %>% 
  filter(ID == "20131111")
routeS<-df %>% 
  filter(ID == "20150728")
# 03: define UI --------------------------------------------------------------
ui <- fluidPage(
  
  # Application title
  #titlePanel(title = "Surface water pCO2 of the Central Baltic Sea"),
  titlePanel(title=div(img(src="title.png"), " ")),
  # Sidebar with a slider input for date, lattitude and longitude
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        inputId="daterange",
        label="Select a date range",
        start = as.Date("2003-06-21 17:57:00"),
        end = as.Date("2018-07-16 05:10:55"),
        min= as.Date("2003-06-21 17:57:00"),
        max= as.Date("2018-07-16 05:10:55"),
        format= "yyyy/mm/dd",
        separator="to"),
      numericInput("lon_low", label = "Lower Longitude Limit[decimal degrees]",min= 10, max = 30, value = 19.5),
      numericInput("lon_high", "High Longitude Limit[decimal degrees]:",min= 10, max = 30, value = 21),
      numericInput("lat_low", label = "Lower Lattitude Limit[decimal degrees]",min= 53, max = 60, value = 57.5),
      numericInput("lat_high", "High Lattitude Limit[decimal degrees]:",min= 53, max= 60, value=59),
      submitButton("Apply Change")
    ),
    # Show plots of the data
    mainPanel(
      plotOutput("mapPlot"), 
      plotlyOutput("scatterPlot_pCO2_temp" ),
      textOutput("ValuesPerPoint")
      
    )
  )
)