# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# This app shows the pCO2, Temperature, Salinity and cO2 measurments - 
# in a given time- and coordinate window,
# chosen by the user- over time. Databasis are continous pCO2 measurments from 
# the ferry Finmaid starting in June 2003 until today. 
#
##########################################################################
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

#df <- data.table(read.csv("../Finnmaid_all_2019.csv", sep = ","))
#df$date<-lubridate::ymd_hms(df$date)

# 02: map attributes and other values

    #defined in "global.R"

# 03: define UI --------------------------------------------------------------
ui <- fluidPage(
  fluidRow(
    column(9,
           # Application title
           titlePanel(title="Baltic Sea Surface Water Observations on VOS Finnmaid"),
           offset = 3),
    fluidRow( 
      column(3, 
             # Sidebar with a slider input for date, lattitude and longitude
             dateRangeInput(
               inputId="daterange",
               label="Select a date range",
               start = as.Date("2003-06-21 17:57:00"),
               end = as.Date(max(df$date, na.rm = TRUE)),
               min= as.Date("2003-06-21 17:57:00"),
               max= as.Date(max(df$date, na.rm = TRUE)),
               format= "yyyy/mm/dd",
               separator="to"),
             checkboxInput("routeE", "Select route E", value = TRUE),
             checkboxInput("routeW", "Select route W", value = TRUE),
             checkboxInput("routeG", "Select route G", value = TRUE),
             checkboxInput("routeP", "Select route P", value = TRUE),
             numericInput("lat_high", "North. box limit [deg N]",
                          min= 53, max= 60, value=59),
             numericInput("lon_high", "East. box limit [deg E]:",
                          min= 10, max = 30, value = 21),
             numericInput("lat_low", label = "South. box limit [deg N]",
                          min= 53, max = 60, value = 57.5),             
             numericInput("lon_low", label = "West. box limit [deg E]",
                          min= 10, max = 30, value = 19.5),
             submitButton("Apply Change"),
             img(src="blank_space.png", width = "100%"),
             img(src="finnmaid.png", width = "100%"),
             selectInput("dataset", "Choose a dataset:",
                         choices = c("Time Series Data", "Hovmoeller Data", "Transect Data")),
             downloadButton("downloadData", "Download"),
             
             offset = 1),
      # Show plots of the data
      column(8, 
             plotOutput("mapPlot"), 
             
             
             tabsetPanel(type = "tabs",
                         tabPanel("Time Series",
                                  textOutput("ValuesPerPoint"),
                                  plotlyOutput("plot_timeseries", inline = TRUE)
                         ),
                         tabPanel("Hovmoeller", 
                                  plotOutput("hov_pCO2_mean"),
                                  plotOutput("hov_temp_mean"),
                                  plotOutput("hov_sal_mean"),
                                  #plotOutput("hov_ch4_mean"),
                                  plotOutput("hov_o2_mean")
                         ),
                         tabPanel("Transektplots",
                                  plotlyOutput("plot_transect", inline = TRUE, height = 1200)
                         )
             )
      )
    )
  )
)