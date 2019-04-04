# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# This app shows the pCO2 measurments - in a given time- and coordinate window,
# chosen by the user- over time. Databasis are continous pCO2 measurments from 
# the ferry Finmaid starting in June 2003 until today. 
#
##########################################################################
# 00: load packages -- ---------------------------------------------------------
library(shiny)
library(data.table)
library(ggplot2)
library(maps)
library(plotly)
library(base)
library(plyr)
library(dplyr)
library(geosphere)


# 01: load data -- -------------------------------------------------------------

df <- data.table(read.csv("../Finnmaid_all_2003-2018.csv"))
df$date<-as.Date(df$date)
df$route<-as.character(df$route)

# 03: define UI --------------------------------------------------------------
ui <- fluidPage(
  fluidRow(
    column(9,
           # Application title
           titlePanel(title=div(img(src="title.png"), " ")),
           offset = 3),
    fluidRow( 
      column(3, 
             # Sidebar with a slider input for date, lattitude and longitude
             dateRangeInput(
               inputId="daterange",
               label="Select a date range",
               start = as.Date("2003-06-21 17:57:00"),
               end = as.Date("2018-07-16 05:10:55"),
               min= as.Date("2003-06-21 17:57:00"),
               max= as.Date("2018-07-16 05:10:55"),
               format= "yyyy/mm/dd",
               separator="to"),
             checkboxInput("routeall", "all routes", value = TRUE),
             checkboxInput("routeE", "route E"),
             checkboxInput("routeW", "route W"),
             checkboxInput("routeG", "route G"),
             checkboxInput("routeP", "route P"),
             checkboxInput("routeS", "route S"),
             numericInput("lon_low", label = "Lower Longitude Limit[decimal degrees]",min= 10, max = 30, value = 19.5),
             numericInput("lon_high", "High Longitude Limit[decimal degrees]:",min= 10, max = 30, value = 21),
             numericInput("lat_low", label = "Lower Lattitude Limit[decimal degrees]",min= 53, max = 60, value = 57.5),
             numericInput("lat_high", "High Lattitude Limit[decimal degrees]:",min= 53, max= 60, value=59),
             
             fluidRow(
               column(3,
                      checkboxInput('pCO2_mean', 'pCO2 Mean', value = TRUE),
                      checkboxInput('temp_mean', 'Temp Mean', value = TRUE),
                      checkboxInput('sal_mean', 'Sal Mean', value = TRUE),
                      checkboxInput('ch4_mean', ' CH4 Mean'),
                      checkboxInput('o2_mean', 'O2 Mean')
               ),
               column(3,
                      checkboxInput('pCO2_min', 'pCO2 Min'),
                      checkboxInput('temp_min', 'Temp Min'),
                      checkboxInput('sal_min', 'Sal Min'),
                      checkboxInput('ch4_min', ' CH4 Min'),
                      checkboxInput('o2_min', 'O2 Min')
               ),
               column(3,
                      checkboxInput('pCO2_max', 'pCO2 Max'),
                      checkboxInput('temp_max', 'Temp Max'),
                      checkboxInput('sal_max', 'Sal Max'),
                      checkboxInput('ch4_max', ' CH4 Max'),
                      checkboxInput('o2_max', 'O2 Max')
               ),
               column(3,
                      checkboxInput('pCO2_sd', 'pCO2 SD'),
                      checkboxInput('temp_sd', 'Temp SD'),
                      checkboxInput('sal_sd', 'Sal SD'),
                      checkboxInput('ch4_sd', ' CH4 SD'),
                      checkboxInput('o2_sd', 'O2 SD')
               )
             ),
             img(src="finnmaid.png", width = "100%"),
             selectInput("dataset", "Choose a dataset:",
                         choices = c("Time Series Data", "Hovmöller Data", "Transect Data")),
             downloadButton("downloadData", "Download"),
             submitButton("Apply Change"),
             offset = 1),
      # Show plots of the data
      column(8, 
             plotOutput("mapPlot"), 
             textOutput("ValuesPerPoint"),
             
             tabsetPanel(type = "tabs",
                         tabPanel("Time Series",
                                  plotlyOutput("plot_checkbox", inline = TRUE)
                         ),
                         tabPanel("Hovmöller", 
                                  textOutput("restrictions"),
                                  plotOutput("hov_pCO2_mean"),
                                  plotOutput("hov_temp_mean"),
                                  plotOutput("hov_sal_mean"),
                                  #plotOutput("hov_ch4_mean"),
                                  plotOutput("hov_o2_mean")),
                         tabPanel("Transektplots",
                                  plotlyOutput("trans_pCO2"),
                                  plotlyOutput("trans_temp"),
                                  plotlyOutput("trans_sal"),
                                  #plotlyOutput("trans_ch4"),
                                  plotlyOutput("trans_o2")))
             
      )
    )
  )
)