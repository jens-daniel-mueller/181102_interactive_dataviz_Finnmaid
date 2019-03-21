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

df <- data.table(read.csv("../Finnmaid_all_2003-2018.csv"))
df$date<-as.Date(df$date)
df$route<-as.character(df$route)
# 02: map attributes  -----------------------------------------------------------
baltic.coastlines <- ggplot2::map_data('world')#, xlim = c(4, 29), ylim = c(50, 66))
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
routeall<-rbind(routeE, routeS, routeP, routeG, routeW)
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
                      checkboxInput('pCO2_mean', 'pCO2 Mean'),
                      checkboxInput('temp_mean', 'Temp Mean'),
                      checkboxInput('sal_mean', 'Sal Mean'),
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
             submitButton("Apply Change"),
             img(src="finnmaid.png", width = "100%"),
             selectInput("dataset", "Choose a dataset:",
                         choices = c("pCO2", "Temperature", "Salinity", "CH4", "O2")),
             downloadButton("downloadData", "Download"),
             offset = 1),
      # Show plots of the data
      column(8, 
             plotOutput("mapPlot"),  
             
             tabsetPanel(type = "tabs",
                         tabPanel("Scatterplot",
                                  plotlyOutput("plot_pCO2_mean"),
                                  plotlyOutput("plot_temp_mean"),
                                  plotlyOutput("plot_sal_mean"),
                                  #plotlyOutput("plot_ch4_mean"),
                                  plotlyOutput("plot_o2_mean"),
                                  plotlyOutput("plot_pCO2_min"),
                                  plotlyOutput("plot_temp_min"),
                                  plotlyOutput("plot_sal_min"),
                                  # plotlyOutput("plot_ch4_min"),
                                  plotlyOutput("plot_o2_min"),
                                  plotlyOutput("plot_pCO2_max"),
                                  plotlyOutput("plot_temp_max"),
                                  plotlyOutput("plot_sal_max"),
                                  # plotlyOutput("plot_ch4_max"),
                                  plotlyOutput("plot_o2_max"),
                                  plotlyOutput("plot_pCO2_sd"),
                                  plotlyOutput("plot_temp_sd"),
                                  plotlyOutput("plot_sal_sd"),
                                  # plotlyOutput("plot_ch4_sd"),
                                  plotlyOutput("plot_o2_sd")
                         ),
                         tabPanel("Hovmöller", 
                                  textOutput("restrictions"),
                                  plotOutput("hov_pCO2_mean"),
                                  plotOutput("hov_temp_mean"),
                                  plotOutput("hov_sal_mean"),
                                  #plotOutput("hov_ch4_mean"),
                                  plotOutput("hov_o2_mean"))),
             textOutput("ValuesPerPoint")
      )
    )
  )
)