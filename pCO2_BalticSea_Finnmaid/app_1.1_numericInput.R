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
library(plyr)
library(dplyr)
library(maps)
library(plotly)

# 01: load data -- -------------------------------------------------------------

df <- data.table(read.csv("../Finnmaid_all_2003-2018.csv"))
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
#Datamanagment
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
  titlePanel("Surface~water~pCO[2]~the~Central~Baltic~Sea"),
   
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
         numericInput("lon_low", label = "Lower Longitude Limit[decimal degrees]",min= 10, max = 30, value = 10.1),
         numericInput("lon_high", "High Longitude Limit[decimal degrees]:",min= 10, max = 30, value = 30.2),
         numericInput("lat_low", label = "Lower Lattitude Limit[decimal degrees]",min= 53, max = 60, value = 54.1),
         numericInput("lat_high", "High Lattitude Limit[decimal degrees]:",min= 53, max= 60, value=60.2)
      ),
      # Show plots of the data
      mainPanel(
        plotOutput("mapPlot"), 
        plotOutput("scatterPlot_pCO2"),
        plotOutput("scatterPlot_temp"),
        plotOutput("scatterPlot_cO2")
      )
   )
)
# 04: Server function ---------------------------------------------------------
# Define server logic required to draw mapPlot and scatterPlot for mainpanel
server <- function(input, output) {
#Output Map 
output$mapPlot <- renderPlot({
    Sub <- df %>% 
    filter(date >=input$daterange[1] & df$date <=input$daterange[2] & 
             Lon >= input$lon_low & Lon <= input$lon_high &
             Lat >=input$lat_low & Lat <= input$lat_high) %>% 
    dplyr::select(date,Lon,Lat,pCO2,Tem,ID)
    
     ggplot() + 
      coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
      geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
      geom_path(data= routeE,aes(x= routeE$Lon, y= routeE$Lat))+
      geom_path(data= routeW,aes(x= routeW$Lon, y= routeW$Lat))+
      geom_path(data= routeG,aes(x= routeG$Lon, y= routeG$Lat))+
      geom_path(data= routeP,aes(x= routeP$Lon, y= routeP$Lat))+
      geom_path(data= routeS,aes(x= routeS$Lon, y= routeS$Lat))+
      geom_rect(data=data.frame(),mapping = aes(xmin= input$lon_low, xmax = input$lon_high, ymin=input$lat_low, ymax= input$lat_high),color="red", fill=NA)+
      labs(x="Longitude (°E)", y="Latitude (°N)")
    })
#Output ScatterPlot_pCO2
output$scatterPlot_pCO2 <- renderPlot({
  
  Sub <- df %>% 
    filter(date >=input$daterange[1] & df$date <=input$daterange[2] & 
             Lon >= input$lon_low & Lon <= input$lon_high &
             Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
  dplyr::select(date,Lon,Lat,pCO2,Tem,ID)
    
  df.sub.mean.pCO2 <- Sub %>% 
    group_by(ID) %>% 
    summarise_all(funs(mean)) %>% 
    select(date, pCO2)
  
 ggplot(df.sub.mean.pCO2, aes(df.sub.mean.pCO2$date, df.sub.mean.pCO2$pCO2))+
    geom_point()+
    ylim(0,800)
   labs(y=expression(pCO[2]~(µatm)), x="Date")
     })

#Output scatterPlot_temp
output$scatterPlot_temp <- renderPlot({
  
  Sub <- df %>% 
    filter(date >=input$daterange[1] & df$date <=input$daterange[2] & 
             Lon >= input$lon_low & Lon <= input$lon_high &
             Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
  dplyr::select(date,Lon,Lat,pCO2,Tem,ID)
  
  df.sub.mean.temp <- Sub %>% 
    group_by(ID) %>% 
    summarise_all(funs(mean)) %>% 
    select(date, Tem)
  
  ggplot(df.sub.mean.temp, aes(df.sub.mean.temp$date, df.sub.mean.temp$Tem))+
    geom_point()+
    labs(y= "Temperature [°C]", x="Date")
  
})
#Output scatterPlot_cO2
}

# 05: Run the app -----------------------------------------------------------
shinyApp(ui = ui, server = server)

