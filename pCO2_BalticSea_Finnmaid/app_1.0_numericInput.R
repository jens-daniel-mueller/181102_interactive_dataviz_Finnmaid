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
   titlePanel("pCO2 in the Baltic Sea"),
   
   # Sidebar with a slider input for date, lattitude and longitude
   sidebarLayout(
     sidebarPanel(
          dateRangeInput(
          inputId="daterange",
          label="Select a date range",
          start = as.Date("2006-06-21 17:57:00"),
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
         plotOutput("scatterPlot")
      )
   )
)
# 04: Server function ---------------------------------------------------------
# Define server logic required to draw mapPlot and scatterPlot for mainpanel
server <- function(input, output) {
#Output Map 
output$mapPlot <- renderPlot({
  S=  subset(df, df$date >=input$daterange[1] & df$date <=input$daterange[2])
  S= subset (S, S$Lon >= input$lon_low &
               S$Lon <= input$lon_high &
               S$Lat >=input$lat_low &
               S$Lat <= input$lat_high)
     ggplot() + 
      coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
      geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
      geom_path(data= routeE,aes(x= routeE$Lon, y= routeE$Lat))+
      geom_path(data= routeW,aes(x= routeW$Lon, y= routeW$Lat))+
      geom_path(data= routeG,aes(x= routeG$Lon, y= routeG$Lat))+
      geom_path(data= routeP,aes(x= routeP$Lon, y= routeP$Lat))+
      geom_path(data= routeS,aes(x= routeS$Lon, y= routeS$Lat))+
      geom_rect(data = S, mapping = aes(xmin= input$lon_low, xmax = input$lon_high, ymin=input$lat_low, ymax= input$lat_high), alpha= 0, color = "black")
    })
#Output ScatterPlot
output$scatterPlot <- renderPlot({
  S=  subset(df, df$date >=input$daterange[1] & df$date <=input$daterange[2])
  S= subset (S, S$Lon >= input$lon_low &
                S$Lon <= input$lon_high &
                S$Lat >=input$lat_low &
                S$Lat <= input$lat_high)
     df.sub.mean <- S[,.(
     date = mean(date),
     mean.pCO2 = mean(pCO2, na.rm = TRUE),
     max.pCO2 = max(pCO2),
     min.pCO2 = min(pCO2)
     ),
     by=.(ID)]
     # plot 
     ggplot(df.sub.mean, aes(df.sub.mean$date, df.sub.mean$mean.pCO2, ymin=df.sub.mean$min.pCO2, ymax=df.sub.mean$max.pCO2))+
       geom_point()
   })
}

# 05: Run the app -----------------------------------------------------------
shinyApp(ui = ui, server = server)

