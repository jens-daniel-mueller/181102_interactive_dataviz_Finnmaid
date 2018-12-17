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

#df <- data.table(read.csv("../Finnmaid_all_2003-2018.csv"))
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
  titlePanel(title = "Surface water pCO2 of the Central Baltic Sea"),
   
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
        plotlyOutput("scatterPlot_pCO2", width = "100%"),
        plotlyOutput("scatterPlot_temp", width = "100%")
        
      )
   )
)
# 04: Server function ---------------------------------------------------------
# Define server logic required to draw mapPlot and scatterPlot for mainpanel
server <- function(input, output) {
#Output Map 
output$mapPlot <- renderPlot({

     ggplot() + 
      coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
      geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
      geom_path(data= routeE,aes(x= routeE$Lon, y= routeE$Lat))+
      geom_path(data= routeW,aes(x= routeW$Lon, y= routeW$Lat))+
      geom_path(data= routeG,aes(x= routeG$Lon, y= routeG$Lat))+
      geom_path(data= routeP,aes(x= routeP$Lon, y= routeP$Lat))+
      geom_path(data= routeS,aes(x= routeS$Lon, y= routeS$Lat))+
      geom_rect(data=data.frame(),mapping = aes(xmin= input$lon_low, xmax = input$lon_high, ymin=input$lat_low, ymax= input$lat_high), fill= "blue", alpha = 0.2
                )+
      labs(x="Longitude (°E)", y="Latitude (°N)")+
      theme_dark()
    })
#Output ScatterPlot_pCO2
output$scatterPlot_pCO2 <- renderPlotly({
  
  Sub_pCO2 <- df %>% 
    filter(date >=input$daterange[1] & date <=input$daterange[2] & 
             Lon >= input$lon_low & Lon <= input$lon_high &
             Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
  dplyr::select(date,Lon,Lat,pCO2,Tem,ID)
    
  df.sub.mean.pCO2 <- Sub_pCO2 %>% 
    group_by(ID) %>% 
    summarise_all(funs(mean, "mean", mean(., na.rm = FALSE))) %>% 
    select(date_mean, pCO2_mean)
  
  df.sub.min.max.pCO2 <- Sub_pCO2 %>% 
    group_by(ID) %>% 
    summarise_if(is.numeric, funs(min,max), na.rm = FALSE) %>% 
    select(pCO2_min, pCO2_max)
  
  df.sub.pCO2<-bind_cols(df.sub.mean.pCO2,df.sub.min.max.pCO2)
  
  ggplotly(
    ggplot(df.sub.pCO2, mapping= aes(df.sub.pCO2$date_mean, df.sub.pCO2$pCO2_mean))+
      geom_ribbon(mapping= aes(ymin= df.sub.pCO2$pCO2_min, ymax= df.sub.pCO2$pCO2_max),fill = "blue", alpha = 0.2)+
      geom_line(aes(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$pCO2_mean))+
      labs(y="pCO2[µatm]", x="Date")+
     scale_y_continuous(
     labels = scales::number_format(accuracy = 0.1))+
     theme_dark()
          )
       })


#Output scatterPlot_temp
output$scatterPlot_temp <- renderPlotly({
  
  Sub_temp <- df %>% 
    filter(date >=input$daterange[1] & date <=input$daterange[2] & 
             Lon >= input$lon_low & Lon <= input$lon_high &
             Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
    select(date,Lon,Lat,pCO2,Tem,ID)
  
  df.sub.mean.temp <- Sub_temp %>% 
    group_by(ID) %>% 
    summarise_all(funs(mean, "mean", mean(., na.rm = FALSE))) %>% 
    select(date_mean, Tem_mean)
  
  df.sub.min.max.temp <- Sub_temp %>% 
    group_by(ID) %>% 
    summarise_if(is.numeric, funs(min,max), na.rm = FALSE) %>% 
    select(Tem_min, Tem_max)
  
  
  df.sub.temp<-bind_cols(df.sub.mean.temp,df.sub.min.max.temp)
  df.sub.temp$date_mean<-as.Date(df.sub.temp$date_mean)
  
  p<-ggplotly(
  ggplot(df.sub.temp)+
    geom_ribbon(data= data.frame(df.sub.temp), mapping= aes(x= df.sub.temp$date_mean, ymin= df.sub.temp$Tem_min, ymax= df.sub.temp$Tem_max),fill = "blue", alpha = 0.2, na.rm= TRUE)+
    geom_line(aes(x= df.sub.temp$date_mean, y= df.sub.temp$Tem_mean))+
    labs(y= "Temperature[°C]", x="Date")+
    scale_y_continuous(
      labels = scales::number_format(accuracy = 0.01))+
    theme_dark()
         )
  style(p, hoverinfo = "none", traces = 1)
})
}
# 05: Run the app -----------------------------------------------------------
shinyApp(ui = ui, server = server)

