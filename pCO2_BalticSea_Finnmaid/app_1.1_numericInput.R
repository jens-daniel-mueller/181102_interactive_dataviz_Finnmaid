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
         numericInput("lon_low", label = "Lower Longitude Limit[decimal degrees]",min= 10, max = 30, value = 19.5),
         numericInput("lon_high", "High Longitude Limit[decimal degrees]:",min= 10, max = 30, value = 21),
         numericInput("lat_low", label = "Lower Lattitude Limit[decimal degrees]",min= 53, max = 60, value = 57.5),
         numericInput("lat_high", "High Lattitude Limit[decimal degrees]:",min= 53, max= 60, value=59),
         submitButton("Apply Change"),
   
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
   
   
   
         img(src="finnmaid.png", width = "100%"),
         selectInput("dataset", "Choose a dataset:",
                    choices = c("pCO2", "Temperature", "Salinity", "CH4", "O2")),
         downloadButton("downloadData", "Download"),
          offset = 1),
      # Show plots of the data

      column(8, 
        plotOutput("mapPlot"), 
        plotlyOutput("scatterPlot_pCO2_temp" ),
        textOutput("ValuesPerPoint")
        )
      )
    )
  )
    
# 04: Server function ---------------------------------------------------------
# Define server logic required to draw mapPlot and scatterPlot for mainpanel
server <- function(input, output) {
  
  
  output$ValuesPerPoint <- renderText({
    
    Sub <<- df %>% 
      filter(date >=input$daterange[1] & date <=input$daterange[2] & 
               Lon >= input$lon_low & Lon <= input$lon_high &
               Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
      dplyr::select(date,Lon,Lat,pCO2,Tem,ID)
    
    df.sub.mean <<- Sub %>% 
      group_by(ID) %>% 
      summarise_all(funs(mean, "mean", mean(., na.rm = FALSE))) %>% 
      select(date_mean, pCO2_mean)
    
    paste("One datapoint represents one crossing of the ferry 'Finnmaid'. Data gets collected every minute. Depending on your selection a different number of measurments are averaged to represent the crossing.
           The mean number of values per datapoint for your selection is", round((nrow(Sub)/nrow(df.sub.mean))), ".")
    
  })
#Output Map 
output$mapPlot <- renderPlot({

     ggplot() + 
      coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
      geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
      geom_path(data= routeE,aes(x= routeE$Lon, y= routeE$Lat, colour = "blue"))+
      geom_path(data= routeW,aes(x= routeW$Lon, y= routeW$Lat, colour = "red"))+
      geom_path(data= routeG,aes(x= routeG$Lon, y= routeG$Lat, colour = "green"))+
      geom_path(data= routeP,aes(x= routeP$Lon, y= routeP$Lat, color = "yellow"))+
      geom_path(data= routeS,aes(x= routeS$Lon, y= routeS$Lat))+
      geom_rect(data=data.frame(),mapping = aes(xmin= input$lon_low, xmax = input$lon_high, ymin=input$lat_low, ymax= input$lat_high), fill= "blue", alpha = 0.2
                )+
      labs(x="Longitude (°E)", y="Latitude (°N)", size = 2)+
      theme_minimal()
    })
#Output ScatterPlot_pCO2_temp
output$scatterPlot_pCO2_temp <- renderPlotly({
  
  ###subplot pCO2
  
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
  
  df.sub.pCO2<<-bind_cols(df.sub.mean.pCO2,df.sub.min.max.pCO2)
  
  ## information for axis titles
  a <- list(
    title = "Date",
    showticklabels = TRUE)
  b<- list (
    title = "pCO2[µatm]",
    showticklabels = TRUE)
  
  
  
  p1<-plot_ly(df.sub.pCO2, name = "pCO2") %>% 
    
    #add_ribbons(x= df.sub.pCO2$date_mean, ymin= df.sub.pCO2$pCO2_min, ymax= df.sub.pCO2$pCO2_max, name= "Min & Max", alpha = 0.2) %>% 
    add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$pCO2_mean,type= 'scatter', mode= 'markers',hoverinfo = 'text',
              text = ~paste('</br> Date', df.sub.pCO2$date_mean,
                            '</br> Mean: ',  round(df.sub.pCO2$pCO2_mean, digits= 2) ,
                            '</br> Max: ', round(df.sub.pCO2$pCO2_max, digits = 2),
                            '</br> Min: ', round(df.sub.pCO2$pCO2_min, digits = 2))) %>% 
    layout(xaxis= a, yaxis = b)
  
  ###subplot Temperature
  
  
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
  
  
  df.sub.temp<<-bind_cols(df.sub.mean.temp,df.sub.min.max.temp)
  df.sub.temp$date_mean<<-as.Date(df.sub.temp$date_mean)
  
  a <- list(
    title = "Date",
    showticklabels = TRUE)
  b<- list (
    title = "Temperature [°C]",
    showticklabels = TRUE)

  
  p2<-plot_ly(df.sub.temp, name = "Temperature") %>% 
    
    #add_ribbons(x= df.sub.pCO2$date_mean, ymin= df.sub.temp$Tem_min, ymax= df.sub.temp$Tem_max, name= "Min & Max", alpha = 0.2) %>%
    add_trace(x= df.sub.temp$date_mean, y= df.sub.temp$Tem_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
              text = ~paste('</br> Date', df.sub.temp$date_mean,
                            '</br> Mean: ',  round(df.sub.temp$Tem_mean, digits= 2) ,
                            '</br> Max: ', round(df.sub.temp$Tem_max, digits = 2),
                            '</br> Min: ', round(df.sub.temp$Tem_min, digits = 2))) %>% 
    layout(xaxis= a, yaxis = b)

  
  ###produce plot
  if (input$pCO2_mean == TRUE)
  { p1 }
  else {NULL}
  
  if (input$temp_mean == TRUE)
  { p2 }
  else {NULL}
  
  #p<-subplot(p1,p2, nrows= 2, shareX = TRUE, titleY = TRUE) %>% 
   # hide_legend()
       })
#download csv


datasetInput <<- reactive({
  switch(input$dataset,
         "pCO2" = df.sub.pCO2,
         "Temperature" = df.sub.temp #,
         #"Salinity" = df.sub.sal,
         # "CH4" = df.sub.ch4,
         #"O2" = df.sub.o2
         )
})


output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$dataset, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(datasetInput(), file, row.names = FALSE)
  }
)




}
# 05: Run the app -----------------------------------------------------------
shinyApp(ui = ui, server = server)

