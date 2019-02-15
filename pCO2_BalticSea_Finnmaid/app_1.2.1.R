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
   
   fluidRow(
   column(3,
   checkboxInput('pCO2_mean', 'pCO2 Mean', value = TRUE),
   checkboxInput('temp_mean', 'Temp Mean', value = TRUE),
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
        plotlyOutput("CheckboxPlots"),
        textOutput("ValuesPerPoint")
        )
      )
    )
  )
    
# 04: Server function ---------------------------------------------------------
# Define server logic required to draw mapPlot and scatterPlot for mainpanel
server <- function(input, output) {
  
# 04a: Output Values per point ------------------------------------------------  
  
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
  
# 04b: Output Map Plot ------------------------------------------------ 
 
output$mapPlot <- renderPlot({

     ggplot() + 
      coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
      geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
      geom_path(data= routeE,aes(x= routeE$Lon, y= routeE$Lat, colour="darkblue"))+
      geom_path(data= routeW,aes(x= routeW$Lon, y= routeW$Lat, colour="red") )+
      geom_path(data= routeG,aes(x= routeG$Lon, y= routeG$Lat, colour = "green"))+
      geom_path(data= routeP,aes(x= routeP$Lon, y= routeP$Lat, colour = "yellow"))+
      geom_path(data= routeS,aes(x= routeS$Lon, y= routeS$Lat, colour = "black"))+
      scale_color_discrete(name = "Routes", labels = c("route E", "route W", "route G", "route P", "route S"))+
      geom_rect(data=data.frame(),mapping = aes(xmin= input$lon_low, xmax = input$lon_high, ymin=input$lat_low, ymax= input$lat_high), fill= "blue", alpha = 0.2
                )+
      labs(x="Longitude (°E)", y="Latitude (°N)", size = 2)+
      theme_minimal()
    })

# 04b: Output Checkbox Plots ------------------------------------------------


# 04b1: Datamanagment ------------------------------------------------


  ###subplots pCO2
  
mydata <<- reactive({

  Sub_pCO2 <- df %>% 
    filter(date >=input$daterange[1] & date <=input$daterange[2] & 
             Lon >= input$lon_low & Lon <= input$lon_high &
             Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
  dplyr::select(date,Lon,Lat,pCO2,Tem,ID)
    
  df.sub.mean.pCO2 <- Sub_pCO2 %>% 
    group_by(ID) %>% 
    summarise_all(funs(mean, "mean", mean(., na.rm = FALSE))) %>% 
  dplyr::select(date_mean, pCO2_mean)
  
  df.sub.min.max.pCO2 <- Sub_pCO2 %>% 
    group_by(ID) %>% 
    summarise_if(is.numeric, funs(min,max), na.rm = FALSE) %>% 
  dplyr::select(pCO2_min, pCO2_max)
  
  df.sub.sd.pCO2<- Sub_pCO2 %>% 
    group_by(ID) %>% 
    summarise_all(funs(sd, "sd"), na.rm = FALSE) %>% 
  dplyr::select(pCO2_sd)
  
  df.sub.pCO2<<-bind_cols(df.sub.mean.pCO2,df.sub.min.max.pCO2, df.sub.sd.pCO2)
  

  
  
  ###subplot Temperature
  
    Sub_temp <- df %>% 
    filter(date >=input$daterange[1] & date <=input$daterange[2] & 
             Lon >= input$lon_low & Lon <= input$lon_high &
             Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
    dplyr::select(date,Lon,Lat,pCO2,Tem,ID)
  
  df.sub.mean.temp <- Sub_temp %>% 
    group_by(ID) %>% 
    summarise_all(funs(mean, "mean", mean(., na.rm = FALSE))) %>% 
  dplyr::select(date_mean, Tem_mean)
  
  df.sub.min.max.temp <- Sub_temp %>% 
    group_by(ID) %>% 
    summarise_if(is.numeric, funs(min,max), na.rm = FALSE) %>% 
  dplyr::select(Tem_min, Tem_max)
  
  df.sub.sd.temp<- Sub_temp %>% 
    group_by(ID) %>% 
    summarise_all(funs(sd, "sd"), na.rm = FALSE) %>% 
  dplyr::select(Tem_sd)
  
  
  df.sub.temp<<-bind_cols(df.sub.mean.temp,df.sub.min.max.temp, df.sub.sd.temp)
  df.sub.temp$date_mean<<-as.Date(df.sub.temp$date_mean)
  
  ## information for axis titles
  a <<- list(
    title = "Date",
    showticklabels = TRUE)
  b<<- list (
    title = "pCO2[µatm]",
    showticklabels = TRUE)
  g<<- list(
    title = "Temperature [°C]",
    showticklabels = TRUE)
  d<<- list(
    title = "Salinity",
    showticklabels = TRUE
  )
  e<<-list(
    title = "CH4",
    showticklabels = TRUE
  )
  f<<- list(
    title= "O2",
    showticklabels = TRUE
  )

})    

  # 04b2: Checkbox Plots ------------------------------------------------


# Plot Mean pCO2

myplots<-reactive({
p1<-plot_ly(df.sub.pCO2, name = "Mean pCO2") %>% 
   add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$pCO2_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
            text = ~paste('</br> Date', df.sub.pCO2$date_mean,
                          '</br> Mean: ',  round(df.sub.pCO2$pCO2_mean, digits= 2) ,
                          '</br> Max: ', round(df.sub.pCO2$pCO2_max, digits = 2),
                          '</br> Min: ', round(df.sub.pCO2$pCO2_min, digits = 2))) %>% 
   layout(xaxis= a, yaxis = b)
# Plot Mean Temperature
p2<-plot_ly(df.sub.mean.temp, name = "Temperature") %>% 
  
   add_trace(x= df.sub.temp$date_mean, y= df.sub.temp$Tem_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
            text = ~paste('</br> Date', df.sub.temp$date_mean,
                          '</br> Mean: ',  round(df.sub.temp$Tem_mean, digits= 2) ,
                          '</br> Max: ', round(df.sub.temp$Tem_max, digits = 2),
                          '</br> Min: ', round(df.sub.temp$Tem_min, digits = 2))) %>% 
  layout(xaxis= a, yaxis = g)
# # Plot Mean Salinity
# p3<- plot_ly(df.sub.mean.sal, name = "Mean Salinity") %>% 
#   add_trace(x= df.sub.sal$date_mean, y= df.sub.sal$sal_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.sal$date_mean,
#                           '</br> Mean: ',  round(df.sub.sal$sal_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.sal$sal_max, digits = 2),
#                           '</br> Min: ', round(df.sub.sal$sal_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = d)
# # Plot Mean CH4
# p4<- plot_ly(df.sub.mean.ch4, name = "Mean CH4") %>% 
#   add_trace(x= df.sub.ch4$date_mean, y= df.sub.ch4$ch4_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.ch4$date_mean,
#                           '</br> Mean: ',  round(df.sub.ch4$ch4_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.ch4$ch4_max, digits = 2),
#                           '</br> Min: ', round(df.sub.ch4$ch4_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = e)
# # Plot Mean O2
# p5<- plot_ly(df.sub.mean.o2, name = "Mean O2") %>% 
#   add_trace(x= df.sub.o2$date_mean, y= df.sub.o2$o2_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.o2$date_mean,
#                           '</br> Mean: ',  round(df.sub.o2$o2_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.o2$o2_max, digits = 2),
#                           '</br> Min: ', round(df.sub.o2$o2_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = f)
# Plot Min  pCO2
p6<- plot_ly(df.sub.pCO2, name = "Min pCO2") %>%
  add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$pCO2_min,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
            text = ~paste('</br> Date', df.sub.pCO2$date_mean,
                          '</br> Mean: ',  round(df.sub.pCO2$pCO2_mean, digits= 2) ,
                          '</br> Max: ', round(df.sub.pCO2$pCO2_max, digits = 2),
                          '</br> Min: ', round(df.sub.pCO2$pCO2_min, digits = 2))) %>%
  layout(xaxis= a, yaxis = b)
# Plot Min  Tempreature
p7<-plot_ly(df.sub.temp, name = "Min Temperature") %>%

  add_trace(x= df.sub.temp$date_mean, y= df.sub.temp$Tem_min,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
            text = ~paste('</br> Date', df.sub.temp$date_mean,
                          '</br> Mean: ',  round(df.sub.temp$Tem_mean, digits= 2) ,
                          '</br> Max: ', round(df.sub.temp$Tem_max, digits = 2),
                          '</br> Min: ', round(df.sub.temp$Tem_min, digits = 2))) %>%
  layout(xaxis= a, yaxis = g)
# # Plot Min Salinity
# p8<- plot_ly(df.sub.mean.sal, name = "Min Salinity") %>% 
#   add_trace(x= df.sub.sal$date_mean, y= df.sub.sal$sal_min,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.sal$date_mean,
#                           '</br> Mean: ',  round(df.sub.sal$sal_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.sal$sal_max, digits = 2),
#                           '</br> Min: ', round(df.sub.sal$sal_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = d)
# # Plot Min CH4
# p9<- plot_ly(df.sub.mean.ch4, name = "Min CH4") %>% 
#   add_trace(x= df.sub.ch4$date_mean, y= df.sub.ch4$ch4_min,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.ch4$date_mean,
#                           '</br> Mean: ',  round(df.sub.ch4$ch4_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.ch4$ch4_max, digits = 2),
#                           '</br> Min: ', round(df.sub.ch4$ch4_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = e)
# # Plot Min O2
# p10<- plot_ly(df.sub.mean.o2, name = "Min O2") %>% 
#   add_trace(x= df.sub.o2$date_mean, y= df.sub.o2$o2_min,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.o2$date_mean,
#                           '</br> Mean: ',  round(df.sub.o2$o2_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.o2$o2_max, digits = 2),
#                           '</br> Min: ', round(df.sub.o2$o2_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = f)
# # Plot Max  pCO2
# p11<- plot_ly(df.sub.pCO2, name = "Max pCO2") %>% 
#   add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$pCO2_max,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.pCO2$date_mean,
#                           '</br> Mean: ',  round(df.sub.pCO2$pCO2_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.pCO2$o2_max, digits = 2),
#                           '</br> Min: ', round(df.sub.pCO2$pCO2_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = b)
# # Plot Max  Tempreature
# p12<-plot_ly(df.sub.temp, name = " MaxTemperature") %>% 
#   
#   add_trace(x= df.sub.temp$date_mean, y= df.sub.temp$Tem_max,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.temp$date_mean,
#                           '</br> Mean: ',  round(df.sub.temp$Tem_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.temp$Tem_max, digits = 2),
#                           '</br> Min: ', round(df.sub.temp$Tem_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = g)
# # Plot Max Salinity
# p13<- plot_ly(df.sub.mean.sal, name = "Max Salinity") %>% 
#   add_trace(x= df.sub.sal$date_mean, y= df.sub.sal$sal_max,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.sal$date_mean,
#                           '</br> Mean: ',  round(df.sub.sal$sal_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.sal$sal_max, digits = 2),
#                           '</br> Min: ', round(df.sub.sal$sal_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = d)
# # Plot Max CH4
# p14<- plot_ly(df.sub.mean.ch4, name = "Max CH4") %>% 
#   add_trace(x= df.sub.ch4$date_mean, y= df.sub.ch4$ch4_max,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.ch4$date_mean,
#                           '</br> Mean: ',  round(df.sub.ch4$ch4_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.ch4$ch4_max, digits = 2),
#                           '</br> Min: ', round(df.sub.ch4$ch4_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = e)
# # Plot Max O2
# p15<- plot_ly(df.sub.mean.o2, name = "Max O2") %>% 
#   add_trace(x= df.sub.o2$date_mean, y= df.sub.o2$o2_max,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.o2$date_mean,
#                           '</br> Mean: ',  round(df.sub.o2$o2_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.o2$o2_max, digits = 2),
#                           '</br> Min: ', round(df.sub.o2$o2_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = f)
# 
# Plot STD  pCO2
p16<- plot_ly(df.sub.pCO2, name = "SD pCO2") %>%
  add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$pCO2_sd,type= 'scatter', mode= 'line',  hoverinfo = 'text',
            text = ~paste('</br> Date', df.sub.pCO2$date_mean,
                          '</br> Mean: ',  round(df.sub.pCO2$pCO2_mean, digits= 2) ,
                          '</br> Max: ', round(df.sub.pCO2$pCO2_max, digits = 2),
                          '</br> Min: ', round(df.sub.pCO2$pCO2_min, digits = 2))) %>%
  layout(xaxis= a, yaxis = b)
# # Plot STD  Tempreature
# p17<-plot_ly(df.sub.temp, name = "SD Temperature") %>% 
#   
#   add_trace(x= df.sub.temp$date_mean, y= df.sub.temp$Tem_std,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.temp$date_mean,
#                           '</br> Mean: ',  round(df.sub.temp$Tem_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.temp$Tem_max, digits = 2),
#                           '</br> Min: ', round(df.sub.temp$Tem_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = g)
# # Plot STD Salinity
# p18<- plot_ly(df.sub.mean.sal, name = "SD Salinity") %>% 
#   add_trace(x= df.sub.sal$date_mean, y= df.sub.sal$sal_std,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.sal$date_mean,
#                           '</br> Mean: ',  round(df.sub.sal$sal_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.sal$sal_max, digits = 2),
#                           '</br> Min: ', round(df.sub.sal$sal_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = d)
# # Plot STD CH4
# p19<- plot_ly(df.sub.mean.ch4, name = "SD CH4") %>% 
#   add_trace(x= df.sub.ch4$date_mean, y= df.sub.ch4$ch4_std,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.ch4$date_mean,
#                           '</br> Mean: ',  round(df.sub.ch4$ch4_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.ch4$ch4_max, digits = 2),
#                           '</br> Min: ', round(df.sub.ch4$ch4_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = e)
# # Plot STD O2
# p20<- plot_ly(df.sub.mean.o2, name = "SD O2") %>% 
#   add_trace(x= df.sub.o2$date_mean, y= df.sub.o2$o2_std,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
#             text = ~paste('</br> Date', df.sub.o2$date_mean,
#                           '</br> Mean: ',  round(df.sub.o2$o2_mean, digits= 2) ,
#                           '</br> Max: ', round(df.sub.o2$o2_max, digits = 2),
#                           '</br> Min: ', round(df.sub.o2$o2_min, digits = 2))) %>% 
#   layout(xaxis= a, yaxis = f)

})

# 04b3: produce plots if checkbox checked ------------------------------------------------

  
   ###produce plot

output$checkboxPlots<- renderPlotly ({
    if (input$pCO2_mean == TRUE)
   { p1 }
   else {NULL}

   if (input$temp_mean == TRUE)
   { p2 }
   else {NULL}
 #  
 #   if (input$sal_mean == TRUE)
 #   { p3 }
 #   else {NULL}
 #  
 #   if (input$ch4_mean == TRUE)
 #   { p4 }
 #   else {NULL}
 #  
 #   if (input$O2_mean == TRUE)
 #   { p5 }
 #   else {NULL}
 #  
 #   if (input$pCO2_min == TRUE)
 #   { p6 }
 #   else {NULL}
 #  
 #   if (input$temp_min == TRUE)
 #   { p7 }
 #   else {NULL}
 #   if (input$sal_min == TRUE)
 #   { p8 }
 #   else {NULL}
 #  
 #   if (input$ch4_min == TRUE)
 #   { p9 }
 #   else {NULL}
 #   if (input$O2_min == TRUE)
 #   { p10 }
 #   else {NULL}
 #  
 #   if (input$pCO2_max == TRUE)
 #   { p11 }
 #   else {NULL}
 #  
 #   if (input$temp_max == TRUE)
 #   { p12 }
 #   else {NULL}
 #  
 #   if (input$sal_max == TRUE)
 #   { p13 }
 #   else {NULL}
 #  
 #   if (input$ch4_max == TRUE)
 #   { p14 }
 #   else {NULL}
 #  
 #   if (input$O2_max == TRUE)
 #   { p15 }
 #   else {NULL}
 #  
 #   if (input$pCO2_sd == TRUE)
 #   { p16 }
 #   else {NULL}
 #  
 #   if (input$temp_sd == TRUE)
 #   { p17 }
 #   else {NULL}
 #   if (input$sal_sd == TRUE)
 #   { p18 }
 #   else {NULL}
 #  
 #   if (input$ch4_sd == TRUE)
 #   { p19 }
 #   else {NULL}
 #   if (input$O2_sd == TRUE)
 #   { p20 }
 # else {NULL}


 })


# 04c: download CSV ------------------------------------------------

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
  filename = function()
    {paste(input$dataset, ".csv", sep = "")},
  content = function(file)
    {write.csv(datasetInput(), file, row.names = FALSE)}
)

} #end server function

# 05: Run the app -----------------------------------------------------------
shinyApp(ui = ui, server = server)

