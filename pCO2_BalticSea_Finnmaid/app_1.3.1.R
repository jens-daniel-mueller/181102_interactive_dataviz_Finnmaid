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
x<-c(0,0)
Hel <- c(24.945831, 60.192059)
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
                         choices = c("Time Series Data", "Hovmoeller Data", "Transect Data")),
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
                         tabPanel("Hovmoeller", 
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
                       plotlyOutput("trans_o2")),
              tabPanel("Table Output",
                       tableOutput("datatable")))
             
      )
    )
  )
)
# 04: Server function ---------------------------------------------------------
# Define server logic required to draw mapPlot and scatterPlot for mainpanel
server <- function(input, output) {
  
  # 04a: Output Values per point ------------------------------------------------  
  output$ValuesPerPoint <- renderText({
    if(input$routeall == TRUE)
      Sub <<- df %>% 
        filter(date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,pCO2,Tem,ID) else {NULL} 
    if (input$routeE == TRUE)
      Sub <<- df %>% 
        filter(route=="E" & date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,pCO2,Tem,ID) else {NULL} 
    if (input$routeW == TRUE)
      Sub <<-rbind(Sub,df %>% 
                     filter(route == "W", date >=input$daterange[1] & date <=input$daterange[2] & 
                              Lon >= input$lon_low & Lon <= input$lon_high &
                              Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                     dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL} 
    if(input$routeS == TRUE)
      Sub <<-rbind(Sub,df %>% 
                     filter(route == "S", date >=input$daterange[1] & date <=input$daterange[2] & 
                              Lon >= input$lon_low & Lon <= input$lon_high &
                              Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                     dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL} 
    if(input$routeP == TRUE)
      Sub <<-rbind(Sub,df %>% 
                     filter(route == "P", date >=input$daterange[1] & date <=input$daterange[2] & 
                              Lon >= input$lon_low & Lon <= input$lon_high &
                              Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                     dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL}
    if(input$routeG == TRUE)
      Sub <<-rbind(Sub,df %>% 
                     filter(route == "G", date >=input$daterange[1] & date <=input$daterange[2] & 
                              Lon >= input$lon_low & Lon <= input$lon_high &
                              Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                     dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL}
    
    
    
    df.sub.mean <<- Sub %>% 
      group_by(ID) %>% 
      summarise_all(funs(mean, "mean", mean(., na.rm = FALSE))) %>% 
      select(date_mean, pCO2_mean)
    
    paste("One datapoint represents one crossing of the ferry 'Finnmaid'. Data gets collected every minute. Depending on your selection a different number of measurments are averaged to represent the crossing.
          The mean number of values per datapoint for your selection is", round((nrow(Sub)/nrow(df.sub.mean))), ".")
  })
  # 04b: Output Map Plot ------------------------------------------------ 
  
  output$mapPlot <- renderPlot({
    if (input$routeall == TRUE)
      
      (p<-ggplot() +
         coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
         geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
         scale_color_discrete(name = "Routes", labels = c("route E", "route W", "route G", "route P", "route S"))+
         #geom_rect(data=data.frame(),mapping = aes(xmin= 15, xmax = 20, ymin=55, ymax=60), fill= "blue", alpha = 0.2
         #)+
         geom_rect(data=data.frame(),mapping = aes(xmin= input$lon_low, xmax = input$lon_high, ymin=input$lat_low, ymax= input$lat_high), fill= "blue", alpha = 0.2
         )+
         labs(x="Longitude (°E)", y="Latitude (°N)", size = 2)+
         theme_minimal()+
         geom_path(data= routeE,aes(x= routeE$Lon, y= routeE$Lat, color = "route E"))+
         geom_path(data= routeW,aes(x= routeW$Lon, y= routeW$Lat, color = "route W")) +
         geom_path(data= routeG,aes(x= routeG$Lon, y= routeG$Lat, color = "route G"))+
         geom_path(data= routeP,aes(x= routeP$Lon, y= routeP$Lat, color ="route P"))+
         geom_path(data= routeS,aes(x= routeS$Lon, y= routeS$Lat, color = "route S"))) else
         {NULL}
    
    if (input$routeall == TRUE)
      (plot(p)) else
      {NULL}
    
    
    if (input$routeE == TRUE)
      (p<-ggplot() +
         coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
         geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
         #geom_rect(data=data.frame(),mapping = aes(xmin= 15, xmax = 20, ymin=55, ymax= 60), fill= "blue", alpha = 0.2)+
         geom_rect(data=data.frame(),mapping = aes(xmin= input$lon_low, xmax = input$lon_high, ymin=input$lat_low, ymax= input$lat_high), fill= "blue", alpha = 0.2
         )+
         labs(x="Longitude (°E)", y="Latitude (°N)", size = 2)+
         theme_minimal()+
         geom_path(data= routeE,aes(x= routeE$Lon, y= routeE$Lat, colour = "route E"))) else  {NULL}
    
    if (input$routeW == TRUE)
      (p<-p +
         geom_path(data= routeW,aes(x= routeW$Lon, y= routeW$Lat, colour="route W"))) else
         {NULL}
    if (input$routeG == TRUE)
      (p<-p +
         geom_path(data= routeG,aes(x= routeG$Lon, y= routeG$Lat, colour = "route G"))) else
         {NULL}
    if (input$routeP == TRUE)
      (p<- p+
         geom_path(data= routeP,aes(x= routeP$Lon, y= routeP$Lat, colour = "route P"))) else
         {NULL}
    if (input$routeS == TRUE)
      (p<-p+
         geom_path(data= routeS,aes(x= routeS$Lon, y= routeS$Lat, colour = "route S")))
    
    if(input$routeall == FALSE)
      (plot(p)) else
      {NULL}
  })
  # 04b: Output Checkbox Plots ------------------------------------------------
  # 04b1: Axis Labels ------------------------------------------------
  ## information for axis titles
  a <- list(title = "Date",
    showticklabels = TRUE)
  b<- list (
    title = "pCO2[µatm]",
    showticklabels = TRUE)
  g<- list(
    title = "Temperature [°C]",
    showticklabels = TRUE)
  d<- list(
    title = "Salinity",
    showticklabels = TRUE)
  e<-list(
    title = "CH4",
    showticklabels = TRUE)
  f<- list(
    title= "O2",
    showticklabels = TRUE)
  # 04b2: Checkbox Plots -----------------------------------------------
  # Plot Mean pCO2
  output$plot_checkbox <- renderPlotly({
    plotlist<- NULL
    if(input$routeall == TRUE)
      Sub_pCO2 <<- df %>% 
        filter(date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,pCO2,Tem,ID,Sal,cO2) else {NULL} 
    if (input$routeE == TRUE)
      Sub_pCO2 <<- df %>% 
        filter(route=="E" & date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,pCO2,Tem,ID, Sal, cO2) else {NULL} 
    if (input$routeW == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "W", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID, Sal, cO2)) else {NULL} 
    if(input$routeS == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "S", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID, Sal, cO2)) else {NULL} 
    if(input$routeP == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "P", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID, Sal, cO2)) else {NULL}
    if(input$routeG == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "G", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID, Sal, cO2)) else {NULL}
    
    df.sub.mean.pCO2 <- Sub_pCO2 %>% 
      group_by(ID) %>% 
      summarise_all(funs(mean, "mean", mean(., na.rm = FALSE))) %>% 
      dplyr::select(ID,date_mean, pCO2_mean, Tem_mean,Sal_mean,cO2_mean) #add ch4_mean here as soon as data is there
    
    df.sub.min.max.pCO2 <- Sub_pCO2 %>% 
      group_by(ID) %>% 
      summarise_if(is.numeric, funs(min,max), na.rm = FALSE) %>% 
      dplyr::select(pCO2_min, pCO2_max, Tem_min, Tem_max, Sal_min, Sal_max, cO2_min, cO2_max)#add ch4_min/max here as soon as data is there
    
    df.sub.sd <- Sub_pCO2 %>% 
      group_by(ID) %>% 
      summarise_if(is.numeric, funs(sd, "sd"), na.rm = FALSE) %>% 
      dplyr::select(pCO2_sd, Tem_sd, Sal_sd,cO2_sd)
    
    
    df.sub.pCO2<<-bind_cols(df.sub.mean.pCO2,df.sub.min.max.pCO2,df.sub.sd)
    
    p1<-plot_ly(df.sub.pCO2, name = "Mean pCO2", height = (400*(length(plotlist))), width = 800) %>% 
      add_trace(x= ~df.sub.pCO2$date_mean, y= ~df.sub.pCO2$pCO2_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                text = ~paste('</br> Mean pCO2',
                              '</br> Date:', df.sub.pCO2$date_mean,
                              '</br> Mean: ',  round(df.sub.pCO2$pCO2_mean, digits= 2) ,
                              '</br> Max: ', round(df.sub.pCO2$pCO2_max, digits = 2),
                              '</br> Min: ', round(df.sub.pCO2$pCO2_min, digits = 2))) %>% 
      layout( xaxis= a, yaxis = b) 
      
    
    if (input$pCO2_mean == TRUE)
      (plotlist<-list(p1)) else
      {plotlist<-plotlist}
 ### PLOT MEAN Temperature ###
    
    p2<-plot_ly(df.sub.pCO2, name = "Mean Temperature", height = (400*(length(plotlist))),  width = 1200) %>% 
      
      add_trace(x= ~df.sub.pCO2$date_mean, y= ~df.sub.pCO2$Tem_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                text = ~paste('</br> Mean Temp',
                              '</br> Date', df.sub.pCO2$date_mean,
                              '</br> Mean: ',  round(df.sub.pCO2$Tem_mean, digits= 2) ,
                              '</br> Max: ', round(df.sub.pCO2$Tem_max, digits = 2),
                              '</br> Min: ', round(df.sub.pCO2$Tem_min, digits = 2))) %>% 
      layout(xaxis = a, yaxis = g, autosize = FALSE)
    
    if (input$temp_mean == TRUE)
      (plotlist<-append(plotlist, list(p2))) else
      {plotlist<-plotlist}
    
### PLOT MEAN SALINITY ### 
  p3<- plot_ly(df.sub.pCO2, name = "Mean Salinity",height = (400*(length(plotlist))), width = 1200) %>% 
      add_trace(x= ~ df.sub.pCO2$date_mean, y= ~ df.sub.pCO2$Sal_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                text = ~paste('</br> Mean Sal',
                              '</br> Date', df.sub.pCO2$date_mean,
                              '</br> Mean: ',  round(df.sub.pCO2$Sal_mean, digits= 2) ,
                              '</br> Max: ', round(df.sub.pCO2$Sal_max, digits = 2),
                              '</br> Min: ', round(df.sub.pCO2$Sal_min, digits = 2))) %>% 
      layout(xaxis= a, yaxis = d,  autosize= FALSE)
    
    if (input$sal_mean == TRUE)
      (plotlist<-append(plotlist, list(p3))) else
      {plotlist<-plotlist}
  
 ### PLOT MEAN CH4 ###
  
# add when data is available
     
### PLOT MEAN O2
     
     p5<- plot_ly(df.sub.pCO2, name = "Mean O2",height = (400*(length(plotlist))),  width = 1200) %>%
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$cO2_mean,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                 text = ~paste('</br> Mean O2',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$cO2_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$cO2_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$cO2_min, digits = 2))) %>%
       layout(xaxis= a, yaxis = f,  autosize= FALSE)
     
     if (input$o2_mean == TRUE)
       (plotlist<-append(plotlist, list(p5))) else
       {plotlist<-plotlist}
    
### PLOT MIN pCO2 ###
     
     p6<- plot_ly(df.sub.pCO2, name = "Min pCO2",height = (400*(length(plotlist))),  width = 1200) %>%
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$pCO2_min,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                 text = ~paste('</br> Min pCO2',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$pCO2_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$pCO2_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$pCO2_min, digits = 2))) %>%
       layout(xaxis= a, yaxis = b,  autosize= FALSE)
     
     if (input$pCO2_min == TRUE)
       (plotlist<-append(plotlist, list(p6))) else
       {plotlist<-plotlist}
     
### PLOT MIN TEMPERATURE ###   
     p7<-plot_ly(df.sub.pCO2, name = "Min Temperature",height = (400*(length(plotlist))),  width = 1200) %>%
       
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$Tem_min,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                 text = ~paste('</br> Min Temp',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$Tem_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$Tem_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$Tem_min, digits = 2))) %>%
       layout(xaxis= a, yaxis = g, autosize = FALSE)
     
     if (input$temp_min == TRUE)
       (plotlist<-append(plotlist, list(p7))) else
       {plotlist<-plotlist}
     
### PLOT MIN SALINITY
     
     p8<- plot_ly(df.sub.pCO2, name = "Min Salinity",height = (400*(length(plotlist))),  width = 1200) %>% 
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$Sal_min,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                 text = ~paste('</br> Min Sal',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$Sal_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$Sal_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$Sal_min, digits = 2))) %>% 
       layout(xaxis= a, yaxis = d, autosize = FALSE)
     
     if (input$sal_min == TRUE)
       (plotlist<-append(plotlist, list(p8))) else
       {plotlist<-plotlist}
     
### PLOT MIN CH4 ###
     
     # fill as soon as data is there
     
### PLOT MIN O2 ###
     p10<- plot_ly(df.sub.pCO2, name = "Min O2",height = (400*(length(plotlist))),  width = 1200) %>% 
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$cO2_min,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                 text = ~paste('</br> Min O2',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$cO2_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$cO2_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$cO2_min, digits = 2))) %>% 
       layout(xaxis= a, yaxis = f, autosize = FALSE)
     
     if (input$o2_min == TRUE)
       (plotlist<-append(plotlist, list(p10))) else
       {plotlist<-plotlist}
     
### PLOT MAX pCO2 ###
     
     p11<- plot_ly(df.sub.pCO2, name = "Max pCO2" ,height = (400*(length(plotlist))),  width = 1200) %>% 
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$pCO2_max,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                 text = ~paste('</br> Max pCO2',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$pCO2_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$pCO2_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$pCO2_min, digits = 2))) %>% 
       layout(xaxis= a, yaxis = b, autosize = FALSE)
     
     if (input$pCO2_max == TRUE)
       (plotlist<-append(plotlist, list(p11))) else
       {plotlist<-plotlist}
     
### PLOT MAX Temperatur ###
     
     p12<-plot_ly(df.sub.pCO2, name = "Max Temperature" ,height = (400*(length(plotlist))),  width = 1200) %>% 
       
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$Tem_max,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                 text = ~paste('</br> Max Temp',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$Tem_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$Tem_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$Tem_min, digits = 2))) %>% 
       layout(xaxis= a, yaxis = g, autosize = FALSE)
     
     if (input$temp_max == TRUE)
       (plotlist<-append(plotlist, list(p12))) else
       {plotlist<-plotlist}
     
### PLOT MAX SALINITY ###
     p13<- plot_ly(df.sub.pCO2, name = "Max Salinity" ,height = (400*(length(plotlist))),  width = 1200) %>% 
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$Sal_max,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                 text = ~paste('</br> Max Sal',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$Sal_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$Sal_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$Sal_min, digits = 2))) %>% 
       layout(xaxis= a, yaxis = d, autosize = FALSE)
     
     if (input$sal_max == TRUE)
       (plotlist<-append(plotlist, list(p13))) else
       {plotlist<-plotlist}
     
### PLOT MAX CH4 ###
     
     # add as soon as data is there
     
### PLOT MAX O2 ###
     
     p15<- plot_ly(df.sub.pCO2, name = "Max O2" ,height = (400*(length(plotlist))),  width = 1200) %>% 
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$cO2_max,type= 'scatter', mode= 'markers',  hoverinfo = 'text',
                 text = ~paste('</br> Max O2',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$cO2_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$cO2_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$cO2_min, digits = 2))) %>% 
       layout(xaxis= a, yaxis = f, autosize = FALSE) 
     
     if (input$o2_max == TRUE)
       (plotlist<-append(plotlist, list(p15))) else
       {plotlist<-plotlist}
     
### PLOT SD pCO2 ###
     
     p16<- plot_ly(df.sub.pCO2, name = "SD pCO2" ,height = (400*(length(plotlist))),  width = 1200) %>%
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$pCO2_sd,type= 'scatter', mode= 'line',  hoverinfo = 'text',
                 text = ~paste('</br> SD pCO2',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$pCO2_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$pCO2_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$pCO2_min, digits = 2))) %>%
       layout(xaxis= a, yaxis = b, autosize = FALSE)
     
     if (input$pCO2_sd == TRUE)
       (plotlist<-append(plotlist, list(p16))) else
       {plotlist<-plotlist}
     
### PLOT SD TEMPRATURE ###
     
     p17<-plot_ly(df.sub.pCO2, name = "SD Temperature" ,height = (400*(length(plotlist))),  width = 1200) %>% 
       
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$Tem_sd,type= 'scatter', mode= 'line',  hoverinfo = 'text',
                 text = ~paste('</br> SD Temp',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$Tem_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$Tem_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$Tem_min, digits = 2))) %>% 
       layout(xaxis= a, yaxis = g, autosize = FALSE)
     
     if (input$temp_sd == TRUE)
       (plotlist<-append(plotlist, list(p17))) else
       {plotlist<-plotlist}

### PLOT SD SALINITY ###
     
      p18<- plot_ly(df.sub.pCO2, name = "SD Salinity" ,height = (400*(length(plotlist))),  width = 1200) %>% 
       add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$Sal_sd,type= 'scatter', mode= 'line',  hoverinfo = 'text',
                 text = ~paste('</br> SD Sal',
                               '</br> Date', df.sub.pCO2$date_mean,
                               '</br> Mean: ',  round(df.sub.pCO2$Sal_mean, digits= 2) ,
                               '</br> Max: ', round(df.sub.pCO2$Sal_max, digits = 2),
                               '</br> Min: ', round(df.sub.pCO2$Sal_min, digits = 2))) %>% 
       layout(xaxis= a, yaxis = d,autosize = FALSE)
      
      if (input$sal_sd == TRUE)
        (plotlist<-append(plotlist, list(p18))) else
        {plotlist<-plotlist} 
### PLOT SD CH4 ###
      
      # add when data is available
      
### PLOT SD O2 ###
      p20<- plot_ly(df.sub.pCO2, name = "SD O2" ,height = (400*(length(plotlist))),  width = 1200) %>% 
        add_trace(x= df.sub.pCO2$date_mean, y= df.sub.pCO2$cO2_sd,type= 'scatter', mode= 'line',  hoverinfo = 'text',
                  text = ~paste('</br> SD O2',
                                '</br> Date', df.sub.pCO2$date_mean,
                                '</br> Mean: ',  round(df.sub.pCO2$cO2_mean, digits= 2) ,
                                '</br> Max: ', round(df.sub.pCO2$cO2_max, digits = 2),
                                '</br> Min: ', round(df.sub.pCO2$cO2_min, digits = 2))) %>% 
        layout(xaxis= a, yaxis = f, autosize = FALSE) 
      
      if (input$o2_sd == TRUE)
        (plotlist<-append(plotlist, list(p20))) else
        {plotlist<-plotlist}
      
    subplot(plotlist, nrows= length(plotlist), shareX = TRUE, titleY = TRUE, titleX = TRUE)
  })
 
  # 04d: CheckboxPlots, Hovmoeller ------------------------------------
#Hovmoeller Plot pCO2 Mean
  
  output$hov_pCO2_mean <- renderPlot({
    
  
    if(input$routeall == TRUE)
    Sub_pCO2 <<- df %>% 
    filter(date >=input$daterange[1] & date <=input$daterange[2] & 
             Lon >= input$lon_low & Lon <= input$lon_high &
             Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
    dplyr::select(date,Lon,Lat,pCO2,Tem,ID) else {NULL} 
  if (input$routeE == TRUE)
    Sub_pCO2 <<- df %>% 
    filter(route=="E" & date >=input$daterange[1] & date <=input$daterange[2] & 
             Lon >= input$lon_low & Lon <= input$lon_high &
             Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
    dplyr::select(date,Lon,Lat,pCO2,Tem,ID) else {NULL} 
  if (input$routeW == TRUE)
    Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                        filter(route == "W", date >=input$daterange[1] & date <=input$daterange[2] & 
                                 Lon >= input$lon_low & Lon <= input$lon_high &
                                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                        dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL} 
  if(input$routeS == TRUE)
    Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                        filter(route == "S", date >=input$daterange[1] & date <=input$daterange[2] & 
                                 Lon >= input$lon_low & Lon <= input$lon_high &
                                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                        dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL} 
  if(input$routeP == TRUE)
    Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                        filter(route == "P", date >=input$daterange[1] & date <=input$daterange[2] & 
                                 Lon >= input$lon_low & Lon <= input$lon_high &
                                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                        dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL}
  if(input$routeG == TRUE)
    Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                        filter(route == "G", date >=input$daterange[1] & date <=input$daterange[2] & 
                                 Lon >= input$lon_low & Lon <= input$lon_high &
                                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                        dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL}
  

  
  #Sub_pCO2$date <- as.POSIXct(strptime(Sub_pCO2$date, format="%Y-%m-%d %H:%M:%S", tz="GMT"))

 
  cut_pCO2<<-Sub_pCO2
  #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= 55, to = 60, by=0.02), labels = seq(from= 55+0.02, to= 60, by=0.02))
  #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= input$lat_low, to = input$lat_high, by=0.02), labels = seq(from= input$lat_low+0.02, to= input$lat_high, by=0.02))
  cut_pCO2$dist.Hel<<-distGeo(cbind(Sub_pCO2$Lon, Sub_pCO2$Lat), Hel)/1e3
  cut_pCO2$dist.Hel.int<<-cut(cut_pCO2$dist.Hel, seq(0, 1200, 50), labels =
                               seq(25, 1175, 50))
  
  cut_pCO2$week <<- cut(cut_pCO2$date, breaks="weeks")
  cut_pCO2$week <<- as.Date(cut_pCO2$week, tz="GMT")
  
  
  cut_pCO2_mean<<-cut_pCO2 %>% 
    dplyr::select(dist.Hel.int, week, pCO2) %>% 
    group_by(dist.Hel.int, week) %>% 
    summarise_all(funs(mean, "mean", mean(.,na.rm = FALSE))) %>% 
    as.data.frame() 
  
  cut_pCO2_mean$pCO2_mean<<-cut_pCO2_mean$mean
  cut_pCO2_mean$dist.Hel.int<<-as.numeric(as.character(cut_pCO2_mean$dist.Hel.int))
  
  cut_pCO2_mean$week<<-as.POSIXct(cut_pCO2_mean$week)
  
  hov1 <-
    ggplot(data= cut_pCO2_mean)+
    geom_raster(aes(week, dist.Hel.int, fill= pCO2_mean))+
    scale_fill_gradientn(colours=c("#fc8d59","#ffffbf","#91bfdb"), name=expression("pCO2[µatm]"))+
   
    #until here working well and looking good
    #xlim(as.POSIXct("2003/1/1"), as.POSIXct("2018/1/1"))+
    geom_vline(xintercept = as.numeric(seq(as.POSIXct("2003/1/1", tz="GMT"), 
                                          as.POSIXct("2018/1/1", tz="GMT"), 
                                          "years")),
               size=1)+
    #scale_color_brewer(palette="Set1", name="pCO2 (µatm)")+
     #                    (name=expression("pCO2[µatm]")+
      #                   limits=c(100, 600))+
    scale_x_datetime(breaks= "1 year", date_labels = "%Y")+
    #scale_x_datetime(breaks = seq(as.POSIXct("2004/1/1", tz="GMT"), as.POSIXct("2016/1/1", tz="GMT"), "2 years"),
     #                expand = c(0,0))+
    scale_y_continuous(breaks = seq(55, 60, 2))+
    labs(y="Distance to Helsinki [km])")+
    theme(
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(1.3, "cm"),
      legend.key.height = unit(0.3, "cm")
    )
  
  
  
  if (input$pCO2_mean == TRUE)
    (hov1) else
    {NULL}
  
  })
#Hovmoeller Plot Temperatur Mean
  output$hov_temp_mean <- renderPlot({
    
    
    if(input$routeall == TRUE)
      Sub_pCO2 <<- df %>% 
        filter(date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,pCO2,Tem,ID) else {NULL} 
    if (input$routeE == TRUE)
      Sub_pCO2 <<- df %>% 
        filter(route=="E" & date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,pCO2,Tem,ID) else {NULL} 
    if (input$routeW == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "W", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL} 
    if(input$routeS == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "S", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL} 
    if(input$routeP == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "P", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL}
    if(input$routeG == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "G", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,Sal,cO2,ID)) else {NULL}
    
    
    
    #Sub_pCO2$date <- as.POSIXct(strptime(Sub_pCO2$date, format="%Y-%m-%d %H:%M:%S", tz="GMT"))
    
   
    cut_temp<-Sub_pCO2
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= 55, to = 60, by=0.02), labels = seq(from= 55+0.02, to= 60, by=0.02))
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= input$lat_low, to = input$lat_high, by=0.02), labels = seq(from= input$lat_low+0.02, to= input$lat_high, by=0.02))
    cut_temp$dist.Hel<-distGeo(cbind(Sub_pCO2$Lon, Sub_pCO2$Lat), Hel)/1e3
    cut_temp$dist.Hel.int<-cut(cut_temp$dist.Hel, seq(0, 1200, 50), labels =
                                 seq(25, 1175, 50))
    
    cut_temp$week <- cut(cut_temp$date, breaks="weeks")
    cut_temp$week <- as.Date(cut_temp$week, tz="GMT")
    
    
    cut_temp_mean<-cut_temp %>% 
      dplyr::select(dist.Hel.int, week, Tem) %>% 
      group_by(dist.Hel.int, week) %>% 
      summarise_all(funs(mean, "mean", mean(.,na.rm = FALSE))) %>% 
      as.data.frame() 
    
    cut_temp_mean$Tem_mean<-cut_temp_mean$mean
    cut_temp_mean$dist.Hel.int<-as.numeric(as.character(cut_temp_mean$dist.Hel.int))
    
    cut_temp_mean$week<-as.POSIXct(cut_temp_mean$week)
    
    hov2 <-
      ggplot(data= cut_temp_mean)+
      geom_raster(aes(week, dist.Hel.int, fill= Tem_mean))+
      scale_fill_gradientn(colours=c("#fc8d59","#ffffbf","#91bfdb"), name=expression("Temperature [°C]"))+
      
      #until here working well and looking good
      #xlim(as.POSIXct("2003/1/1"), as.POSIXct("2018/1/1"))+
      geom_vline(xintercept = as.numeric(seq(as.POSIXct("2003/1/1", tz="GMT"), 
                                             as.POSIXct("2018/1/1", tz="GMT"), 
                                             "years")),
                 size=1)+
      #scale_color_brewer(palette="Set1", name="pCO2 (µatm)")+
      #                    (name=expression("pCO2[µatm]")+
      #                   limits=c(100, 600))+
      scale_x_datetime(breaks= "1 year", date_labels = "%Y")+
      #scale_x_datetime(breaks = seq(as.POSIXct("2004/1/1", tz="GMT"), as.POSIXct("2016/1/1", tz="GMT"), "2 years"),
      #                expand = c(0,0))+
      scale_y_continuous(breaks = seq(55, 60, 2))+
      labs(y="Distance to Helsinki [km])")+
      theme(
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.3, "cm"),
        legend.key.height = unit(0.3, "cm")
      )
    
    
    
    if (input$temp_mean == TRUE)
      (hov2) else
      {NULL}
    
  })
  
  
#Hovmoeller Plot Salinity Mean
  output$hov_sal_mean <- renderPlot({
    
    
    if(input$routeall == TRUE)
      Sub_pCO2 <- df %>% 
        filter(date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,ID, Sal) else {NULL} 
    if (input$routeE == TRUE)
      Sub_pCO2 <- df %>% 
        filter(route=="E" & date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,ID, Sal) else {NULL} 
    if (input$routeW == TRUE)
      Sub_pCO2 <-rbind(Sub_pCO2,df %>% 
                          filter(route == "W", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,ID, Sal)) else {NULL} 
    if(input$routeS == TRUE)
      Sub_pCO2 <-rbind(Sub_pCO2,df %>% 
                          filter(route == "S", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,ID, Sal)) else {NULL} 
    if(input$routeP == TRUE)
      Sub_pCO2 <-rbind(Sub_pCO2,df %>% 
                          filter(route == "P", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,ID, Sal)) else {NULL}
    if(input$routeG == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "G", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,ID, Sal)) else {NULL}
    
    
    
    #Sub_pCO2$date <- as.POSIXct(strptime(Sub_pCO2$date, format="%Y-%m-%d %H:%M:%S", tz="GMT"))
    
    
    cut_sal<-Sub_pCO2
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= 55, to = 60, by=0.02), labels = seq(from= 55+0.02, to= 60, by=0.02))
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= input$lat_low, to = input$lat_high, by=0.02), labels = seq(from= input$lat_low+0.02, to= input$lat_high, by=0.02))
    cut_sal$dist.Hel<-distGeo(cbind(cut_sal$Lon, cut_sal$Lat), Hel)/1e3
    cut_sal$dist.Hel.int<-cut(cut_sal$dist.Hel, seq(0, 1200, 50), labels =
                                 seq(25, 1175, 50))
    
    cut_sal$week <- cut(cut_sal$date, breaks="weeks")
    cut_sal$week <- as.Date(cut_sal$week, tz="GMT")
    
    
    cut_sal_mean<-cut_sal %>% 
      dplyr::select(dist.Hel.int, week, Sal) %>% 
      group_by(dist.Hel.int, week) %>% 
      summarise_all(funs(mean, "mean", mean(.,na.rm = FALSE))) %>% 
      as.data.frame() 
    
    cut_sal_mean$Sal_mean<-cut_sal_mean$mean
    cut_sal_mean$dist.Hel.int<-as.numeric(as.character(cut_sal_mean$dist.Hel.int))
    
    cut_sal_mean$week<-as.POSIXct(cut_sal_mean$week)
    
    hov3 <-
      ggplot(data= cut_sal_mean)+
      geom_raster(aes(week, dist.Hel.int, fill= Sal_mean))+
      scale_fill_gradientn(colours=c("#fc8d59","#ffffbf","#91bfdb"), name=expression("Salinity[‰]"))+
      
      #until here working well and looking good
      #xlim(as.POSIXct("2003/1/1"), as.POSIXct("2018/1/1"))+
      geom_vline(xintercept = as.numeric(seq(as.POSIXct("2003/1/1", tz="GMT"), 
                                             as.POSIXct("2018/1/1", tz="GMT"), 
                                             "years")),
                 size=1)+
      #scale_color_brewer(palette="Set1", name="pCO2 (µatm)")+
      #                    (name=expression("pCO2[µatm]")+
      #                   limits=c(100, 600))+
      scale_x_datetime(breaks= "1 year", date_labels = "%Y")+
      #scale_x_datetime(breaks = seq(as.POSIXct("2004/1/1", tz="GMT"), as.POSIXct("2016/1/1", tz="GMT"), "2 years"),
      #                expand = c(0,0))+
      scale_y_continuous(breaks = seq(0, 600, 50))+
      labs(y="Distance to Helsinki [km])")+
      theme(
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.3, "cm"),
        legend.key.height = unit(0.3, "cm")
      )
    
    
    
    if (input$sal_mean == TRUE)
      (hov3) else
      {NULL}
    
  })
  
  
  
#HovmÃÂ¶ller Plot CH4 Mean
  
  
  
#HovmÃÂ¶ller Plot O2
  output$hov_o2_mean <- renderPlot({
    
    
    if(input$routeall == TRUE)
      Sub_pCO2 <<- df %>% 
        filter(date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,pCO2,Tem,ID,cO2, Sal) else {NULL} 
    if (input$routeE == TRUE)
      Sub_pCO2 <<- df %>% 
        filter(route=="E" & date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,pCO2,Tem,ID, cO2, Sal) else {NULL} 
    if (input$routeW == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "W", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID, cO2, Sal)) else {NULL} 
    if(input$routeS == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "S", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID, cO2, Sal)) else {NULL} 
    if(input$routeP == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "P", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID, cO2, Sal)) else {NULL}
    if(input$routeG == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          filter(route == "G", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,Sal,cO2,ID)) else {NULL}
    
    
    
    #Sub_pCO2$date <- as.POSIXct(strptime(Sub_pCO2$date, format="%Y-%m-%d %H:%M:%S", tz="GMT"))
    
    
    cut_o2<<-Sub_pCO2
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= 55, to = 60, by=0.02), labels = seq(from= 55+0.02, to= 60, by=0.02))
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= input$lat_low, to = input$lat_high, by=0.02), labels = seq(from= input$lat_low+0.02, to= input$lat_high, by=0.02))
    cut_o2$dist.Hel<-distGeo(cbind(cut_o2$Lon, cut_o2$Lat), Hel)/1e3
    cut_o2$dist.Hel.int<-cut(cut_o2$dist.Hel, seq(0, 1200, 50), labels =
                                seq(25, 1175, 50))
    
    cut_o2$week <- cut(cut_o2$date, breaks="weeks")
    cut_o2$week <- as.Date(cut_o2$week, tz="GMT")
    
    
    cut_o2_mean<-cut_o2 %>% 
      dplyr::select(dist.Hel.int, week, cO2) %>% 
      group_by(dist.Hel.int, week) %>% 
      summarise_all(funs(mean, "mean", mean(.,na.rm = FALSE))) %>% 
      as.data.frame() 
    
    cut_o2_mean$Sal_mean<-cut_o2_mean$mean
    cut_o2_mean$dist.Hel.int<-as.numeric(as.character(cut_o2_mean$dist.Hel.int))
    
    cut_o2_mean$week<-as.POSIXct(cut_o2_mean$week)
    
    hov4 <-
      ggplot(data= cut_o2_mean)+
      geom_raster(aes(week, dist.Hel.int, fill= Sal_mean))+
      scale_fill_gradientn(colours=c("#fc8d59","#ffffbf","#91bfdb"), name=expression("O2[‰]"))+
      #xlim(as.POSIXct("2003/1/1"), as.POSIXct("2018/1/1"))+
      geom_vline(xintercept = as.numeric(seq(as.POSIXct("2003/1/1", tz="GMT"), 
                                             as.POSIXct("2018/1/1", tz="GMT"), 
                                             "years")),
                 size=1)+
      #scale_color_brewer(palette="Set1", name="pCO2 (µatm)")+
      #                    (name=expression("pCO2[µatm]")+
      #                   limits=c(100, 600))+
      scale_x_datetime(breaks= "1 year", date_labels = "%Y")+
      #scale_x_datetime(breaks = seq(as.POSIXct("2004/1/1", tz="GMT"), as.POSIXct("2016/1/1", tz="GMT"), "2 years"),
      #                expand = c(0,0))+
      scale_y_continuous(breaks = seq(55, 60, 2))+
      labs(y="Distance to Helsinki [km])")+
      theme(
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.3, "cm"),
        legend.key.height = unit(0.3, "cm")
      )
    
    
    
    if (input$o2_mean == TRUE)
      (hov4) else
      {NULL}
    
  }) 
  
  
  
  # 04e: Textoutput Hovmoeller, restrictions
  
  output$restrictions <-renderText({
    paste("For Hovmoeller Plots only the options 'pCO2 mean', 'temp mean', 'Sal mean', 'CH4 mean' and ' O2 mean' are available.")
  })
  # 04e: Transect Plots -------------------------------------------------- 

### Transect Plot pCO2 ###
  output$trans_pCO2 <- renderPlotly({
    
    if(input$routeall == TRUE)
    trans_sub <<- df %>% 
        filter(date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,pCO2, Lat, Sal, Tem, cO2) else {NULL} 
    if (input$routeE == TRUE)
      trans_sub <- df %>% 
        filter(route == "E", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,pCO2, Lat, Sal, Tem, cO2) else {NULL} 
    if (input$routeW == TRUE)
      trans_sub <-rbind(trans_sub,df %>% 
                          filter(route == "W", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}  
    if(input$routeS == TRUE)
      trans_sub <-rbind(trans_sub,df %>% 
                          filter(route == "S", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL} 
    if(input$routeP == TRUE)
      trans_sub <-rbind(trans_sub,df %>% 
                          filter(route == "P", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}
    if(input$routeG == TRUE)
      trans_sub <-rbind(trans_sub,df %>% 
                          filter(route == "G", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}
    
    cut_trans<-trans_sub
    cut_trans$dist.Hel<-distGeo(cbind(cut_trans$Lon, cut_trans$Lat), Hel)/1e3
    cut_trans$dist.Hel.int<-cut(cut_trans$dist.Hel, seq(0, 1200, 50), labels =
                               seq(25, 1175, 50))
    #cut_trans$week <- cut(cut_trans$date, breaks="weeks")
    #cut_trans$week <- as.Date(cut_trans$week, tz="GMT")
    
    # irgendwie so wird es funktionieren  
    cut_trans$date<-as.character(cut_trans$date)
    cut_trans<-cut_trans %>% arrange(desc(date))
    
    t1<-cut_trans %>% 
        group_by(date) %>% 
        ggplot(aes(x= dist.Hel, y= pCO2, color = date))+
        geom_line()+
        scale_color_brewer(palette="RdGy", direction = -1)
    
    if (input$pCO2_mean == TRUE)
      (t1) else
      {NULL}
  })
    
### Transect Plot Temperature ###
    
output$trans_temp <- renderPlotly({
      
      if(input$routeall == TRUE)
        trans_sub <<- df %>% 
          filter(date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                   Lon >= input$lon_low & Lon <= input$lon_high &
                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
          dplyr::select(date,Lon,pCO2, Lat, Sal, Tem, cO2) else {NULL} 
      if (input$routeE == TRUE)
        trans_sub <- df %>% 
          filter(route == "E", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                   Lon >= input$lon_low & Lon <= input$lon_high &
                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
          dplyr::select(date,Lon,pCO2, Lat, Sal, Tem, cO2) else {NULL} 
      if (input$routeW == TRUE)
        trans_sub <-rbind(trans_sub,df %>% 
                            filter(route == "W", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                     Lon >= input$lon_low & Lon <= input$lon_high &
                                     Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                            dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}  
      if(input$routeS == TRUE)
        trans_sub <-rbind(trans_sub,df %>% 
                            filter(route == "S", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                     Lon >= input$lon_low & Lon <= input$lon_high &
                                     Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                            dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL} 
      if(input$routeP == TRUE)
        trans_sub <-rbind(trans_sub,df %>% 
                            filter(route == "P", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                     Lon >= input$lon_low & Lon <= input$lon_high &
                                     Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                            dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}
      if(input$routeG == TRUE)
        trans_sub <-rbind(trans_sub,df %>% 
                            filter(route == "G", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                     Lon >= input$lon_low & Lon <= input$lon_high &
                                     Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                            dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}
      
      cut_trans<-trans_sub
      cut_trans$dist.Hel<-distGeo(cbind(cut_trans$Lon, cut_trans$Lat), Hel)/1e3
      cut_trans$dist.Hel.int<-cut(cut_trans$dist.Hel, seq(0, 1200, 50), labels =
                                    seq(25, 1175, 50))
      #cut_trans$week <- cut(cut_trans$date, breaks="weeks")
      #cut_trans$week <- as.Date(cut_trans$week, tz="GMT")
      
      # irgendwie so wird es funktionieren  
      cut_trans$date<-as.character(cut_trans$date)
      cut_trans<-cut_trans %>% arrange(desc(date))
      
      t2<-cut_trans %>% 
        group_by(date) %>% 
        ggplot(aes(x= dist.Hel, y= Tem, color = date))+
        geom_line()+
        scale_color_brewer(palette="RdGy", direction = -1)
      
      if (input$temp_mean == TRUE)
        (t2) else
        {NULL}
      
    })
    
    
### Transect Plot Salinity ###
    
output$trans_sal <- renderPlotly({
        
        if(input$routeall == TRUE)
          trans_sub <<- df %>% 
            filter(date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                     Lon >= input$lon_low & Lon <= input$lon_high &
                     Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
            dplyr::select(date,Lon,pCO2, Lat, Sal, Tem, cO2) else {NULL} 
        if (input$routeE == TRUE)
          trans_sub <- df %>% 
            filter(route == "E", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                     Lon >= input$lon_low & Lon <= input$lon_high &
                     Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
            dplyr::select(date,Lon,pCO2, Lat, Sal, Tem, cO2) else {NULL} 
        if (input$routeW == TRUE)
          trans_sub <-rbind(trans_sub,df %>% 
                              filter(route == "W", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                       Lon >= input$lon_low & Lon <= input$lon_high &
                                       Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                              dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}  
        if(input$routeS == TRUE)
          trans_sub <-rbind(trans_sub,df %>% 
                              filter(route == "S", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                       Lon >= input$lon_low & Lon <= input$lon_high &
                                       Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                              dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL} 
        if(input$routeP == TRUE)
          trans_sub <-rbind(trans_sub,df %>% 
                              filter(route == "P", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                       Lon >= input$lon_low & Lon <= input$lon_high &
                                       Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                              dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}
        if(input$routeG == TRUE)
          trans_sub <-rbind(trans_sub,df %>% 
                              filter(route == "G", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                       Lon >= input$lon_low & Lon <= input$lon_high &
                                       Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                              dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}
        
        cut_trans<-trans_sub
        cut_trans$dist.Hel<-distGeo(cbind(cut_trans$Lon, cut_trans$Lat), Hel)/1e3
        cut_trans$dist.Hel.int<-cut(cut_trans$dist.Hel, seq(0, 1200, 50), labels =
                                      seq(25, 1175, 50))
        #cut_trans$week <- cut(cut_trans$date, breaks="weeks")
        #cut_trans$week <- as.Date(cut_trans$week, tz="GMT")
        
        # irgendwie so wird es funktionieren  
        cut_trans$date<-as.character(cut_trans$date)
        cut_trans<-cut_trans %>% arrange(desc(date))
        
        t3<-cut_trans %>% 
          group_by(date) %>% 
          ggplot(aes(x= dist.Hel, y= Sal, color = date))+
          geom_line()+
          scale_color_brewer(palette="RdGy", direction = -1)
        
        if (input$sal_mean == TRUE)
          (t3) else
          {NULL}
})

### Transect Plot O2 ###

output$trans_o2 <- renderPlotly({
  
  if(input$routeall == TRUE)
    trans_sub <<- df %>% 
      filter(date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
               Lon >= input$lon_low & Lon <= input$lon_high &
               Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
      dplyr::select(date,Lon,pCO2, Lat, Sal, Tem, cO2) else {NULL} 
  if (input$routeE == TRUE)
    trans_sub <- df %>% 
      filter(route == "E", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
               Lon >= input$lon_low & Lon <= input$lon_high &
               Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
      dplyr::select(date,Lon,pCO2, Lat, Sal, Tem, cO2) else {NULL} 
  if (input$routeW == TRUE)
    trans_sub <-rbind(trans_sub,df %>% 
                        filter(route == "W", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                 Lon >= input$lon_low & Lon <= input$lon_high &
                                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                        dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}  
  if(input$routeS == TRUE)
    trans_sub <-rbind(trans_sub,df %>% 
                        filter(route == "S", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                 Lon >= input$lon_low & Lon <= input$lon_high &
                                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                        dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL} 
  if(input$routeP == TRUE)
    trans_sub <-rbind(trans_sub,df %>% 
                        filter(route == "P", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                 Lon >= input$lon_low & Lon <= input$lon_high &
                                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                        dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}
  if(input$routeG == TRUE)
    trans_sub <-rbind(trans_sub,df %>% 
                        filter(route == "G", date >=input$daterange[1] & date <=(input$daterange[1]+30) & 
                                 Lon >= input$lon_low & Lon <= input$lon_high &
                                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                        dplyr::select(date,Lon,Lat,pCO2, Sal, Tem, cO2)) else {NULL}
  
  cut_trans<<-trans_sub
  cut_trans$dist.Hel<<-distGeo(cbind(cut_trans$Lon, cut_trans$Lat), Hel)/1e3
  cut_trans$dist.Hel.int<<-cut(cut_trans$dist.Hel, seq(0, 1200, 50), labels =
                                seq(25, 1175, 50))
  #cut_trans$week <- cut(cut_trans$date, breaks="weeks")
  #cut_trans$week <- as.Date(cut_trans$week, tz="GMT")
  
  # irgendwie so wird es funktionieren  
  cut_trans$date<<-as.character(cut_trans$date)
  cut_trans<<-cut_trans %>% arrange(desc(date))
  
  t5<-cut_trans %>% 
    group_by(date) %>% 
    ggplot(aes(x= dist.Hel, y= cO2, color = date))+
    geom_line()+
    scale_color_brewer(palette="RdGy", direction = -1)
  
  if (input$o2_mean == TRUE)
    (t5) else
    {NULL}

})

  
  
  # 04f: download CSV ------------------------------------------------
  #https://shiny.rstudio.com/articles/download.html  
  
   datasetInput <- reactive({
  
    switch(input$dataset,
           "Time Series Data"=df.sub.pCO2,
           
           "Transect Data"= cut_trans,
           
           "Hovmoeller Data"=cut_pCO2_mean
    )
    })

output$datatable<- renderTable({
  datasetInput()
})

  output$downloadData <- downloadHandler(
    filename = function(){
    paste(input$dataset, ".csv", sep = "")
      },
    content = function(file){
    write.csv(datasetInput(), file, row.names = FALSE)
      }
  )
  

  
  } #end server function
# 05: Run the app -----------------------------------------------------------
shinyApp(ui = ui, server = server)
