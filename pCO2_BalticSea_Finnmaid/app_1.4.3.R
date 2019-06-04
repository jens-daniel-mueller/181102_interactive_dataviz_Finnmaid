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
library(ggmap)
#library(lubridate)

library(maps)
library(mapdata)
library(plotly)
library(viridis)
library(base)
library(geosphere)

library(plyr)
library(dplyr)


# 01: load data -- -------------------------------------------------------------

#df <- data.table(read.csv("Finnmaid_all_2019.csv", sep = ","))
#df$date<-lubridate::ymd_hms(df$date)

df$route<-as.character(df$route)
x<-c(0,0)
trav<-c(10.8605315,53.9414096)
#Hel <- c(24.945831, 60.192059)

# 02: map attributes  -----------------------------------------------------------
#baltic.coastlines<-maps::map(database = "world")#, xlim = c(4, 29), ylim = c(50, 66))
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
  filter(ID == "20190312")
routeG<-df %>% 
  filter(ID == "20120727")
routeP<-df %>% 
  filter(ID == "20160328")

#routeall<-rbind(routeE, routeP, routeG, routeW)

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
               #end = as.Date("2018-07-16 05:10:55"),
               min= as.Date("2003-06-21 17:57:00"),
               max= as.Date(max(df$date, na.rm = TRUE)),
               #max= as.Date("2018-07-16 05:10:55"),
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
                                  plotOutput("hov_o2_mean")),
              tabPanel("Transektplots",
                       plotlyOutput("trans_pCO2"),
                       plotlyOutput("trans_temp"),
                       plotlyOutput("trans_sal"),
                       #plotlyOutput("trans_ch4"),
                       plotlyOutput("trans_o2"))#,
              #tabPanel("Table Output",
                       #tableOutput("datatable"))
              )
      )
    )
  )
)
# 04: Server function ---------------------------------------------------------
# Define server logic required to draw mapPlot and scatterPlot for mainpanel
server <- function(input, output) {

  
# 04a: interactive subset generation ------------------------------------------    
 
   #reactive expression df.sub: generates subset of all datapoints for chosen parameters in app
  df.sub <- reactive({
    
    routelist<- as.vector(NULL)
    
    if (input$routeE == TRUE)
      routelist <- c(routelist, "E") else {NULL} 
    
    if (input$routeW == TRUE)
      routelist <- c(routelist, "W") else {NULL} 
    
    if(input$routeP == TRUE)
      routelist <- c(routelist, "P") else {NULL}
    
    if(input$routeG == TRUE)
      routelist <- c(routelist, "G") else {NULL}
    
    df %>% 
         filter(route %in% routelist &
           date >=input$daterange[1] & date <=input$daterange[2] & 
                  Lon >= input$lon_low & Lon <= input$lon_high &
                  Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
         dplyr::select(date,Lon,Lat,pCO2,Tem,Sal, cO2,ID)
  })
  
  # reactive expression df.sub.timeseries: generates dataframe to build timeseries plots, means of subset as generated by df.sub
  df.sub.timeseries<-reactive ({
    
    sub.df.timeseries<-df.sub()
    
    sub.df.timeseries %>% 
    group_by(ID) %>% 
      summarise_all(list(~mean(.,na.rm = TRUE),
                         ~min(., na.rm= TRUE),
                         ~max(., na.rm= TRUE),
                         ~sd(., na.rm = TRUE))) %>% 
      dplyr::select(ID,date_mean, pCO2_mean, Tem_mean,Sal_mean,
                    cO2_mean,pCO2_min, pCO2_max, Tem_min, Tem_max,
                    Sal_min, Sal_max, cO2_min, cO2_max,
                    pCO2_sd, Tem_sd, Sal_sd,cO2_sd)
  })
  
  #reactive expression df.sub.hov: generates cut dataframe for hovmoeller plots
  
  df.sub.hov<-reactive({
    
    sub.df.hov<-df.sub()
    
    sub.df.hov$dist.trav <- distGeo(cbind(sub.df.hov$Lon, sub.df.hov$Lat), trav) / 1e3
    sub.df.hov$dist.trav.int<-cut(sub.df.hov$dist.trav, seq(0, 1200, 50), labels =
                                     seq(25, 1175, 50))
      
    sub.df.hov$week <- cut(sub.df.hov$date, breaks="weeks")
    sub.df.hov$week <- as.Date(sub.df.hov$week, tz="GMT")
    
    
    sub.df.hov.mean<-sub.df.hov %>% 
      dplyr::select(dist.trav.int, week, pCO2) %>% 
      group_by(dist.trav.int, week) %>% 
      summarise_all(list(~mean(.,na.rm=TRUE))) %>% 
      as.data.frame() 
    
    #sub.df.hov.mean$dist.trav.int<<-as.numeric(as.character(sub.df.hov.mean$dist.trav.int))
    
    sub.df.hov.mean$week<<-as.POSIXct(sub.df.hov.mean$week)
  })
  
  #reactive expression df.sub.transect: generates cut dataframe for transect plots
  df.sub.transect<-reactive({
    
    sub.df.transect<-df.sub()
    
    sub.df.transect$dist.trav<-distGeo(cbind(sub.df.transect$Lon, sub.df.transect$Lat), trav)/1e3
    sub.df.transect$dist.trav.int<-cut(sub.df.transect$dist.trav, seq(0, 1200, 50), labels =
                                  seq(25, 1175, 50))
    
    #sub.df.transect$week <- cut(sub.df.transect$date, breaks="weeks")
    #sub.df.transect$week <- as.Date(sub.df.transect$week, tz="GMT")
    
    sub.df.transect$date<-as.character(lubridate::ymd(sub.df.transect$ID))
    sub.df.transect<-sub.df.transect %>% 
                                    arrange(desc(date))
    
  })
  # 04a: Output Values per point ------------------------------------------------  
  output$ValuesPerPoint <- renderText({
  
    sub.df.timeseries.data<-df.sub.timeseries()   #means
    sub.df<-df.sub()                              #all observations
    
    paste("One datapoint represents one crossing of the ferry 'Finnmaid'. Data gets collected every minute. Depending on your selection a different number of measurments are averaged to represent the crossing.
          The mean number of values per datapoint for your selection is", round((nrow(sub.df)/nrow(sub.df.timeseries.data))), ".")
  })
  # 04b: Output Map Plot ------------------------------------------------ 
  
  output$mapPlot <- renderPlot({
    p<-ggplot() +
      coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
      geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
      #geom_rect(data=data.frame(),mapping = aes(xmin= 15, xmax = 20, ymin=55, ymax= 60), fill= "blue", alpha = 0.2)+
      geom_rect(data=data.frame(), 
                mapping = aes(xmin= input$lon_low, xmax = input$lon_high, 
                              ymin=input$lat_low, ymax= input$lat_high), 
                alpha = 0.2, col="black")+
      labs(x="Longitude [deg E]", y="Latitude [deg N]", size = 2)+
      theme_minimal()+
      scale_color_brewer(palette = "Set1", name="Example Routes")
    
    if (input$routeE == TRUE)
      (p<-p+
         geom_path(data= routeE,aes(x= routeE$Lon, y= routeE$Lat, colour = "E"))) else  {NULL}
    
    if (input$routeW == TRUE)
      (p<-p +
         geom_path(data= routeW,aes(x= routeW$Lon, y= routeW$Lat, colour="W"))) else
         {NULL}
    if (input$routeG == TRUE)
      (p<-p +
         geom_path(data= routeG,aes(x= routeG$Lon, y= routeG$Lat, colour = "G"))) else
         {NULL}
    if (input$routeP == TRUE)
      (p<- p+
         geom_path(data= routeP,aes(x= routeP$Lon, y= routeP$Lat, colour = "P"))) else
         {NULL}
    p

  })
  # 04b: Output Checkbox Plots ------------------------------------------------
  # 04b1: Axis Labels ------------------------------------------------
  ## information for axis titles
  a <- list(title = "Date",
    showticklabels = TRUE)
  b<- list (
    title = "pCO2 [ppm]",
    showticklabels = TRUE)
  g<- list(
    title = "Temperature [deg C]",
    showticklabels = TRUE)
  d<- list(
    title = "Salinity",
    showticklabels = TRUE)
  e<-list(
    title = "CH4",
    showticklabels = TRUE)
  f<- list(
    title= "O2 [umol L-1]",
    showticklabels = TRUE)
  h<-list("Distance to Travemuende [km]", showticklabels = TRUE)
  # 04b2: Time Series Plots -----------------------------------------------
  # Plot Mean pCO2
  output$plot_timeseries <- renderPlotly({
    
    plotlist<- NULL
    sub.df.timeseries.data<-df.sub.timeseries()
    
### PLOT MEAN pCO2 ###

  p1<-plot_ly(sub.df.timeseries.data, name = "Mean pCO2", showlegend = FALSE) %>% #, height = (400*(length(plotlist))), width = 800) %>% 
       add_trace(x= ~sub.df.timeseries.data$date_mean, y= ~sub.df.timeseries.data$pCO2_mean,type= 'scatter', 
                 mode= 'markers', marker = list(symbol= "circle", size=4, color="grey"), 
                 #error_y= ~list(type = "data", array=df.sub$pCO2_sd, color= "grey"), 
                 hoverinfo = 'text',
                 text = ~paste('</br> Mean pCO2',
                               '</br> Date:', sub.df.timeseries.data$date_mean,
                               '</br> Mean: ',  round(sub.df.timeseries.data$pCO2_mean, digits= 2) ,
                               '</br> Max: ', round(sub.df.timeseries.data$pCO2_max, digits = 2),
                               '</br> Min: ', round(sub.df.timeseries.data$pCO2_min, digits = 2))) %>% 
       layout( xaxis= a, yaxis = b) 
     
 ### PLOT MEAN Temperature ###
    
    p2<-plot_ly(sub.df.timeseries.data, name = "Mean Temperature", showlegend = FALSE, height = (400*4),  width = 1200) %>%

      add_trace(x= ~sub.df.timeseries.data$date_mean, y= ~sub.df.timeseries.data$Tem_mean,type= 'scatter',
                mode= 'markers', marker = list(symbol= "circle", size=4, color="grey"),
                #error_y= ~list(type = "data", array=df.sub$Tem_sd,color= "grey"),
                hoverinfo = 'text',
                text = ~paste('</br> Mean Temp',
                              '</br> Date', sub.df.timeseries.data$date_mean,
                              '</br> Mean: ',  round(sub.df.timeseries.data$Tem_mean, digits= 2) ,
                              '</br> Max: ', round(sub.df.timeseries.data$Tem_max, digits = 2),
                              '</br> Min: ', round(sub.df.timeseries.data$Tem_min, digits = 2))) %>%
      layout(xaxis = a, yaxis = g, autosize = FALSE)

   
### PLOT MEAN SALINITY ###
  p3<- plot_ly(sub.df.timeseries.data, name = "Mean Salinity", showlegend = FALSE,height = (400*4), width = 1200) %>%
      add_trace(x= ~ sub.df.timeseries.data$date_mean, y= ~ sub.df.timeseries.data$Sal_mean,type= 'scatter',
                mode= 'markers', marker = list(symbol= "circle", size=4, color="grey"),
                #error_y= ~list(type = "data", array=sub.df.timeseries.data$Sal_sd,color= "grey"),
                hoverinfo = 'text',
                text = ~paste('</br> Mean Sal',
                              '</br> Date', sub.df.timeseries.data$date_mean,
                              '</br> Mean: ',  round(sub.df.timeseries.data$Sal_mean, digits= 2) ,
                              '</br> Max: ', round(sub.df.timeseries.data$Sal_max, digits = 2),
                              '</br> Min: ', round(sub.df.timeseries.data$Sal_min, digits = 2))) %>%
      layout(xaxis= a, yaxis = d,  autosize= FALSE)


 ### PLOT MEAN CH4 ###

# add when data is available

### PLOT MEAN O2

     p5<- plot_ly(sub.df.timeseries.data, name = "Mean O2", showlegend = FALSE,height = (400*4),  width = 1200) %>%
       add_trace(x= sub.df.timeseries.data$date_mean, y= sub.df.timeseries.data$cO2_mean,type= 'scatter',
                 mode= 'markers', marker = list(symbol= "circle", size=4, color="grey"),
                 #error_y= ~list(type = "data", array=sub.df.timeseries.data$cO2_sd, color= "grey"),
                 hoverinfo = 'text',
                 text = ~paste('</br> Mean O2',
                               '</br> Date', sub.df.timeseries.data$date_mean,
                               '</br> Mean: ',  round(sub.df.timeseries.data$cO2_mean, digits= 2) ,
                               '</br> Max: ', round(sub.df.timeseries.data$cO2_max, digits = 2),
                               '</br> Min: ', round(sub.df.timeseries.data$cO2_min, digits = 2))) %>%
       layout(xaxis= a, yaxis = f,  autosize= FALSE)


  plotlist<-list(p1,p2,p3,p5)

    subplot(plotlist, nrows= length(plotlist), shareX = TRUE, titleY = TRUE)
  })
 
  # 04d: Hovmoeller Plots ------------------------------------
#Hovmoeller Plot pCO2 Mean
  
  output$hov_pCO2_mean <- renderPlot({
    
    sub.df.hov.data<-df.sub.hov()
    
  hov1 <-
    sub.df.hov.data %>% 
    filter(!is.na(pCO2_mean)) %>% 
    ggplot()+
    geom_raster(aes(week, dist.trav.int, fill= pCO2_mean))+
    scale_fill_viridis_c(name="pCO2 [ppm]", direction = -1)+
    geom_vline(xintercept = as.numeric(seq(as.POSIXct("2003/1/1", tz="GMT"), 
                                          as.POSIXct("2018/1/1", tz="GMT"), 
                                          "years")),
               size=1)+
    scale_x_datetime(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    labs(y="Distance to Travemuende [km])")+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(1.3, "cm"),
      legend.key.height = unit(0.3, "cm")
    )
  
  hov1
  
  #if (input$pCO2_mean == TRUE)
  #  (hov1) else
  #  {NULL}
  
  })
#Hovmoeller Plot Temperatur Mean
  output$hov_temp_mean <- renderPlot({
    
    
    # if(input$routeall == TRUE)
    #   Sub_pCO2 <<- df %>% 
    #     filter(date >=input$daterange[1] & date <=input$daterange[2] & 
    #              Lon >= input$lon_low & Lon <= input$lon_high &
    #              Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
    #     dplyr::select(date,Lon,Lat,pCO2,Tem,ID) else {NULL} 
    
    if (input$routeE == TRUE)
      Sub_pCO2 <<- df %>% 
        filter(route=="E" & date >=input$daterange[1] & date <=input$daterange[2] & 
                 Lon >= input$lon_low & Lon <= input$lon_high &
                 Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
        dplyr::select(date,Lon,Lat,pCO2,Tem,ID) else {NULL} 
    if (input$routeW == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          dplyr::filter(route == "W", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL} 
    if(input$routeP == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          dplyr::filter(route == "P", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL}
    if(input$routeG == TRUE)
      Sub_pCO2 <<-rbind(Sub_pCO2,df %>% 
                          dplyr::filter(route == "G", date >=input$daterange[1] & date <=input$daterange[2] & 
                                   Lon >= input$lon_low & Lon <= input$lon_high &
                                   Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
                          dplyr::select(date,Lon,Lat,pCO2,Tem,ID)) else {NULL}
    
    
    
    #Sub_pCO2$date <- as.POSIXct(strptime(Sub_pCO2$date, format="%Y-%m-%d %H:%M:%S", tz="GMT"))
    
   
    cut_temp<-df.sub
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= 55, to = 60, by=0.02), labels = seq(from= 55+0.02, to= 60, by=0.02))
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= input$lat_low, to = input$lat_high, by=0.02), labels = seq(from= input$lat_low+0.02, to= input$lat_high, by=0.02))
    cut_temp$dist.trav<-distGeo(cbind(df.sub$Lon, df.sub$Lat), trav)/1e3
    cut_temp$dist.trav.int<-cut(cut_temp$dist.trav, seq(0, 1200, 50), labels =
                                 seq(25, 1175, 50))
    
    cut_temp$week <- cut(cut_temp$date, breaks="weeks")
    cut_temp$week <- as.Date(cut_temp$week, tz="GMT")
    
    
    cut_temp_mean<-cut_temp %>% 
      dplyr::select(dist.trav.int, week, Tem) %>% 
      group_by(dist.trav.int, week) %>% 
      #changed here funs(mean = mean)
      summarise_all(funs(mean=mean)) %>% 
      as.data.frame() 
    
    cut_temp_mean$Tem_mean<-cut_temp_mean$mean
    cut_temp_mean$dist.trav.int<-as.numeric(as.character(cut_temp_mean$dist.trav.int))
    
    cut_temp_mean$week<-as.POSIXct(cut_temp_mean$week)
    
    hov2 <-
      ggplot(data= cut_temp_mean)+
      geom_raster(aes(week, dist.trav.int, fill= mean))+ #changed here: fill = Tem_mean to mean
      scale_fill_gradientn(colours=c("#fc8d59","#ffffbf","#91bfdb"), name=expression("Temperature [C]"))+
      
      #until here working well and looking good
      #xlim(as.POSIXct("2003/1/1"), as.POSIXct("2018/1/1"))+
      geom_vline(xintercept = as.numeric(seq(as.POSIXct("2003/1/1", tz="GMT"), 
                                             as.POSIXct("2018/1/1", tz="GMT"), 
                                             "years")),
                 size=1)+
      #scale_color_brewer(palette="Set1", name="pCO2 (µatm)")+
      #                    (name=expression("pCO2[µatm]")+
      #                   limits=c(100, 600))+
      scale_x_datetime(date_breaks= "1 year", date_labels = "%Y")+
      #scale_x_datetime(breaks = seq(as.POSIXct("2004/1/1", tz="GMT"), as.POSIXct("2016/1/1", tz="GMT"), "2 years"),
      #                expand = c(0,0))+
      scale_y_continuous(breaks = seq(0, 1000, 50))+
      labs(y="Distance to Travemuende [km])")+
      theme(
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.3, "cm"),
        legend.key.height = unit(0.3, "cm")
      )
    
    
    hov2
    
    #if (input$temp_mean == TRUE)
    #  (hov2) else
    #  {NULL}
    
  })
  
  
#Hovmoeller Plot Salinity Mean
  output$hov_sal_mean <- renderPlot({
    
    
    # if(input$routeall == TRUE)
    #   Sub_pCO2 <- df %>% 
    #     filter(date >=input$daterange[1] & date <=input$daterange[2] & 
    #              Lon >= input$lon_low & Lon <= input$lon_high &
    #              Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
    #     dplyr::select(date,Lon,Lat,ID, Sal) else {NULL} 
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
    
    
    cut_sal<-df.sub
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= 55, to = 60, by=0.02), labels = seq(from= 55+0.02, to= 60, by=0.02))
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= input$lat_low, to = input$lat_high, by=0.02), labels = seq(from= input$lat_low+0.02, to= input$lat_high, by=0.02))
    cut_sal$dist.trav<-distGeo(cbind(cut_sal$Lon, cut_sal$Lat), trav)/1e3
    cut_sal$dist.trav.int<-cut(cut_sal$dist.trav, seq(0, 1200, 50), labels =
                                 seq(25, 1175, 50))
    
    cut_sal$week <- cut(cut_sal$date, breaks="weeks")
    cut_sal$week <- as.Date(cut_sal$week, tz="GMT")
    
    
    cut_sal_mean<-cut_sal %>% 
      dplyr::select(dist.trav.int, week, Sal) %>% 
      group_by(dist.trav.int, week) %>% 
      summarise_all(funs(mean=mean)) %>% 
      as.data.frame() 
    
    cut_sal_mean$Sal_mean<-cut_sal_mean$mean
    cut_sal_mean$dist.trav.int<-as.numeric(as.character(cut_sal_mean$dist.trav.int))
    
    cut_sal_mean$week<-as.POSIXct(cut_sal_mean$week)
    
    hov3 <-
      ggplot(data= cut_sal_mean)+
      geom_raster(aes(week, dist.trav.int, fill= mean))+ #changed here: fill = Sal_mean to mean
      scale_fill_gradientn(colours=c("#fc8d59","#ffffbf","#91bfdb"), name=expression("Salinity[]"))+
      
      #until here working well and looking good
      #xlim(as.POSIXct("2003/1/1"), as.POSIXct("2018/1/1"))+
      geom_vline(xintercept = as.numeric(seq(as.POSIXct("2003/1/1", tz="GMT"), 
                                             as.POSIXct("2018/1/1", tz="GMT"), 
                                             "years")),
                 size=1)+
      #scale_color_brewer(palette="Set1", name="pCO2 (µatm)")+
      #                    (name=expression("pCO2[µatm]")+
      #                   limits=c(100, 600))+
      scale_x_datetime(date_breaks= "1 year", date_labels = "%Y")+
      #scale_x_datetime(breaks = seq(as.POSIXct("2004/1/1", tz="GMT"), as.POSIXct("2016/1/1", tz="GMT"), "2 years"),
      #                expand = c(0,0))+
      scale_y_continuous(breaks = seq(0, 1000, 50))+
      labs(y="Distance to Travemuende [km])")+
      theme(
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.3, "cm"),
        legend.key.height = unit(0.3, "cm")
      )
    
    
    hov3
    #if (input$sal_mean == TRUE)
    #  (hov3) else
    #  {NULL}
    
  })
  
  
  
#Hovmoeller Plot CH4 Mean
  
  
  
#Hovmoeller Plot O2
  output$hov_o2_mean <- renderPlot({
    
    
    # if(input$routeall == TRUE)
    #   Sub_pCO2 <<- df %>% 
    #     filter(date >=input$daterange[1] & date <=input$daterange[2] & 
    #              Lon >= input$lon_low & Lon <= input$lon_high &
    #              Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
    #     dplyr::select(date,Lon,Lat,pCO2,Tem,ID,cO2, Sal) else {NULL} 
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
    
    
    cut_o2<<-df.sub
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= 55, to = 60, by=0.02), labels = seq(from= 55+0.02, to= 60, by=0.02))
    #cut_pCO2$Lat.int <-cut(cut_pCO2$Lat, breaks=seq(from= input$lat_low, to = input$lat_high, by=0.02), labels = seq(from= input$lat_low+0.02, to= input$lat_high, by=0.02))
    cut_o2$dist.trav<-distGeo(cbind(cut_o2$Lon, cut_o2$Lat), trav)/1e3
    cut_o2$dist.trav.int<-cut(cut_o2$dist.trav, seq(0, 1200, 50), labels =
                                seq(25, 1175, 50))
    
    cut_o2$week <- cut(cut_o2$date, breaks="weeks")
    cut_o2$week <- as.Date(cut_o2$week, tz="GMT")
    
    
    cut_o2_mean<-cut_o2 %>% 
      dplyr::select(dist.trav.int, week, cO2) %>% 
      group_by(dist.trav.int, week) %>% 
      summarise_all(funs(mean=mean)) %>% 
      as.data.frame() 
    
    cut_o2_mean$Sal_mean<-cut_o2_mean$mean
    cut_o2_mean$dist.trav.int<-as.numeric(as.character(cut_o2_mean$dist.trav.int))
    
    cut_o2_mean$week<-as.POSIXct(cut_o2_mean$week)
    
    hov4 <-
      ggplot(data= cut_o2_mean)+
      geom_raster(aes(week, dist.trav.int, fill= mean))+ #changed here: fill = Tem_mean to mean
      scale_fill_gradientn(colours=c("#fc8d59","#ffffbf","#91bfdb"), name=expression("O2[]"))+
      #xlim(as.POSIXct("2003/1/1"), as.POSIXct("2018/1/1"))+
      geom_vline(xintercept = as.numeric(seq(as.POSIXct("2003/1/1", tz="GMT"), 
                                             as.POSIXct("2018/1/1", tz="GMT"), 
                                             "years")),
                 size=1)+
      #scale_color_brewer(palette="Set1", name="pCO2 (µatm)")+
      #                    (name=expression("pCO2[µatm]")+
      #                   limits=c(100, 600))+
      scale_x_datetime(date_breaks= "1 year", date_labels = "%Y")+
      #scale_x_datetime(breaks = seq(as.POSIXct("2004/1/1", tz="GMT"), as.POSIXct("2016/1/1", tz="GMT"), "2 years"),
      #                expand = c(0,0))+
      scale_y_continuous(breaks = seq(0, 1000, 50))+
      labs(y="Distance to Travemuende [km])")+
      theme(
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.3, "cm"),
        legend.key.height = unit(0.3, "cm")
      )
    
    
    hov4
    # if (input$o2_mean == TRUE)
    #   (hov4) else
    #   {NULL}
    
  }) 
  
  # 04e: Transect Plots -------------------------------------------------- 

### Transect Plot pCO2 ###
  output$trans_pCO2 <- renderPlotly({
   
    sub.df.transect<-df.sub.transect()
    
    t1<-sub.df.transect %>%
      group_by(date) %>% 
      ggplot(aes(x= dist.trav, y= pCO2, color = date))+
      geom_line()+
      scale_color_viridis_d(direction = -1)+
      ylab("pCO2 [ppm]")+
      xlab("Distance to Travemuende [km]")+
      theme_bw()
    
    t1
  })
    
### Transect Plot Temperature ###
    
output$trans_temp <- renderPlotly({
  routelist<- as.vector(NULL)
  
  sub.df.transect<-df.sub.transect()
      
      t2<-sub.df.transect %>% 
        group_by(date) %>% 
        ggplot(aes(x= dist.trav, y= Tem, color = date))+
        geom_line()+
        scale_color_brewer(palette="RdGy", direction = -1)+
        ylab("Temperature [°C]")+
        xlab("Distance to Travemuende [km]")
      
      t2
    })
    
    
### Transect Plot Salinity ###
    
output$trans_sal <- renderPlotly({
  
  sub.df.transect<-df.sub.transect()
        
        t3<-sub.df.transect %>% 
          group_by(date) %>% 
          ggplot(aes(x= dist.trav, y= Sal, color = date))+
          geom_line()+
          scale_color_brewer(palette="RdGy", direction = -1)+
          ylab("Salinity [‰]")+
          xlab("Distance to Travemuende [km]")
        
        
        t3
})

### Transect Plot O2 ###

output$trans_o2 <- renderPlotly({
  
  sub.df.transect<-df.sub.transect()
  
  t5<-sub.df.transect %>% 
    group_by(date) %>% 
    ggplot(aes(x= dist.trav, y= cO2, color = date))+
    geom_line()+
    scale_color_brewer(palette="RdGy", direction = -1)+
    ylab("O2[‰]")+
    xlab("Distance to Travemuende [km]")
    
  t5
})

  
  
  # 04f: download CSV ------------------------------------------------
  #https://shiny.rstudio.com/articles/download.html  
  
   datasetInput <- reactive({
  
    switch(input$dataset,
           "Time Series Data" = df.sub.timeseries(),
           
           "Transect Data" = df.sub.transect(),
           
           "Hovmoeller Data" = df.sub.hov()
    )
    })

#output$datatable<- renderTable({
 # datasetInput()
#})

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
