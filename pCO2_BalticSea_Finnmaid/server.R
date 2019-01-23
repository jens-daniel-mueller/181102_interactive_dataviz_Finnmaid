# 04: Server function ---------------------------------------------------------
# Define server logic required to draw mapPlot and scatterPlot for mainpanel
server <- function(input, output) {
  
  output$ValuesPerPoint <- renderText({
    
    Sub <- df %>% 
      filter(date >=input$daterange[1] & date <=input$daterange[2] & 
               Lon >= input$lon_low & Lon <= input$lon_high &
               Lat >=input$lat_low & Lat <= input$lat_high ) %>% 
      dplyr::select(date,Lon,Lat,pCO2,Tem,ID)
    
    df.sub.mean <- Sub %>% 
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
    
    df.sub.pCO2<-bind_cols(df.sub.mean.pCO2,df.sub.min.max.pCO2)
    
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
    
    
    df.sub.temp<-bind_cols(df.sub.mean.temp,df.sub.min.max.temp)
    df.sub.temp$date_mean<-as.Date(df.sub.temp$date_mean)
    
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
    p<-subplot(p1,p2, nrows= 2, shareX = TRUE, titleY = TRUE) %>% 
      hide_legend()
  })
}