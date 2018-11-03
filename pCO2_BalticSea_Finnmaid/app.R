#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("pCO2 in the Baltic Sea"),
   
   # Sidebar with a slider input for date, lattitude and longitude
   sidebarLayout(
      sidebarPanel(
         sliderInput("date",
                     "Choose a time range:",
                     min = 2013,
                     max = 2017,
                     value = c(2013, 2017)),
         sliderInput("lon", "Choose a Longitude range",
                     min = 53, max = 60,
                     value = c(53,56)),
         sliderInput("lat", "Choose a Lattitude range",
                     min = 10, max = 30,
                     value = c(12,15))
      ),
      
      # Show a plot of the data
      mainPanel(
         plotOutput("mapPlot"), 
         plotOutput("scatterPlot")
      )
   )
)

# Define server logic required to draw mapPlot and scatterPlot
server <- function(input, output) {
   
   output$mapPlot <- renderPlot({
     ggplot(df, aes(df$Lon,df$Lat))+
       geom_point()
      })
   output$scatterPlot <- renderPlot({
     # datamanagment
     df.sub.mean <- df.sub[,.(
     date = mean(as.numeric(date)),
     mean.pCO2 = mean(pCO2, na.rm = TRUE),
     max.pCO2 = max(pCO2),
     min.pCO2 = min(pCO2)),
     by=.(ID)]
     # plot 
     ggplot(df.sub.mean, aes(date, mean.pCO2, ymin=min.pCO2, ymax=max.pCO2))+
       geom_point()
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

