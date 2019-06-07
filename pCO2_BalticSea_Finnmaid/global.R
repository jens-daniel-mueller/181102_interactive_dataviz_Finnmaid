#global.R
df$route<-as.character(df$route)

trav<-c(10.8605315,53.9414096)

# 02: map attributes  -----------------------------------------------------------

baltic.coastlines <- ggplot2::map_data('world')
land.colour   <- "grey75"
border.colour <- "grey10"
basemap= baltic.coastlines
xmin= 10
xmax= 31.5
ymin=53.5
ymax=61
# definition of routes to plot in map plot
#I chose one ID for each route (E,W,G,P) randomly and saved the corresponding data 
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
