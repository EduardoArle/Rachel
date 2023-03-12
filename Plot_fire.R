#load packages
library(rgdal);library(raster)

#list wds
wd_shp <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Rachel/SpatialData'
wd_data <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Rachel'
wd_map_stuff <- '/Users/carloseduardoaribeiro/Documents/Soup/Map stuff'

#load shp
shp_grid <- readOGR('BehrmannMeterGrid_WGS84_land', dsn = wd_shp)

#load fire table
setwd(wd_data)
table <- read.csv('trait_df.csv')

#include fireseason info into shp attribute table
shp_grid$fireseason <- NA
shp_grid$phylacine <- NA

for(i in 1:nrow(shp_grid))
{
  a <- which(table$WorldID == shp_grid$WorldID[i])
  
  if(length(a) > 0){
    shp_grid$fireseason[i] <- table$fireseason[a]
    shp_grid$phylacine[i] <- table$phylacine[a]
  }
  
  print(i)
}

### plot maps

# Load world map frame and continent outline
setwd(wd_map_stuff)

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
shp2 <- spTransform(shp_grid,CRS(proj4string(world)))

# #create colour ramp to represent the values
# colramp <- colorRampPalette(c("#9e0142", "#d53e4f", "#f46d43",
#                               "#fdae61", "#fee08b", "#ffffbf",
#                               "#e6f598", "#abdda4", "#66c2a5",
#                               "#3288bd", "#5e4fa2"))
#                                        
# 
# #populate the table with the colours to be plotted 
# shp2$colours_fire <- colramp(100)[cut(c(0,9,shp_grid$fireseason), 
#                                  breaks = 100)][-c(1,2)]
# 
# shp2$colours_fire[which(is.na(shp2$colours_fire))] <- 'gray80'
# 
# ### PLOT
# par(mar=c(2,2,2,2), bg = 'white')
# plot(shp2, border = NA , col = shp2$colours_fire)
# plot(worldmapframe , add = T)
# 
# #plot icons legend (first load function)
# myGradientLegend(valRange = c(min(shp_grid$fireseason, na.rm = T), 
#                               max(shp_grid$fireseason, na.rm = T)),
#                  pos=c(0.3,0.1,0.8,0.12),
#                  color = colramp(100),
#                  side = 1,
#                  n.seg = 0,
#                  values = c(min(shp_grid$fireseason, na.rm = T), 
#                             max(shp_grid$fireseason, na.rm = T)),
#                  cex = 2)
# 
# myGradientLegend(valRange = c(min(shp_grid$fireseason, na.rm = T), 
#                               max(shp_grid$fireseason, na.rm = T)),
#                  pos=c(0.21,0.1,0.225,0.12),
#                  color = 'gray80',
#                  side = 1,
#                  n.seg = 0,
#                  values = c(NA,'NA'),
#                  cex = 2)


#### alternative

#create colour ramp to represent the values
colramp <- colorRampPalette(c("#3288bd", "#66c2a5", "#abdda4",
                              "#abdda4", "#e6f598", "#ffffbf",
                              "#fee08b", "#fdae61", "#f46d43",
                              "#d53e4f", "#9e0142"))                                       

#populate the table with the colours to be plotted 
shp2$colours_fire <- colramp(100)[cut(c(0,9,shp_grid$fireseason), 
                                      breaks = 100)][-c(1,2)]

shp2$colours_fire[which(is.na(shp2$colours_fire))] <- 'gray80'
  
### PLOT
par(mar=c(2,2,2,2), bg = 'white')
plot(shp2, border = NA , col = shp2$colours_fire)
plot(worldmapframe , add = T)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$fireseason, na.rm = T), 
                              max(shp_grid$fireseason, na.rm = T)),
                 pos=c(0.27,0.1,0.68,0.12),
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$fireseason, na.rm = T), 
                            max(shp_grid$fireseason, na.rm = T)),
                 cex = 2)

myGradientLegend(valRange = c(min(shp_grid$fireseason, na.rm = T), 
                              max(shp_grid$fireseason, na.rm = T)),
                 pos=c(0.73,0.1,0.743,0.12),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 2)


#plot animal presence

#populate the table with the colours to be plotted 
shp2$colours_phylacine <- colramp(100)[cut(c(min(shp_grid$phylacine, na.rm =T),
                                             max(shp_grid$phylacine, na.rm =T),
                                             shp_grid$phylacine), 
                                      breaks = 100)][-c(1,2)]


shp2$colours_phylacine[which(is.na(shp2$colours_phylacine ))] <- 'gray80'
  
### PLOT
par(mar=c(2,2,2,2), bg = 'white')
plot(shp2, border = NA , col = shp2$colours_phylacine)
plot(worldmapframe , add = T)

#plot icons legend (first load function)
myGradientLegend(valRange = c(min(shp_grid$phylacine, na.rm = T), 
                              max(shp_grid$phylacine, na.rm = T)),
                 pos=c(0.27,0.1,0.68,0.12),
                 color = colramp(100),
                 side = 1,
                 n.seg = 0,
                 values = c(min(shp_grid$phylacine, na.rm = T), 
                            max(shp_grid$phylacine, na.rm = T)),
                 cex = 2)

myGradientLegend(valRange = c(min(shp_grid$phylacine, na.rm = T), 
                              max(shp_grid$phylacine, na.rm = T)),
                 pos=c(0.73,0.1,0.743,0.12),
                 color = 'gray80',
                 side = 1,
                 n.seg = 0,
                 values = c(NA,'NA'),
                 cex = 2)

