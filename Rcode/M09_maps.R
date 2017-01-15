# install our packages, skip if you already have
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("maptools")
install.packages("rgeos")
install.packages("rgdal")
install.packages("mapdata")
install.packages("gridExtra")
install.packages("leaflet")
# quickstart - render choropleth maps of census data in R
library(choroplethr)
library(choroplethrMaps)

# go through tutorials and code for this package
browseVignettes("choroplethr")


library(maps)
library(mapdata)
library(gridExtra)
library(rgeos)
library(ggplot2)
library(sp)


# Download boston neighborhood shape file from
# https://data.cityofboston.gov/City-Services/Boston-Neighborhood-Shapefiles/af56-j7tb
# load shapefile
library(maptools)
bos <- readShapeSpatial("data/BOSnbhds/Bos_neighborhoods_new.shp")
plot(bos)

# this is an alternative (more powerful) library for loading shapefiles 
library(rgdal)
bos <- readOGR(dsn="data/BOSnbhds", layer="Bos_neighborhoods_new")
plot(bos)


#explore the data contained in the shape file
summary(bos)          # the summary - mean coord, areas etc.
str(bos,max.level=2)  # the shape file has a complex class structure
bos@data              # the data frame of attributes of each polygon 
bos@data$Name         # names of the neighborhoods
bos@bbox              # bounding box - these are not lat/lon coordinates, but the massachusetts state coordinate system, NAD83

# plot shapefile with ggplot
# first, we need to convert the shapefile so that we can use it in ggplot, result is a data frame
library(ggplot2)
bos.fort <- fortify(bos,region='OBJECTID')

#now plot it as polygons
g <- ggplot(data = bos.fort, aes(x = long, y = lat, group=group)) + geom_polygon(color='grey',fill="white", size=I(0.3)) 
g

# add a constraint for 1:1 aspect ratio
g <- g + coord_fixed()
g

# set white background, no axes
g <- g + theme_minimal()
g

# now lets load additional data and display it
cc <- read.csv("data/citizensconnect_columns.txt", sep = "\t")

g <- g + geom_point(data=cc,aes(x=CASE_X, y=CASE_Y,group=NULL, shape =SUBJECT, color=REASON), size=I(3), alpha=I(0.7))
g

# use qualitative palette from brewer color
g <- g + scale_color_brewer(type="qual")
g

# we can save the data as a shapefile
# first, we create a second data set that only contains the elements with geo-coordinates
cc2 <- cc[!is.na(cc$CASE_X),]

# then we convert this new data frame into a spatial data frame
coordinates(cc2)<- c("CASE_X", "CASE_Y")
class(cc2)
# we can save this spatial data frame now as a shapefile, which we can open in cartoDB or GIS software
writePointsShape(cc2, "out/cc.shp")

# interactive maps with leaflet
# consult http://rstudio.github.io/leaflet 

library(leaflet)
boswgs84 <- readOGR(dsn="data/bos_tracts/", layer="bos_tracts")
cclatlon <- read.csv(file="data/citizensconnect_latlon.csv")
pal <- colorNumeric( palette = "Blues", domain = boswgs84@data$POP100_RE)

map <- leaflet()
map %>% 
  addPolygons(data=boswgs84, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, color = ~pal(boswgs84@data$POP100_RE), opacity=0.5) %>% 
  addCircleMarkers(~lon,~lat,radius = 2, weight = 4, popup=~service_name,  color='yellow', data=cclatlon)
  

# further ideas for working with projections
# http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot
#