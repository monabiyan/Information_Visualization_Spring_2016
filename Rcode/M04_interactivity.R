# install devtools and rCharts, skip if already installed
install.packages("devtools")
require(devtools); install_github('rCharts', 'ramnathv')

# load rCharts library
library(rCharts)

# lets use again the builtin Motor Trend Car Road Tests data set
help(mtcars); head(mtcars)

p <- rPlot(mpg ~  wt , data = mtcars, size="disp", color = 'cyl', type = 'point')
p

# add interactive controlls
p$addControls("size", value = "disp", values = c('disp', 'cyl','carb','am','gear','vs'))
p$addControls("x", value = "wt", values = c('wt','disp','hp'))
p$addControls("y", value = "mpg", values = c('mpg','wt','disp','qsec','hp'))
p

# use the nvd3 plotting function 
# nvd3 is a visualization library on top of the visualization library d3 (http://nvd3.org)
# see more at http://ramnathv.github.io/posts/rcharts-nvd3/index.html

p <- nPlot(mpg ~ wt, group = 'cyl', data = mtcars, type = 'scatterChart')
p

# add other chart elements
p$chart(showDistX = TRUE, showDistY = TRUE)
p$chart(showControls = TRUE)
p$chart(color = c('darkred','red','orange','yellow'))
p$xAxis(axisLabel = 'Weight')
p$yAxis(axisLabel = 'Miles per Gallon')
p 

# add interactive controls, i.e. for picking the groups
p$addControls("group", value = "cyl", values = c('cyl','carb','am','gear','vs'))
p$addControls("x", value = "wt", values = c('wt','disp','hp'))
p$addControls("y", value = "mpg", values = c('mpg','wt','disp','qsec','hp'))
# add html template 
p$setTemplate(script = system.file('libraries', 'nvd3', 'controls', 'datgui.html', package = 'rCharts'))

p

# work through examples in the introduction at http://ramnathv.github.io/posts/rcharts-nvd3/index.html
# also https://github.com/ramnathv/rCharts/blob/master/inst/libraries/nvd3/examples.R

hubway <- read.csv(file="data/hubway [Nodes].csv")
library(leaflet)

m <- leaflet(data = hubway) %>%
  addTiles() %>%  # Add generic OpenStreetMap map tiles
  addCircleMarkers(~lon,~lat,radius = 4,weight = 2, color='red', popup=~name) #add markers
m  # Print the map

# read more at https://rstudio.github.io/leaflet/
