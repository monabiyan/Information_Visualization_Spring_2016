# Color in R
#
# install packages, skip if you already have
install.packages("colorspace") 
install.packages("RColorBrewer") 

# start
colors()                  # list all color names in R
palette()                 # standard color palette
rgb(1.0, 1.0, 0.0)        # color from rgb value
col2rgb("magenta")        # rgb value from color
source("http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.R")  # generate color chart pdf

#create & use palettes
t <- Titanic[,2,2,2]      # take a slice from the built in Titanic data set (female adult survivors)
pie(t)                    # render as pie chart

p <- colorRampPalette(c("red", "yellow"))  # a simple continuous color ramp 
pie(t, col=p(4))          # generate a palette from p with 4 steps         
p <- terrain.colors(4)   # type help(terrain.colors) for more info
pie(t, col=p)

# Cynthia Brewer's color palettes
library(RColorBrewer)
par(mar=c(2,2,2,2)) # add margins, to print the labels correctly
display.brewer.all()
display.brewer.pal(7,"Accent")  # use one of the predefined Brewer palettes, display conveniently

# create a brewer palette with 8 color
p <- brewer.pal(8, "Blues")
pie(t,col=p, main = "Female Adult Survivors of the Titanic by Class" ) 
pie(t,col=brewer.pal(8, "Greens"), main = "Female Adult Survivors of the Titanic by Class" ) 
pie(t,col=brewer.pal(8, "YlOrRd"), main = "Female Adult Survivors of the Titanic by Class" ) 

# package colorspace
library(colorspace)       # a package for advanced HCL color spaces in R
browseVignettes("colorspace")

pal <- choose_palette()   # create a new palette and store it in the Pal function
pal(100)                  # print 100 color values of our palette function

palette()                 # print the current palette
palette(pal(100))         # set the R default color palette with 100 values of our palette

n <- 12
rep(1,n)                  # create a vector with n x the value 1 (for demonstration only)
pie(rep(1,n), col=pal(n)) # draw a pie chart with n segments using our palette 
pie(rep(1,n), col=heat_hcl(n))        # other predefined palettes from the colorspace package
pie(rep(1,n), col=terrain_hcl(n))

filled.contour(volcano, color = terrain.colors) # a heatmap of a matrix, using the volcano data set and the terrain color palette
filled.contour(volcano, color = topo.colors) # a heatmap of a matrix, using the volcano data set and the topo color palette



