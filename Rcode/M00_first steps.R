# install GGplot2 package
install.packages("ggplot2")

# load the ggplot2 library
library(ggplot2)
# or 
require(ggplot2)

# ggplot2 hello world using built-in data sets
# qplot is the short command of ggplot2 for quick tests
qplot(rivers)

# lets explore a second built-in data set more in detail - earthquakes
quakes
# the first few lines
head(quakes)

# info about the structure of the data set
# it is a data frame with five variables (columns)
str(quakes)

# access variables with the $ sign
quakes$lat
quakes$lon

# or with their order: the first 10 entries of the second column
quakes[1:10,2]

# the whole first row
quakes[1,]
# the whole first column (in this case, lat)
quakes[,1]

# plot the lat/long of the data set, we will map it to x and y axes
# qplot will automatically use points for this chart
qplot(data=quakes, x=long, y=lat)

# we can add more mappings: the mag column to the pointsize
qplot(data=quakes, x=long, y=lat, size=mag)
# color to depth
qplot(data=quakes, x=long, y=lat, size=mag, color=depth)
# and make everything transparent
# in this case, we use alpha (transparency) as a setting, not as a data mapping
# I() means "as is" (type ?I for help) allows us to specify a concrete value
qplot(data=quakes, x=long, y=lat, size=mag, color=depth, alpha = I(0.7))

# we can also subset the data or use expressions
qplot(data=quakes, x=long, y=lat, size=mag, color=long<180)
qplot(data=quakes[quakes$lon>180,], x=long, y=lat, size=mag)

# lets compare the relationship between two other columns
qplot(data=quakes, x=stations, y=mag, size = depth, alpha = I(0.25))

# the full, more formally correct syntax 
# the mapping between data and visuals is now covered using the aes (aesthetics) command. 
# alpha is now outside the aes(), therefore it is interpreted as a setting
ggplot(data=quakes) + geom_point(aes(x=stations, y=mag, size = depth), alpha= 0.5)
# compare:
ggplot(data=quakes) + geom_point(aes(x=stations, y=mag, alpha = depth))


# also the following commands produce the same results
# this expanded syntax allows us to define different layers with different data / geometries 
ggplot(data=quakes, aes(x=stations, y=mag, size = depth)) + geom_point(alpha= 0.5)
ggplot() + geom_point(data=quakes, aes(x=stations, y=mag, size = depth), alpha= 0.5)

# multiple geometries - example of a heatmap
ggplot(data=quakes, aes(x=stations, y=mag)) + 
  geom_bin2d(binwidth = c(10, 0.25)) +
  geom_point(aes(size = depth), color="white",alpha= 0.2) +
  ggtitle("Earthquakes") 

# exploring the relationship between number of quakes, number of stations, magnitude and depth
ggplot(data=quakes, aes(x=stations, y=mag)) + 
  geom_bin2d(binwidth = c(10, 0.25))

# change the binwidth numbers and see what happens
ggplot(data=quakes, aes(x=depth, y=mag) ) + 
  geom_bin2d(binwidth = c(50, 0.25))

ggplot(data=quakes, aes(x=depth, y=stations) ) + 
  geom_bin2d(binwidth = c(50, 10))

# check http://docs.ggplot2.org/current/

