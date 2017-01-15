# load the hubway network, the node file and the edge file
hubN <- read.csv("data/hubway [Nodes].csv")
hubE <- read.csv("data/hubway [Edges].csv")


# create a merged data frame containing both the trips and the locations of the station
hub <- merge(hubE,hubN, by.x = "Source", by.y = "Id")
hub <- merge(hub,hubN, by.x = "Target", by.y = "Id")

# plot the spatial network
ggplot(data=hub) + geom_segment(aes(x=lon.x, y=lat.x,xend=lon.y, yend=lat.y), col="blue", alpha=I(0.02))
