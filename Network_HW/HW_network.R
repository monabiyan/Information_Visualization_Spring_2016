rm(list = ls())          #clear workspace

#install.packages("igraph")   #install and load igrph library





library(igraph)

# quickstart
# load edge list as tab delimited text file into a new data frame "bikes"
bikes<- read.csv("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Rcode/data/hubway [Edges].csv", header = FALSE)
head(bikes)
mis <- graph.data.frame(bikes[,1:2])   # parse the dataframe into the igraph object mis
plot(mis)                         # render the igraph object

# Taking a random Sub Date for better visualization

x1<- runif(100,1,length(bikes$V1))
bikes_sub<-bikes[x1,]
mis2 <- graph.data.frame(bikes_sub[,1:2])   # parse the dataframe into the igraph object mis
#mis3<-graph.data.frame(bikes_sub) 
plot(mis2)                         # render the igraph object




# now again more in detail
# parse the same data frame as undirected graph
#mis <- graph.data.frame(bikes, directed = TRUE)
#plot(mis)
# basic functions
vcount(mis2)           # list table of vertices (nodes)
ecount(mis2)           # list table of edges 
degree(mis2)           # list table of degree values
betweenness(mis2)      # list table of betweenness values
closeness(mis2)        # list table of closeness values

V(mis2)                # get vector with all vertices (nodes)
E(mis2)                # get vector with all edges

# create a color palette with 10 steps
capabilities("tcltk")
library(colorspace)
pal <- choose_palette()
ncol <- 25           # we will use 25 color steps
palette(pal(ncol))

#attach centralities as attributes to data the graph, normalized to 0-1
V(mis2)$degree       <-  degree(mis2, normalized = TRUE)
V(mis2)$betweenness  <-  betweenness(mis2, normalized = TRUE)
V(mis2)$closeness    <-  closeness(mis2, normalized = TRUE)

# set default options for layout
# for other layout options check http://igraph.org/r/doc/layout.html
# for other igraph options check http://igraph.org/r/doc/plot.common.html
igraph.options(layout=layout.kamada.kawai,
               vertex.label = NA,           # dont show labels by default
               vertex.size = 15,            # default node size
               vertex.frame.color = NA,     # the border color of the node
               vertex.color = "lightblue")  # default node color 

# set node size to degree
V(mis2)$size  <- V(mis2)$degree * 100   # assign size to closeness, adjust value for good size
V(mis2)$color <- V(mis2)$betweenness * ncol  # assign color to betweenness, scaled to number of colors
plot(mis2)
V(mis2)$color <- V(mis2)$closeness   * ncol  # assign color to closeness, scaled to number of colors
plot(mis2)

# now lets try an interactive network plot
# https://christophergandrud.github.io/networkD3/
#install.packages("networkD3")
library(networkD3)

# for the d3 network we need the original data frame with the edge list
bikes <- read.delim("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Rcode/data/bikes.txt", header = FALSE)
n <- simpleNetwork(bikes_sub[,1:2])
n
saveNetwork(n, file = "bikes.html") #save our javascript network to a html file


###################################


#More Visualization from kateto website

#install.packages("igraph")
#install.packages("network") 
#install.packages("sna")
#install.packages("ndtv")


library("network")
library("sna")
library("ndtv")



############    1 ##################
col <- rep("grey40", vcount(mis2))



plot(mis2, mark.groups=list(c(1,4,5,8), c(15:17)), 
     mark.col=c("blue","orange"), mark.border=NA)


##############   2    #################
summary(mis2)
netm <- get.adjacency(mis2,  sparse=F)
colnames(netm) <- V(mis2)$media
rownames(netm) <- V(mis2)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,78:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )


#########  3 #####################


dd <- degree.distribution(mis2, cumulative=T, mode="all")
plot(dd, pch=19, cex=1, col="orange", xlab="Degree", ylab="Cumulative Frequency")


#########  4 #####################





##################################







