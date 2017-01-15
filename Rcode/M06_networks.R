


# Visit   http://kateto.net/network-visualization     very useful for Network visuali


rm(list = ls())          #clear workspace

install.packages("igraph")   #install and load igrph library
library(igraph)

# quickstart
# load edge list as tab delimited text file into a new data frame "lesmis"
# you can also use read.csv("data/lesmis.txt", sep="\t", header = FALSE)
lesmis <- read.delim("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Rcode/data/lesmis.txt", header = FALSE)
head(lesmis)
mis <- graph.data.frame(lesmis)   # parse the dataframe into the igraph object mis
plot(mis)                         # render the igraph object

# now again more in detail
# parse the same data frame as undirected graph
mis <- graph.data.frame(lesmis, directed = FALSE)

# basic functions
vcount(mis)           # list table of vertices (nodes)
ecount(mis)           # list table of edges 
degree(mis)           # list table of degree values
betweenness(mis)      # list table of betweenness values
closeness(mis)        # list table of closeness values

V(mis)                # get vector with all vertices (nodes)
E(mis)                # get vector with all edges

# create a color palette with 10 steps
capabilities("tcltk")
library(colorspace)
pal <- choose_palette()
ncol <- 25           # we will use 25 color steps
palette(pal(ncol))

#attach centralities as attributes to data the graph, normalized to 0-1
V(mis)$degree       <-  degree(mis, normalized = TRUE)
V(mis)$betweenness  <-  betweenness(mis, normalized = TRUE)
V(mis)$closeness    <-  closeness(mis, normalized = TRUE)

# set default options for layout
# for other layout options check http://igraph.org/r/doc/layout.html
# for other igraph options check http://igraph.org/r/doc/plot.common.html
igraph.options(layout=layout.kamada.kawai,
               vertex.label = NA,           # dont show labels by default
               vertex.size = 15,            # default node size
               vertex.frame.color = NA,     # the border color of the node
               vertex.color = "lightblue")  # default node color 

# set node size to degree
V(mis)$size  <- V(mis)$degree * 25   # assign size to closeness, adjust value for good size
V(mis)$color <- V(mis)$betweenness * ncol  # assign color to betweenness, scaled to number of colors
plot(mis)
V(mis)$color <- V(mis)$closeness   * ncol  # assign color to closeness, scaled to number of colors
plot(mis)

# now lets try an interactive network plot
# https://christophergandrud.github.io/networkD3/
install.packages("networkD3")
library(networkD3)

# for the d3 network we need the original data frame with the edge list
lesmis <- read.delim("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Rcode/data/lesmis.txt", header = FALSE)
n <- simpleNetwork(lesmis)
n
saveNetwork(n, file = "lesmis.html") #save our javascript network to a html file

