
# dentrogram using a hierarchical cluster
# more infos: http://gastonsanchez.com/blog/how-to/2012/10/03/Dendrograms.html

d <- dist(mtcars)   #generate a distance matrix 
hc <- hclust(d)     #hierarchical clustering of the distance matrix
plot(hc)            #draw dendrogram

# use ggplot2
#install.packages("ggdendro")
library(ggdendro)
vignette("ggdendro")  # browse sample code

ggdendrogram(hc) 
ggdendrogram(hc, rotate=TRUE) 

# create a treemap
#install.packages("treemap")
library(treemap)

data(GNI2010)
treemap(GNI2010,index=c("continent", "iso3"),vSize="population",vColor="GNI",type="value")
