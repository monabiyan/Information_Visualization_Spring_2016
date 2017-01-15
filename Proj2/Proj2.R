
library("ggplot2")
library("ggmap")




df1<-read.csv("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Proj2/hubway_2011_07_through_2013_11/hubway_trips.csv",header=TRUE)
summary(df1)
head(df1)


df2<-read.csv("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Proj2/hubway_2011_07_through_2013_11/hubway_stations.csv",header=TRUE)
head(df2)


#=========================================  relative Frequency of requests based on starting station
df2$init_freq<-c()
for (i in 1:nrow(df2))
{  
  df2$init_freq[i]<-(sum(na.omit(df1$strt_statn==df2$id[i])))
}
df2$init_freq_normalized<-df2$init_freq/sum(df2$init_freq)*100*5


head(df2)




myMap1 <-get_map(location="Downtown Crossing Boston",zoom=13,color='bw',source="google",maptype="roadmap",crop=FALSE)

g1<-ggmap(myMap1)+geom_point(aes(x = lng, y = lat), data =df2,alpha = .9, color="red",size = df2$init_freq_normalized)
g1<-g1+xlab("Longitude")+ylab("Latitude")+ggtitle("Usage Frequency based on Starting Destination")
g1<-g1+scale_colour_continuous(low = "red", high = "blue", space = "Lab", guide = "colorbar")
g1



head(df2)
head(df1)
#===========================================================

df2[1:100,]
#=========================================  relative Frequency of requests based on ending station
df2$end_freq<-c()

for (i in 1:nrow(df2))
{  
  df2$end_freq[i]<-(sum(na.omit(df1$end_statn==df2$id[i])))
}

df2$end_freq_normalized<-df2$end_freq/sum(df2$end_freq)*100*5

head(df2)

g2<-ggmap(myMap1,legend = "right")+geom_point(aes(x = lng, y = lat), data =df2,alpha = .9, color="red", size = df2$end_freq_normalized)

g2<-g2+xlab("Longitude")+ylab("Latitude")+ggtitle("Usage Frequency based on End Destination")+scale_colour_continuous(low = "red", high = "blue", space = "Lab", guide = "colorbar")
g2


g2
#======================================

#=========================================  Average Duration takes for each ride based on starting station
df2$duration<-0
for (i in 1:nrow(df2))
{  
  if (sum(na.omit(df1$strt_statn==df2$id[i]))!=0)
  {
    a<-df1$strt_statn==df2$id[i]
    a[is.na(a)]<-FALSE
    df2$duration[i]<-(sum(df1$duration[a]))
  }
}

head(df2)

df2$duration[is.na(df2$duration)]<-0
df2$duration


df2$duration_normalized<-df2$duration/df2$init_freq/sum(df2$duration)*1000000*7
df2$duration_normalized[is.na(df2$duration_normalized)]<-0
head(df2)

g3<-ggmap(myMap1)+geom_point(aes(x = lng, y = lat), data =df2,alpha = .9, color="dark green", size = df2$duration_normalized)
g3<-g3+xlab("Longitude")+ylab("Latitude")+ggtitle("Duration of Use after picking the bikes from Starting Destination")
g3
#=========================================



#=========================================  Average Duration takes for each ride based on end station
df2$duration2<-0
for (i in 1:nrow(df2))
{  
  if (sum(na.omit(df1$end_statn==df2$id[i]))!=0)
  {
    a<-df1$end_statn==df2$id[i]
    
    a[is.na(a)]<-FALSE
    df2$duration2[i]<-(sum(df1$duration[a]))
  }
  
}
df2$duration2[is.na(df2$duration2)]<-0
df2$duration2

df2$duration2_normalized<-df2$duration2/df2$end_freq/sum(df2$duration2)*1000000*7
df2$duration2_normalized[is.na(df2$duration2_normalized)]<-0

head(df2)
g4<-ggmap(myMap1)+geom_point(aes(x = lng, y = lat), data =df2,alpha = .9, color="dark green", size = df2$duration2_normalized)
g4<-g4+xlab("Longitude")+ylab("Latitude")+ggtitle("Duration of Use before returning bike to the End Destination")
g4
#=========================================
g1
g2
g3
g4

#################################### taking the most five 
head(df2)
head(df1)

df2_max_init<-df2[order(-df2$init_freq),]
df2_max_init6<-df2_max_init[1:10,]
df2_max_init6


g5<-ggmap(myMap1)+geom_point(aes(x = lng, y = lat), data =df2_max_init6,alpha = .5, color="dark green", size = df2_max_init6$init_freq_normalized)
g5<-g5+xlab("Longitude")+ylab("Latitude")+ggtitle("Most Crowded Departures")
g5



df2_max_end<-df2[order(-df2$end_freq),]
df2_max_end6<-df2_max_end[1:10,]
df2_max_end6

g6<-ggmap(myMap1)+geom_point(aes(x = lng, y = lat), data =df2_max_end6,alpha = .5, color="dark blue", size = df2_max_end6$end_freq_normalized)
g6<-g6+xlab("Longitude")+ylab("Latitude")+ggtitle("Most Crowded Destinations")
g6


###################################################
#Most crowded unique Paths


head(df1)
library(stringr)
smpl <- sample(1:nrow(df1), 50000)
start_end<-c()


j<-0
for (i in smpl)
{
  
  print(j)
  if ((is.na(df1$strt_statn[i]))|(is.na(df1$end_statn[i])))
  {
    #start_end[j]<-str_c(as.character(df1$strt_statn[i]),"E",as.character(df1$end_statn[i]))
    next()
  }
  
  else if (df1$strt_statn[i]==df1$end_statn[i]) 
  { next()}
  
  else 
    {
      start_end[j]<-str_c(as.character(df1$strt_statn[i]),"D",as.character(df1$end_statn[i]))
      j<-j+1
    }
}

head(start_end)
trip<-table(start_end)
head(trip)
trip<-as.data.frame(trip)
trip<-trip[order(-trip$Freq),]
head(trip)
trip

oneway<-data.frame()

oneway
for (i in 1:200)
{
  print(i)
  if(!str_detect(start_end[i],"E"))
  {
  oneway[i,1]<-str_split(trip$start_end[i],"D")[[1]][1]
  oneway[i,2]<-str_split(trip$start_end[i],"D")[[1]][2]
  }
}



h<-200
top_routs<-data.frame(start_id=rep(0,h),end_id=rep(0,h),lat1=rep(0,h),lng1=rep(0,h),lat2=rep(0,h),lng2=rep(0,h))


for (i in 1:h)
{
  top_routs$start_id[i]<-oneway$V1[i]
  top_routs$end_id[i]<-oneway$V2[i]
  
  top_routs$lat1[i]<-df2[(df2$id==oneway$V1[i]),5]
  top_routs$lng1[i]<-df2[(df2$id==oneway$V1[i]),6]
  top_routs$lat2[i]<-df2[(df2$id==oneway$V2[i]),5]
  top_routs$lng2[i]<-df2[(df2$id==oneway$V2[i]),6]
  top_routs$weight[i]<-trip$Freq[i]
}

top_routs
popular<-top_routs[c(1:11,13,15,16,19,37),]
#popular$weight[1]<-popular$weight[2]
#popular$weight[2]<-20

popular[] <- lapply(popular, as.numeric)
popular
myMap1 <-get_map(location="Beacon Hill, Boston",zoom=14,color='bw',source="google",maptype="terrain",crop=FALSE)
g6<-ggmap(myMap1)+geom_point(aes(x = lng1, y = lat1), data =popular,alpha = .9, color="navy", size =12)
g6<-g6+xlab("Longitude")+ylab("Latitude")+ggtitle("Most Favorite Paths")
g6

g6<-ggmap(myMap1)+geom_point(aes(x = lng1, y = lat1), data =popular[popular[,1]==53,],alpha = .9, color="dark red", size =10)
#size =(popular$weight/10)
library(igraph)

map1<-graph.data.frame(top_routs)

################################################################



library(igraph)
connections<-cbind(df1$strt_statn,df1$end_statn)
connections<-na.omit(connections)
mis3<-graph.data.frame(connections)
mis3
plot(mis3[1:20,])
vcount(mis3)           # list table of vertices (nodes)
ecount(mis3)           # list table of edges 
hist(degree(mis3))          # list table of degree values
betweenness(mis3)      # list table of betweenness values
closeness(mis3)  
V(mis3)                # get vector with all vertices (nodes)
E(mis3)                # get vector with all edges







# quickstart
# load edge list as tab delimited text file into a new data frame "bikes"
bikes<- read.csv("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Rcode/data/hubway [Edges].csv", header = TRUE)
head(bikes)

mis <- graph.data.frame(bikes[,1:2])   # parse the dataframe into the igraph object mis
plot(mis)                         # render the igraph object

mis3<-graph.data.frame(df1[,c(2,8)]) 

# Taking a random Sub Date for better visualization

x1<- c(1:length(bikes$V1))
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

#########  Time #####################
spring22=0 #Chinatown
summer22=0
fall22=0
winter22=0

spring40=0 #Martin j Walsh
summer40=0
fall40=0
winter40=0

spring53=0 #Beacon
summer53=0
fall53=0
winter53=0

spring38=0  #WestEnd
summer38=0
fall38=0
winter38=0

library(lubridate)
j=0
hh<-nrow(df1)
for (i in (1:nrow(df1)))
{
  j=j+1
  print(j/hh)
  if (sum(c(is.na(df1$start_date[i]),is.na(df1$end_date[i]),is.na(df1$strt_statn[i])))==0)
  {
   s<-mdy_hm(df1$start_date[i])
   if(df1$strt_statn[i]==22)
   {
     if((month(s)==3)|(month(s)==4)|(month(s)==5)){spring22<-spring22+1}
     else if(month(s)==6|month(s)==7|month(s)==8){summer22<-summer22+1}
     else if(month(s)==9|month(s)==10|month(s)==11){fall22<-fall22+1}
     else if(month(s)==12|month(s)==1|month(s)==2){winter22<-winter22+1}
   }
   else if(df1$strt_statn[i]==40)
   {
     if(month(s)==3|month(s)==4|month(s)==5){spring40<-spring40+1}
     else if(month(s)==6|month(s)==7|month(s)==8){summer40<-summer40+1}
     else if(month(s)==9|month(s)==10|month(s)==11){fall40<-fall40+1}
     else if(month(s)==12|month(s)==1|month(s)==2){winter40<-winter40+1}
   }
   else if(df1$strt_statn[i]==53)
   {
     if(month(s)==3|month(s)==4|month(s)==5){spring53<-spring53+1}
     else if(month(s)==6|month(s)==7|month(s)==8){summer53<-summer53+1}
     else if(month(s)==9|month(s)==10|month(s)==11){fall53<-fall53+1}
     else if(month(s)==12|month(s)==1|month(s)==2){winter53<-winter53+1}
   }
   else if (df1$strt_statn[i]==38)
   {
     if(month(s)==3|month(s)==4|month(s)==5){spring38<-spring38+1}
     else if(month(s)==6|month(s)==7|month(s)==8){summer38<-summer38+1}
     else if(month(s)==9|month(s)==10|month(s)==11){fall38<-fall38+1}
     else if(month(s)==12|month(s)==1|month(s)==2){winter38<-winter38+1}
   }
 }
}

############


summary_df<-data.frame(values[1:4],values[5:8],values[9:12],values[13:16])
values=c(spring22,summer22,fall22,winter22,spring40,summer40,fall40,winter40,spring53,summer53,fall53,winter53,spring38,summer38,fall38,winter38)
season<-c("spring","summer","fall","summer","spring","summer","fall","summer","spring","summer","fall","summer","spring","summer","fall","summer")
name<-c("22","22","22","22","40","40","40","40","53","53","53","53","38","38","38","38")
df_season<-data.frame(values,season,name)
library(ggplot2)

lp1 <- ggplot(data=df_season, aes(x=season, y=values, group=name,colour=name)) + geom_line() + geom_point()
lp1


first<-c(spring22,summer22,fall22,winter22)
second<-c(spring40,summer40,fall40,winter40)
third<-c(spring53,summer53,fall53,winter53)
forth<-c(spring38,summer38,fall38,winter38)

seasons<-data.frame(rbind(first,second,third,forth))
seasons<-

sprg<-rbind(first[1],second[1],third[1],forth[1])
summ<-rbind(first[2],second[2],third[2],forth[2])




hhh<-c()

for (i in 1:nrow(df2))
{
  print(i)
  hhh[i]<-df2$duration[i]/df2$init_freq[i]
}
max(na.omit(hhh))/60



