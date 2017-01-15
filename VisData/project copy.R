#Mohsen Nabian, HW7 Solution

#problem: I have <U+0092> for "'" in some rest. names. But it doesn't go away with gsub. 




#QUESTION: 
#The objective of this assignment is to learn how to extract data from web pages through R
#programming.
#??? Select a website from which to scrape the data
#??? Declare (in your R code as comments) exactly what data the code scrapes and from
#where
#??? Make the scraping parameterized, i.e., allow a data scientist to select search parameters choose
#a website that uses GET requests
#??? Write a function to scrape the data and return the data as a data frame
#??? Perform some simple statistical analysis or searching to ensure that the data was
#retrieved properly, i.e., provide scaffolding to test your code

#################################################################################################



cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.
require(RCurl)    #Downloading Package
require(XML)
library(RCurl)     #Using package in this program
library(XML)
library(stringr)
#Set the working directory to your workspace
setwd("C:/Users/nabian.m/Desktop/VisData")

#YelpParse Function parses data in the YELP search pages. It provides Name,Type,Price, the number of reviews and the Tell number for each element.
#The Elements could be restaurant or bars or coffee-shops or so many other places.
#It is requiered to find the link in YELP website and put it in the YelpParse function here.
#User is able to choose which information he/she wants as output by putting TRUE or FALSE in the correponding places for input.
#TYPE,PRICE,REVIEW,TELL should be substituted by TRUE or FALSE based on User's need.
YelpParse<-function(link)   
{
  
  ########## To understand which information the user wants######
  #choice=0;
  #choice[1]=1
  #choice[2]=as.numeric(TYPE)*2
  #choice[3]=as.numeric(PRICE)*3
  #choice[4]=as.numeric(REVIEW)*4
  #choice[5]=as.numeric(TELL)*5
  ################################################################
  # This is the URL of the website we need scrape to get information on the 
  
  theurl <- link
  theurl<-gsub(" ","",theurl);   #No extra spaces
  webpage <- getURL(theurl)
  # convert the page into a line-by-line format rather than a single string
  tc <- textConnection(webpage)
  webpage <- readLines(tc) #webpage is now a vector of string each elament is a line of string
  close(tc)
  pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)  #pagetree is now in html format and parseable with xpath syntax.
  
  
  
  #########################  NAME  ###############################
  
  restaurant.name<- unlist(xpathApply(pagetree,"//*/span[@class='indexed-biz-name']/a[@*][@*][@*]",xmlValue))
  if(length(restaurant.name)==11)   #Sometimes it gives 11 ellements and the first one is wrong" 
  {restaurant.name<-restaurant.name[2:11]}
  restaurant.name<-as.character(restaurant.name)
  restaurant.name<-gsub("<U+0092>","",restaurant.name)
  
  ###### Removing <U+0092> ####  I Found this in Internet
  Encoding(restaurant.name) <- "latin1"  # (just to make sure)
  iconv(restaurant.name, "latin1", "ASCII", sub="")
  #####
  
  
  restaurant.name
  
  
  
  ############################   REVIEW COUNT  #############################
  
  review.count<-unlist(xpathApply(pagetree,"//*/span[@class='review-count rating-qualifier']",xmlValue))
  review.count
  review.count<-gsub("\n            ", "",review.count) #Removing extra characters"
  review.count<-gsub("\n    ", "",review.count)
  review.count<-gsub(" reviews", "",review.count)
  if(length(review.count)==11)
  {review.count<-review.count[2:11]}
  review.count<-as.numeric(review.count)
  review.count
  
  ############################# PRICE #############################
  
  restaurant.price<-unlist(xpathApply(pagetree,"//*/span[@class='business-attribute price-range']",xmlValue))
  print(restaurant.price)
  for (i in 1:length(restaurant.price))     #Scaling price notations to 1,2,3,4 accordingly where 4 is very epensive.
  {
    if (restaurant.price[i]=="$")  {restaurant.price[i]="1"; }
    if (restaurant.price[i]=="$$")  {restaurant.price[i]="2"; }
    if (restaurant.price[i]=="$$$")  {restaurant.price[i]="3"; }
    if (restaurant.price[i]=="$$$$")  {restaurant.price[i]="4"; }
  }
  restaurant.price
  if(length(restaurant.price)==11)
  {restaurant.price<-restaurant.price[2:11]}
  restaurant.price<-as.numeric(restaurant.price)
  restaurant.price
  
  
  #########################  TYPE   ############################
  
  restaurant.type<-unlist(xpathApply(pagetree,"//*/span[@class='category-str-list']/a[@*][1]",xmlValue))  ##Some times there are several <a> tags. We need the first one.
  if(length(restaurant.type)==11)
  {restaurant.type<-restaurant.type[2:11]}
  restaurant.type<-as.character(restaurant.type)
  restaurant.type
  
  ########################   star  ###############################
  restaurant.star<-unlist(xpathApply(pagetree,"//*/div[@class='rating-large']/i",xmlAttrs))
  restaurant.star<-as.character(restaurant.star)
  
  restaurant.star<-gsub(" star rating", "",restaurant.star)
  hh=0;
  for (i in 1:(length(restaurant.star)/2))
  {hh[i]<-restaurant.star[2*i]}
  restaurant.star<-hh
  restaurant.star<-as.numeric(restaurant.star)
  
  
  
  ########################   Neighborhood  ###############################
  restaurant.neighborhood<-unlist(xpathApply(pagetree,"//*/span[@class='neighborhood-str-list']",xmlValue))
  
  restaurant.neighborhood<-gsub("\n            ", "",restaurant.neighborhood)
  restaurant.neighborhood<-gsub("        ", "",restaurant.neighborhood)
  if(length(restaurant.neighborhood)==11)
  {restaurant.neighborhood<-restaurant.neighborhood[2:11]}
  restaurant.neighborhood<-as.character(restaurant.neighborhood)
  restaurant.neighborhood
  
  
  ########################   ADDRESS  ###############################
  restaurant.address<-unlist(xpathApply(pagetree,"//*/address",xmlValue))
  
  restaurant.address<-gsub("\n            ", "",restaurant.address)
  restaurant.address<-gsub("        ", "",restaurant.address)
  restaurant.address<-gsub("\n", "",restaurant.address)
  restaurant.address<-gsub(city,paste(" ",city),restaurant.address)
  if(length(restaurant.address)==11)
  {restaurant.address<-restaurant.address[2:11]}
  restaurant.address<-as.character(restaurant.address)
  restaurant.address
  restaurant.zip<-str_sub(restaurant.address,-5,-1)
  ############################  TELL  ##############################
  
  restaurant.tell<-unlist(xpathApply(pagetree,"//*/div[@class='secondary-attributes']/span[@class='biz-phone']",xmlValue))
  restaurant.tell<-gsub("\n        ", "",restaurant.tell)
  restaurant.tell<-gsub("\n    ", "",restaurant.tell)
  if(length(restaurant.tell)==11)
  {restaurant.tell<-restaurant.tell[2:11]}
  restaurant.tell<-as.character(restaurant.tell)
  restaurant.tell
  
  
  ############################### Putting ALL in a DATA FRAME ##############################
  min_length=min(length(restaurant.name),length(restaurant.type),length(restaurant.price),length(review.count),length(restaurant.tell),length(restaurant.address),length(restaurant.star))
  restaurant.data<-data.frame(NAME=restaurant.name[1:min_length],TYPE=restaurant.type[1:min_length],PRICE= restaurant.price[1:min_length],REVIEW_COUNT=review.count[1:min_length],STAR=restaurant.star[1:min_length],TELL=restaurant.tell[1:min_length],ADDRESS=restaurant.address[1:min_length],ZIPCODE=restaurant.zip[1:min_length])
  return(restaurant.data)
}

##################Statistical and Searching Questions   ###############

#print(restaurant.data[,choice])
#print(paste("The average price levels is",mean(restaurant.data$PRICE)))
#print(paste("The standard deviation of price levels is",sd(restaurant.data$PRICE)))
#print(paste("The average number of reviews for restaurant is",mean(restaurant.data$REVIEW_COUNT) ))
#print(paste(restaurant.data$NAME[restaurant.data$PRICE==1],"  is inexpensive. ENJOY!!"))


#####################  Examples of Using the function#################




########################################  Finding Geographical Coordinates Using Google API
##Google API and use of static map
## Author - Martin Schedlbauer and Yatish Jain
## Version 1.1

library(RCurl)
library(RJSONIO)
library(plyr)



megacities<-c("Herverhill,MA")
#megacities<-c("Herverhill,MA","Lawrence,MA","Lowell,MA","Malden,MA","Revere,MA","Somerville")
#megacities<-c("Boston,MA","Brockton,MA","Brookline,MA","Cambridge,MA","Chicopee,MA","Framingham,MA","Heverhill,MA","Lawrence,MA","Lowell,MA","Lynn,MA","Malden,MA","Newton,MA","Plympton,MA","Quincy,MA","Revere,MA","Somerville,MA","Springfiled,MA","Taunton,MA","Weymouth,MA","Worcester,MA")

#getLocation function returns the latitude and longitude of any address via google maps api.

#fetchUrl function gives the url to fetch json string.
fetchUrl <- function(address) 
{
  root <- "http://maps.google.com/maps/api/geocode/"   #root url 
  u <- paste(root, "json", "?address=", address, "&sensor=false", sep = "") 
  return(URLencode(u)) #encoding the url
}





getLocation <- function(address) 
{
  url <- fetchUrl(address) #getting the url for the address, I suggest to take a look at this URL before further exploring the code.
  json <- getURL(url)
  x <- fromJSON(json,simplify = FALSE) #getting the json
  if(x$status=="OK") { #checking if the Address url used produces the correct json string 
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA,NA))
  }
}



AddSalary<-function(df)
{
  zip_info<-read.csv("MedianZIP-3.csv",header=TRUE,stringsAsFactors=FALSE)
  
  
  zip_info$Zip<-as.character(zip_info$Zip)
  for (i in 1:length(zip_info[,1]))
  {
    if(nchar(zip_info$Zip[i])==4)
    {
      zip_info$Zip[i]<-paste("0",zip_info$Zip[i])
    }
  }
  zip_info$Zip<-gsub(" ", "",zip_info$Zip)
  
  
  zip_info$Median<-as.character(zip_info$Median)
  zip_info$Mean<-as.character(zip_info$Mean)
  zip_info$Pop<-as.character(zip_info$Pop)
  df$ZIPCODE<-as.character(df$ZIPCODE)
  
  
  MEDIAN_SAL<-0;
  MEAN_SAL<-0;
  POP<-0;
  
  
  
  for(i in 1:length(df[,1]))
  {
    if (sum(df$ZIPCODE[i]==zip_info$Zip)==0)
    {
      MEDIAN_SAL[i]=0;
      MEAN_SAL[i]=0;
      POP[i]=0;
    }
    else
    {
      MEDIAN_SAL[i]<-zip_info$Median[df$ZIPCODE[i]==zip_info$Zip]
      MEAN_SAL[i]<-zip_info$Mean[df$ZIPCODE[i]==zip_info$Zip]
      POP[i]<-zip_info$Pop[df$ZIPCODE[i]==zip_info$Zip]
    }
  }
  df<-cbind(df,MEDIAN_SAL,MEAN_SAL,POP)
  return(df)
  
}





##############################################################




#megacities<-c("New York,NY","Chicago,IL","Boston,MA","Los Angeles,CA","Houston,TX","Philadelphia,PA","San Francisco,CA","Houston,TX","Washington ,DC","Phoenix,AZ","Seattle,WA","Baltimore,MD","Cleaveland,OH","Las Vegas,NV","Austin,TX","Oklahama City,OK")

n<-25

for (location in megacities)
{
  print(location)
  comma<-unlist(str_locate_all(pattern =',',location))[1]
  comma<-as.numeric(comma)
  city<-str_sub(location,start=1,end=(comma-1))
  state<-str_sub(location,start=(comma+1),end=nchar(location))
  print(city)
  address<-paste("http://www.yelp.com/search?find_desc=Restaurants&find_loc=",city,"%2C+",state,"&start=",as.character(0),sep="")
  all<-YelpParse(address)
  
  print(all)
  
  #ads <- all$ADDRESS
  #locations <- ldply(ads, function(x) getLocation(x))
  #names(locations) <- c("LATTITUDE", "LONGITUDE", "location_type", "formatted")
  #all<-cbind(all,locations)
  
  
  
  for (i in 1:n)
  {
    print(city)
    print(i)
    address<-paste("http://www.yelp.com/search?find_desc=Restaurants&find_loc=",city,"%2C+",state,"&start=",as.character(i*10),sep="")
    df<-YelpParse(address)
    
    #ads <- df$ADDRESS
    #locations <- ldply(address, function(x) getLocation(x))
    #names(locations) <- c("LATTITUDE", "LONGITUDE", "location_type", "formatted")
    
    #hh<-cbind(df,locations)
    #all<-rbind(all,hh)
    all<-rbind(all,df)
    Sys.sleep(5)
  }    
  
  
  print("Hiiii")
  all<-AddSalary(all)  #Update with the salaries and populations
  all<-cbind(all,city)
  all<-all[-which((all$POP==0)==TRUE),]
  print("Hiiii3")
  head(all)
  print("Hi")
  write.csv(all,file=paste(city,"_res.csv",sep=""),row.names = FALSE)
  
}


#########################################################################


######################### Adding City as a new column ###############


#megacities<-c("New York,NY","Chicago,IL","Boston,MA","Los Angeles,CA","Houston,TX","Philadelphia,PA","San Francisco,CA","Houston,TX","Washington ,DC","Phoenix,AZ","Seattle,WA","Baltimore,MD","Cleaveland,OH","Las Vegas,NV","Austin,TX","Oklahama City,OK")

for (location in megacity)
{
  print(location)
  comma<-unlist(str_locate_all(pattern =',',location))[1]
  comma<-as.numeric(comma)
  city<-str_sub(location,start=1,end=(comma-1))
  state<-str_sub(location,start=(comma+1),end=nchar(location))
  
  
  all<-read.csv(file=paste(city,"_res.csv",sep=""))
  all<-cbind(all,city)
  write.csv(all,file=paste(city,"_res.csv",sep=""),row.names = FALSE)
}

###############################################################


############ earasing comma "," from Salary and Population ####


#megacities<-c("New York,NY","Chicago,IL","Boston,MA","Los Angeles,CA","Houston,TX","Philadelphia,PA","San Francisco,CA","Houston,TX","Washington ,DC","Phoenix,AZ","Seattle,WA","Baltimore,MD","Cleaveland,OH","Las Vegas,NV","Austin,TX","Oklahama City,OK")


for (location in megacities)
{
  print(location)
  comma<-unlist(str_locate_all(pattern =',',location))[1]
  comma<-as.numeric(comma)
  city<-str_sub(location,start=1,end=(comma-1))
  state<-str_sub(location,start=(comma+1),end=nchar(location))
  
  
  all<-read.csv(file=paste(city,"_res.csv",sep=""))
  
  all$MEDIAN_SAL<-gsub(",","",all$MEDIAN_SAL)
  all$MEAN_SAL<-gsub(",","",all$MEAN_SAL)
  all$POP<-gsub(",","",all$POP)
  
  write.csv(all,file=paste(city,"_res.csv",sep=""),row.names = FALSE)
}


########################################################



########### Making One DataFarme of All Files ############################################


library(stringr)

megacity<-"Boston,MA"
for (location in megacity)
{
  print(location)
  comma<-unlist(str_locate_all(pattern =',',location))[1]
  comma<-as.numeric(comma)
  city<-str_sub(location,start=1,end=(comma-1))
  state<-str_sub(location,start=(comma+1),end=nchar(location))
  
  
  all<-read.csv(file=paste(city,"_res.csv",sep=""))
}

for (location in megacities[2:length(megacities)])
{
  
  comma<-unlist(str_locate_all(pattern =',',location))[1]
  comma<-as.numeric(comma)
  city<-str_sub(location,start=1,end=(comma-1))
  state<-str_sub(location,start=(comma+1),end=nchar(location))
  print(location)
  df<-read.csv(file=paste(city,"_res.csv",sep=""))
  all<-rbind(df,all)
}

write.csv(all,file="All.csv",row.names = FALSE)


#####################################################


all<-read.csv(file="All.csv")

nrow(all)
for ( i in (1:nrow(all)))
{
  if (is.na(all$REVIEW_COUNT[i]))
  {
    all$REVIEW_COUNT[i]<-0
  }
}

write.csv(all,file="All.csv",row.names = FALSE)

all<-read.csv(file="All.csv")






























####################  Doing Some Statistical Analysis on 'all' dataframe #############################

all<-read.csv(file="All.csv",stringsAsFactors=FALSE);

#price distribution
hist(all$PRICE,breaks=20, col="red",xlab="price",main="Restaurant Price Distribution in US");

#Restaurants Type
tp<-as.data.frame(table(as.factor(all$TYPE)));
tp<-tp[with(tp, order(-tp$Freq)),];  # sorting restaurant types based of frequency
tp[1:20,];
pie(tp$Freq[1:30], labels = tp$Var1[1:30], main="Restaurants in MA",col=rainbow(30));




###########################  Name
i<-1
setwd("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/VisData")
NAME<-c()
TYPE<-c()
PRICE<-c()
PREVIEW<-c()
REVIEW_COUNT<-c()
STAR<-c()
TELL<-c()
ADDRESS<-c()
ZIPCODE<-c()
MEDIAN_SAL<-c()
MEAN_SAL<-c()
POP<-c()
CITY<-c()


for (name in unique(all$NAME))
{
  print(i)
  NAME[i]<-name;
  TYPE[i]<-as.character(unique(all$TYPE[all$NAME==name]))[1]
  PRICE[i]<-as.character(unique(all$PRICE[all$NAME==name]))[1]
  REVIEW_COUNT[i]<-as.character(unique(all$REVIEW_COUNT[all$NAME==name]))[1]
  STAR[i]<-as.character(unique(all$STAR[all$NAME==name]))[1]
  TELL[i]<-as.character(unique(all$TELL[all$NAME==name]))[1]
  ADDRESS[i]<-as.character(unique(all$ADDRESS[all$NAME==name]))[1]
  ZIPCODE[i]<-as.character(unique(all$ZIPCODE[all$NAME==name]))[1]
  MEDIAN_SAL[i]<-as.character(unique(all$MEDIAN_SAL[all$NAME==name]))[1]
  MEAN_SAL[i]<-as.character(unique(all$MEAN_SAL[all$NAME==name]))[1]
  POP[i]<-as.character(unique(all$POP[all$NAME==name]))[1]
  CITY[i]<-as.character(unique(all$city[all$NAME==name]))[1]
  i<-i+1
}
ALL_UNIQUE<-data.frame(NAME,TYPE,PRICE,REVIEW_COUNT,STAR,TELL,ADDRESS,ZIPCODE,MEDIAN_SAL,MEDIAN_SAL,POP,CITY)
write.csv(ALL_UNIQUE,file="ALL_UNIQUE.csv",row.names = FALSE)

ALL_UNIQUE<-read.csv(file="ALL_UNIQUE.csv",stringsAsFactors=FALSE);





######## For Each zip Code Medians and Means


i=1;
median_price=0;
mean_price=0; 
median_reviews=0;
mean_reviews=0;
median_star=0;
mean_star=0;
median_salary=0;
population=0;
city_name=0;
zip_code=0;
zip_count=0;
all<-ALL_UNIQUE


all <- data.frame(lapply(all, as.character), stringsAsFactors=FALSE)
for (zip in unique(all$ZIPCODE))
{
  
  print(zip)
  
  
  
  median_price[i]<-median(as.numeric((all$PRICE[all$ZIPCODE==zip])),na.rm = TRUE)
  
  mean_price[i]<-mean(as.numeric((all$PRICE[all$ZIPCODE==zip])),na.rm = TRUE)
  
  median_reviews[i]<-median(as.numeric((all$REVIEW_COUNT[all$ZIPCODE==zip])),na.rm = TRUE)
  mean_reviews[i]<-mean(as.numeric((all$REVIEW_COUNT[all$ZIPCODE==zip])),na.rm = TRUE)
  
  median_star[i]<-median(as.numeric((all$STAR[all$ZIPCODE==zip])),na.rm = TRUE)
  mean_star[i]<-mean(as.numeric((all$STAR[all$ZIPCODE==zip])),na.rm = TRUE)
  
  median_salary[i]<-as.numeric((all$MEDIAN_SAL[all$ZIPCODE==zip]))[1]
  
  population[i]<-as.numeric((all$POP[all$ZIPCODE==zip]))[1]
  
  city_name[i]<-as.character(all$CITY[all$ZIPCODE==zip])[1]
  
  zip_code[i]<-zip
  
  zip_count[i]<-sum(all$ZIPCODE==zip)
  i=i+1
  
}
sum(all$NAME=="NA")
sum(all$TYPE=="NA")
sum(all$PRICE=="NA")
sum(is.na(all$REVIEW_COUNT))
sum(is.na(all$STAR))
sum(is.na(all$MEDIAN_SAL))
sum(is.na(all$City))








mean_price
median_reviews
mean_reviews
median_star
mean_star
median_salary
population
city_name
zip_code
zip_count

zip_info<-data.frame(zip_code,city_name,mean_price,median_price,median_reviews,mean_reviews,median_star,mean_star,median_salary,population,zip_count)
write.csv(zip_info,file="zip_info.csv",row.names = FALSE)

zip_credit<-zip_info[zip_info$zip_count>30,]  # Zip codes that have more than 50 restaurants

zipcode_coord<-na.omit(read.csv(file="zipcode_coord.csv",stringsAsFactors=FALSE));

zip_info<-read.csv(file="zip_info.csv",stringsAsFactors=FALSE);


for (i in 1:nrow(zip_info))
{
  zip_info$Latitude[i]<-zipcode_coord$latitude[zipcode_coord$zip==as.numeric(zip_info$zip_code[i])]
  zip_info$Longitude[i]<-zipcode_coord$longitude[zipcode_coord$zip==as.numeric(zip_info$zip_code[i])]
}
write.csv(zip_info,file="zip_info.csv",row.names = FALSE)
#################################################

############ Highest Score average score for zip codes ######
zip_credit$zip_code[sort(zip_credit$mean_star,decreasing = TRUE)==zip_credit$mean_star]

zip_mean_star_ordered<-zip_credit[order(-zip_credit$mean_star),]
zip_mean_star_ordered

zip_mean_price_ordered<-zip_credit[order(-zip_credit$mean_price),]
zip_mean_price_ordered

zip_median_price_ordered<-zip_credit[order(-zip_credit$median_price),]
zip_median_price_ordered

zip_mean_number_review_ordered<-zip_credit[order(-zip_credit$mean_reviews),]
zip_mean_number_review_ordered

zip_median_number_review_ordered<-zip_credit[order(-zip_credit$median_reviews),]
zip_median_number_review_ordered
###############################################################

########## plot star vs price ######################

plot(zip_credit$mean_price,zip_credit$mean_star) #No relation

plot(zip_credit$median_salary,zip_credit$mean_star) # No relation

plot(zip_credit$median_salary,zip_credit$mean_price) #linear relation

plot(zip_credit$population,zip_credit$median_reviews) # No relation

plot(zip_credit$population,zip_credit$mean_reviews)  #Linear relation
###################################################

























######## For Each city Medians and Means


#megacities<-c("New York,NY","Chicago,IL","Boston,MA","Los Angeles,CA","Houston,TX","Philadelphia,PA","San Francisco,CA","Houston,TX","Washington ,DC","Phoenix,AZ","Seattle,WA","Baltimore,MD","Cleaveland,OH","Las Vegas,NV","Austin,TX")
#megacities<-c("Brockton,MA","Brookline,MA","Cambridge,MA","Chicopee,MA","Framingham,MA","Heverhill,MA","Lawrence,MA")

i<-1;


median_price=0;
mean_price=0;
median_reviews=0;
mean_reviews=0;
median_star=0;
mean_star=0;
mean_of_median_salary=0;
population=0;
city_name=0;
for (location in megacities)
{
  comma<-unlist(str_locate_all(pattern =',',location))[1]
  comma<-as.numeric(comma)
  city<-str_sub(location,start=1,end=(comma-1))
  print(city)
  state<-str_sub(location,start=(comma+1),end=nchar(location))
  
  
  median_price[i]<-median(as.numeric((all$PRICE[all$city==city])),na.rm = TRUE)
  
  mean_price[i]<-mean(as.numeric((all$PRICE[all$city==city])),na.rm = TRUE)
  
  median_reviews[i]<-median(as.numeric((all$REVIEW_COUNT[all$city==city])),na.rm = TRUE)
  mean_reviews[i]<-mean(as.numeric((all$REVIEW_COUNT[all$city==city])),na.rm = TRUE)
  
  median_star[i]<-median(as.numeric((all$STAR[all$city==city])),na.rm = TRUE)
  mean_star[i]<-mean(as.numeric((all$STAR[all$city==city])),na.rm = TRUE)
  
  mean_of_median_salary[i]<-median(as.numeric((all$MEDIAN_SAL[all$city==city])),na.rm = TRUE)  
  
  population[i]<-sum(as.numeric((all$POP[all$city==city])),na.rm = TRUE)
  
  city_name[i]<-city
  i=i+1
}
median_price
mean_price
median_reviews
mean_reviews
median_star
mean_star
mean_of_median_salary
population
city_name



city_info<-data.frame(city_name,mean_price,median_price,median_reviews,mean_reviews,median_star,mean_star,mean_of_median_salary,population)

city_info                      
write.csv(city_info ,file="city_info.csv",row.names = FALSE)           



###################### finding alt long for each restaurant
geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}
for (i in 1:nrow(restaurants))
{
  restaurants$lat[i]<-geocodeAdddress(as.character(restaurants$ADDRESS[i]))[1]
  print(lat)
  restaurants$lng[i]<-geocodeAdddress(as.character(restaurants$ADDRESS[i]))[2]
  print(lng)
}

geocodeAdddress("Time Square, New York City")

lat

######################## Plots

setwd("/Users/mohsennabian/Desktop/VisData")
zip_info<-read.csv(file="zip_info.csv",stringsAsFactors=FALSE);
restaurants<-read.csv(file="ALL_UNIQUE.csv",stringsAsFactors=FALSE);
hist(zip_info$median_salary)
hist(restaurants$PRICE)
hist(restaurants$REVIEW_COUNT)
hist(restaurants$STAR)
restaurants$ADDRESS
library(RDSTK)
street2coordinates("2543 Graystone Place, Simi Valley, CA 93065")






###################### finding alt long for each restaurant
geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}
geocodeAdddress("Time Square, New York City")
