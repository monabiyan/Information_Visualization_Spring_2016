# http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html

# load json files
#install.packages("jsonlite")
library(jsonlite)

# learn how to use jsonlite by browsing the vignettes
browseVignettes("jsonlite")

# a json data set of 10 food inspections in Boston
# see https://data.cityofboston.gov/Health/Food-Establishment-Inspections/qndu-wx8w
# and http://dev.socrata.com/consumers/getting-started.html
url <- "https://data.cityofboston.gov/resource/qndu-wx8w.json?$limit=150"
food <- fromJSON(url)

#inspect the loaded file
head(food)
str(food)

# here we have a date field in internet ISO format
food$issdttm

# we create a new variable using only the date
# in this case, as.Date automatically recognizes the format
food$date <- as.Date(food$issdttm)
food$date

# if necessary we could also specify the format manually, for example
as.Date("04/23/2015", format = "%m/%d/%Y")

# calculation with time
# how many inspections on weekdays, months, quarters?
weekdays(food$date)
months(food$date)
quarters(food$date)

#plot inspections per month 
library(ggplot2)
qplot(months(food$date))

# result per weekdays
ggplot(data=food, aes(x=weekdays(date), fill=result)) + geom_bar() 

ggplot(data=food, aes(months(date))) + geom_histogram() 

# modify the last plot to a polar representation
last_plot() + coord_polar()

# is the first date later than the second?
food$date[1] > food$date[2]

#how many days between the two?
food$date[1] - food$date[2]

# print only years
format(food$date, "%Y")
# print only years by numbers
as.numeric(format(food$date, "%Y"))
# list only those dates later than 2010
food$date[format(food$date, "%Y") > "2010"]


#posix is a format that combines date and time 
as.POSIXlt(food$issdttm)
tm <- as.POSIXlt("2013-07-24 23:55:26")

# the units are seconds. substract one hour (1000 sec)
tm
tm - 3600

# POSIXlt stores date and time as a list - you can print it via
unlist(tm)

# and access components
tm$mon

# years are calculated as difference from 1970 (the beginning of UNIX date)
tm$year

# these operations can be made more convenient with the lubridate package
#install.packages("lubridate")
library(lubridate)

#learn how to use the package functions by reading the vignette
browseVignettes("lubridate")

#this gives us months, days, years as numbers. 
month(food$date)
day(food$date)
year(food$date) > 2010

# also correctly resolves unix years
year(tm)

# show a histogram only starting the year 2009
f10 <- food[year(food$date)>2009,]
ggplot(f10,aes(x=date)) + geom_histogram()

hour(food$posix)
