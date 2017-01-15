# load a csv file and store it into a new object "earthquakes"
earthquakes <- read.csv(file = "data/earthquake.csv")

earthquakes
head(earthquakes)
summary(earthquakes)
str(earthquakes)

library(ggplot2)

ggplot(data = earthquakes) + 
  geom_point(aes(x=longitude,y=latitude, size=mag, color=type), alpha = 0.6) +
  coord_fixed() + 
  ggtitle("Earthquakes")
  
# save our plot as pdf
ggsave("out/earthquakes.pdf")

# check options with ?ggsave to add parameters
# then open it in a vector program (illustrator) and tweak
ggsave("out/earthquakes.pdf", dpi=300 )


# import JSON files

# install the package (if you have not done so yet)
install.packages("jsonlite")

# load the libary
library(jsonlite)

# load weather data file
weather <- fromJSON("data/weather_simple.json")

str(weather)
summary(weather)

ggplot(data=wmain) +
  geom_line(aes(x=date, y=temp/10)) + 
  geom_line(aes(x=date, y=temp_min/10), color="grey") + 
  geom_line(aes(x=date, y=temp_max/10), color="grey") + 
  ggtitle("Temperature Min Max in Celsius")


# for the more advanced: build the data frame from the more complex original file
# source: http://api.openweathermap.org/data/2.5/history/city?q=Boston
weather <- fromJSON("data/weather.json")

# the structure of the generated file is quite complicated - 
# JSON files should be understood as a collection of objects, 
str(weather)

# but there is a table in there (in form of a nested data.frame)
# we will build a new data frame using only a part of it:
# the date identifier (in weather$list$dt) and the main values (in weather$list$main)
dt <- as.POSIXlt(weather$list$dt, origin="1970-1-1")
wmain <- data.frame(date=dt, weather$list$main)

ggplot(data=wmain) +
  geom_line(aes(x=date, y=temp/10)) + 
  geom_line(aes(x=date, y=temp_min/10), color="grey") + 
  geom_line(aes(x=date, y=temp_max/10), color="grey") + 
  ggtitle("Temperature Min Max in Celsius")

# again, save
ggsave("out/weather.pdf")
