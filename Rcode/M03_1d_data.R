#
# basic plot types 
# check http://www.statmethods.net/graphs/creating.html
# we will work with the built in EPA fuel economy dataset
#

# list the data set
rivers

# standard plot command. by default, the index (sequence) is used as X value
plot(rivers)

# stripchart, a 1-d scatterplot
stripchart(rivers)
stripchart(rivers, pch=4) # we can change the symbol with the pch parameter (1-25)
stripchart(rivers, pch="š") # we can also use arbitrary characters 

# jittering, to prevent overplotting 
stripchart(rivers, method = "jitter")
# a histogram-like view, with rounded distances
stripchart(round(rivers/50)*50, method = "stack", pch=1) 

# basic histogram
hist(rivers, breaks=50, xlab="River length (miles)")
# probability density
hist(rivers, breaks=50, freq = F, xlab="River length (miles)")
# restyle it a bit - check http://www.statmethods.net/advgraphs/parameters.html
hist(rivers, breaks=50, xlab="River length (miles)", col = "black", border = "white", col.axis= "darkgrey", fg = "darkgrey")


# boxplot
boxplot(rivers)
boxplot(rivers, col = "lightgrey", border = "black", pch=20 )

# quantile-quantile plot
qqnorm(rivers)

# barchart for categorical data, using mpg data set
table(mpg$class)

plot(mpg$class)
plot(mpg$class, col="black", border = "white")

#
# now everything with ggplot2
#
library(ggplot2)

# ggplot always prefers data frames, so it is good practice to convert a data set, if it is not already a df
riv <- data.frame(length=rivers)		
ggplot(data=riv,aes(x=length, y="")) + geom_point(size=5, alpha = 0.1) + ylab("rivers") + coord_fixed(ratio = 400)
ggplot(data=riv,aes(x=length, y="")) + geom_point(size=5, shape = 1) + ylab("rivers") + coord_fixed(ratio = 400)
ggplot(data=riv,aes(x=length, y="")) + geom_jitter(size=3, alpha = .6) + ylab("rivers") + coord_fixed(ratio = 400)

# we can shorten it with the qplot command, but the full ggplot is more flexible and easier to understand
qplot(x=rivers, y=" ", geom="point",size=I(5), alpha = I(0.1), ylab="")

# histogram, different syntax
qplot(x=rivers, geom="histogram", xlab="River lengths (miles)")
ggplot(data=riv,aes(x=length)) + geom_histogram() + xlab("River lengths (miles)")
  
ggplot(data=riv,aes(x=length)) + geom_freqpoly() + xlab("River lengths (miles)")

# boxplot, diff. syntax
qplot(x="set",y=rivers, geom="boxplot", ylab="River length (miles)", xlab = "")
g <- ggplot(riv,aes(x="set", y=length))
g + geom_boxplot() + ylab("River length (miles)") + xlab("")
g

# qq plot
ggplot(riv, aes(sample=length)) + stat_qq()
qplot(sample=rivers, stat="qq")

# bar chart
ggplot(mpg,aes(mpg$class))+ geom_bar() + xlab("Number of cars in dataset by class")
qplot(data=mpg,x=class,geom="bar")
