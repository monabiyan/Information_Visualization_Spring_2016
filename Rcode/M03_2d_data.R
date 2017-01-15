#
# basic plot types 
# check http://www.statmethods.net/graphs/creating.html
# we will work with the built in EPA fuel economy dataset
#

# print the whole dataset
mpg
# print a single column cyl
mpg$cyl
# print the first few lines
head(mpg)
# information about the data set
help(mpg)
# data structure
str(mpg)
# summary statistics
summary(mpg)	
# print row names of the data frame (by default numbers, but can be arbitrary string)
row.names(mpg)

# plot displacement vs. miles per gallon on highway
plot(data=mpg, displ~cty)
# same result (except axis titles)
plot(mpg$cty,mpg$displ)

# use a different point symbol (can be 1-25), color ... 
plot(data=mpg, displ~cty, pch=3)

# list all default graphical parameters
# check http://www.statmethods.net/advgraphs/parameters.html
par()
# modify a few parameters
plot(data=mpg, displ~cty, pch=21, col='red', bg='yellow', cex=2, lwd=0.5 )
plot(data=mpg, displ~cty)
     
# fuel efficiency by car class
# note that plot function automatically selects bar charts due to categorical x variable
plot(data=mpg, hwy~class)

# mosaicplot
mosaicplot(data=mpg, class~drv, color=TRUE)

# linechart
plot(data=economics, unemploy~date, type='l', ylab="Unemployment", xlab="Year")

#
# now the same with the ggplot2 library 
# check http://docs.ggplot2.org
#

ggplot(mpg,aes(displ,cty)) + geom_point(alpha=0.2,size=3)
# same in the abbreviated qplot format
qplot(data=mpg,x=displ,y=cty, geom="point", alpha=I(0.2), size=I(3))

# boxplot, two syntax versions
ggplot(mpg,aes(class,hwy)) + geom_boxplot()
qplot(data=mpg,x=class,y=hwy,geom="boxplot")

# use color, categorical var
qplot(data=mpg,x=displ,y=cty, color=class)
# use color, continuous var
qplot(data=mpg,x=hwy,y=cty, color=displ)
# use shape, categorical var.
qplot(data=mpg,x=displ,y=cty, shape=drv, alpha=I(0.7))

# plots with categorical variables
qplot(data=mpg,x=class,y=drv,stat="bin2d", geom="tile") + coord_equal()
qplot(data=mpg,x=class,fill=drv, geom="bar") 

# linecharts
qplot(data=economics, x=date,y=unemploy/pop, geom="line", ylab="Unemployment rate") 
