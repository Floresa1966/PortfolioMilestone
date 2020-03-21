# Allan Flores
# IST719
# Project

## 1) Libraries
packages=c("gplots","dplyr","plyr","tidyverse","ggplot2")

# Use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


# Google API
library(ggmap)
#register_google(key=" ")

library(RColorBrewer)
display.brewer.all()

#Setting a working directory
setwd("C:/Syracuse/IST719/Project")

border <- read.csv("BoarderCrossing.csv",sep=",", header = TRUE)
head(border)

# Abbreviate state names

revalue(border$State, c("Texas" = "TX")) -> border$State
revalue(border$State, c("California" = "CA")) -> border$State
revalue(border$State, c("Arizona" = "AZ")) -> border$State
revalue(border$State, c("New York" = "NY")) -> border$State
revalue(border$State, c("Michigan" = "MI")) -> border$State
revalue(border$State, c("Washington" = "WA")) -> border$State
revalue(border$State, c("Maine" = "ME")) -> border$State
revalue(border$State, c("Vermont" = "VT")) -> border$State
revalue(border$State, c("Minnesota" = "MN")) -> border$State
revalue(border$State, c("North Dakota" = "ND")) -> border$State
revalue(border$State, c("New Mexico" = "NM")) -> border$State
revalue(border$State, c("Montana" = "MT")) -> border$State
revalue(border$State, c("Idaho" = "ID")) -> border$State
revalue(border$State, c("Alaska" = "AK")) -> border$State
revalue(border$State, c("Ohio" = "OH")) -> border$State

# Rename mode of entry

revalue(border$Measure, c("Bus Passengers" = "Bus")) -> border$Measure
revalue(border$Measure, c("Buses" = "Bus")) -> border$Measure
revalue(border$Measure, c("Truck Containers Empty" = "Truck")) -> border$Measure
revalue(border$Measure, c("Truck Containers Full" = "Truck")) -> border$Measure
revalue(border$Measure, c("Trucks" = "Truck")) -> border$Measure
revalue(border$Measure, c("Rail Containers Full" = "Train")) -> border$Measure
revalue(border$Measure, c("Rail Containers Empty" = "Train")) -> border$Measure
revalue(border$Measure, c("Trains" = "Train")) -> border$Measure
revalue(border$Measure, c("Train Passengers" = "Train")) -> border$Measure
revalue(border$Measure, c("Personal Vehicle Passengers" = "Car")) -> border$Measure
revalue(border$Measure, c("Personal Vehicles" = "Car")) -> border$Measure
revalue(border$Measure, c("Pedestrians" = "Pedestrian")) -> border$Measure

# Rename border
unique(border$Border)
revalue(border$Border, c("US-Canada Border" = "US-CAN")) -> border$Border
revalue(border$Border, c("US-Mexico Border" = "US-MEX")) -> border$Border

str(border)

unique(border$Date)

byport.name<-round(tapply(border$Value, list(border$Port.Code), sum),0)
statesum <- tapply(border$Value,border$State,sum)
str(statesum)
dfsimple <- data.frame(avemedst,popst)
library(ggplot2)
us<-map_data("state")  # Ensure to get a map
#make sure everything is lowercase
dfsimple2$statename<-tolower(dfsimple2$statename)
dfsimple2$state<-tolower(dfsimple2$state)

library(tidyverse)
install.packages("mapdata")
library(mapdata)
library(maps)
library(stringr)
library(viridis)

usa <- map_data("usa")
head(usa)
usa <- map_data("usa") # we already did this, but we can do it again
ggplot() +  geom_polygon(data = usa, aes(x = long, y = lat, group = group)) +   coord_quickmap()
states <- map_data("state")

ggplot(data = border) + geom_polygon(aes(x = longitude, y = latitude, fill = border$Value, group = group), color = "white") + coord_quickmap() +
  guides(fill = FALSE)  # d

str(border)

########################

us<-map_data("state")
dummyDF<- data.frame(state.name,stringsAsFactors = FALSE)
dummyDF$state<-tolower(dummyDF$state.name)
map.simple<-ggplot(dummyDF, aes(map_id=state))
map.simple<-map.simple + geom_map(map=us,fill="white",color="black")
map.simple<-map.simple+expand_limits(x=us$long,y=us$lat)
map.simple<-map.simple + coord_map() + ggtitle("Basic Map of Continental USA")
map.simple

border$State<-tolower(border$State)

map.entrycolor<-ggplot(border, aes(map_id=State))
map.entrycolor<-map.entrycolor + geom_map(map=us, aes(fill=border$Value))
map.entrycolor<-map.entrycolor+expand_limits(x=us$long,y=us$lat)

str(border)

df <- group_by(border, State)

head(df)
str(df)



num<-344370

innerFunc <- function(subDf){
  # we create the aggregated row by taking the first row of the subset
  row <- head(subDf,1)
  # we set the x column in the result row to the sum of the first "num"
  # elements of the subset
  row$x <- sum(head(subDf$Value,num))
  return(row)
}


d2 <- do.call(rbind,by(data=border,INDICES=border$State,FUN=innerFunc))
d2$State<-tolower(d2$State)


map.simple<-ggplot(dummyDF,aes(map_id=state))
map.simple<-map.simple+geom_map(map=us,fill="white",color="black")
map.simple<- map.simple+geom_map(data=d2,map=us, aes(fill=x,map_id=State),color="black",na.rm=TRUE)
map.simple<-map.simple + expand_limits(x=us$long,y=us$lat)
map.simple<-map.simple+coord_map()+ggtitle("Border Entries")
map.simple



# Question 1: How many have entered through US and Mexico from Y1996 to 2018 by mode of entry?

par(mfrow = c(2,1))
mode <- tapply(border$Value, list(border$Measure), sum)
barplot(mode)

at<-barplot(sort(mode,decreasing=TRUE), xaxt="n",xlab="Mode of Entry",ylab="Count",col="orange",#ylim=c(0,8000000000),
            main="Number of Recorded Entries by Mode from 1996 to 2018")


y2018 <- subset(border, Date=="2018")
mode2<-tapply(y2018$Value, list(y2018$Measure), sum)

at2<-barplot(sort(mode2,decreasing=TRUE), xaxt="n",xlab="Mode of Entry",ylab="Count",col="orange",
            main="Number of Recorded Entries by Mode in 2018")




# Question 2: How many have entered through US and Mexico from Y1996 to 2018?

par(mfrow = c(2,1))

str(border)
entry.byyear.canmex <- tapply(border$Value, list(border$Date, border$Border), sum)

entry.canmex <- tapply(border$Value, list(border$Border), sum)

table(entry.canmex)

pie(entry.canmex, col=c("lightblue","blue")) # Y1996 to Y2018

canmex2018 <- subset(border, Date=="2018")

entry.canmex2018 <- tapply(canmex2018$Value, list(canmex2018$Border), sum)

pie(entry.canmex2018, col=c("lightblue","blue")) # Y2018

table(entry.canmex2018)

# Barplot trends

par(mfrow = c(2,1))
barplot(entry.byyear.canmex,ylim=c(0,400000000),beside = T,
        main="Number of Entries Though CANADA & MEXICO",col="blue")

# Question 3: What are the top 10 Ports of Entry from Y1996 to Y2018
par(mfrow=c(2,1)) 

entry.port <- tapply(border$Value, list(border$Port.Name), sum)
top10.entry.port<-head(sort(entry.port,decreasing=TRUE), n = 10)

barplot(top10.entry.port, las =2, ylim=c(0.,1200000000), col="blue", main="Top 10 Ports of Entry from Y1996 to Y2018")

# Question 4: What are the top 10 Ports of Entry in Y2018

#  subset in r example 
y2018 <- subset(border, Date=="2018")

entry.port.2018 <- tapply(y2018$Value, list(y2018$Port.Name), sum)
top10.entry.port.2018<-head(sort(entry.port.2018,decreasing=TRUE), n = 10)

barplot(top10.entry.port.2018,ylim=c(0,50000000), las =2,col="blue", main="Top 10 Ports of Entry in Y2018")


# Question 5: How many have entered by State from Y1996 to 2018 and Y2018 alone?

par(mfrow=c(2,1))
entry.state <- tapply(border$Value, list(border$State), sum)
entry.state.sort<-sort(entry.state,decreasing=TRUE)

barplot(entry.state.sort, las =2, ylim=c(0.,3500000000), col="blue", main="State Entry from Y1996 to Y2018")

entry.state2018 <- tapply(y2018$Value, list(y2018$State), sum)
entry.state2018<-sort(entry.state2018,decreasing=TRUE)

barplot(entry.state2018, las =2, col="blue", main="State Entry in Y2018")

# Question 6: Number of People Crossing the border each year - Trend.

border$Date<-as.character(border$Date)
str(border)
data.2 <- tapply(border$Value,list(border$Date),sum)
barplot(data.2,xlab="Year", ylab="Number of People Crossing the Border",ylim=c(0,600000000),las=2,col="orange",
        main="Number of People Crossing the Border Each Year", cex.main=2,cex.lab=0.8, cex.axis=0.8)


# Question 7: Mode of Entry from 1996 to Y2018 

entry.measure <- tapply(border$Value, list(border$Measure), sum)
entry.measure<-sort(entry.measure,decreasing=TRUE)
barplot(entry.measure, las =2, ylim=c(0.,8000000000), col="blue", main="Mode Entry of Y1996 to Y2018")

entry.measure2018 <- tapply(y2018$Value, list(y2018$Measure), sum)
entry.measure2018<-sort(entry.measure2018,decreasing=TRUE)
barplot(entry.measure2018, las =2, col="blue", main="Mode of Entry Y2018")


# PART 2 - PEDESTRIAN ANALYSES
# Question 8: Pedestrian crossing locations.

# Taking only pedestrian data
#  subset in r example 
pedestrian <- subset(border, Measure=="Pedestrian")
pedestrian$Date<-as.character(pedetrian$Date)
pedestriantrend <- tapply(pedestrian$Value,list(pedestrian$Date),sum)
barplot(pedestriantrend,xlab="Year", ylab="Number of People Crossing the Border",las=2,col="orange",
        main="Pedestrians Crossing the Border Each Year", cex.main=2,cex.lab=0.8, cex.axis=0.8)



# Question 10: From 1996 to 2018 which, states are they coming from
par(mfrow=c(2,1))
pedestrianstate <- tapply(pedestrian$Value, list(pedestrian$State), sum)
pedestrianstate.sort<-sort(pedestrianstate,decreasing=TRUE)
barplot(pedestrianstate.sort,xlab="State", ylab="Number of Pedestrian",las=2,col="orange",
        main="Pedestrians Crossing the Border by State", cex.main=2,cex.lab=0.8, cex.axis=0.8)


# Question 11: In 2018, states are they coming from

pedestrianstate2018 <- subset(pedestrian, Date=="2018")
pedestrian2018 <- tapply(pedestrianstate2018$Value, list(pedestrianstate2018$State), sum)
pedestrian2018.sort<-sort(pedestrian2018,decreasing=TRUE)
barplot(pedestrian2018.sort,xlab="State", ylab="Number of Pedestrian",las=2,col="orange",
        main="Pedestrians Crossing the Border by State in Y2018", cex.main=2,cex.lab=0.8, cex.axis=0.8)

# Question 10: Texas breakdown

texas <- subset(border, State=="tx")

texas$Date<-as.character(texas$Date)
texastrend <- tapply(texas$Value,list(texas$Date),sum)
barplot(texastrend,xlab="Year", ylab="Number of People Crossing through Texas",ylim=c(0,250000000),las=2,col="orange",
        main="Number of People Crossing Through TExas Each Year", cex.main=2,cex.lab=0.8, cex.axis=0.8)

#####################################################################################################################################

#Setting a working directory
setwd("C:/Syracuse/IST719/Project")

df <- read.csv("BoarderCrossing.csv",sep=",", header = TRUE)
head(df)

agg.dat<-aggregate(df$Value, list(df$State),sum)

colnames(agg.dat)<-c("state", "entries")

num.cols<-10
my.color.vec<-rev(gray.colors(num.cols))

library(RColorBrewer)

brewer.pal(4, "Dark2")
display.brewer.all()


# My colors to look like

pie(rep(1,num.cols), col=my.color.vec)

my.color.vec[1]

library(plotrix)
agg.dat$index<-round(rescale(x=agg.dat$entries,c(1,num.cols)),0)
agg.dat
agg.dat$color<-my.color.vec[agg.dat$index]
agg.dat

map("state")
m<-map("state")
m$names

state.order<-match.map(database="state", regions=agg.dat$state, exact=FALSE, warn=TRUE)
cbind(m$names, agg.dat$state[state.order])

map("state", col=agg.dat$color[state.order], fill=TRUE, resolution =0, lty=1, projection="polyconic", border="black", main="Border Entries from 1996 to 2018")
legend("topright", as.character(unique(agg.dat$state)), fill = unique(agg.dat$color))

#############  Single Dimension

# Number of Pedestrians Crosing by Port (All Ports)
pedestrianport <- subset(border, Measure=="Pedestrian")
pedestrianport2018 <- subset(pedestrianport, Date=="2018")

plot(pedestrianport2018$Value,col="blue",pch=19, cex=1)

pedestrianport2018TX <- subset(pedestrianport2018, State=="TX")
plot(pedestrianport2018TX$Value,col="blue",pch=19, cex=1)
