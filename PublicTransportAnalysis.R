install.packages('rworldmap',dependencies=TRUE)
install.packages(c("kohonen", "mapdata", "ggplot2", "maps", "rworldmap", "reshape2", "rgeos"))
library(ggplot2)
library(maps)
library(mapdata)
library(rworldmap)

??maps

getwd()
setwd("C:/INFO411/Assignment 1/taxi.csv")
taxi <- read.csv(file = "subset.csv")
taxi <- read.csv(file = "taxi.csv")

latitude <- taxi$Latitude
longitude <- taxi$Longitude

plot(latitude, longitude, main="the location points", xlab = "Latitude", ylab= "longitude", pch=19)



italy <- map_data("italy")


graph <- ggplot(taxi, aes(x = longitude, y = latitude) ) +
  geom_polygon(data = italy, aes(x = long, y = lat, group = group), color="gray", fill= "white")+
  geom_point(aes(col=DriveNo), size=1) +
  coord_fixed(1.3)


plot(graph)
graph <- ggplot(taxi, aes(x = Longitude, y = Latitude) ) +
  geom_point(size=1) +
  coord_fixed(1.3)

graph


memory.limit()  

memory.limit(size = 6500000)

utils:::menuInstallPkgs()
head(taxi)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)


points(taxi$Latitude, taxi$Longitude, col = "red", cex = .6)
range(taxi$Latitude)
range(taxi$Longitude)
plot(newmap,xlim=c(-0.146,16.3),ylim=c(39.2,51.6),asp=1)



points(taxi$lat, taxi$long, col = "red", cex = .6)
range(taxi$lat)
range(taxi$long)
plot(newmap,xlim=c(-0.146,16.3),ylim=c(39.2,51.6),asp=1)


??geom_polygo


??coord_fixed


points(taxi$Latitude, taxi$Longitude, col = "red", cex = .6)
plot(newmap,location="Europe",asp=1)
points(taxi$Latitude, taxi$Longitude, col = "red", cex = .6)
points(head(taxi)$Longitude, head(taxi)$Latitude, col = "red", cex = .6)
plot(newmap,xlim=c(-0.146,16.3),ylim=c(39.2,51.6),asp=1)
points(head(taxi)$Longitude, head(taxi)$Latitude, col = "red", cex = .6)
plot(newmap,xlim=c(-0.146,16.3),ylim=c(39.2,51.6),asp=1)
points(taxi$Longitude, taxi$Latitude, col = "red", cex = .6)
plot(newmap,xlim=c(-0.146,16.3),ylim=c(39.2,51.6))
points(head(taxi)$Longitude, head(taxi)$Latitude, col = "red", cex = .1)
points(head(taxi)$Longitude, head(taxi)$Latitude, col = "red", cex = .2)
plot(newmap,xlim=c(-0.146,16.3),ylim=c(39.2,51.6))
points(head(taxi)$Longitude, head(taxi)$Latitude, col = "red", cex = .3)
points(head(taxi)$Longitude, head(taxi)$Latitude, col = "bleu", cex = .3)
points(head(taxi)$Longitude, head(taxi)$Latitude, col = "blue", cex = .3)
plot(newmap,xlim=c(-0.146,16.3),ylim=c(39.2,51.6))




range(taxi$Latitude)
range(taxi$Longitude)


longRome<-taxi[taxi$Longitude>11.5,] #833

longRome<-longRome[longRome$Longitude<=13.5,] 

d<-taxi[taxi$Longitude>0,]

d<-d[d$Longitude<=15,]

latRome2<-taxi[taxi$Latitude<42.5,]  #21817833

latRome<-latRome2[latRome2$Latitude>=41.5,] #21817828



dim(d)
latlest<-d[d$Latitude<=40,]
dim(latlest)
latgreat<-d[d$Latitude>=50,]



install.packages("sf")
setwd("C:/INFO411/Assignment 1/taxi.csv")
taxi <- read.csv(file = "taxi.csv")
library(ggplot2)
library(maps)
library(mapdata)
library(rworldmap)
library(sf)

memory.limit()  

memory.limit(size = 6500000)

#taxiclean has ..830 obs

taxi_Clean <- taxi %>% select(DriveNo, Date.and.Time, Latitude, Longitude) %>% filter(Longitude>11.5, Longitude<=13.5, Latitude<42.4, Latitude>=41.4)



min(taxi_Clean$Longitude)
max(taxi_Clean$Longitude)
min(taxi_Clean$Latitude)
max(taxi_Clean$Latitude)
mean(taxi_Clean$Latitude)
??select()


driverList <- unique(taxi_Clean$DriveNo)


DriveDuration <- {}

for (DriveNo in driverList) {
  DriverID <- taxi_Clean[which(taxi_Clean$DriveNo == DriveNo),]
  
  DriverInfo <- as.POSIXlt(DriverID$Date.and.Time)
  
  timeBegin <- DriverInfo[1: length(DriverInfo) -1]
  
  timeEnd <- DriverInfo[2: length(DriverInfo)]
  
  timeLength <- difftime(timeEnd, timeBegin, units="secs")
  
  timeLength_clean <- timeLength[which(timeLength<300)]
  
  DriveDuration <- rbind(DriveDuration, sum(timeLength_clean))
}



min(DriveDuration)
max(DriveDuration)
mean(DriveDuration)




taxi_Clean_124 <- taxi %>% select(DriveNo, Date.and.Time, Latitude, Longitude) %>% filter(Longitude>11.5, Longitude<=13.5, Latitude<42.4, Latitude>=41.4, DriveNo == 124)


graph_driver124 <- ggplot(taxi_Clean_124, aes(x = Longitude, y = Latitude) ) +
  geom_polygon(data = italy, aes(x = long, y = lat, group = group), color="white", fill= "grey")+
  geom_point(aes(col=DriveNo), size=1)


??coord_sf

plot(graph_driver124)


min(taxi_Clean_124$Longitude)
max(taxi_Clean_124$Longitude)
mean(taxi_Clean_124$Longitude)
min(taxi_Clean_124$Latitude)
max(taxi_Clean_124$Latitude)
mean(taxi_Clean_124$Latitude)




taxi_Clean_124 <- taxi %>% select(DriveNo, Date.and.Time, Latitude, Longitude) %>% filter(Longitude>11.5, Longitude<=13.5, Latitude<42.4, Latitude>=41.4, DriveNo == 124)


driverList <- unique(taxi_Clean_124$DriveNo)


DriveDuration124 <- {}

for (DriveNo in driverList) {
  DriverID <- taxi_Clean_124[which(taxi_Clean_124$DriveNo == DriveNo),]
  
  DriverInfo <- as.POSIXlt(DriverID$Date.and.Time)
  
  timeBegin <- DriverInfo[1: length(DriverInfo) -1]
  
  timeEnd <- DriverInfo[2: length(DriverInfo)]
  
  timeLength <- difftime(timeEnd, timeBegin, units="secs")
  
  timeLength_clean <- timeLength[which(timeLength<300)]
  
  DriveDuration124 <- rbind(DriveDuration124, sum(timeLength_clean))
}



print(DriveDuration124)



minLong <- min(taxi_Clean_124$Longitude)
maxLong<- max(taxi_Clean_124$Longitude)
meanLat<- mean(taxi_Clean_124$Longitude)
minLat <- min(taxi_Clean_124$Latitude)
maxLat <- max(taxi_Clean_124$Latitude)
meanLat<- mean(taxi_Clean_124$Latitude)

dLong <- maxLong - minLong
dLat <- maxLat - minLat


radius <- 6371000


a <- (sin(dLat/2))^2 + cos(minLat) * cos(maxLat) * (sin(dLong/2))^2


c <- 2*atan2(sqrt(a), sqrt(1-a))


distance <- radius * c

print(distance)