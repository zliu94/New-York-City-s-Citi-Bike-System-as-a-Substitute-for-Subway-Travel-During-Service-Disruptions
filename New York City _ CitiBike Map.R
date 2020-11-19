citibike <- read.csv("C://ZLIU/SIT/658/Project/DATA/201808-citibike-tripdata.csv", header = T, sep = ",")
citibike[citibike=="NULL"]<-NA
citibike_delete<-na.omit(citibike)
point <- read.csv("C://ZLIU/SIT/658/Project/station.csv", header = T, sep = ",")
start <- point[,c(1,5,6)]
colnames(start) <- c("start.station.id", "start.location", "s_m_location")
end <- point[,c(1,5,6)]
colnames(end) <- c("end.station.id", "end.location", "e_m_location")

newdata <- merge(citibike_delete, start, by = "start.station.id", all.x = T)
newdata <- merge(newdata, end, by = "end.station.id", all.x = T)

newdata["across"] <- as.data.frame(ifelse(newdata$s_m_location==newdata$e_m_location,"0","1"))
table(newdata$across)#4.69%

#missing <- newdata[!complete.cases(newdata),]
#write.csv(missing,file="C://ZLIU/SIT/658/Project/missing.csv",row.names = T)

#MAP(point&plot)
library(ggmap)
library(tidytext)
register_google(key = "AIzaSyAP4qAE6z-ODegyhBtSJcahn_9X0QP-8nU")
map <- get_googlemap(center = c(lon=-73.967207, lat=40.737686), zoom = 12, maptype = "roadmap")
ggmap(map)

freq_s <- as.data.frame(table(newdata$start.station.id))
colnames(freq_s) <- c("start.station.id", "start_freq")
freq_e <- as.data.frame(table(newdata$end.station.id))
colnames(freq_e) <- c("start.station.id", "end_freq")
freq <- merge(point,freq_s, by = "start.station.id", all.x = T)
freq <- merge(freq,freq_e, by = "start.station.id", all.x = T)

start_map <- ggmap(map)+
  geom_point(data = freq,aes(x=start.station.longitude, y=start.station.latitude, color=start_freq, size=0.01))+
  scale_color_continuous(low = "yellow",high = "red")+ 
  guides(size=F)
start_map

#"qsec"

end_map <- ggmap(map)+
  geom_point(data = freq,aes(x=start.station.longitude, y=start.station.latitude, color=end_freq, size=0.01))+
  scale_color_continuous(low = "light green",high = "dark green")+ 
  guides(size=F)
end_map

#combine start and end stations number
freq$all_freq <- freq$start_freq+freq$end_freq
all_map <- ggmap(map)+
  geom_point(data = freq,aes(x=start.station.longitude, y=start.station.latitude, color=all_freq, size=0.01))+
  scale_color_continuous(low = "light blue",high = "blue")+ 
  guides(size=F)
all_map

#, alpha=1

#shdensityMap <- ggmap(map, extent = "device") + 
#  geom_density2d(data = newdata, aes(x = start.station.longitude, y = start.station.latitude), size = 0.1) + 
#  stat_density2d(data = newdata, aes(x = start.station.longitude, y = start.station.latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 16, geom = "polygon") + 
#  scale_fill_gradient(low = "yellow", high = "red", guide = FALSE) + 
#  scale_alpha(range = c(0, 1), guide = FALSE)
#densityMap



#MH_noacross <- subset(newdata, newdata$s_m_location!="NJ or QB" & across=="0")

MH_start_plot <- ggmap(map)+geom_point(data=MH_noacross, aes(x=start.station.longitude, y=start.station.latitude))

#MAP(point&plot)
library(ggmap)
register_google(key = "AIzaSyAP4qAE6z-ODegyhBtSJcahn_9X0QP-8nU")
map <- get_googlemap(center = c(lon=-73.967207, lat=40.772686), zoom = 12, maptype = "roadmap")
ggmap(map)


MH <- subset(point, location=="Dict 1"|location=="Dict 2"|location=="Dict 3"|location=="Dict 4"|location=="Dict 5"|location=="Dict 6"|location=="Dict 7"|location=="Dict 8"|location=="Dict 9")
NJ <- subset(point, location=="New Jersey")
QB <- subset(point, location=="Queens & Brooklyn")


MHmap <- ggmap(map)+geom_point(data=MH, aes(x=start.station.longitude, y=start.station.latitude))
MHmap
NJmap <- ggmap(map)+geom_point(data=NJ, aes(x=start.station.longitude, y=start.station.latitude))
NJmap
QBmap <- ggmap(map)+geom_point(data=QB, aes(x=start.station.longitude, y=start.station.latitude))
QBmap


#trading map
library(geosphere)
library(maps)
map('state', regions="New York",col="grey", fill=TRUE, bg="white", lwd=0.05,mar=rep(1,4), ylim=c(-80,40) )

map('county', region = c('new york'),
    fill = TRUE, col = "grey")


testdata <- citibike[1,]
startpoint <- c(testdata$start.station.longitude, testdata$start.station.latitude)
endpoint <- c(testdata$end.station.longitude, testdata$end.station.latitude)
inter <- gcIntermediate(startpoint,endpoint,n=50,breakAtDateLine=FALSE,addStartEnd=FALSE,sp=FALSE,sepNA)
lines(inter, col="red", lwd=2)
map

ggmap(map)
