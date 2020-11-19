#2018
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

newdata$date <- as.Date(newdata$starttime)

Gline <- subset(newdata, 
                start.station.name=="Union Ave & Wallabout St" | 
                  start.station.name=="Tompkins Ave & Hopkins St" | 
                  start.station.name=="Willoughby Ave & Tompkins Ave" | 
                  start.station.name=="Kosciuszko St & Tompkins Ave" | 
                  start.station.name=="Marcy Ave & Lafayette Ave" | 
                  start.station.name=="Myrtle Ave & Marcy Ave" | 
                  start.station.name=="Nostrand Ave & Myrtle Ave" | 
                  start.station.name=="Kosciuszko St & Nostrand Ave" | 
                  start.station.name=="Greene Ave & Nostrand Ave" | 
                  start.station.name=="DeKalb Ave & Franklin Ave" | 
                  start.station.name=="Lafayette Ave & Classon Ave" | 
                  start.station.name=="Lexington Ave & Classon Ave" | 
                  start.station.name=="Lafayette Ave & St James Pl" | 
                  start.station.name=="Washington Ave & Greene Ave" | 
                  start.station.name=="Clermont Ave & Lafayette Ave" | 
                  start.station.name=="DeKalb Ave & Vanderbilt Ave" | 
                  end.station.name=="Union Ave & Wallabout St" | 
                  end.station.name=="Tompkins Ave & Hopkins St" | 
                  end.station.name=="Willoughby Ave & Tompkins Ave" | 
                  end.station.name=="Kosciuszko St & Tompkins Ave" | 
                  end.station.name=="Marcy Ave & Lafayette Ave" | 
                  end.station.name=="Myrtle Ave & Marcy Ave" | 
                  end.station.name=="Nostrand Ave & Myrtle Ave" | 
                  end.station.name=="Kosciuszko St & Nostrand Ave" | 
                  end.station.name=="Greene Ave & Nostrand Ave" | 
                  end.station.name=="DeKalb Ave & Franklin Ave" | 
                  end.station.name=="Lafayette Ave & Classon Ave" | 
                  end.station.name=="Lexington Ave & Classon Ave" | 
                  end.station.name=="Lafayette Ave & St James Pl" | 
                  end.station.name=="Washington Ave & Greene Ave" | 
                  end.station.name=="Clermont Ave & Lafayette Ave" | 
                  end.station.name=="DeKalb Ave & Vanderbilt Ave"
)
table(Gline$date)


#2017
citibike <- read.csv("C://ZLIU/SIT/658/Project/DATA/201708-citibike-tripdata.csv", header = T, sep = ",")
citibike[citibike=="NULL"]<-NA
citibike_delete<-na.omit(citibike)
point <- read.csv("C://ZLIU/SIT/658/Project/station.csv", header = T, sep = ",")
start <- point[,c(1,5,6)]
colnames(start) <- c("start.station.id", "start.location", "s_m_location")
end <- point[,c(1,5,6)]
colnames(end) <- c("end.station.id", "end.location", "e_m_location")

newdata <- merge(citibike_delete, start, by = "start.station.id", all.x = T)
newdata <- merge(newdata, end, by = "end.station.id", all.x = T)

newdata$date <- as.Date(newdata$starttime)

Gline <- subset(newdata, 
                start.station.name=="Union Ave & Wallabout St" | 
                  start.station.name=="Tompkins Ave & Hopkins St" | 
                  start.station.name=="Willoughby Ave & Tompkins Ave" | 
                  start.station.name=="Kosciuszko St & Tompkins Ave" | 
                  start.station.name=="Marcy Ave & Lafayette Ave" | 
                  start.station.name=="Myrtle Ave & Marcy Ave" | 
                  start.station.name=="Nostrand Ave & Myrtle Ave" | 
                  start.station.name=="Kosciuszko St & Nostrand Ave" | 
                  start.station.name=="Greene Ave & Nostrand Ave" | 
                  start.station.name=="DeKalb Ave & Franklin Ave" | 
                  start.station.name=="Lafayette Ave & Classon Ave" | 
                  start.station.name=="Lexington Ave & Classon Ave" | 
                  start.station.name=="Lafayette Ave & St James Pl" | 
                  start.station.name=="Washington Ave & Greene Ave" | 
                  start.station.name=="Clermont Ave & Lafayette Ave" | 
                  start.station.name=="DeKalb Ave & Vanderbilt Ave" | 
                  end.station.name=="Union Ave & Wallabout St" | 
                  end.station.name=="Tompkins Ave & Hopkins St" | 
                  end.station.name=="Willoughby Ave & Tompkins Ave" | 
                  end.station.name=="Kosciuszko St & Tompkins Ave" | 
                  end.station.name=="Marcy Ave & Lafayette Ave" | 
                  end.station.name=="Myrtle Ave & Marcy Ave" | 
                  end.station.name=="Nostrand Ave & Myrtle Ave" | 
                  end.station.name=="Kosciuszko St & Nostrand Ave" | 
                  end.station.name=="Greene Ave & Nostrand Ave" | 
                  end.station.name=="DeKalb Ave & Franklin Ave" | 
                  end.station.name=="Lafayette Ave & Classon Ave" | 
                  end.station.name=="Lexington Ave & Classon Ave" | 
                  end.station.name=="Lafayette Ave & St James Pl" | 
                  end.station.name=="Washington Ave & Greene Ave" | 
                  end.station.name=="Clermont Ave & Lafayette Ave" | 
                  end.station.name=="DeKalb Ave & Vanderbilt Ave"
)
table(Gline$date)


#freq map
freq_s_G <- as.data.frame(table(Gline$start.station.id))
colnames(freq_s_G) <- c("start.station.id", "start_freq")
freq_e_G <- as.data.frame(table(Gline$end.station.id))
colnames(freq_e_G) <- c("start.station.id", "end_freq")
freq_G <- merge(point,freq_s_G, by = "start.station.id", all.x = T)
freq_G <- merge(freq_G,freq_e_G, by = "start.station.id", all.x = T)
freq_G$all_freq <- freq_G$start_freq+freq_G$end_freq
freq_G$all_freq[freq_G$all_freq==0] <- NA

library(ggmap)
register_google(key = "AIzaSyAP4qAE6z-ODegyhBtSJcahn_9X0QP-8nU")
map <- get_googlemap(center = c(lon=-73.967207, lat=40.737686), zoom = 12, maptype = "roadmap")
ggmap(map)

Gline_map <- ggmap(map)+
  geom_point(data = freq_G,aes(x=start.station.longitude, y=start.station.latitude, color=all_freq, size=0.01, alpha=1/10))+
  scale_color_continuous(low = "yellow", high = "red", na.value = "light grey")+ 
  guides(size=F)
Gline_map

#shutdown weekend
Gline_SD <- subset(Gline, date=="2018-08-18" | date=="2018-08-19")
freq_G_SD <- as.data.frame(table(Gline_SD$start.station.id,Gline_SD$end.station.id))
colnames(freq_G_SD) <- c("start.station.id","end.station.id", "freq")
freq_G_SD <- merge(freq_G_SD, point, by = "start.station.id", all.x = T)
freq_G_SD <- freq_G_SD[,c("end.station.id","start.station.id","start.station.latitude","start.station.longitude","freq")]
point_end <- read.csv("C://ZLIU/SIT/658/Project/station-end.csv", header = T, sep = ",")
freq_G_SD <- merge(freq_G_SD, point_end, by = "end.station.id", all.x = T)
freq_G_SD <- subset(freq_G_SD, freq!=0)

link_map_SD <- ggmap(map)+
  geom_segment(data = freq_G_SD,aes(x=start.station.longitude, y=start.station.latitude,xend=end.station.longitude, yend=end.station.latitude, colour = freq, alpha=1))+
  scale_color_continuous(low = "yellow", high = "black")+
  geom_point(data = freq_G_SD,aes(x=start.station.longitude, y=start.station.latitude, color=freq, size=0.01))
link_map_SD



#not shutdown weekend
Gline_NSD <- subset(Gline, date=="2018-08-04" | date=="2018-08-05" | date=="2018-08-11" | date=="2018-08-12" | date=="2018-08-25" | date=="2018-08-26")
freq_G_NSD <- as.data.frame(table(Gline_NSD$start.station.id,Gline_NSD$end.station.id))
colnames(freq_G_NSD) <- c("start.station.id","end.station.id", "freq")
freq_G_NSD <- merge(freq_G_NSD, point, by = "start.station.id", all.x = T)
freq_G_NSD <- freq_G_NSD[,c("end.station.id","start.station.id","start.station.latitude","start.station.longitude","freq")]
point_end <- read.csv("C://ZLIU/SIT/658/Project/station-end.csv", header = T, sep = ",")
freq_G_NSD <- merge(freq_G_NSD, point_end, by = "end.station.id", all.x = T)
freq_G_NSD <- subset(freq_G_NSD, freq!=0)

link_map_NSD <- ggmap(map)+
  geom_segment(data = freq_G_NSD,aes(x=start.station.longitude, y=start.station.latitude,xend=end.station.longitude, yend=end.station.latitude, colour = freq, alpha=1))+
  scale_color_continuous(low = "yellow", high = "black")+
  geom_point(data = freq_G_NSD,aes(x=start.station.longitude, y=start.station.latitude, color=freq, size=0.01))
link_map_NSD
