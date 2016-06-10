
setwd("/Volumes/MacHDD/workspace/R_Study/R_JS")

#---------------------------------------------------------------------------

library(ggmap)

loc <- read.csv("data/library_blind_seoul.csv", header=T, encoding = "UTF-8")
loc
str(loc)

kor <- get_map("seoul", zoom=11, maptype = "roadmap")

kor.map <- ggmap(kor)+geom_point(data=loc, aes(x=LON, y=LAT),size=5,alpha=0.7)

kor.map + geom_text(data=loc, aes(x = LON, y = LAT+0.01, label=guName),size=3)

ggsave("g:/temp/r_temp/lib.png",dpi=500)


#---------------------------------------------------------------------------

library(ggmap)
library(grid)

pop <- read.csv("data/population_201404.csv", header=T, encoding = "UTF-8")
pop

lon <- pop$LON
lat <- pop$LAT
data <- pop$poeple
df <- data.frame(lon,lat,data)
df

map1 <- get_map("Jeonju",zoom=7 , maptype='roadmap')
map1 <- ggmap(map1)
map1

map1 + geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)

ggsave("pop.png",scale=1,width=7,height=4,dpi=1000)


# maptype='terrain'
map2 <- get_map("Jeonju",zoom=7 , maptype='terrain')
map2 <- ggmap(map2)
map2
map2 + geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)

ggsave("pop2.png",scale=1,width=7,height=4,dpi=1000)


# maptype='satellite'
map3 <- get_map("Jeonju",zoom=7 , maptype='satellite')
map3 <- ggmap(map3)
map3
map3 + geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)

ggsave("pop3.png",scale=1,width=7,height=4,dpi=1000)


# maptype='hybrid'
map4 <- get_map("Jeonju",zoom=7 , maptype='hybrid')
map4 <- ggmap(map4)
map4
map4 + geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)

ggsave("pop4.png",scale=1,width=7,height=4,dpi=700)


# 버블 모양 변경
map5 <- get_map("Jeonju",zoom=7 , maptype='hybrid')
map5 <- ggmap(map5)
map5 + stat_bin2d(aes(x=lon,y=lat,colour=data,fill=factor(data),size=data),data=df)

ggsave("pop5.png",scale=2,width=7,height=4,dpi=700)


#---------------------------------------------------------------------------


