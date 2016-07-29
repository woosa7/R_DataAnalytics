#----------------------------------------------------------------------------
# 지도 데이터 활용하기 (google map)
#----------------------------------------------------------------------------

require(ggmap)

#---------------------------------------------------------------------------
locData <- read.csv("data/library_blind_seoul.csv", header=T, encoding = "UTF-8")
head(locData)

# 서울 지도 가져오기
kor <- get_map("seoul", zoom=11, maptype = "roadmap")
# geom_point = x y 좌표 : 위도 경도 / size : 표시점 크기
# geom_text = 라벨 표시
kor.map <- ggmap(kor) + geom_point(data=locData, aes(x=LON, y=LAT), size=7, alpha=0.7)
kor.map + geom_text(data=locData, aes(x = LON, y = LAT+0.01, label=libCode), size=3)

ggsave("24_ggmap_library.png", dpi=1000)


#---------------------------------------------------------------------------
popData <- read.csv("data/population_201404.csv", header=T, encoding = "UTF-8")
popData

population <- round(popData$poeple/10000)  # 인구수(만명)

df <- data.frame(popData$LON, popData$LAT, population)
df

# maptype = roadmap / terrain / satellite / hybrid
map1 <- ggmap( get_map("Jeonju", zoom=7, maptype='roadmap') )
map1
map1 + geom_point(aes(x=popData$LON, y=popData$LAT, colour=population, size=population), data=df)

ggsave("24_ggmap_population.png", scale=1, width=7, height=4, dpi=1000)


# 버블 모양 변경
map2 <- ggmap( get_map("Jeonju", zoom=7, maptype='roadmap') )
map2 + stat_bin2d(aes(x=popData$LON, y=popData$LAT, colour=population, fill=factor(population), size=population), data=df)

ggsave("24_ggmap_population_bubble.png", scale=2, width=7, height=4, dpi=700)


#---------------------------------------------------------------------------
location <- read.csv("data/jeju_tour_course.csv", header=T, encoding = "UTF-8")
location

kor <- get_map("Hallasan", zoom=10, maptype = "roadmap")
kor.map <- ggmap(kor) + geom_point(data=location, aes(x=LON, y=LAT), size=3, alpha=0.7, col="red")

# geom_path : 경로 표시.
kor.map + geom_path(data=location, aes(x=LON, y=LAT), size=1, linetype=2, col="green") +
    geom_text(data=location, aes(x=LON, y=LAT+0.005, label=point), size=2)

ggsave("24_ggmap_jeju_tour_path.png", dpi=700)




#----------------------------------------------------------------------------
# Google Motion Chart
#----------------------------------------------------------------------------

require(googleVis)

#----------------------------------------------------------------------------
Fruits      # 내장 연습용 데이터

# 각 과일별 연도별 판매량
fchart <- gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year")
plot(fchart)     # 자동으로 웹브라우저 실행됨



#----------------------------------------------------------------------------
# 시간대별 1-4호선 승하차 승객수

pdata <- read.csv("data/line_1_4_passengers.csv", header = T, sep = ",")
head(pdata)

pchart <- gvisMotionChart(pdata, idvar = "line_no", timevar = "time", options = list(width=1000, height=500))
plot(pchart)




