#-------------------------------------------------------------------------------------
# 정형 데이터 분석 (엑셀 테이블)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# 1. Barplot
#-------------------------------------------------------------------------------------

#-----------------------------------------------
# 1-1. 국가별 면적과 인구밀도 비교

d <- read.csv("data/nations_land_area.csv", sep=",")

names(d)
dim(d)                  # row count / column count
str(d)

# ordering
d[order(d$Nation), ]
d[order(d$LandArea, decreasing = T), ]  

d$Nation
# gsub(pattern = " ", replacement = "", x = d$Nation);          #특정 패턴의 문자를 치환

d$Group <- ifelse(d$LandArea > 500000, "Group_A", "Group_B");    # add new column 
#d[d$PopDensity > 500, ]$PopDensity <- 500                      # change column value.

table(d$PopDensity)
counts <- table(d$Group);counts

barplot(counts, main="Land Area Group", xlab="500,000(km2) break")

bp <- barplot(d$PopDensity, names.arg = d$Nation, main="Popultion Density", col="lightcyan", ylim=c(0,1000), cex.names=0.7, las=2)
text(x = bp, y = d$PopDensity*0.9, labels = d$PopDensity, col = "red", cex = 0.7)

# Sort : PopDensity
data <- d[order(d$PopDensity, decreasing = T), ]  
data

bp <- barplot(data$PopDensity, names.arg = data$Nation, main="Popultion Density", col="lightcyan", ylim=c(0,1000), cex.names=0.7, las=2)
text(x = bp, y = data$PopDensity*0.9, labels = data$PopDensity, col = "red", cex = 0.7)


#-----------------------------------------------
# 1-2. 대중교통 수단별 이용 현황

d <- read.csv("data/usage_public_transportation.csv",sep=",", stringsAsFactors = FALSE)

names(d)
dim(d)

d[order (d$LINE),]
unitNumber <- 1000000


# Vertical Barplot

bp <- barplot(d$M01/unitNumber, ylim=c(0,50),
              names.arg = d$LINE, main = "Seoul Subway Usage per Line(January)",
              col = "lightcyan", cex.names=0.7, las = 3, ylab="people(/million)", xlab="<Line>")

text(x=bp, y=d$M01/unitNumber*0.95, labels = round(d$M01/unitNumber, 2), col = "red", cex = 0.7)


# Horizontal Barplot

bp <- barplot(d$M01/unitNumber, xlim=c(0,50),
              names.arg = d$LINE, main = "Seoul Subway Usage per Line(January)",
              col = "gray", cex.names=0.7, las = 1, xlab="people(/million)", ylab="<Line>",
              horiz=T)

text(y=bp, x=d$M01/unitNumber + 2, labels=round(d$M01/unitNumber,2), col = "red", cex = 0.7)



#-----------------------------------------------------------------------------------------------
# 3. Line Chart : plot / type="o"
#-----------------------------------------------------------------------------------------------

rm(list=ls())

# csv : fileEncoding="CP949"
f <- read.csv("data/passengers_line2_station.csv", sep=",", stringsAsFactors = FALSE, fileEncoding="CP949")
head(f)

a <- f$y2011
b <- f$y2012
c <- f$y2013

l <- head(f$station, 25)
x <- head(f$y2011, 25)
y <- head(f$y2012, 25)
z <- head(f$y2013, 25)

plot(x, xlab="", ylab="", ylim=c(0,250000), axes=FALSE, type="o", col="red", main="2호선역 일평균 승객수(명)")
axis(1, at=1:length(l), lab=l, las=2)
axis(2, las=1)

abline(h=c(50000,100000,150000,200000,250000), v=c(1:25), lty=2)   # grid line

lines(y, col="blue", type="o")       
lines(z, col="green", type="o")

colors <- c("red","blue","green")
legend(5, 220000, c("2011년","2012년","2013년"), cex=0.8, col=colors, lty=1, lwd=2)   # 범례 위치와 색



#-----------------------------------------------------------------------------------------------
# 4. Barplot : as.matrix
#-----------------------------------------------------------------------------------------------

gangnam <- read.csv("data/line2_gangnam.csv")
gangnam
mode(gangnam)

as.matrix(gangnam)

barplot(as.matrix(gangnam)/1000, main="강남역 시간대별 승하차 현황",
        ylab="인원수(천명)", beside=TRUE, las=2, ylim=c(0,500))

#abline(h=seq(3,400,10), col="white", lwd=2)
abline(h=c(50,100,150,200,250,300,350,400,450),lty=2)

legend("topright", c("승차","하차"), cex=0.8, fill=c("black","white"))



#-----------------------------------------------------------------------------------------------
# 5. Line Chart : plot / type="o"
#-----------------------------------------------------------------------------------------------

f <- read.csv("data/passengers_seoulmetro_line2_total.csv", sep=",", stringsAsFactors = FALSE, fileEncoding="CP949")
f

getIn <- (f$getIn/10000)
getOut <- (f$getOut/10000)

yrange <- range(0, getIn, getOut)     # 최저값과 최고값 사이의 범위
yrange

plot(getIn, xlab="", ylab="", ylim=yrange, axes=FALSE, type="o", col="red", main="2호선 역별 승하차인원(만명)")

axis(1, at=1:50, lab=c(f$station), las=2)
axis(2,las=1)

abline(h=c(50,100,150,200,250,300), v=c(5,10,15,20,25,30,35,40,45),lty=2)

lines(getOut, col="blue", type="o")

colors <- c("red","blue")
legend(35,300,c("승차","하차"),cex=0.8,col=colors,lty=1,lwd=2)



#----------------------------------------------------------------------------
# 6.

noodle <- read.csv("data/inflation_ramyon_rate.csv", header=T, sep=",", fileEncoding="CP949")

# ann : x, y 축 타이틀 표시 여부

plot(noodle$년도, noodle$누적물가상승율, type="o", ylim=c(-3,1200), ann=FALSE, col="red", lwd=2)
par(new=T)                          # 그래프를 겹쳐 그린다는 의미
plot(noodle$년도, noodle$누적상승율, type="o", ylim=c(-3,1200), axes=FALSE, ann=FALSE, col="blue", lwd=2)

title(main="물가상승률 및 라면값 상승율 비교")
title(xlab="년도", col.lab="blue")
title(ylab="누적상승율(단위:%)", col.lab="red")

abline(h=seq(50,1200,50), col="gray", lty=2, lwd=0.5)
abline(v=seq(1980,2015,1), col="gray", lty=2, lwd=0.5)

colors <- c("red","blue")
legend(1982, 1150, c("물가상승율","라면값상승율"), cex=0.8, col=colors, lty=1, lwd=2, fill="white", bg="white")


