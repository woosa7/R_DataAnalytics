#-------------------------------------------------------------------------------------
# 정형 데이터 분석 (엑셀 테이블)
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
# 1. Barplot

d <- read.csv("data/국가별_국토면적_및_인구밀도_비교.csv", sep=",")

names(d)
dim(d)                  # row count / column count

# ordering
d[order(d$국명), ]
d[order(d$국토면적, decreasing = T), ]  


d$국명
gsub(pattern = " ", replacement = "", x = d$국명);          #특정 패턴의 문자를 치환

d$그룹 <- ifelse(d$국토면적 > 500000, "그룹A", "그룹B");    # add new column 
#d[d$인구밀도 > 500, ]$인구밀도 <- 500                      # change column value.

table(d$인구밀도)
counts <- table(d$그룹);counts

barplot(counts, main="국토면적구분", xlab="국토면적 500,000(㎢) 기준")

bp <- barplot(d$인구밀도, names.arg = d$국명, main="국가별 인구밀도", col="lightcyan", ylim=c(0,1000), cex.names=0.7, las=2)
text(x = bp, y = d$인구밀도*0.9, labels = d$인구밀도, col = "red", cex = 0.7)


#-------------------------------------------------------------------------------------
# 2. Horizontal Barplot

rm(list=ls())                 # 모든 변수 값 삭제

# stringsAsFactors = TRUE면 CHARACTER, FALSE면 FACTOR로 문자열을 변환
d <- read.csv("data/대중교통수단별이용현황.csv", sep=",", stringsAsFactors = FALSE)

# 불러온 자료의 열이름 및 행열 개수를 확인
names(d)
dim(d)          # row count / column count

d[order (d$노선명, decreasing = T),]
d$노선명

bp <- barplot(d$X1월/1000000, xlim=c(0,50), names.arg = d$노선명, main = "지하철 노선별 이용 승객 현황(1월)",
              col = "gray", cex.names=0.7, las = 1, xlab="승객수(백만명)", ylab="<Line>", horiz=T)

text(y=bp, x=d$X1월/1000000 + 2, labels=round(d$X1월/1000000,1), col = "red", cex = 0.7)



#-----------------------------------------------------------------------------------------------
# 3. Line Chart : plot / type="o"

rm(list=ls())

f <- read.csv("data/2호선역별이용인원수.csv", sep=",", stringsAsFactors = FALSE)
names(f)

a <- f$X2011년
b <- f$X2012년
c <- f$X2013년

l <- head(f$역명, 25)
x <- head(f$X2011년, 25)
y <- head(f$X2012년, 25)
z <- head(f$X2013년, 25)


plot(x, xlab="", ylab="", ylim=c(0,250000), axes=FALSE, type="o", col="red", main="2호선역 일평균 승객수(단위:명)")
axis(1, at=1:length(l), lab=l, las=2)
axis(2, las=1)

abline(h=c(50000,100000,150000,200000,250000), v=c(1:25), lty=2)   # grid line

lines(y, col="blue", type="o")       
lines(z, col="green", type="o")

colors <- c("red","blue","green")
legend(5, 220000, c("2011년","2012년","2013년"), cex=0.8, col=colors, lty=1, lwd=2)   # 범례 위치와 색


#-----------------------------------------------------------------------------------------------
# 4. Barplot : as.matrix

line2 <- read.csv("data/강남역승하차.csv")

as.matrix(line2)

barplot(as.matrix(line2)/1000, main="강남역 시간대별 승하차 현황",
        ylab="인원수(단위:천명)", beside=TRUE, las=2, ylim=c(0,500))

abline(h=seq(3,400,10), col="white", lwd=2)
abline(h=c(50,100,150,200,250,300,350,400,450),lty=2)

legend("topright", c("승차","하차"), cex=0.8, fill=c("black","white"))


#-----------------------------------------------------------------------------------------------
# 5. Line Chart : plot / type="o"

f <- read.csv("data/서울지하철_역별_시간대별_승하차인원수_total.csv", sep=",", stringsAsFactors = FALSE)

a <- (f$승차합계/10000)
b <- (f$하차합계/10000)

yrange <- range(0,a,b)     # 최저값과 최고값 사이의 범위

plot(a, xlab="", ylab="", ylim=yrange, axes=FALSE, type="o", col="red", main="5월 역별 승하차 인원수(단위:만명)")

axis(1, at=1:50, lab=c(f$역명), las=2)
axis(2,las=1)

abline(h=c(50,100,150,200,250,300), v=c(5,10,15,20,25,30,35,40,45),lty=2)

lines(b, col="blue", type="o")

colors <- c("red","blue")
legend(35,300,c("승차","하차"),cex=0.8,col=colors,lty=1,lwd=2)



#----------------------------------------------------------------------------
# 6.

noodle <- read.csv("data/라면가격변동과 물가상승율.csv", header=T, sep=",")

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


