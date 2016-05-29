#실전 미션 12

rm(list=ls()) # 모든 변수의 값을 삭제합니다.

# stringsAsFactors argument가 TRUE면 CHARACTER 형식, FALSE면 FACTOR 형식으로 문자열을 읽어옴.
d <- read.csv("대중교통수단별이용현황.csv", sep=",", stringsAsFactors = FALSE)

# 불러온 자료의 열이름 및 행열 개수를 확인
names(d)
dim(d)          # row count / column count

d[order (d$노선명),]     # 행렬에서 sorting option

d$노선명                 # 특정 컬럼명 지정.

bp <- barplot(d$X1월/1000000, ylim=c(0,50), names.arg = d$노선명, main = "지하철 노선별 이용 승객 현황(1월)",
              col = "gray", cex.names=0.7, las = 3, ylab="승객수(백만명)", xlab="<호선>")

text(x = bp, y =d$X1월/1000000*0.95, labels =d$X1월/1000000, col = "red", cex = 0.7)

savePlot("1월현황.png", type="png")


# -----------------------------------------------------------------------------------------------

#실전 미션 13 답 :

rm(list=ls())

f <- read.csv("2호선역별이용인원수.csv", sep=",", stringsAsFactors = FALSE)
names(f)

a <- f$X2011년
b <- f$X2012년
c <- f$X2013년

l <- head(f$역명, 10)
x <- head(f$X2011년, 10)
y <- head(f$X2012년, 10)
z <- head(f$X2013년, 10)


plot(x, xlab="", ylab="", ylim=c(0,250000), axes=FALSE, type="o", col="red", main="2호선역 일평균 승객수(단위:명)")
axis(1, at=1:length(l), lab=l, las=2)
axis(2, las=1)

abline(h=c(50000,100000,150000,200000,250000), v=c(1:10), lty=2)   # grid line

lines(y, col="blue", type="o")       
lines(z, col="green", type="o")

colors <- c("red","blue","green")
legend(5, 220000, c("2011년","2012년","2013년"), cex=0.8, col=colors, lty=1, lwd=2)   # 범례 위치와 색

savePlot("line2_1.png", type="png")



# ----------------------------------------------------------------------------

noodle <- read.csv("라면가격변동과 물가상승율.csv", header=T, sep=",")

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

# -------------------------------------------------------

#2. ggplot2( ) 패키지를 설치한 후 로딩합니다.
install.packages("ggplot2")  
library(ggplot2)

#3. csv 파일을 읽어서 데이터 프레임을 생성합니다.
stemp <- read.csv("서울의기온변화.csv", header=T)
stemp

#4. ggplot > boxplot
ggplot(stemp, aes(factor(Month), MeanTemp)) + geom_boxplot()














