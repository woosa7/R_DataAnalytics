################################################################
#
# R을 활용한 통계분석 : 정여진 교수 (2016 여름특강)
#
# 1. 경영통계 요약정리
#
################################################################

#---------------------------------------------------------------
# 질적 자료 (범주형) : 명목척도 / 서열척도
# 양적 자료 : 구간척도 (원점 없음) / 비율척도 (원점 존재)
#---------------------------------------------------------------

#---------------------------------------------------------------
# 양적 자료의 요약
#---------------------------------------------------------------

library(reshape)

attach(tips)    # dataframe name 호출하지 않아도 됨.
summary(tips)

library(psych)
describe(tips)
headTail(tips)

# 양적자료인 size를 범주형 자료로 처리하길 원하는 경우.
tips2 <- tips
tips2$size = factor(tips2$size)
summary(tips2)

# median : 극단값이 있는 경우 중심위치 측정에 선호
tips2$tip[1] = 100
quantile(tips$tip)
quantile(tips2$tip)

# 분산 / 표준편차
var(tip)
sd(tip)

# 변동계수(Coefficient of Variation) = 표준편차 / 평균
# 표준편차가 평균에 비하여 얼마나 큰지 나타냄. 두 집단의 변동성 비교시 사용.
# 평균이 클수록 표준편차가 큰 경향이 있기 때문에 표준편차로 변동성 비교할 수 없음.
sd(tip) / mean(tip)

# 사분위수 범위 (interquartile range, IQR)
# Q1과 Q3의 차이. 자료의 중간 50%의 범위. 극단값에 상대적으로 덜 민감.
IQR(tip)

# plot
boxplot(tip, col = "red", horizontal = T, xlab = "Tip")

hist(tip)
hist(tip, 20, probability = T, xlim = c(0,10), ylim = c(0,0.5))   # break : 20 / probability : 갯수가 아닌 확률로 표시
lines(density(tip), col="blue")

# Q-Q Normality Plot : 자료가 정규분포에 얼마나 근접한지 판단
qqnorm(tip)
qqline(tip) # 점들이 선위에 가까이 있을수로 정규분포를 따름.


#---------------------------------------------------------------
# 질적자료의 요약
#---------------------------------------------------------------

# table() : 도수분포표
# barplot()
# pie()

summary(day)
barplot(table(day))

tips$day <- factor(day, levels = c("Thur","Fri","Sat","Sun"))   # 요일 순서대로 나오도록 factor levels 변경
summary(day)
mytable <- table(day)
mytable

barplot(mytable)
barplot(mytable / sum(mytable))

lbl <- paste(names(mytable), ", ", round(mytable/sum(mytable)*100), "%", sep="")
pie(mytable, labels = lbl, col = rainbow(length(mytable)), main = "Day of Tips")


#---------------------------------------------------------------
# 두 변수의 요약
#---------------------------------------------------------------

# 두 범주형 변수의 요약
# xtabs(~그룹변수1 + 그룹변수2, data) : 분할표

# 성별~요일별
mytable2 <- xtabs(~sex+day, tips)
mytable2

barplot(mytable2, legend.text = c("Female", "Male"), ylim = c(0,100))
barplot(mytable2, legend.text = c("Female", "Male"), ylim = c(0,80), beside = T)

barplot(xtabs(~sex+smoker, tips), legend.text = c("Female", "Male"), ylim = c(0,100), beside = T, xlab = "smoker")
barplot(xtabs(~size+time, tips), legend.text = c(1,2,3,4,5,6), beside = T, xlab = "size - time")

mosaicplot(mytable2)     # 성별 기준으로 요일별 비교
mosaicplot(t(mytable2))  # 요일 기준으로 성별 비교

# 범주형 변수와 양적 변수의 요약 
boxplot(tip~day, data=tips, ylab="tips", xlab="day")

# 두 양적변수의 요약 (주문금액 대비 팁)
plot(tip~total_bill, tips)


#--------------------------------------------
# practice
#--------------------------------------------

kmovie <- read.csv("movie_utf8.csv", stringsAsFactors = F)
View(kmovie)
str(kmovie)

quantile(kmovie$total_seen)
kmovie$total_seen <- kmovie$total_seen/1000

quantile(kmovie$total_sales)
kmovie$total_sales <- kmovie$total_sales/1000000

attach(kmovie)

#--------------------------------------------
# 1. 총관객수(total_seen)에 대해 통계량

mean(total_seen)
median(total_seen)
sd(total_seen)

quantile(total_seen)
summary(total_seen)
IQR(total_seen)

boxplot(total_seen, horizontal=T)
hist(total_seen, 20)   # ---> 우측에 outlier가 너무 많다. 적당히 변환 후 분석 진행 필요 

#--------------------------------------------
# 2. 등급별 평균 매출액

unique(rating)
aggregate(total_sales~rating, data = kmovie, mean)
par(las = 2, mar = c(10,5,5,5))   
boxplot(total_sales~rating, data = kmovie, ylab="sales (x1,000,000)", xlab="rating")
boxplot(log(total_sales)~rating, data = kmovie, ylab="sales : log(x1,000,000)", xlab="rating")

library(plyr)
msales <- ddply(kmovie, ~rating, summarise, mean_sales=mean(total_sales))
msales
barplot(msales[,2], names.arg = msales[,1])

#--------------------------------------------
# 3. 등급별 개봉 영화 수 : barplot & pie chart
tableOfRating <- table(rating)
tableOfRating

# las : style of axis labels. mar : plot margin, c(bottom, left, top, right)
bp <- barplot(tableOfRating, ylab="Count", xlab="Rating", ylim = c(0,100), las = 2, mar = c(10,5,5,5))
text(x = bp, y = tableOfRating*0.9, labels = tableOfRating, col = "red", cex = 0.9)

lbl2 <- paste(names(tableOfRating), "\n", round(prop.table(tableOfRating)* 100,2), "%", sep="")
pie(tableOfRating, labels = lbl2, col = rainbow(4))

#--------------------------------------------
# 4. 등급과 장르 사용해 분할표 생성. bar plot과 mosaic plot 그리기.

tab = xtabs(~genre+rating, kmovie)
tab

barplot(tab, legend.text=row.names(tab), col = rainbow(9), beside=T, ylim = c(0,40))

mosaicplot(tab)
mosaicplot(t(tab))
