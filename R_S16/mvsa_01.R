###############################################################
#
# 다변량 통계분석 1 (정여진 교수)
#
###############################################################

library(MVA)        # 교재 package
demo("Ch-MVA")
par(mfcol=c(1,1))

#--------------------------------------------------------------
# 공분산, 상관계수, 거리
#--------------------------------------------------------------

# 데이터 내의 어떤 구조나 패턴이 변수들 사이의 관계에 의해서, 또는 다른 개체들과의 
# 상대적인 거리에 의해서 나타날 수 있을까?

# 두 확률변수의 선형 의존성에 대한 측도 = 상관계수(correlation coefficient)
# 두 변수 사이의 관계가 선형이 아니라면 상관계수는 오해를 줄 수 있다.

measure
summary(measure)

df <- measure[, 1:3]
male <- measure[measure$gender == "male", 1:3]
female <- measure[measure$gender == "female", 1:3]

cor(df)
cor(male)
cor(female)

library(psych)
pairs.panels(male)
pairs.panels(female)

# 그러므로 데이터내의 개체들 간의 거리(distance) 개념이 중요.
# 일반적으로 유클리드 거리 사용
# 변수들은 서로 다른 척도(단위)를 갖기 때문에 먼저 표준화를 한 후 거리를 계산.

scale_measure <- scale(df, center = F)
scale_measure <- scale(df)              # 표준화 : 평균 0, 표준편차 1 로 변환

scale_measure
summary(scale_measure)
pairs.panels(scale_measure)

round(dist(scale_measure), 2)


#--------------------------------------------------------------
# outlier 찾기
#--------------------------------------------------------------

df <- USairpollution

head(df)
pairs.panels(df)

library(DMwR)
outlier.score <- lofactor(df, k = 5)
outlier.score
plot(density(outlier.score), main = "outlier score")

sort(outlier.score, decreasing = T)[1:10]
outliers <- order(outlier.score, decreasing = T)[1:3]  # score > 2.2 
outliers
df[outliers, ]

pairs.panels(df[-outliers, ])


#--------------------------------------------------------------
# 다변량 데이터의 시각화
#--------------------------------------------------------------

attach(USairpollution)
head(USairpollution)


# 1. 산점도 scatterplot

plot(popul ~ manu, data = USairpollution)

# 1-1. Bivariate boxplot

# 영역을 3개로 분할
layer <- matrix(c(2,0,1,3), nrow = 2, byrow = T)
layout(layer, widths = c(2,1), heights = c(1,2), respect = T)
# 각 영역에 순서대로 표시
xlim <- range(manu) * 1.1
plot(popul ~ manu, data = USairpollution, cex = 0.9, type = "n", xlim = xlim)
text(manu, popul, cex = 0.6, labels = row.names(USairpollution))
hist(manu, main = "", xlim = xlim)
boxplot(popul)
par(mfcol=c(1,1))

# outlier 있고 없고 상관계수 비교
df <- USairpollution
rownames(df) <- rep(1:nrow(df))
plot(popul ~ manu, data = df, cex = 0.9, type = "n")
text(manu, popul, cex = 0.9, labels = rownames(df))

outlier <- c(7, 30, 14)     # 시카고, 필라델피아, 디트로이트
cor(df$manu, df$popul)                      # 0.955
cor(df$manu[-outlier], df$popul[-outlier])  # 0.769 : 상관계수가 오히려 줄었다.


# 2. boxplot

chickwts
summary(chickwts)

boxplot(weight ~ feed, chickwts)
# casein & horsebean : 평균과 범위가 서로 많이 차이나기 때문에 독립적이라고 할 수 있다.
# linseed & meatmeal : 평균은 차이가 나지만 범위가 비슷하기 때문에 독립적이라고 하기 어렵다.



# 3. Bubble chart - 변수 3개 표현

symbols(temp, wind, circles = SO2, inches = 0.5)
legend(70, 13, "circle : SO2")

# 크기 2배 -> 면적은 4배
# sqrt 변환 : 크기가 비슷해져 변별력이 떨어진다.
symbols(temp, wind, circles = sqrt(SO2), inches = 0.5) 


ylim <- range(wind) * c(0.95, 1)
plot(wind ~ temp, data = USairpollution, pch = 10, ylim = ylim)
symbols(temp, wind, circles = SO2, inches = 0.5, add = T)



# 4. mosaic plot

UCBAdmissions

adm <- UCBAdmissions
adm[,,1]
adm[,1,]

mosaicplot(~ Dept + Gender, data = adm, color = T)
mosaicplot(~ Gender + Dept, data = adm, color = T)

library(reshape)
ucb <- melt(UCBAdmissions)
head(ucb)

df <- xtabs(value ~ Dept + Gender, data = ucb)
mosaicplot(~ Dept + Gender, data = df, color = T)

df2 <- xtabs(value ~ Dept + Admit, data = ucb)
mosaicplot(~ Dept + Admit, data = df2, color = T)



# 5. star plot

stars(USairpollution)

stars(USairpollution, key.loc = c(15, 2), cex = 0.8) # legend

stars(USairpollution, key.loc = c(15, 2), cex = 0.8, draw.segments = T)



# 6. Heat map : 군집분석을 함께 수행한다. 

bball <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
head(bball)

rownames(bball) <- bball$Name
bball <- bball[ , -1]
bball <- as.matrix(bball)

heatmap(bball) # 밝은 색이 높은 값. 변수별 값의 차이로 scaling 필요.

heatmap(bball, scale = "column", Colv = NA) 



# 7. 이변량자료 분포 시각화 & 군집화

library(MASS)
attach(geyser)
head(geyser)
dim(geyser)

density1 <- kde2d(waiting, duration, n = 25)
image(density1, xlab = "waiting", ylab = "duration")
str(density1)

density2 <- kde2d(waiting, duration, n = 100)
image(density2, xlab = "waiting", ylab = "duration")

# 등고선
contour(density2)



#--------------------------------------------------------------
# practice
#--------------------------------------------------------------

# 2005년 미국의 범죄율 데이터
# 인구 100,000명 중의 발생 비율

data <- read.csv("crime.csv", header = T)
head(data, 10)

crime <- data[-1, ]                     # United States (미국 전체) 제거
rownames(crime) <- c(1:nrow(crime))
head(crime, 10)


#--------------------------------------------------------------
# A. 살인(murder)와 절도(burglary) 사이의 산점도를 단변량 분포와 함께 그리시오. 
# 상관계수도 함께 살피시오.

cdata <- crime[ , c("murder", "burglary")]
head(cdata)

# 발생비율의 단위가 다르므로 비교를 위해 scaling 한다.
boxplot(scale(cdata))   # murder 가 특이하게 높은 이상점 존재
cor.test(cdata$murder, cdata$burglary)

library(psych)
pairs.panels(cdata)     # cor = 0.28


#--------------------------------------------------------------
# B. 위를 통해 이상점 존재여부를 판단하고 존재한다면 해당 주를 확인하고 제거하시오. 
# 제거 후 변수들 사이의 관계가 어떻게 변화하는지 살피시오.

# 사분위수를 이용한 outlier 확인
# fivenum : minimum, lower-hinge, median, upper-hinge, maximum
a <- cdata$murder
which(a > fivenum(a)[4] + 1.5*IQR(a))   # 9

# 산점도를 통한 outlier 확인
cdata <- crime[ , c("state", "murder", "burglary")]
head(cdata, 10)

plot(burglary ~ murder, data = cdata, cex = 0.8, type = "n")
text(cdata$murder, cdata$burglary, cex = 0.8, labels = rownames(cdata))   # outlier = 9

outlier = 9
cdata[outlier, c("state", "murder", "burglary")]   # District of Columbia 

# outlier 제거 후 상관계수 확인
cdata <- cdata[-outlier, ]
head(cdata, 10)

cor.test(cdata$murder, cdata$burglary)
pairs.panels(cdata[ , c(2:3)])          # cor = 0.62 상관계수 증가


#--------------------------------------------------------------
# C. 살인, 절도와 인구(population)의 관계를 함께 관찰하기 위해 bubble plot을 그리고 
# 관찰한 사실을 기술하시오.

# 원본
symbols(crime$murder, crime$burglary, circles = crime$population, inches = 0.5)

# outlier 제거시
cdata <- crime[-outlier, c("state", "murder", "burglary", "population")]
symbols(cdata$murder, cdata$burglary, circles = cdata$population, inches = 0.5)

# 살인과 절도 발생률은 어느 정도 상관관계가 있으나, 인구와는 상관관계가 없다.


#--------------------------------------------------------------
# D. 7가지 범죄의 발생 건수를 heatmap, 별그림, 나이팅게일 차트로 표현하고 
# 범죄 발생 특징 간의 패턴이 비슷한 주들이 있는지 살피시오.

cdata2 <- crime
rownames(cdata2) <- cdata2$state
cdata2 <- cdata2[ , -c(1,9)]     # state를 rowname으로 변경. 인구 변수 제거.
head(cdata2, 10)

# star & nightingale - dataframe
# star
stars(cdata2, key.loc = c(11, 2), cex = 0.8, ncol = 10)
# nightingale
stars(cdata2, key.loc = c(11, 2), cex = 0.8, draw.segments = T, ncol = 10)

# heatmap - matrix
cdata2 <- as.matrix(cdata2)
heatmap(cdata2, scale = "column", Colv = NA, cexCol = 0.9, margins = c(8, 5))



