################################################################
#
# ADSP Lecture
#
################################################################


################################################################
# 회귀분석
################################################################

#--------------------------------------------------------------
# 상관계수 correlation coefficient
#--------------------------------------------------------------

data("mtcars")
mtcars
str(mtcars)

attach(mtcars)

# disp 배기량 / drat 후방차축비율 / wt 차량 무게
plot(disp, drat)
plot(disp, wt)

cov(disp, wt)   # 공분산
cor(disp, wt)   # 상관계수

cov(mtcars)
cor(mtcars)     # 모든 변수간의 상관계수 테이블 표시

library(psych)
pairs.panels(mtcars[ , c("disp", "wt", "drat")])

# 피어슨 상관계수 - 일반적인 경우. 연속형 변수.
# 스피어만 상관계수 - 서열척도(순서형 변수)인 경우 사용.

library(Hmisc)

rcorr(as.matrix(mtcars), type = "pearson")$r

rcorr(disp, wt, type = "pearson")   # P : 유의확률. 0.05 이하 값 유의미 
rcorr(disp, wt, type = "spearman")  


# 순서형변수 샘플 데이터

set.seed(9)
sno <- seq(1001, 1050)
Korean <- sample(1:50, replace = F)
English <- sample(1:50, replace = F)
Math <- sample(1:50, replace = F)

rank <- cbind(sno, Korean, English, Math)
rownames(rank) <- seq(1,50)
rank    

rcorr(as.matrix(rank), type = "spearman")    


#--------------------------------------------------------------
# 모델 선택법
#--------------------------------------------------------------

x1 <- c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10)
x2 <- c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68)
x3 <- c(6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8)
x4 <- c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12)
y <- c(78, 74, 104, 87, 95, 109, 102, 72, 93, 115, 83, 113, 109)

df <- data.frame(x1, x2, x3, x4, y)
df

# (1) Backward

model <- lm(y ~ x1 + x2 + x3 + x4, data = df)
summary(model)
        # 회귀계수 중 p-value 가장 높은 x3 제거

model <- lm(y ~ x1 + x2 + x4, data = df)
summary(model)
        # 회귀계수 중 p-value 가장 높은 x4 제거

model <- lm(y ~ x1 + x2, data = df)
summary(model)


# (2) 모델 선택 자동화

model <- lm(y ~ x1 + x2 + x3 + x4, data = df)

# 전체 AIC 보다 낮은 AIC를 가진 회귀계수 제거
step(model, direction = "backward")
step(model, direction = "both")


library(MASS)
attach(hills)
head(hills)

step(lm(time ~ dist + climb, data = hills), direction = "backward")

step(lm(time ~ 1, data = hills), 
     scope = list(lower = ~ 1, upper = ~ dist + climb), direction = "forward")



################################################################
# 시계열 분석 Time Series
################################################################

# 1. 시계열 자료 - 시간의 흐름에 따라 관찰된 데이터
#
#
# 2. 정상성
# 대부분의 시계열 자료는 다루기 어려운 비정상성 시계열 자료이기 때문에
# 분석하기 쉬운 정상성 시계열 자료로 변환
# (1) 평균이 일정 : 모든 시점에 대해 일정한 평균을 가진다.
# - 평균이 일정하지 않은 시계열은 차분(difference)을 통해 정상화
# - 차분은 현시점 자료에서 이전 시점 자료를 빼는 것
# (2) 분산도 시점에 의존하지 않음
# - 분산이 일정하지 않은 시계열은 변환(transformation)을 통해 정상화
# (3) 공분산도 시차에만 의존할 뿐, 특정 시점에는 의존하지 않음
#
#
# 3. 시계열 모형
#
# (1) 자기회귀 모형 (Autoregressive model, AR)
#
# P 시점 이전의 자료가 현재 자료에 영향을 줌
# 오차항 = 백색잡음과정(white noise process)
# 자기상관함수(Autocorrelation Function, ACF) : k 기간 떨어진 값들의 상관계수
# 부분자기상관함수(partial ACF) : 서로 다른 두 시점의 중간에 있는 값들의 영향을 제외시킨 상관계수
# ACF 빠르게 감소, PACF는 어느 시점에서 절단점을 갖는다
# PACF가 2시점에서 절단점 가지면 AR(1) 모형
#
# (2) 이동평균 모형 (Moving average model, MA)
#
# 유한한 갯수의 백색잡음 결합이므로 항상 정상성 만족
# ACF가 절단점을 갖고, PACF는 빠르게 감소
#
# (3) 자기회귀누적이동평균 모형 (Autoregressive integrated moving average model, ARIMA)
#
# 비정상 시계열 모형
# 차분이나 변환을 통해 AR, MA, 또는 이 둘을 합한 ARMA 모형으로 정상화
# ARIMA(p, d, q) - d : 차분 차수 / p : AR 모형 차수 / q : MA 모형 차수
#
# (4) 분해 시계열
#
# 시계열에 영향을 주는 일반적인 요인을 시계열에서 분리해 분석하는 방법
# 계절 요인(seasonal factor), 순환 요인(cyclical), 추세 요인(trend), 불규칙 요인(random)

#--------------------------------------------------------------
# R functions for TimeSeries
#--------------------------------------------------------------

# 1) 소스 데이터를 시계열 데이터로 변환
ts(data, frequency = n, start = c(시작년도, 월))

# 2) 시계열 데이터를 x, trend, seasonal, random 값으로 분해
decompose(data)

# 3) 시계열 데이터를 이동평균한 값 생성
SMA(data, n = 이동평균수)

# 4) 시계열 데이터를 차분
diff(data, differences = 차분횟수)

# 5) ACF 값과 그래프를 통해 래그 절단값을 확인
acf(data, lag.max = 래그수)

# 6) PACF 값과 그래프를 통해 래그 절단값을 확인
pacf(data, lag.max = 래그수)

# 7) 데이터를 활용하여 최적의 ARIMA 모형을 선택
auto.arima(data)

# 8) 선정된 ARIMA 모형으로 데이터를 보정(fitting)
arima(data, order = c(p, d, q))

# 9) ARIMA 모형에 의해 보정된 데이터를 통해 미래값을 예측
forecast.Arima(fittedData, h = 미래예측수)

# 10) 시계열 데이터를 그래프로 표현
plot.ts(시계열데이터)

# 11) 예측된 시계열 데이터를 그래프로 표현
plot.forecast(예측된시계열데이터)


#--------------------------------------------------------------
# Decompose non-seasonal data
# 영국왕들의 사망시 나이
#--------------------------------------------------------------

library(TTR)
library(forecast)

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip = 3)
kings

# 시계열 데이터로 변환
kings_ts <- ts(kings)
kings_ts
plot.ts(kings_ts)

# 이동평균
kings_sma3 <- SMA(kings_ts, n = 3)
kings_sma8 <- SMA(kings_ts, n = 8)
kings_sma12 <- SMA(kings_ts, n = 12)

par(mfrow = c(2,2))

plot.ts(kings_ts)
plot.ts(kings_sma3)
plot.ts(kings_sma8)
plot.ts(kings_sma12)

# 차분을 통해 데이터 정상화
kings_diff1 <- diff(kings_ts, differences = 1)
kings_diff2 <- diff(kings_ts, differences = 2)
kings_diff3 <- diff(kings_ts, differences = 3)

plot.ts(kings_ts)
plot.ts(kings_diff1)    # 1차 차분만 해도 어느정도 정상화 패턴을 보임
plot.ts(kings_diff2)
plot.ts(kings_diff3)

par(mfrow = c(1,1))

mean(kings_diff1); sd(kings_diff1)

# 1차 차분한 데이터로 ARIMA 모형 확인
acf(kings_diff1, lag.max = 20)      # lag 2부터 점선 안에 존재. lag 절단값 = 2. --> MA(1)
pacf(kings_diff1, lag.max = 20)     # lag 4에서 절단값 --> AR(3)
                                    # --> ARIMA(3,1,1)

# 자동으로 ARIMA 모형 확인
auto.arima(kings)   # --> ARIMA(0,1,1)

# 예측
kings_arima <- arima(kings_ts, order = c(3,1,1))    # 차분통해 확인한 값 적용
kings_arima
kings_fcast <- forecast.Arima(kings_arima, h = 5)
kings_fcast
plot.forecast(kings_fcast)

kings_arima1 <- arima(kings_ts, order = c(0,1,1))   # auto.arima 추천값 적용
kings_arima1
kings_fcast1 <- forecast.Arima(kings_arima1, h = 5)
kings_fcast1
plot.forecast(kings_fcast1)


#--------------------------------------------------------------
# Decompose seasonal data
# 1946년 1월부터 1959년 12월까지 뉴욕의 월별 출생자 수 데이터
#--------------------------------------------------------------

data <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
data

birth <- ts(data, frequency = 12, start = c(1946, 1))
birth
plot.ts(birth, main = "뉴욕 월별 출생자 수")

# 데이터 분해 - trend, seasonal, random 데이터 추세 확인
birth_comp <- decompose(birth)
plot(birth_comp)

birth_comp$trend
birth_comp$seasonal

# 시계열 데이터에서 계절성 요인 제거
birth_adjusted <- birth - birth_comp$seasonal
plot.ts(birth_adjusted, main = "birth - seasonal factor")

# 차분을 통해 정상성 확인
birth_diff1 <- diff(birth_adjusted, differences = 1)
plot.ts(birth_diff1, main = "1차 차분")   
        # 분산의 변동성이 크다

acf(birth_diff1, lag.max = 20)
pacf(birth_diff1, lag.max = 20)
        # PACF 절단값이 명확하지 않아 ARIMA 모형 확정이 어렵다.

# Auto.Arima 함수 사용
auto.arima(birth)   # ARIMA(2,1,2)(1,1,1)[12]

birth_arima <- arima(birth, order = c(2,1,2), seasonal = list(order = c(1,1,1), period = 12))
birth_arima
birth_fcast <- forecast.Arima(birth_arima)
birth_fcast
plot(birth_fcast, main = "Forecasts 1960 & 1961")


#--------------------------------------------------------------
# 1987년 1월부터 1993년 12월까지 리조트 기념품매장 매출액
#--------------------------------------------------------------

data <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
data

fancy <- ts(data, frequency = 12, start = c(1987, 1))
fancy
plot.ts(fancy)   # 분산이 증가하는 경향 --> log 변환으로 분산 조정

fancy_log <- log(fancy)
plot.ts(fancy_log)

fancy_diff <- diff(fancy_log, differences = 1)
plot.ts(fancy_diff)   
        # 평균은 어느정도 일정하지만 특정 시기에 분산이 크다 
        # --> ARIMA 보다는 다른 모형 적용 추천

acf(fancy_diff, lag.max = 100)
pacf(fancy_diff, lag.max = 100)

auto.arima(fancy)   # ARIMA(1,1,1)(0,1,1)[12]

fancy_arima <- arima(fancy, order = c(1,1,1), seasonal = list(order = c(0,1,1), period = 12))
fancy_fcast <- forecast.Arima(fancy_arima)
plot(fancy_fcast)


#--------------------------------------------------------------
# 1500년부터 1969년까지 화산폭발 먼지량
#--------------------------------------------------------------

data <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip = 1)
data

dust <- ts(data, start = c(1500))
dust
plot.ts(dust)  # 한두개 데이터를 제외하고는 평균과 분산이 어느정도 일정하다 --> 차분 안함.

acf(dust, lag.max = 20)     # lag = 4 : MA(3)
pacf(dust, lag.max = 20)    # lag = 3 : AR(2)

auto.arima(dust)            # ARIMA(1,0,2)

# d = 0 이므로 AR(2) / MA(3) / ARIMA(2,0,3) 중 선택해서 적용 가능.
# 모수가 가장 적은 모형을 선택하는 것을 추천 --> AR(2) 적용

dust_arima <- arima(dust, order = c(2,0,0))
dust_fcast <- forecast.Arima(dust_arima, h = 30)
plot.forecast(dust_fcast)



################################################################
# 다차원 척도법 Multi-Dimensional Scaling
################################################################

# 개체들 사이의 유사성/비유사성을 측정하여 2차원 또는 3차원 공간상에 점으로 표현하는 분석 방법.
# 개체들간의 근접성(proximity)을 시각화하여 데이터 속에 잠재해 있는 패턴이나 구조를 찾아내는 통계 기법.
# 개체들간의 거리 계산은 유클리드 거리 행렬을 사용한다.
# 상대적 거리의 정확도를 높이기 위해 적합한 정도를 스트레스 값(stress value)으로 나타낸다.

# 1. 계량적 MDS

# 데이터가 연속형 변수(구간척도, 비율척도)인 경우 사용.
# 각 개체들간의 유클리드 거리 행렬을 계산하고 개체들간의 비유사성을 공간상에 표현한다.

library(MASS)
data(eurodist)              # 유럽 도시들간의 거리
eurodist

loc <- cmdscale(eurodist)   # 2차원으로 도시들을 mapping
loc

x <- loc[ , 1]
y <- -loc[ , 2]             # 북쪽 도시를 상단에 표시하기 위해 부호 변경

plot(x, y, type = "n", asp = 1, main = "Metric MDS")   # asp : y/x aspect ratio
text(x, y, rownames(loc), cex = 0.8)
abline(v = 0, h = 0, lty = 2, lwd = 1)



# 2. 비계량적 MDS

# 데이터가 순서척도인 경우 사용.
# 개체들간의 거리가 순서로 주어진 경우에는 순서척도를 거리의 속성과 같도록 변환하여 거리를 생성.

data(swiss)
head(swiss)     # 스위스 연방 주들의 사회경제적 지표

# (1) isoMDS : Kruskal's Non-metric Multidimensional Scaling

swissA <- as.matrix(swiss)
dist <- dist(swissA)        # make distance matrix
dist
mds <- isoMDS(dist)         # make points & stress
mds

plot(mds$points, type = "n")
text(mds$points, labels = rownames(swissA), cex = 0.7)
abline(v = 0, h = 0, lty = 2, lwd = 1)


swissC <- as.matrix(swiss[ , -2])    # Agriculture 제외하고 비교한 경우
distC <- dist(swissC)
mdsC <- isoMDS(distC)

plot(mdsC$points, type = "n")
text(mdsC$points, labels = rownames(swissC), cex = 0.8)
abline(v = 0, h = 0, lty = 2, lwd = 1)


# (2) sammon : Non-Linear Mapping

swissK <- as.matrix(swiss)
sam <- sammon(dist(swissK))

plot(sam$points, type = "n", main = "Nonmetric MDS : sammon")
text(sam$points, labels = rownames(swissK), cex = 0.7)
abline(v = 0, h = 0, lty = 2, lwd = 1)



################################################################
# 주성분 분석 (Pricipal Component Analysis, PCA)
################################################################

# 상관관계가 있는 변수들을 선형결합하여 변수를 축약하는 기법. 요인 분석의 한 종류. 
# 첫번째 주성분이 전체 변동을 가장 많이 설명하고, 두번째 주성분은 첫번째 주성분과 상관성이 낮아 
# 첫번째 주성분이 설명하지 못하는 나머지 변동을 정보의 손실없이 가장 많이 설명할 수 있도록 변수들을 조합.


# 1. 목적
# - 여러 변수들 간에 내재하는 상관관계, 연관성을 이용해 소수의 주성분으로 차원을 축소
# - 다중공선성이 존재하는 경우, 상관성이 적은 주성분으로 변수들을 축소하여 모형 개발에 활용
# - 주성분분석을 통해 차원을 축소한 후 군집분석을 수행하면 결과와 연산속도를 개선할 수 있음
# - 다량의 센서 데이터를 주성분분석으로 차원 축소한 후 시계열로 분포나 추세의 변화를 분석하여 고장 징후를 사전에 파악


# 2. 주성분 선택법
# 주성분분석 결과에서 누적분산비율(cumulative proportion)이 85%이상인 주성분까지 선택
# screen plot에서 고유값이 수평을 유지하기 전단계로 주성분의 수를 선택

df <- USArrests
library(psych)
pairs.panels(df)

pcomp <- princomp(df, cor = T)
summary(pcomp)
        # 제2 주성분까지의 cumulative proportion이 86.74% 이므로 
        # 2개의 주성분 변수를 활용하여 전체 데이터의 86.75%를 설명할 수 있다.

screeplot(pcomp, npcs = 4, type = "lines", main = "USArrests princomp")

# 변수들이 각 주성분에 기여하는 가중치 표시
pcomp$loadings
        # 제1 주성분에는 4개의 변수가 평균적으로 기여한다.
        # 제2 주성분에서는 (Murder, Assault)와 (UrbanPop, Rape) 계수의 부호가 다르다.

head(pcomp$scores);tail(pcomp$scores)

# 2개 주성분에 의한 plot
biplot(pcomp)
        # 알라스카, 루이지애나는 살인 비율이 높다
        # 미시건, 텍사스는 강간 비율이 높다.
        # 아이다호, 뉴햄프셔, 아이오와는 인구 비율이 낮으면서 강력범죄도 낮다.



################################################################
# Data Mart
################################################################

#--------------------------------------------------------------
# Reshape
#--------------------------------------------------------------

library(reshape)

# Example 1
id <- c(1, 1, 2, 2)
time <- c(1, 2, 1, 2)
x1 <- c(5, 3, 7, 2)
x2 <- c(6, 5, 1, 4)

mydata <- data.frame(id, time, x1, x2)
mydata

md <- melt(mydata, id = c("id", "time"))    # melt
md

cast(md, id + time ~ variable)
cast(md, id + variable ~ time)
cast(md, id ~ variable + time)

cast(md, id ~ variable, mean)   # with aggregate
cast(md, time ~ variable, mean)


# Example 2
airquality
head(airquality)

airData <- melt(airquality, id = c("Month", "Day"), na.rm = T)

head(airData);tail(airData)

cast(airData, Month ~ Day ~ variable)  # 3차원

cast(airData, Month ~ variable, mean)
cast(airData, Month ~ variable, mean, margins = c("grand_row", "grand_col"))    # mean of row & column
cast(airData, Month ~ variable, mean, subset = variable == "Ozone")
cast(airData, Month ~ variable, range)


#--------------------------------------------------------------
# sqldf
#--------------------------------------------------------------

library(sqldf)

iris
head(iris);tail(iris)
str(iris)
summary(iris)

sqldf("select * from iris limit 10")

sqldf("select * from iris where Species like 'ver%'")

sqldf("select * from iris where Species in ('setosa', 'virginica')")

sqldf('select * from iris where "Sepal.Length" between 5.0 and 6.0')

sqldf('select * from iris where "Sepal.Length" > 7.0')


#--------------------------------------------------------------
# plyr
#--------------------------------------------------------------

library(plyr)

# Example 1
data <- airquality
head(data)

# 1
ddply(
    .data = subset(data, Ozone >= 30),
    .variables = c('Month'),
    .fun = function(p) {
        summarize(p, mean_Temper = mean(Temp))
    }
)

# 위와 동일
ddply(subset(data, Ozone >= 30), c('Month'), summarize, mean_Temper = mean(Temp))


# 2
head(CO2)
summary(CO2)

ddply(subset(CO2), c('Plant', 'Type'), summarize, mean_uptake = mean(uptake))


# Example 2
set.seed(1)
data <- data.frame(year = rep(2000:2002, each = 6), count = round(runif(18, 0, 20)))
data

funcM <- function(x) {
    mean <- mean(x$count)
    sd <- sd(x$count)
    cv <- sd / mean
    data.frame(cv)      # only return cv
}

ddply(data, "year", funcM)

ddply(data, "year", summarize, mean = mean(count))  # summarize : year 기준 요약

ddply(data, "year", transform, total = sum(count))  # transform : 기존 데이터에 추가



#--------------------------------------------------------------
# data.table
#--------------------------------------------------------------

library(data.table)

?data.table

titanic <- read.csv("titanic.csv", header = T)
head(titanic)
str(titanic)

dt <- data.table(titanic)
class(dt)

dt[1, ]
dt[ , 1, with = F]

setkey(dt, pclass)
tables()

dt[J("1st")]    # pclass == 1st

dt[J("1st"), mean(survived)]        # 생존율 61.9%
dt[pclass == "1st", mean(survived)]

dt[ , mean(survived), by = "pclass"]
dt[ , mean(survived), by = c("pclass", "sex")]



###############################################################
# 데이터 가공 & 관리
###############################################################

#--------------------------------------------------------------
# 변수의 중요도
#--------------------------------------------------------------

library(klaR)
data("B3")      # West German Business Cycles 1955-1994
?B3

head(B3)
str(B3)

# AIC 이용한 변수 선택법 : PHASEN이 범주형 변수이므로 사용할 수 없음.
# step(model, direction = "both")

# Wilks.lambda : 집단내 분산 / 총분산
# 종속변수에 미치는 영향력에 따라 변수의 중요도를 정리 (작을수록 적합)

greedy.wilks(PHASEN ~ ., data = B3, niveau = 0.1)

    # 13개 변수 중에 8개 선택됨
    # PHASEN ~ EWAJW + LSTKJW + ZINSK + CP91JW + IAU91JW + PBSPJW + ZINSLR + PCPJW


#--------------------------------------------------------------
# (연속형) 변수의 구간화
#--------------------------------------------------------------

# Binning : 각각 동일한 갯수의 데이터를 50개 이하의 구간에 할당한 후 구간을 병합하면서 구간을 줄여나가는 방식
# 의사결정나무 : 연속형 데이터의 구간을 나누는 분기점을 찾을 수 있다.

data(iris)
head(iris)

# 한 변수를 기준으로 구간 분석
iris2 <- iris[ , c(1,3,5)]
head(iris2)

plineplot(Species ~ ., data = iris2, method = "lda", x = iris[ , 4], xlab = "Petal.Width")
#plineplot(Species ~ ., data = iris, method = "lda", x = iris[ , 4], xlab = "Petal.Width")
        # 0.6 / 1.7 지점에서 구간을 나누는 것이 좋다.


# 모든 변수를 기준으로 구간 분석
m <- NaiveBayes(Species ~ ., data = iris)
plot(m)


# 의사결정트리를 통해 구간 분석
library(party)

m <- ctree(Species ~ ., data = iris)
m
plot(m)


#--------------------------------------------------------------
# 결측값 처리
#--------------------------------------------------------------

# 1. 단순 대치법 (Single Imputation)
# 
# - completes analysis : 결측값 존재하는 레코드 삭제
# - mean imputaion : 관측 데이터의 평균으로 대치(비조건부) 또는 회귀분석을 활용한 대치(조건부)
# 
# 2. 다중 대치법 (Multiple Imputation)
# 
# - 단순 대치법을 m 번 수행. imputation - analysis - combination step.
# - Amelia : time series cross sectional dataset을 활용
# 
# 3. imputation in R
# 
# - rfImpute() : Random Forest 모델은 결측값 존재시 바로 에러 발생. 이 함수 이용하여 결측값 대치 후 알고리즘 적용.
# - complete.cases() : 데이터 내에 결측값 있으면 False
# - is.na() : 결측값이 NA 인지 체크
# - centralImputation() : DMwR 패키지. 중위수 또는 최빈값(factor)으로 대치
# - knnImputation() : DMwR 패키지. knn 분류 알고리즘 사용
# - amelia()


# Impute
library(Hmisc)

df <- data.frame(age = c(11, 23, NA, 40, 35, 15), gender = c('female', 'male'))
df

df$imputed_age <- with(df, impute(age, mean))
df$imputedR_age <- with(df, impute(age, "random"))
df


# Amelia
library(Amelia)
?amelia

trade <- freetrade
head(trade)     # tariff 관세 - NA's :58
summary(trade)

missmap(trade)  # 결측치 분포 시각화

# 결측치 대치값 생성. 시작값 = min(tariff)
am_data <- amelia(trade, m = 5, ts = "year", cs = "country", startvals =  7.10)
        # m	: the number of imputed datasets to create.
        # ts : time series column
        # cs : cross section variable

am_data
summary(am_data$imputations[[1]]$tariff)
summary(am_data$imputations[[2]]$tariff)
summary(am_data$imputations[[3]]$tariff)

# 최소값이 0보다 큰 첫번째 데이터 적용
impute1 <- am_data$imputations[[1]]$tariff
hist(impute1, col = "grey", border = "black")

trade$tariff <- impute1     # 결측치 대치
missmap(trade)

plot(am_data)
par(mfrow=c(1,1))



#--------------------------------------------------------------
# 이상치(outlier) 찾기 및 처리
#--------------------------------------------------------------

# outlier 식별
# - EDS (Extreme Studentized Deviation) : 평균에서 3 표준편차 이상 떨어진 값
# - 사분위수 이용. boxplot outer fence 벗어난 값

# outlier 처리방법
# - 절단(trimming) : outlier 포함된 레코드 삭제
# - 조정(winsorizing) : outlier를 상한 또는 하한 값으로 조정

#--------------------------------------------------------------
# Case 1. iris

head(iris)

# (1) 사분위수 이용 : 1개 변수에 대한 이상치

boxplot(iris)
boxplot(iris$Sepal.Width)

a <- iris$Sepal.Width

# fivenum : minimum, lower-hinge, median, upper-hinge, maximum
which(a < fivenum(a)[2] - 1.5*IQR(a))
which(a > fivenum(a)[4] + 1.5*IQR(a))


# (2) lofactor 함수 (local outlier factor algorithm) : 모든 변수 고려한 이상치

library(DMwR)
outlier.score <- lofactor(iris[ , 1:4], k = 5)                  # k : outlier 계산을 위한 이웃 갯수
plot(density(outlier.score), main = "outlier score of iris")    # score 가 2.0, 2.5인 데이터가 outlier

sort(outlier.score, decreasing = T)[1:10]   # score > 1.9 인 3개 데이터를 이상치로 결정
outliers <- order(outlier.score, decreasing = T)[1:3]
outliers

# outler 만 plot에서 번호 표시
labels <- 1:nrow(iris)
labels[-outliers] <- "."

# 주성분분석
biplot(prcomp(iris[ , 1:4]), cex = 0.8, xlabs = labels)

pch <- rep(".", nrow(iris))
pch[outliers] <- "@"

col <- rep("black", nrow(iris))
col[outliers] <- "red"

pairs(iris[ , 1:4], pch = pch, col = col)


#--------------------------------------------------------------
# Case 2. 

library(psych)
library(MVA)

df <- USairpollution
head(df)

outlier.score <- lofactor(df, k = 5)
plot(density(outlier.score), main = "outlier score of USairpollution")

sort(outlier.score, decreasing = T)     # score > 3 인 이상치 2개
outliers <- order(outlier.score, decreasing = T)[1:2]
outliers
df[outliers, ]

# outler 만 plot에서 번호 표시
labels <- 1:nrow(df)
labels[-outliers] <- "."

# 주성분분석
biplot(prcomp(df), cex = 0.8, xlabs = labels)

# plot 1
pch <- rep(4, nrow(df))
pch[outliers] <- 1

col <- rep("black", nrow(df))
col[outliers] <- "red"

pairs(df, pch = pch, col = col)

# plot 2
df$col <- "normal"
df[outliers, ]$col <- "outlier"
df$col <- factor(df$col)
head(df, 10)

pairs.panels(df[1:7], bg = c("black", "yellow")[df$col], pch = 21)



###############################################################
# 정형 데이터 마이닝
###############################################################

#--------------------------------------------------------------
# 분류분석 (Classification) - Decision Tree
#--------------------------------------------------------------

head(iris)

#--------------------------------------------------------------
# party package 이용한 Decision Tree

library(party)
library(caret)

# train / test data 분리 (6:4 or 7:3)
idx <- sample(2, nrow(iris), replace = T, prob = c(0.6, 0.4))
table(idx)
train_1 <- iris[idx == 1, ]
test_1 <- iris[idx == 2, ]

# train data 이용한 모델 
tree_model <- ctree(Species ~ ., data = train_1)
tree_model
plot(tree_model)
plot(tree_model, type = "simple")

# 예측된 데이터와 실제 데이터 비교
table(train_1$Species)                        # real data
train_1$pred <- predict(tree_model)
confusionMatrix(train_1$pred, train_1$Species)  # Accuracy : 0.9762

# Test Data로 검증
test_1$pred <- predict(tree_model, newdata = test_1)
confusionMatrix(test_1$pred, test_1$Species)    # Accuracy : 0.9394


#--------------------------------------------------------------
# C50 패키지 이용한 Decision Tree

library(C50)

# train / test data 분리 (6:4)
idx <- sample(2, nrow(iris), replace = T, prob = c(0.6, 0.4))
table(idx)
train_2 <- iris[idx == 1, ]
test_2 <- iris[idx == 2, ]

# modeling
c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)

c5_model <- C5.0(Species ~ ., data = train_2, control=c5_options, rules=FALSE)
summary(c5_model)
plot(c5_model)

# validation
train_2$pred <- predict(c5_model, newdata = train_2)
confusionMatrix(train_2$pred, train_2$Species)      # Accuracy : 0.9885

test_2$pred <- predict(c5_model, newdata = test_2)
confusionMatrix(test_2$pred, test_2$Species)        # Accuracy : 0.9048



#--------------------------------------------------------------
# Decision Tree - Ensemble - Bagging
#--------------------------------------------------------------

library(party)
library(caret)

head(iris)

# bootstrap data 생성
data_boot1 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot2 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot3 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot4 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot5 <- iris[sample(1:nrow(iris), replace = T), ]

# Modeling
tree1 <- ctree(Species ~ ., data_boot1)
tree2 <- ctree(Species ~ ., data_boot2)
tree3 <- ctree(Species ~ ., data_boot3)
tree4 <- ctree(Species ~ ., data_boot4)
tree5 <- ctree(Species ~ ., data_boot5)

plot(tree1)
plot(tree2)
plot(tree3)
plot(tree4)
plot(tree5)

pred1 <- predict(tree1, iris)
pred2 <- predict(tree2, iris)
pred3 <- predict(tree3, iris)
pred4 <- predict(tree4, iris)
pred5 <- predict(tree5, iris)

# 각각의 예측 결과를 취합
test <- data.frame(Species = iris$Species, pred1, pred2, pred3, pred4, pred5)
head(test)

# 5개 분류기의 결과를 취합하여 최종 결과를 voting
funcResultValue <- function(x) {
    result <- NULL
    for (i in 1:nrow(x)) {
        xtab <- table(t(x[i, ]))
        rvalue <- names(sort(xtab, decreasing = T)[1])
        result <- c(result, rvalue)
    }
    return(result)
}

test$result <- funcResultValue(test[ , 2:6])
confusionMatrix(test$result, test$Species)


#--------------------------------------------------------------
# Decision Tree - Ensemble - Boosting
#--------------------------------------------------------------

library(tree)

data(kyphosis, package = "rpart")
data <- kyphosis
head(data)

totalCount <- nrow(data)
totalCount
    
boost <- function(k, compare) {
    # 첫번째 표본 추출 확률을 모두 동일하게 설정
    pr <- rep(1/totalCount, totalCount)
    
    # 결과에 대한 확률 및 모델의 정확도를 저장할 객체
    result <- matrix(0, k, 3) # k row 3 col
    
    # k개 만큼 tree model 생성
    for (j in 1:k) {
        # 배깅과 달리 각 인덱스에 설정된 확률로 샘플링
        data.boost <- data[sample(1:totalCount, prob = pr, replace = T), ]
        
        # 샘플링 데이터에 대한 tree 생성
        data.tree <- tree(Kyphosis ~ ., data.boost)

        # 각 row에 대한 예측을 저장할 객체  
        pred <- matrix(0, totalCount, 1)
        
        for (i in 1:totalCount) {
            # predict - absent / present 확률
            if (predict(data.tree, data[i, ])[ , 1] > 0.5) {
                pred[i, 1] <- "absent"
            } else {
                pred[i, 1] <- "present"
            }
        }
        
        # test data (compare) 한 개에 대한 예측 확률
        result[j, 1] <- predict(data.tree, compare)[ , 1]
        result[j, 2] <- predict(data.tree, compare)[ , 2]
        result[j, 3] <- length(which(as.matrix(data)[ , 1] == pred)) / totalCount # 정확도
        
        pr <- rep(1/totalCount, totalCount)
        # 오분류 표본의 확률을 2배로 설정하여 2번째 loop 수행
        pr[as.matrix(data)[ , 1] != pred] <- 2/totalCount   
    }
    return(result)
}

# 80번째 데이터로 10회 반복해서 측정
boost.result <- boost(10, data[80, ])
boost.result

a <- t(boost.result[,1])%*%(boost.result[,3])    # absent 확률
b <- t(boost.result[,2])%*%(boost.result[,3])    # present 확률
a;b

# b가 a 보다 확률이 높기 때문에 80번째 데이터는 present로 최종 예측



#--------------------------------------------------------------
# Decision Tree - Ensemble - Random Forest
#--------------------------------------------------------------

head(iris)

idx <- sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))
trainData <- iris[idx == 1, ]
testData <- iris[idx == 2, ]

library(randomForest)

# ntree = 100 : 100 개의 tree 만듬.
# proximity = T : 다양한 트리 분할 시도
model <- randomForest(Species ~ ., data = trainData, ntree = 100, proximity = T)
model

table(trainData$Species, predict(model))

importance(model)  
# 지니계수. 값이 높은 변수가 클래스를 분류하는데 가장 큰 영향을 줌.

plot(model, main = "randomForest model of iris")
# tree가 40개 이상일 경우 오차가 안정적으로 나타난다.

varImpPlot(model)
# 변수의 상대적 중요도를 표시

pred <- predict(model, newdata = testData)
table(testData$Species, pred)

plot(margin(model, testData$Species))


#--------------------------------------------------------------
# ROCR
#--------------------------------------------------------------

# http://blog.naver.com/woosa7/220840338896

library(C50)
library(ROCR)

data(churn)     # C50 dataset. 서비스 제공자를 바꾸는 고객.
summary(churnTrain)

# Modeling
c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
model <- C5.0(churn ~ ., data = churnTrain, control = c5_options, rules = FALSE)
summary(model)

# 가지가 너무 많아서 Attribute usage 가 작은 변수 제거하고 다시 모델링
drops <- c("total_day_charge", "account_length", "total_eve_calls", "total_day_calls", "total_eve_minutes")
churnTrain2 <- churnTrain[, !(names(churnTrain) %in% drops)]

model2 <- C5.0(churn ~ ., data = churnTrain2, control = c5_options, rules = FALSE)
summary(model2)
plot(model2, type = "simple")

pred_train <- predict(model2, churnTrain2, type = "class")
confusionMatrix(pred_train, churnTrain2$churn)   # Accuracy : 0.9583

# Test
head(churnTest)

churnTest$pred <- predict(model2, churnTest, type = "class")        # 예측결과
churnTest$pred_prob <- predict(model2, churnTest, type = "prob")    # 확률
confusionMatrix(churnTest$pred, churnTest$churn)                    # Accuracy : 0.9478

# Model Evaluation by ROCR chart
head(churnTest$pred_prob)
c5_pred <- prediction(churnTest$pred_prob[, "yes"], churnTest$churn)
c5_model.perf <- performance(c5_pred, "tpr", "fpr")
    # True positive rate (tpr) = Sensitivity
    # False positive rate (fpr) = 1 - Specificity

c5_model.perf

plot(c5_model.perf, col = "red")

AUROC <- performance(c5_pred, "auc")
AUROC@y.values
    # 0.8804094 : Good

c5_model.lift <- performance(c5_pred, "lift", "rpp")  # rpp : Rate of positive predictions
plot(c5_model.lift, col = "red")








