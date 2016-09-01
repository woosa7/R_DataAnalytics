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
# 시계열분석
################################################################

# 자기회귀모형(AR, Autoregressive model)
# 이동평균모형(MA, Moving average model)
# 자기회귀 누적 이동평균 모형 (ARIMA)

# 비정상시계열 모형인 ARIMA를 차분이나 변환을 통해 AR, MA, ARMA 모형으로 정상화
# 평균 비정상 : 차분 / 분산 비정상 : 변환
# d : 차분, p : AR모형 차수, q : MA 모형 차수

# 자기상관계수 함수(ACF, Autocorrelation Function)
# 부분 자기상관계수 함수(PACF, Partial ACF)

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
plot.ts(birth)

# 데이터 분해 - trend, seasonal, random 데이터 추세 확인
birth_comp <- decompose(birth)
plot(birth_comp)

birth_comp$trend
birth_comp$seasonal

# 시계열 데이터에서 계절성 요인 제거
birth_adjusted <- birth - birth_comp$seasonal
plot.ts(birth_adjusted)

# 차분을 통해 정상성 확인
birth_diff1 <- diff(birth_adjusted, differences = 1)
plot.ts(birth_diff1)   
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
plot(birth_fcast)


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

# 2
ddply(subset(data, Ozone >= 30), c('Month'), summarize, mean_Temper = mean(Temp))


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

# Wilks.lambda : 집단내 분산 / 총분산
#                종속변수에 미치는 영향력에 따라 변수의 중요도를 정리 (작을수록 적합)

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

# Impute
library(Hmisc)

df <- data.frame(age = c(11, 23, NA, 40, 35, 15), gender = c('female', 'male'))
df

df$imputed_age <- with(df, impute(age, mean))
df

# Amelia
library(Amelia)

data(freetrade)
head(freetrade)     # tariff 관세
summary(freetrade)

?amelia

data <- amelia(freetrade, m = 5, ts = "year", cs = "country")
data
        # m	: the number of imputed datasets to create.
        # ts : time series column
        # cs : cross section variable

# 4_3_7








