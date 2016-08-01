################################################################
#
# ADSP
#
################################################################

#--------------------------------------------------------------
# 회귀분석
#--------------------------------------------------------------

#--------------------------------------------------------------
# 상관계수 correlation coefficient

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
cor(mtcars)  # 모든 변수간의 상관계수 테이블 표시

library(psych)
pairs.panels(mtcars[, c("disp", "wt", "drat")])

# 피어슨 상관계수 - 일반적인 경우. 연속형 변수.
# 스피어만 상관계수 - 서열척도(순서형 변수)인 경우 사용.

install.packages("Hmisc")
library(Hmisc)

rcorr(as.matrix(mtcars), type = "pearson")$r

rcorr(disp, wt, type = "pearson")   # P : 유의확률. 0.05 이하 값 유의미 
rcorr(disp, wt, type = "spearman")  


# 순서형변수 샘플 데이터

sno <- seq(1001,1050)
Korean <- sample(1:50, replace = F)
English <- sample(1:50, replace = F)
Math <- sample(1:50, replace = F)

rank <- cbind(sno, Korean, English, Math)
rownames(rank) <- seq(1,50)
rank    
    
rcorr(as.matrix(rank), type = "spearman")    


#--------------------------------------------------------------
# 모델 선택법

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

step(lm(time ~ dist + climb, data = hills), direction = "backward")

step(lm(time ~ 1, data = hills), 
     scope = list(lower = ~ 1, upper = ~ dist + climb), direction = "forward")



#--------------------------------------------------------------
# 시계열분석
#--------------------------------------------------------------

#--------------------------------------------------------------
# R functions

# 1) 소스 데이터를 시계열 데이터로 변환
ts(data, frequency = n, start = c(시작년도, 월))
# 2) 시계열 데이터를 x, trend, seasonal, random 값으로 분해
decompose(data)
# 3) 시계열 데이터를 이동평균한 값 생성
SMA(data, n = 이동평균수)
# 4) 시계열 데이터를 차분
diff(data, differences = 차분수)
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

install.packages("TTR")
install.packages("forecast")

library(TTR)
library(forecast)

#--------------------------------------------------------------
# Decompose non-seasonal data
# 영국왕들의 사망시 나이

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip = 3)
kings
















