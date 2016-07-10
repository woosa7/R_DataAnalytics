
# 매월 매출액 1000만원인지 - 한집단의 평균

# 마케팅 전/후 각 표본 1000명 - 독립표본 T 검정
# 동일한 표본 - 쌍체표본 T 검정

# 한 집단 1000명이 브랜드 인지율 - 한집단의 비율

#------------------------------------
# 상관분석

x <- 1:10
y <- x^2
plot(x,y)

# 상관계수
cor(x,y)

# 자료가 순서형/순위자료일 경우.
cor(x,sqrt(y))
cor(x,y, method = "kendall")
cor(x,y, method = "spearman")
cor.test(x,y)


# 직원 설문조사
attitude
cov(attitude)
round( cor(attitude), 3)

pairs(attitude)

library(psych)
pairs.panels(attitude)

plot(attitude$rating, attitude$complaints)
cor.test(attitude$rating, attitude$complaints)
with(attitude, cor.test(rating, complaints))



#------------------------------------
# 단순회귀분석 (Simple Linear Regression)
# 최소자승법 (Least Squares Method)

# 차의 속도와 브레이크 제동거리
cars
str(cars)
summary(cars)
plot(cars)

out <- lm(dist~speed, data = cars)  # 회귀분석(종속변수~설명변수)
plot(out)
summary(out)

anova(out)

plot(dist~speed, data = cars, col = "blue")
abline(out, col = "red")


# 회귀진단
out <- lm(dist~speed+0, data = cars)
out
summary(out)
plot(out)

par(mfcol=c(1,2))
plot(log(dist)~speed+0, data = cars)
plot(sqrt(dist)~speed+0, data = cars)
par(mfcol=c(1,1))

# sqrt 변환후 회귀분석
out2 <- lm(sqrt(dist)~speed+0, data = cars)
summary(out2)
plot(out2)

shapiro.test(resid(out2))


# 추정과 예측
nspeed <- data.frame("speed" = c(10,30))
predict(out2, nspeed)
predict(out2, nspeed) ^ 2 # 위 결과값은 sqrt 값이므로 변환 필요

predict(out2, nspeed, interval = "confidence") # 95% 신뢰구간
predict(out2, nspeed, interval = "prediction") # 95% 예측구간

fitted <- fitted(out2)^2
cbind(cars, fitted)

plot(cars$speed, fitted(out2))



#--------------------------------------------------
# excercise

ucar <- read.csv("sonata.csv")
summary(ucar)

plot(Price~Odometer, ucar)
cor(ucar$Price, ucar$Odometer)

out <- lm(Price~Odometer, data = ucar)
summary(out)
plot(out)

nOdometer <- data.frame("Odometer" = c(36))
predict(out, nOdometer, interval = "confidence") # 평균가격
predict(out, nOdometer, interval = "prediction") # 내차팔

