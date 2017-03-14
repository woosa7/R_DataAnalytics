################################################################
#
# R을 활용한 통계분석 : 정여진 교수 (2016 여름특강)
#
# 4. 상관분석 / 단순회귀분석
#
################################################################

#---------------------------------------------------------------
# 상관계수
#---------------------------------------------------------------

# 공분산 : 두 변수가 같은 방향으로 움직이는 정도. 측정단위에 영향을 받는 단점.
# 상관계수 : 공분산을 표준편차로 나누 값

# cov(x,y) = SUM(x - x_bar)(y - y_bar) / n - 1
# corr(x,y) = cov(x,y) / sd(x)sd(y)

# Pearson 상관계수
# --- 직선관계의 정도를 나타냄
# --- -1 ~ 1 사이의 값. 1에 가까울수록 강한 상관관계 / 0에 가까울수록 관계 없음

# Spearman 상관계수
# --- 서열척도일 경우 사용하는 비모수적 방법
# --- 직선관계가 아니어도 상관관계가 있으면 1에 가까운 값을 갖는다.

x <- 1:10
y <- x^2
plot(x, y)

# 상관계수
cor(x, y)

# 자료가 순서형/순위자료일 경우.
cor(x, y, method = "kendall")
cor(x, y, method = "spearman")

# 상관계수가 유의한지 검정
# H0 : cor = 0 (상관관계가 없다) : p-value < 0.05 이면 H0 기각 = 유의한 상관관계가 있다
cor.test(x, y)


#---------------------------------------------------------------
# 30개 부서에서 부서당 35명의 직원 설문조사
# 데이터 숫자는 해당 질문에 긍정한 직원의 비율

attitude
cov(attitude)
round( cor(attitude), 3)

attach(attitude)

library(psych)
pairs.panels(attitude)

# complaints & rating 상관계수가 가장 높음
plot(rating, complaints)
cor.test(rating, complaints)



#---------------------------------------------------------------
# 단순회귀분석 (Simple Linear Regression)
#---------------------------------------------------------------

# 하나의 종속변수(y)와 하나의 설명변수(x) 간의 관계를 직선으로 표현하는 방법
# 인과관계가 아닌 상관관계!

# y = b0 + b1x + e

# b0 : y 절편
# b1 : 기울기
# e (epsilon) : 오차. 확률변수. 평균 0, 표준편차 sigma인 정규분포를 따른다.

# 최소자승법 (Least Squares Method)
# 선(y 추정값)으로부터 각 점(y 관찰값)이 얼마나 떨어져 있는지 수치화
# 오차제곱합을 최소로 하는 추정방법
# 각 점이 선에 가까이 붙어있을수록 추정된 회귀식이 유의미하다.


#---------------------------------------------------------------
# 회귀분석 in R

# model <- lm(y ~ x, data)  : 회귀분석
# plot(model)               : 회귀분석 관련 그래프 출력
#                           (plot, residuals vs fitted, Normal QQ, scale-location, residuals vs leverage)
# summary(model)            : 회귀분석 결과 출력
# abline(model)             : 그래프에 직선 추가
# abline(intercept, slope)
#---------------------------------------------------------------

# 차의 속도와 브레이크 제동거리
cars
summary(cars)

plot(cars)

out <- lm(dist ~ speed, data = cars)    # 회귀분석(종속변수~설명변수)
plot(out)
summary(out)

library(ggplot2)
ggplot(cars, aes(speed, dist)) + geom_point() + geom_smooth(method = "lm")


#---------------------------------------------------------------
# 회귀계수 추정과 해석
# 
# (1) F-검정
#
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12
# --- 회귀모형에 대한 유의성 검정
# --- H0 : 회귀모형이 유의하지 않다.
# --- p-value < 0.05 이므로 H0 기가. 회귀모형이 유의하다.
# --- 단순회귀분석에서는 b1 = 0 (회귀계수)를 검정하는 t-test와 동일
#
# (2) t-검정
#
# Coefficients: Est Std.    Error       t value     Pr(>|t|)    
# (Intercept)   -17.5791    6.7584      -2.601      0.0123 *        --> b0
#     speed     3.9324      0.4155      9.464       1.49e-12 ***    --> b1
# 
# --- 회귀계수에 대한 t-test. H0 : b1 = 0. p-value < 0.05 이면 유의미
# --- y = -17.5791 + 3.9324 * x
# --- 속력이 0 일때 제동거리가 
#
# (3) 결정계수 (R-squared)
#
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
# --- 결정계수 = SSR / SST
# --- 1에 가까울수록 모형이 변동량에 대해 설명력을 갖는다. 
#---------------------------------------------------------------

# ANOVA table (Residuals:잔차. 평균 = 0)

# Regression    SSR (회귀식에 의해 설명되는 변동량)
# Residual      SSE (회귀식에 의해 설명되지 않는 변동량)
# Total         SST (총변동량)

anova(out)

plot(dist ~ speed, data = cars, col = "blue")
abline(out, col = "red")



#---------------------------------------------------------------
# 회귀진단
#---------------------------------------------------------------

# No Intercept Model
# 속도가 0이면 제동거리가 0 인 것이 당연하다. b0 = 0 으로 고정.

out <- lm(dist ~ speed + 0, data = cars)
out
summary(out)    # y = 2.909 * x

# 오차항(e, 잔차)이 추세를 보인다면 무언가 중요 정보가 모형에 포함되지 않았다는 의미
# e는 평균 0, 표준편차 sigma인 정규분포를 따르는 확률변수이다.
# --> 잔차도(residual plot)의 패턴 확인 필요!
# --> 잔차가 0과 가까울수록 좋다.

par(mfcol=c(2,2))
plot(out)
par(mfcol=c(1,1))
# Residuals vs Fitted plot (잔차도) / Normal Q-Q plot (정규성 검정)
# --> 선형 패턴이 아니고, 분산이 증가하는 경향 --> 종속변수의 log 또는 sqrt 변환 시도

par(mfcol=c(1,2))
plot(log(dist) ~ speed + 0, data = cars)
plot(sqrt(dist) ~ speed + 0, data = cars)  # sqrt (root) 변환이 적절한 패턴을 보임
par(mfcol=c(1,1))

out2 <- lm(sqrt(dist) ~ speed + 0, data = cars)
str(out2)
summary(out2)   # sqrt 변환후 회귀분석 --> R-squared 값 높아짐

par(mfcol=c(2,2))
plot(out2)      # Residuals vs Fitted plot / Normal Q-Q plot 좋은 패턴을 보임
par(mfcol=c(1,1))

shapiro.test(out2$residuals)    # 잔차가 정규분포를 따른다 (p-value > 0.05)

# 최종 모형으로 추정된 회귀식
# sqrt(dist) =  0.397 * speed
# dist = (0.397 * speed)^2



#---------------------------------------------------------------
# 추정과 예측
#---------------------------------------------------------------

# 속도가 10 또는 30일때의 제동거리 예측
nspeed <- data.frame("speed" = c(10,30))
nspeed

predict(out2, nspeed)
# predict(out2, nspeed) ^ 2   # 결과값은 sqrt(dist) 이므로 실제값은 변환 필요

# "평균" 제동거리의 95% 신뢰구간
predict(out2, nspeed, interval = "confidence")

# 새로운 한 차량에 대한 95% 예측구간
predict(out2, nspeed, interval = "prediction")

# 모든 관측치에 대한 추정치
pred_dist <- fitted(out2)^2
cbind(cars, pred_dist)
plot(cars$speed, pred_dist)

# 관측치 속도의 최대값 25 --> 데이터 범위 밖에서 예측하는 것은 주의 해야한다!
newspeed2 <- data.frame("speed"= c(50, 70))
predict(out2, newspeed2)^2


#---------------------------------------------------------------
# Outlier (이상점) / Influential Points (영향점)
#---------------------------------------------------------------

# Outlier
# - 측정상/실험상의 과오로 모집단에 속하지 않는다고 의심이 될 정도로 정상범위 밖에 떨어진 점
# - 대개 큰 잔차를 가짐

# Influential Points
# 소수의 관측치이지만 통계량에 큰 영향을 미침
# Leverage plot 에서 점선 영역 밖에 위치 (Cook' distance)



#---------------------------------------------------------------
# practice 4
#---------------------------------------------------------------

# 중고차. Odometer (주행거리 / 100 mile)에 따른 Price (가격 / $1000)

ucar <- read.csv("data/sonata.csv")
head(ucar)
summary(ucar)

ucar <- ucar[-3]
attach(ucar)

# 산점도와 상관계수를 통해 선형관계를 판단
plot(Price ~ Odometer, ucar)
cor(Price, Odometer)
pairs.panels(ucar)    # cor = -0.81

# 회귀식 추정
model <- lm(Price ~ Odometer, data = ucar)
summary(model)

# price = 17.249 - 0.067 * Odometer
# F-test : 회귀모형 유의함
# t-test : 회귀계수 유의함
# R-squared : 0.648

plot(model)     # 잔차도 적절


# 주행거리 3600 마일인 소나타
nOdometer <- data.frame("Odometer" = c(36, 46))
predict(model, nOdometer, interval = "confidence") # 평균가격
predict(model, nOdometer, interval = "prediction") # 특정한 차를 팔때 예측가격

