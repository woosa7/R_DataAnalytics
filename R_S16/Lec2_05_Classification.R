###############################################################
#
# 다변량 통계분석 6 - 판별분석/분류분석 Classification Analysis
#
###############################################################

# 분류되어 있는 집단 간의 차이를 의미있게 설명해 줄 수 있는 독립변수들을 찾음.
# 변수의 결합으로 판별식(Discriminant function)을 만들어 관측치를 판별하는 규칙 만듬.
# • 예 - 서비스 이용 불만 고객의 성향 분석, SKT/KT/LGT 가입고객 판별변수 및 판별함수 유도

# 비교: 군집분석
# • 관측치를 그룹으로 구분함
# • 판별분석과는 다르게 데이터에 집단을 나타내는 변수 없음

# 비교: 회귀분석
# • 종속변수가 이산형인 logistic regression은 두 집단 판별분석의 한 방법
# • 판별분석은 집단이 2개 이상의 범주를 갖는 범주형변수인 경우 가능
# • 판별분석은 집단을 구분하는 판별식 유도
# • 회귀분석은 집단에 속하는 확률 예측


library(ISLR)

# 개인의 연봉과 월 신용카드 잔고를 사용해 파산 예측
head(Default)
summary(Default)

# 선형회귀모형은 적당치 않음
plot(Default$balance, Default$income, pch=as.numeric(Default$default), col=as.numeric(Default$default))
boxplot(balance ~ default, Default, col=c('yellow','red'), ylab='Balance')
boxplot(income ~ default, Default, col=c('yellow','red'), ylab='Income')


#--------------------------------------------------------------
# Simple Logistic regression
#--------------------------------------------------------------

# Balance를 사용해 default=Yes일 확률 예측

glm.fit = glm(default ~ balance, data = Default, family = binomial)
summary(glm.fit)

# --> balance의 회귀계수 p-value<0.0001 : 파산확률과 카드잔고 사이에 관계가 있음

# log-odds(logit) - Odds: 성공확률/실패확률
# balance의 회귀계수 : 5.499e-03
# X가 1단위 증가할 때 e의 b1승 배 증가 : odds ratio
exp(5.499e-03 * 100)  # balance 가 100 증가할 때 파산할 odds가 73% 증가한다.

# 카드 잔고가 $1000인 사람의 파산 확률은?
a = predict(glm.fit, data.frame(balance = 1000))  # logit
exp(a)/(1+exp(a))
predict(glm.fit, data.frame(balance = 1000), type = "response") # P(x)


#--------------------------------------------------------------
# Multiple logistic regression
#--------------------------------------------------------------

model1 = glm(default ~ ., data = Default, family = binomial)
summary(model1)

model2 = glm(default ~ student, data = Default, family = binomial)
summary(model2)

boxplot(balance ~ student, Default, ylab='Balance', xlab='Student')

# --> 현재잔고와 학생여부는 파산과 연관이 있음
# • 학생더미 변수의 계수가 음수
# • Simple logistic regression에서는 양수
# 학생이면 파산가능성이 더 낮은가?

# deviance
# H0 : 두 모형의 차이가 없다.
# Null deviance : 설명변수가 의미없는 경우
# Residual deviance : 설명변수가 추가된 만큼의 자유도가 떨어진 경우

exp(coef(model1)[2])
exp(coef(model2)[2])
# Confounding effect
# • 같은 수준의 income과 balance를 가지고 있을 경우 학생의 파산가능성(odds)이 더 낮음
# • 학생일 경우 credit card balance가 더 높은 경향이 있음
# • 학생인 경우 아닌 경우보다 파산가능성(odds)이 1.5배
# • 같은 수준의 income과 balance를 가지고 있을 경우 학생의 파산가능성(odds)은 0.5배


#--------------------------------------------------------------
# 모형비교: Deviance Goodness-of-fit Test

anova(model1, test = "Chisq")

anova(model1, model2, test = "Chisq") 
# balance + income 이 동시에 미치는 영향
# balance + income의 영향이 ddefault 확률을 예측하는데 유의하다.


#--------------------------------------------------------------
# 반복 측정된 자료 (summarised data)

# 한 개의 X값에서 여러 개의 Y가 측정된 경우
# Binomial Distribution

# x1  x2  y
# 1   1   20 (1이 20번 반복 측정됨)
# 1   2   5

# Example: Coupon Effectiveness
# 가격 할인 쿠폰의 효과를 검증하기 위해 무작위로 추출된 각 200개의 가구에 5,10,15,20,30 달러의 쿠폰을 제공.

coupon = read.csv("data/coupon.csv")
coupon

# Yes , No 갯수
model4 = glm(cbind(N_redeemed, N-N_redeemed) ~ Price_reduc, data = coupon, family = binomial)   
summary(model4)

exp(0.096834)      # = 1.10 : 쿠폰 할인액이 1달러 증가할 때 쿠폰을 사용할 Odds는 10% 증가.
exp(0.096834 * 5)  # 10달러 쿠폰의 5달러 쿠폰 대비 사용 확률


#--------------------------------------------------------------
# Cutoff

head(model1$fitted.values)

x = data.frame(default = Default$default, fit = model1$fitted.values)
head(x)

xtabs(~default + (fit>0.5), data = x)   # ---> confusion matrix

xtabs(~default + (fit>0.3), data = x)


library(ROCR)
pred = prediction(x$fit, x$default)
plot(performance(pred, "tpr", "fpr"))
plot(performance(pred, "err"))




#--------------------------------------------------------------
# Practice 5
#--------------------------------------------------------------

# S&P500 지수의 수익률을 1250일간 기록한 데이터이다. Lag1-Lag5는 1일에서 5일 전의 수익률을 나타내고 Volume은 전일 거래량(in billions), Today는 오늘의 수익률, Direction은 오늘 시장의 상승(up)과 하락(down)을 나타낸다. 로지스틱 회귀분석을 이용하여 아래의 질문에 답하시오.

head(Smarket)
summary(Smarket)

df = Smarket[, -1]
head(df)

boxplot(df)

# Lag변수들과 Volume을 사용하여 시장의 상승 하락을 예측하는 모형을 추정.
model = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = df, family = binomial)
summary(model)

anova(model, test = "Chisq")

# Cutoff를 0.5로 설정하여 분할표를 출력하고 민감도, 특이도, 오분류율을 계산하시오.
x = data.frame(Direction = Smarket$Direction, fit = model$fitted.values)
head(x)

xtabs(~Direction + (fit>0.3), data = x)   # ---> confusion matrix

err = performance(pred, "err")
err@x.values[[1]][err@y.values[[1]]==min(err@y.values[[1]])]

# ROC curve를 그리고 모형의 적절성을 판단하시오.
pred = prediction(x$fit, x$Direction)
plot(performance(pred, "tpr", "fpr"))
plot(performance(pred, "err"))




#--------------------------------------------------------------
# 분류 데이터가 희귀한 경우. rara event : 희귀병 등 yes인 경우가 30개 미만인 경우

# model = glm(......, family = binomial(link = "cloglog")) # complementary log-log link


