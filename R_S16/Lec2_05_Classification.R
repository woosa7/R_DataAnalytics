###############################################################
#
# 다변량 통계분석 6 - 판별분석/분류분석 Classification Analysis
#
###############################################################

#--------------------------------------------------------------
# Logistic regression
#--------------------------------------------------------------

library(ISLR)

head(Default)
summary(Default)

boxplot(balance ~ default, Default)
boxplot(balance ~ income, Default)

glm.fit = glm(default ~ balance, data = Default, family = binomial)
summary(glm.fit)

# balance      5.499e-03
# X가 1단위 증가할 때 e의 b1승 배 증가 : odds ratio
exp(5.499e-03 * 100)  # balance 가 100 증가할 때 파산할 odds가 73% 증가한다.

predict(glm.fit, data.frame(balance = 1000))  # logit

predict(glm.fit, data.frame(balance = 1000), type = "response") # P(x)



# Multiple logistic regression

model1 = glm(default ~ ., data = Default, family = binomial)
summary(model1)

model2 = glm(default ~ student, data = Default, family = binomial)
summary(model2)

boxplot(balance ~ student, Default)


# deviance
# H0 : 두 모형의 차이가 없다.
# Null deviance : 설명변수가 의미없는 경우
# Residual deviance : 설명변수가 추가된 만큼의 자유도가 떨어진 경우

anova(model1, test = "Chisq")

anova(model1, model2, test = "Chisq") 
# balance + income 이 동시에 미치는 영향
# balance + income의 영향이 ddefault 확률을 예측하는데 유의하다.






#--------------------------------------------------------------
# 반복 측정된 자료 (summarised data)

# x1  x2  y
# 1   1   20 (1이 20번 반복 측정됨)
# 1   2   5

coupon = read.csv("data/coupon.csv")
coupon

# Yes , No 갯수
model4 = glm(cbind(N_redeemed, N-N_redeemed) ~ Price_reduc, data = coupon, family = binomial)   
summary(model4)

exp(0.096834)   # 1 달러당
exp(0.096834 * 5)  # 10달러 쿠폰의 5달러 쿠폰 대비 사용 확


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

library(ISLR)

head(Smarket)
summary(Smarket)

df = Smarket[, -1]
head(df)

boxplot(df)

model = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = df, family = binomial)
summary(model)

anova(model, test = "Chisq")

head(model$fitted.values)


err = performance(pred, "err")
err@x.values[[1]][err@y.values[[1]]==min(err@y.values[[1]])]


