###############################################################
#
# 다변량 통계분석 7 - 다중회귀분석 (범주형 설명변수)
#
###############################################################

insu = read.csv("insurance.csv", header = T)
insu

summary(insu)

library(ggplot2)
ggplot(insu, aes(y=time, x=size, color=type)) + geom_point(size=3)

model1.matrix(time ~ size+type, insu)

model1 = lm(time ~ size+type, insu)
summary(model1)

# type = 1 if stock
# Mutual    : y = b0 + b1x1
# Stock     : y = (b0 + b2) + b1x1

insu$fit = model1$fitted.values

ggplot(insu, aes(y=fit, x=size, group=type, color=type)) +
    geom_line() + 
    geom_point(aes(y=time, x=size, color=type))


#-------------------------------------------------------------
# 기준이 되는 집단(reference level)을 변경

insu$type = relevel(insu$type, ref = "Stock")

model2 = lm(time ~ size+type, insu)
summary(model2)

model.matrix(time ~ size+type, insu)

#-------------------------------------------------------------
# Effect Codeing
# 특정 집단을 기준으로 하는 것이 아니라 전체의 평균을 기준으로 비교

model3 = lm(time ~ size+type, insu, contrasts = list(type=contr.sum))
summary(model3)

model.matrix(time ~ size+type, insu, contrasts = list(type=contr.sum))


#-------------------------------------------------------------
# 두 개 이상의 level을 가진 범주형 변수
#-------------------------------------------------------------

cdata = read.csv("CDI.csv", header = T)
head(cdata)
summary(cdata)

cdata$region = factor(cdata$region)

model1 = lm(crime ~ pop + region, cdata)
summary(model1)
model.matrix(crime ~ pop + region, cdata)

plot(model1) # 6 : outlier

cdata = cdata[-6, ]
model1 = lm(crime ~ pop + region, cdata)
summary(model1)

anova(model1) # H0 : b1 = b2 = b3 = 0


model2 = lm(crime ~ pop + region, cdata, contrasts = list(region=contr.sum))
summary(model2)

model.matrix(crime ~ pop + region, cdata, contrasts = list(region=contr.sum))


#-------------------------------------------------------------
# Interaction 교호작용
#-------------------------------------------------------------

model3 = lm(crime ~ pop + region + pop*region, cdata)
summary(model3)

anova(model3)



insumodel = lm(time ~ size + type + size*type, insu)
summary(insumodel)

anova(insumodel)



#-------------------------------------------------------------
# 두 개의 연속형 변수의 교호작용
#-------------------------------------------------------------
head(cdata)

model4 = lm(crime ~ pop + unemployment + pop*unemployment, cdata)
summary(model4)

plot(model4)

summary(cdata$unemployment)

# 다중공선성 문제로 centering 하여 사용.
# X1 - meanX
# X2 - meanX





