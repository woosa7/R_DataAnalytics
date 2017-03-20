###############################################################
#
# 다변량 통계분석 7 - 다중회귀분석 (범주형 설명변수)
#
###############################################################

# 보험업계의 혁신에 대한 연구
# • 혁신을 받아들이는데 걸리는 시간이 회사 규모와 유형에 따라 달라지는가?
# • Y: 혁신을 받아들이는데까지 걸리는기간을 월 단위로측정
# • X1: 회사의자산규모
# • X2: 회사 유형 (stock, mutual)

insu = read.csv("data/insurance.csv", header = T)
head(insu)
summary(insu)

library(ggplot2)
ggplot(insu, aes(y=time, x=size, color=type)) + geom_point(size=3)

model.matrix(time ~ size + type, insu)

model1 = lm(time ~ size + type, insu)
summary(model1)

# 회귀식 : y = b0 + b1*X1 + b2*X2 + e
# X1 = size
# X2 = type : 1 if stock, 0 if mutual
# Mutual    : y = b0 + b1*X1
# Stock     : y = (b0 + b2) + b1*X1

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

# County demographic information (CDI)
# • 미국의 가장인구가 많은 440개 county의자료
# • Crime: 범죄 발생수(y)
# • Pop: 인구 (X1)
# • Region: 지역(1=NE, 2=NC, 3=S, 4=W)
# • 범죄발생 건수가 인구, 실업률과 지역에 따라 어떻게 달라지는가?

cdata = read.csv("data/CDI.csv", header = T)
head(cdata)
summary(cdata)

cdata$region = factor(cdata$region)

model1 = lm(crime ~ pop + region, cdata)
summary(model1)
model.matrix(crime ~ pop + region, cdata)

plot(model1) # 6 : outlier

# outlier 제거한 후 다시 모델링
cdata = cdata[-6, ]
model1 = lm(crime ~ pop + region, cdata)
summary(model1)

anova(model1) # H0 : b1 = b2 = b3 = 0 ---> 가설 기각

cdata$fitted = model1$fitted.values
ggplot(cdata, aes(y=fitted, x=pop, color=region)) +
    geom_line() + 
    geom_point(aes(y=crime, x=pop, shape=region))


# contrasts
model2 = lm(crime ~ pop + region, cdata, contrasts = list(region=contr.sum))
summary(model2)

model.matrix(crime ~ pop + region, cdata, contrasts = list(region=contr.sum))


#-------------------------------------------------------------
# Interaction 교호작용
#-------------------------------------------------------------

# 지역에 따라 인구 규모가 범죄건수에 미치는 영향의 정도가 다를까?
model3 = lm(crime ~ pop + region + pop*region, cdata)
summary(model3)

# pop:region2 - pop의 기울기가 NE에 비해 NC 지역에서 얼마나 높은가
# pop:region3 - pop의 기울기가 NE에 비해 S 지역에서 얼마나 높은가
# pop:region4 - pop의 기울기가 NE에 비해 W 지역에서 얼마나 높은가

# 지역별로 기울기의 차이가 있는가?
anova(model3)

cdata$fitted = model3$fitted.values
ggplot(cdata, aes(y=fitted, x=pop, color=region)) +
    geom_line() + 
    geom_point(aes(y=crime, x=pop, shape=region))


insumodel = lm(time ~ size + type + size*type, insu)
summary(insumodel)

anova(insumodel)



#-------------------------------------------------------------
# 두 개의 연속형 변수의 교호작용
#-------------------------------------------------------------

model4 = lm(crime ~ pop + unemployment + pop*unemployment, cdata)
summary(model4)

# unemployment 의 p-value가 유의하지 않다고 나왔지만, pop:unemployment는 유의함.
# 주효과가 유의하지 않더라도 교호작용이 유의하면 제거하지 않음.

plot(model4)

summary(cdata$unemployment)

intc = -1.708e+03
ipop = 6.085e-02
iunemp = -4.492e+02
ipopunemp = 2.598e-03

ggplot(cdata, aes(x=pop, y=crime, colour="data")) +
    geom_point() +
    geom_abline(intercept = intc + iunemp*2.2, slope = ipop + ipopunemp*2.2, aes(colour='min')) +
    geom_abline(intercept = intc + iunemp*6.59, slope = ipop + ipopunemp*6.59, aes(colour='mean')) +
    geom_abline(intercept = intc + iunemp*21.3, slope = ipop + ipopunemp*21.3, aes(colour='max')) +
    scale_color_discrete(name="Unemployment")
           
# 동일한 실업률 수준이 유지될 때 인구가 증가할수록 범죄 건수가 증가함
# 기울기가 실업률이 높을 수록 가파름




