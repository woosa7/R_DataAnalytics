###############################################################
#
# 다변량 통계분석 8 - Panel (longitudinal) Data Analysis
#
###############################################################

# 횡단면 자료 cross-sectional
# 종단면 자료 time series
# 패널 자료 : 횡단면 자료 + 종단면 자료

# 한 사람에 대해 (시간에 따라) 여러번 측정 - 시간 흐름에 관계없는 경우도 있음.
#   id  time    y   x
#   1   1
#   1   2
#   1   3
#   2   1
#   2   2
#   2   3

library(mice)
head(potthoffroy)

data = reshape(potthoffroy, idvar = "id", varying = list(3:6), v.names = "dist", direction = "long")
head(data)

library(ggplot2)

ggplot(data, aes(y=dist, x=time, group=id, color=sex)) +
    geom_line(aes(linetype=sex), stat="identity") +
    geom_point()

library(plyr)

data2 = ddply(data, ~sex+time, summarise, mean=mean(dist))
data2

ggplot(data2, aes(y=mean, x=time, group=id, color=sex)) +
    geom_line(aes(linetype=sex), stat="identity") +
    geom_point()


# anova
model1 = lm(dist~sex, data)  
anova(model1)               # 한 사람에대한 반복 측정되었기 때문에 사용 불가


# Autoregressive 자기상관
# 시간이 지남에 따라 상관계수가 작아진다.
# -1 <= ro <= 1
# ro > ro^2

# Compound symmetry
# 여러 그룹에 대해 측정한 경우 한 그룹내의 사람들은 동일한 상관계수를 갖는다.



# repeated measure anova
library(nlme)

model2 = gls(dist~sex, data, correlation = corAR1(form = ~1|id))  # correlation이 일어나는 그룹
anova(model2)
summary(model2)

# Parameter estimate(s): Phi = ro (첫번째 상관계수)

model3 = gls(dist~sex, data, correlation = corCompSymm(form = ~1|id))
anova(model3)
summary(model3)

# random intercept model
library(lme4)
model4 = lmer(dist~sex+time+sex:time + (1|id), data)
summary(model4)

# 남자/여자의 기울기 차이가 보임.
# 개인별 기울기 차이를 고려할 경우 - random intercept and slope model


# random intercept and slope model
model5 = lmer(dist~sex+time+sex:time + (1+time|id), data)
summary(model5)

# correlation of intercept / slope of time = -0.28
# 평균선을 기준으로 y 절편이 작은 아이의 기울기가 크고, 큰 아이는 기울기가 작은 경향이 있다.



# timber
head(timber)

model6 = lmer(loads~slippage+I(slippage^2)+(1+slippage|specimen), timber)
summary(model6)





















