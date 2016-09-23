###############################################################
#
# 다변량 통계분석 1 (정여진 교수)
#
###############################################################

library(MVA)        # 교재 package
demo("Ch-MVA")

#--------------------------------------------------------------
# 공분산, 상관계수, 거리
#--------------------------------------------------------------

# 데이터 내의 어떤 구조나 패턴이 변수들 사이의 관계에 의해서, 또는 다른 개체들과의 
# 상대적인 거리에 의해서 나타날 수 있을까?

# 두 확률변수의 선형 의존성에 대한 측도 = 상관계수(correlation coefficient)
# 두 변수 사이의 관계가 선형이 아니라면 상관계수는 오해를 줄 수 있다.

measure
summary(measure)

df <- measure[, 1:3]
male <- measure[measure$gender == "male", 1:3]
female <- measure[measure$gender == "female", 1:3]

cor(df)
cor(male)
cor(female)

library(psych)
pairs.panels(male)
pairs.panels(female)

# 그러므로 데이터내의 개체들 간의 거리(distance) 개념이 중요.
# 일반적으로 유클리드 거리 사용
# 변수들은 서로 다른 척도(단위)를 갖기 때문에 먼저 표준화를 한 후 거리를 계산.

scale_measure <- scale(df, center = F)
scale_measure <- scale(df)              # 표준화 : 평균 0, 표준편차 1 로 변환

scale_measure
summary(scale_measure)
pairs.panels(scale_measure)

round(dist(scale_measure), 2)


#--------------------------------------------------------------
# outlier 찾기
#--------------------------------------------------------------

df <- USairpollution

head(df)
pairs.panels(df)

library(DMwR)
outlier.score <- lofactor(df, k = 5)
outlier.score
plot(density(outlier.score), main = "outlier score")

sort(outlier.score, decreasing = T)[1:10]
outliers <- order(outlier.score, decreasing = T)[1:3]  # score > 2.2 
outliers
df[outliers, ]

pairs.panels(df[-outliers, ])


#--------------------------------------------------------------
# 다변량 데이터의 시각화
#--------------------------------------------------------------

attach(USairpollution)
head(USairpollution)


#--------------------------------------------------------------
# 1. 산점도 scatterplot

plot(popul ~ manu, data = USairpollution)

# 1-1. Bivariate boxplot

# 영역을 3개로 분할
layer <- matrix(c(2,0,1,3), nrow = 2, byrow = T)
layout(layer, widths = c(2,1), heights = c(1,2), respect = T)
# 각 영역에 순서대로 표시
xlim <- range(manu) * 1.1
plot(popul ~ manu, data = USairpollution, cex = 0.9, type = "n", xlim = xlim)
text(manu, popul, cex = 0.6, labels = row.names(USairpollution))
hist(manu, main = "", xlim = xlim)
boxplot(popul)

par(mfcol=c(1,1))

# outlier 있고 없고 상관계수 비교
df <- USairpollution
rownames(df) <- rep(1:nrow(df))
plot(popul ~ manu, data = df, cex = 0.9, type = "n")
text(manu, popul, cex = 0.9, labels = rownames(df))

outlier <- c(7,30,14)
cor(df$manu, df$popul)                      # 0.9552693
cor(df$manu[-outlier], df$popul[-outlier])  # 0.7698125
    # -> 상관계수가 오히려 줄었다.


#--------------------------------------------------------------
# 2. Bubble chart - 변수 3개 표현

symbols(temp, wind, circles = SO2, inches = 0.5)
legend(70, 13, "circle : SO2")

# 크기 2배 -> 면적은 4배
# sqrt 변환 : 크기가 비슷해져 변별력이 떨어진다.
symbols(temp, wind, circles = sqrt(SO2), inches = 0.5) 


ylim <- range(wind) * c(0.95, 1)
plot(wind ~ temp, data = USairpollution, pch = 10, ylim = ylim)
symbols(temp, wind, circles = SO2, inches = 0.5, add = T)


#--------------------------------------------------------------
# 3. mosaic plot
UCBAdmissions

library(reshape)
melt(UCBAdmissions)

adm <- UCBAdmissions
adm[,,1]
adm[,1,]

mosaicplot(~Dept + Gender, data = adm, color = T)
mosaicplot(~Gender + Dept, data = adm, color = T)

ucb <- melt(UCBAdmissions)
head(ucb)

df <- xtabs(value ~ Dept + Gender, data = ucb)
mosaicplot(~ Dept + Gender, data = df, color = T)

df2 <- xtabs(value ~ Dept + Admit, data = ucb)
mosaicplot(~ Dept + Admit, data = df2, color = T)


#--------------------------------------------------------------
# practice

crime <- read.csv("crime.csv", header = T)
head(crime, 10)
pairs.panels(crime[-1])

plot(~ murder + burglary, data = crime)
cor.test(crime$murder, crime$burglary)

outlier <- crime[crime$murder > 20, ]
outlier

crime2 <- crime[-10, ]
crime2

plot(~ murder + burglary, data = crime2)
cor.test(crime2$murder, crime2$burglary)

symbols(crime$murder, crime$burglary, circles = population)

dim(crime2)

out <- match(c("United States", ))
clr <- rep(1, nrow(crime))
clr






