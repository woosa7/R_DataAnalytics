###############################################################
#
# 다변량 통계분석 2 (정여진 교수)
#
###############################################################

library(MVA)
demo("Ch-PCA")

#------------------------------------------------------------
# 주성분 분석
#------------------------------------------------------------

library("MVA")
demo("Ch-PCA")

# 상관계수 행렬을 이용한 선형 결합


df <- read.csv("open_closed.csv")
head(df)

library(psych)
pairs.panels(df)

pca <- prcomp(df, scale = T)   # default : 공분산행렬, scale : 상관계수행렬 이용
summary(pca)
# 주성분 갯수는 70~90 %에서 선택
# 또는 Standard deviation(고유값)이 0.7 보다 작은 것은 버림

pca
# pc1 : 전체적으로 잘하고 못하는 학생들을 분리
# pc2 : mechanics 는 잘하지만 statistics는 못하는 특성의 학생들 분리
# scaling 했기 때문에 각 주성분의 분산 = 1, 전체 총분산 = 5 (변수 5개)
# 고유값 = 각 주성분의 분산
# 고유벡터 = 각 주성분의 

a1 <- pca$rotation[, 1]
a1

pc1 <- pca$x[, 1]
pc2 <- pca$x[, 2]
cor(pc1, pc2)     # pc1, pc2의 공분산은 0

df_s <- scale(df)
summary(df_s)

plot(df_s%*%a1, pca$x[, 1])


screeplot(pca, type = "l")

biplot(pca)

#------------------------------------------------
# Example

detach("package:MVA")
library(MVA)

df <- heptathlon
df
pairs.panels(df)

# 달리기는 숫자가 작은 것이 좋은 것이기 때문에 반대로 변환.
df$hurdles <- with(df, max(hurdles) - hurdles)
df$run200m <- with(df, max(run200m) - run200m)
df$run800m <- with(df, max(run800m) - run800m)

# delete outlier
df2 <- df[df$hurdles > 0, ]
df2
pairs.panels(df2)

pca <- prcomp(df2[, -8], scale = T)
summary(pca)

plot(pca, type = "l")

plot(df2$score, pca$x[,1])
cor(df2$score, pca$x[,1])

biplot(pca)

# pc1 축과 평행일수록 상관관계가 높고 roatation 값이 높다.
# pc2 축과 수직인 변수들은 별로 영향력이 없음을 의미한다.
# pc2 : 높이뛰기는 못하지만 장거리를 잘하는 선수들이라고 할 수 있다.

biplot(pca, cex = 0.8, choices = c(1,3)) # pc1 & pc3


# qqnorm(pca$x[,1])
# qqplot(pca$x[,1])

#------------------------------------------------------------
# Practice 2

bulls <- read.csv("bulls.csv", header = T)
head(bulls)

attach(bulls)

library(psych)
pairs.panels(bulls[ , -c(1,2)])

# BkFat - 한쪽을 치우져 있어서 변환이 필요할 수도.
# 로그변환 했으나 cor 증가가 미미하므로 변환하지 않고 분석진행도 가능

plot(FtFrBody, PrctFFB)
cor.test(FtFrBody, PrctFFB)

boxplot(FtFrBody)
boxplot(PrctFFB)

plot(PrctFFB ~ FtFrBody, data = bulls, cex = 0.8, type = "n")
text(FtFrBody, PrctFFB, cex = 0.8, labels = rownames(bulls))   # outlier = 51
outlier <- 51

bulls2 <- bulls[-outlier, ]
plot(bulls2$FtFrBody, bulls2$PrctFFB)
cor.test(bulls2$FtFrBody, bulls2$PrctFFB)

# cov
pca_cov <- prcomp(bulls[ , -c(1,2)])
summary(pca_cov)
biplot(pca_cov)

# 설문조사처럼 같은 scale 점수화가 된 경우에는 cov 사용해도 되지만
# 변수들의 scale이 많이 다른 경우 특정 변수가 전체적인 경향을 좌우하기 때문에 
# cor 사용하여 분석하는 것이 좋다.

# cor
pca <- prcomp(bulls[ , -c(1,2)], scale = T)
summary(pca)
biplot(pca)


qqnorm(pca$x[,1])
qqline(pca$x[,1])

summary(bulls)
head(bulls2)

