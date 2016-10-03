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


