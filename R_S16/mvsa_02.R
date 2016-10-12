###############################################################
#
# 다변량 통계분석 2 (정여진 교수) - 주성분분석 PCA
#
###############################################################

library(MVA)
demo("Ch-PCA")

df <- read.csv("open_closed.csv")
head(df)

library(psych)
pairs.panels(df)

# 설문조사처럼 같은 scale 점수화가 된 경우에는 cov 사용해도 되지만
# 변수들의 scale이 많이 다른 경우 특정 변수가 전체적인 경향을 좌우하기 때문에 
# cor 사용하여 분석하는 것이 좋다.

pca_c <- prcomp(df)     # default : 공분산 행렬을 이용한 선형 결합
summary(pca_c)
pca_c

pca_r <- prcomp(df, scale = T)   # scale : 상관계수행렬 이용
summary(pca_r)
# 주성분 갯수는 70~90 %에서 선택
# 또는 Standard deviation(고유값)이 0.7 보다 작은 것은 버림 (상관계수 이용시)

pca_r
# pc1 : 전체적으로 잘하고 못하는 학생들을 분리
# pc2 : mechanics 는 잘하지만 statistics는 못하는 특성의 학생들 분리
# scaling 했기 때문에 각 주성분의 분산 = 1, 전체 총분산 = 5 (변수 5개)

pca_r$sdev              # 고유값 = 각 주성분의 분산(Standard deviations)
pca_r$rotation[, 1]     # 고유벡터 = 각 주성분의 Rotation 값

pc1 <- pca_r$x[, 1]
pc2 <- pca_r$x[, 2]
cor(pc1, pc2)     # pc1, pc2의 공분산은 0 에 수렴

df_s <- scale(df)
summary(df_s)

rot1 <- pca_r$rotation[, 1]
plot(df_s%*%a1, pca_r$x[, 1])  # 완벽한 상관관계를 보인다.

screeplot(pca_r, type = "l")   # 떨어지는 각도가 완만해지는 2까지 주성분으로 선택.

biplot(pca_r)


#------------------------------------------------
# Example

library(MVA)

df <- heptathlon   # 여자 철인 7종 경기
df
pairs.panels(df)

# 달리기는 숫자가 작은 것이 좋은 것이기 때문에 반대로 변환.
df$hurdles <- with(df, max(hurdles) - hurdles)
df$run200m <- with(df, max(run200m) - run200m)
df$run800m <- with(df, max(run800m) - run800m)
df

# delete outlier : 모든 종목에서 이상치 보이는 Launa (PNG) 삭제
df2 <- df[df$hurdles > 0, ]
df2
pairs.panels(df2)

# 주성분 분석
pca <- prcomp(df2[, -8], scale = T)   # score 변수 제외
summary(pca)
pca

plot(pca, type = "l")   # PC2까지 주성분으로 선택

plot(df2$score, pca$x[,1])
cor(df2$score, pca$x[,1])
# PC1과 score는 -1 의 상관관계. 전 종목에서 잘하고 못하는 선수 구분

biplot(pca)
# pc1 축과 평행일수록 상관관계가 높고 roatation 값이 높다.
# pc2 축과 수직인 변수들은 별로 영향력이 없음을 의미한다.
# pc2 : 높이뛰기는 못하지만 장거리를 잘하는 선수들이라고 할 수 있다.

biplot(pca, cex = 0.8, choices = c(1,3)) # pc1 & pc3


qqnorm(pca$x[,1])
qqline(pca$x[,1])


#------------------------------------------------------------
# Practice 2

# Bulls : 경매시장에서 거래된 76마리의 2살 이하 황소의 특성과 거래가격(SalePr)에 관한 자료
# SalePr와 Breed 변수를 제외한 7개의 변수를 사용해 주성분분석을 시행하여 아래의 질문에 답하시오. 
# 공분산 행렬과 상관계수 행렬을 사용하여 각각 분석하고 비교하시오.

# 참조 : blog.naver.com/woosa7/220810261277

bulls <- read.csv("bulls.csv", header = T)
head(bulls)

bullsV7 <- bulls[, -c(1,2)]   # Breed, SalePr 변수 제거
head(bullsV7)

library(psych)
pairs.panels(bullsV7)   # BkFat - 한쪽에 치우져 있어서 변환이 필요한 것처럼 보인다.

cor(bullsV7$YrHgt, bullsV7$BkFat)       # -0.34
cor(bullsV7$YrHgt, log(bullsV7$BkFat))  # -0.41

cor(bullsV7$SaleWt, bullsV7$BkFat)       # 0.21
cor(bullsV7$SaleWt, log(bullsV7$BkFat))  # 0.15

# 로그 변환 했으나 correlation 증가가 미미하므로 변환하지 않고 분석을 진행하는 것으로 한다.


#------------------------------------------------------------
# 1. 각 주성분의 표준편차와 그 주성분을 계산하는데 사용된 rotation값을 찾으시오.

# (1) 공분산 행렬 이용
pca_cov <- prcomp(bullsV7)
pca_cov

# (2) 상관계수 행렬 이용
pca <- prcomp(bullsV7, scale = T)
pca

# 주성분의 표준편차(Standard deviation) = 고유값
# 주성분을 계산하는데 사용된 rotation = 고유벡터


#------------------------------------------------------------
# 2. 적절한 주성분의 개수를 선택하고 근거를 설명하시오.

# (1) 공분산 행렬 이용

plot(pca_cov, type = "l")
# plot에서 제3 주성분부터 기울기가 완만하게 변하고 그 이후로는 거의 의미가 없기 때문에
# 제3 주성분까지 3개의 주성분을 선택한다.

summary(pca_cov)
# 각 주성분의 중요도(Proportion of Variance) 누적합인 Cumulative Proportion을 보면
# 제1, 제2 주성분 만으로도 전체 데이터의 99.96%를 설명할 수 있다.
# 그러므로 2개의 주성분만 선택한다.


# (2) 상관계수 행렬 이용

plot(pca, type = "l")
# plot에서는 기울기가 완만하게 변하는 팔꿈치 부분을 명확하게 정하기 쉽지 않다.
# 제2 ~ 제5 주성분의 기울기가 비슷해 보인다.

summary(pca)
# 각 주성분의 중요도 누적합인 Cumulative Proportion을 보면
# 제3 주성분까지 선택할 경우 전체 데이터 88.56% 에 대한 설명력을 갖는다.
# 또한 제4 주성분의 고유값(Standard deviation)이 0.7 보다 작기 때문에 제3 주성분까지 3개의 주성분을 선택한다.


#------------------------------------------------------------
# 3. 각 주성분의 rotation값을 표와 그래프를 사용해 비교하고 주성분의 의미를 해석하시오.

# (1) 공분산 행렬 이용

# (2) 상관계수 행렬 이용
a1 <- pca$rotation[,1]
a1

bulls_s <- scale(bullsV7)

plot(bulls_s%*%a1, pca$x[, 1])

# PC1 = (-0.4499313 * YrHgt) + (-0.4123256 * FtFrBody) + (-0.3555618 * PrctFFB) +
#       (-0.4339569 * Frame) + (0.1867048 * BkFat) + (-0.4528538 * SaleHt) + 
#       (-0.2699470 * SaleWt) 


#------------------------------------------------------------
# 4. 행렬도를 사용해 원변수와 주성분의 관계, 원변수 간의 상관관계, 특이한 관측치의 존재 유무 등을 파악하고 설명하시오. 

# (1) 공분산 행렬 이용
biplot(pca_cov)
# (2) 상관계수 행렬 이용
biplot(pca)


#------------------------------------------------------------
# 5. 첫 두개의 주성분을 사용해 산점도를 그리고 Breed를 서로 다른 색깔과 기호로 표시하시오. 
# 주성분에 의해 다른 종의 황소를 구분할 수 있는가? 이상점이 있는가? 있다면 어떤 특성을 가진 소인가?

# (1) 공분산 행렬 이용
# (2) 상관계수 행렬 이용

bulls$Breed <- factor(bulls$Breed)
summary(bulls)

plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab = "PC2", 
     pch = as.numeric(bulls$Breed), col = as.numeric(bulls$Breed))

text(pca$x[,1], pca$x[,2], labels = as.character(bulls$Breed), 
     cex = 0.7, pos = 3, col = as.numeric(bulls$Breed))


bulls[16, ]
boxplot(bulls$BkFat)

bulls[51, ]
boxplot(bulls$FtFrBody)


#------------------------------------------------------------
# 6. 첫 주성분을 사용해 Q-Q plot을 그리고 해석하시오.

# (1) 공분산 행렬 이용

# (2) 상관계수 행렬 이용

qqnorm(pca$x[,1])
qqline(pca$x[,1])



