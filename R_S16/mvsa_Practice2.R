#------------------------------------------------------------
# Practice 2
#------------------------------------------------------------

# Bulls : 경매시장에서 거래된 76마리의 2살 이하 황소의 특성과 거래가격(SalePr)에 관한 자료
# SalePr와 Breed 변수를 제외한 7개의 변수를 사용해 주성분분석을 시행하여 아래의 질문에 답하시오. 
# 공분산 행렬과 상관계수 행렬을 사용하여 각각 분석하고 비교하시오.

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

a1 <- pca$rotation[,1]
a1
bulls_x <- scale(bullsV7) %*% a1
plot(bulls_x, pca$x[, 1])

# 7개의 각 변수값에 rotation 값을 행렬곱한 결과가 주성분의 값이다.
# 즉, 주성분의 rotation값은 각 변수에 대해 다음과 같은 설명력을 갖는다.
# PC1 = (-0.4499313 * YrHgt) + (-0.4123256 * FtFrBody) + (-0.3555618 * PrctFFB) +
#       (-0.4339569 * Frame) + (0.1867048 * BkFat) + (-0.4528538 * SaleHt) + (-0.2699470 * SaleWt) 


#------------------------------------------------------------
# 4. 행렬도를 사용해 원변수와 주성분의 관계, 원변수 간의 상관관계, 특이한 관측치의 존재 유무 등을 파악하고 설명하시오. 

# (1) 공분산 행렬 이용

biplot(pca_cov)

# 제1, 제2 주성분 만으로도 전체 데이터의 99.96%를 설명할 수 있지만
# 변수들의 단위가 다르기 때문에 결국 큰 값을 가진 FtFrBody와 SaleWt 두 변수가 주성분을 좌우하게 된다.
# 그러므로 변수간 단위가 크게 차이나는 경우 scaling이 적용되는 상관계수 행렬을 사용해야 한다.

# (2) 상관계수 행렬 이용

biplot(pca)

# BkFat, SaleWt를 제외한 모든 변수가 제1 주성분에 대해 비슷한 정도로 기여를 하고 있다.
# BkFat, SaleWt는 제2 주성분을 특징짓는 변수이다.
# BkFat 변수는 제1 주성분의 특징과 반대되는 성향을 보인다. 
# 즉 BkFat 값이 큰 경우 SaleHt, YrHgt, Frame 등의 값은 작은 경향을 보인다.


#------------------------------------------------------------
# 5. 첫 두개의 주성분을 사용해 산점도를 그리고 Breed를 서로 다른 색깔과 기호로 표시하시오. 
# 주성분에 의해 다른 종의 황소를 구분할 수 있는가? 이상점이 있는가? 있다면 어떤 특성을 가진 소인가?

bulls$Breed <- factor(bulls$Breed)
summary(bulls)

# pch : 점모양 / col = 색상
# text : 각 점에 해당하는 라벨 출력
# plot의 각 옵션은 여기 참조 : github.com/woosa7/R_Study/blob/master/R_JS/C2_plot.R

plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab = "PC2",
     pch = as.numeric(bulls$Breed), col = as.numeric(bulls$Breed))
text(pca$x[,1], pca$x[,2], labels = as.character(bulls$Breed),
     cex = 0.7, pos = 3, col = as.numeric(bulls$Breed))

# 이 plot을 biplot과 같이 보면
# 제1 주성분으로 8번종의 소는 구별해낼 수 있지만,
# 1번과 5번 종의 소는 비슷한 특성으로 보이며 섞여 있다.
# 하지만 1번종은 BkFat, SaleWt이 5번 종보다 큰 경향을 보인다.

# 이상점(outlier)

# 16번 소는 BkFat에서 특이하게 큰 값을 보인다.
bulls[16, ]
boxplot(bulls$BkFat)

# 51번 소는 FtFrBody에서 특이하게 큰 값을 보인다.
bulls[51, ]
boxplot(bulls$FtFrBody)


#------------------------------------------------------------
# 6. 첫 주성분을 사용해 Q-Q plot을 그리고 해석하시오.

qqnorm(pca$x[,1])
qqline(pca$x[,1])
