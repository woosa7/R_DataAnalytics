    # Practice 2

# Bulls : 경매시장에서 거래된 76마리의 2살 이하 황소의 특성과 거래가격(SalePr)에 관한 자료
# SalePr와 Breed 변수를 제외한 7개의 변수를 사용해 주성분분석을 시행하여 아래의 질문에 답하시오. 
# 공분산 행렬과 상관계수 행렬을 사용하여 각각 분석하고 비교하시오.

bulls <- read.csv("bulls.csv", header = T)
head(bulls)

bullsV7 <- bulls[, -c(1,2)]    # Breed, SalePr 변수 제거
head(bullsV7)

pairs.panels(bullsV7)

#------------------------------------------------------------
# 1. 각 주성분의 표준편차와 그 주성분을 계산하는데 사용된 rotation값을 찾으시오.


# (1) 공분산 행렬 이용
pca_cov <- prcomp(bullsV7)
pca_cov

# (2) 상관계수 행렬 이용
pca <- prcomp(bullsV7, scale = T)
pca


#------------------------------------------------------------
# 2. 적절한 주성분의 개수를 선택하고 근거를 설명하시오.

# (1) 공분산 행렬 이용
plot(pca_cov, type = "l")
summary(pca_cov) 

# (2) 상관계수 행렬 이용
plot(pca, type = "l")
summary(pca)


#------------------------------------------------------------
# 3. 각 주성분의 rotation값을 표와 그래프를 사용해 비교하고 주성분의 의미를 해석하시오.

# (1) 공분산 행렬 이용
# (2) 상관계수 행렬 이용
pca$rotation[,1]

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

