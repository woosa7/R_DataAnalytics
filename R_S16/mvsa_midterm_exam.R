
#-----------------------------------------------------------------------------
# 1. PCA
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# 데이터 탐색

cereal = read.csv("cereal.csv", header = T)
head(cereal)
summary(cereal)


#-----------------------------------------------------------------------------
# 1-A. 영양성분 상 특성을 시리얼 별로 한눈에 비교하기 위한 그래프를 그린 후 
# 비슷한 영양성분을 가지는 시리얼들을 탐색적으로 구분하여 서술하시오.

cereal_m = cereal
rownames(cereal_m) = paste(cereal_m$Brand, '(', as.character(cereal_m$Manufacturer), ')')
heatmap(as.matrix(cereal_m[,3:10]), scale = "column", Colv = NA)


#-----------------------------------------------------------------------------
# find outlier 

library(psych)
library(ggplot2)

pairs.panels(cereal[, 3:10])

# Fiber, Potassium 산점도에서 이상치 데이터 보임
# 산점도로 이상치 확인

ggplot(cereal, aes(Potassium, Fiber)) + 
geom_text(aes(Potassium, Fiber, label = rownames(cereal), colour = Manufacturer), hjust = 2)

outlier = 18
cereal[outlier, ]


# 전체 변수를 기준으로 이상치 확인

library(DMwR)
outlier.score <- lofactor(cereal[,3:10], k = 5)  # 이웃 5개 데이터 기준
outlier.score
plot(density(outlier.score), main = "cereal outlier score")

sort(outlier.score, decreasing = T)[1:10]
outliers <- order(outlier.score, decreasing = T)[1]  # score > 1.9 인 데이터를 outlier로 결정
outliers
cereal[outliers, ]   # 역시 outlier 는 AllBran 제품

pairs.panels(cereal[, 3:10])
pairs.panels(cereal[-outlier, 3:10])

# outlier 제거하기 전 Fiber, Potassium의 correlation = 0.93
# outlier 제거 후 correlation = 0.90
# 두 변수가 correlation의 변동량이 크지 않고, 다른 변수에서 큰 영향이 없기 때문에 outlier 포함하고 분석 진행.



#-----------------------------------------------------------------------------
# 주성분분석

# 적절한 주성분의 개수는 무엇인가? 
# 각 주성분은 어떤 의미를 가지는가?
# 이상치가 있는가? 있다면 어떤 성질을 가지는가?
# 주성분 분석의 결과를 활용하여 볼 때 각 제조사가 생산하는 시리얼 별로 영양성분 상의 특성이 다른가?

pca <- prcomp(cereal[, 3:10], scale = T)   # 상관계수 행렬 이용
plot(pca, type = "l")
summary(pca)

# 주성분의 갯수
# Cumulative Proportion을 기준으로 87.99 %까지 설명이 가능한 제4 주성분까지 4개의 주성분 선택


# 각 주성분은 어떤 의미를 가지는가?

par(mfrow=c(2,2))
barplot(pca$rotation[,1], col = rainbow(8), ylim = c(-0.6,0.4), las = 2, main = "PC1")
abline(h = -0.4, col="blue")
barplot(pca$rotation[,2], col = rainbow(8), ylim = c(-0.4,0.8), las = 2, main = "PC2")
abline(h = 0.4, col="blue")
barplot(pca$rotation[,3], col = rainbow(8), ylim = c(-0.4,0.8), las = 2, main = "PC3")
abline(h = 0.4, col="blue")
barplot(pca$rotation[,4], col = rainbow(8), ylim = c(-0.8,0.4), las = 2, main = "PC4")
abline(h = -0.4, col="blue")
par(mfrow=c(1,1))


# 이상치가 있는가? 있다면 어떤 성질을 가지는가?

par(mfcol=c(1,2))
biplot(pca)
biplot(pca, choices = c(3,4))
par(mfcol=c(1,1))

ord = order(cereal$Potassium, decreasing = T)[1:10]
cereal[ord, ]   # 18 : Potassium, Fiber 함량이 다른 제품에 비해 상당히 높다.

ord = order(cereal$Calories)[1:6]
cereal[ord, ]   # 41, 42 : Calories, Sugar 함량이 다른 제품에 비해 상당히 낮다.

ord = order(cereal$Fat, decreasing = T)[1:6]
cereal[ord, ]   # 41, 42 : Calories, Sugar 함량이 다른 제품에 비해 상당히 낮다.


# 주성분 분석 결과를 볼 때 각 제조사가 생산하는 시리얼 별로 영양성분 상의 특성이 다른가

# 색깔을 구분하기 위해 제조사별로 번호 지정
pcolors = ifelse(cereal$Manufacturer == 'G', 1, ifelse(cereal$Manufacturer == 'K', 2, 3))
plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab = "PC2", 
     xlim = c(-7,3), ylim = c(-5,6), pch = pcolors, col = pcolors)
text(pca$x[,1], pca$x[,2], labels = rownames(cereal), cex = 0.7, pos = 3, col = pcolors)
legend("topright", c("G","K","Q"), col = c("black", "red", "green"), fill = c("black", "red", "green"))
# 주성분 화살표의 방향과 길이
lambda <- pca$sdev * sqrt(nrow(pca$x))
rot <- t(t(pca$rotation)*lambda)
arrows(rep(0,nrow(pca$rotation)), rep(0,nrow(pca$rotation)), rot[,1], rot[,2], col = "grey")
text(rot[,1:2], rownames(rot), col = "blue")   # 화살표 제목




#-----------------------------------------------------------------------------
# 2. Factor Analysis
#-----------------------------------------------------------------------------

library(psych)
data("Thurstone.33")

df = Thurstone.33   # 상관계수 행렬. 이미 표준화되어 있기 때문에 scaling 할 필요 없음.
df

# 1 : Definitions, 2 : Arithmetical_Problems, 3 : Classification, 4 : Artificial_Languange, 
# 5 : Antonyms, 6 : Number_Series_Completion, 7 : Analogies, 8 : Logical_Inference, 9 : Paragraph_Reading


#-----------------------------------------------------------------------------
# A. 이 데이터를 사용하여 요인분석을 진행하여 9개의 테스트 결과에 영향을 주는 잠재요인을 파악하시오.
# (적절한 요인 개수와 요인회전 고려) 

library(GPArotation)

fa1 = fa(df, 4, rotate = "varimax")
fa2 = fa(df, 5, rotate = "varimax")
fa3 = fa(df, 4, rotate = "quartimax")
fa4 = fa(df, 5, rotate = "quartimax")

# 4-factor varimax
print(fa1, digits = 2, sort = T)

plabel = paste(c(1:9), "\n", colnames(df))
plot(fa1$loadings, type = "n", main = "4-factor : varimax")
text(fa1$loadings, labels = plabel, cex = 0.8)

# 5-factor varimax
print(fa2, digits = 2, sort = T)

plot(fa2$loadings, type = "n", main = "4-factor : varimax")
text(fa2$loadings, labels = plabel, cex = 0.8)

# 4-factor quartimax
print(fa3, digits = 2, sort = T)

# 5-factor quartimax
print(fa4, digits = 2, sort = T)


# 최종적으로 4-factor varimax 결과를 선택
fa.diagram(fa1)


#-----------------------------------------------------------------------------
# B. 잠재요인에 의해 가장 설명이 잘되는 원변수와 가장 설명이 안되는 원변수를 찾으시오.






