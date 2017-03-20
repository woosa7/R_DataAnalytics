###############################################################
#
# 다변량 통계분석 5 - 군집분석 Cluster Analysis
#
###############################################################

#--------------------------------------------------------------
# 계층적 군집분석
#--------------------------------------------------------------

# 1.  개요
# 
# 각 객체의 유사성을 측정하여 유사성이 높은 대상 집단을 분류하고, 군집 간의 상이성을 규명하는 분석 방법.
# 특성에 따라 관측치들을 여러 개의 배타적인 집단으로 나눔.
# 범주(그룹)에 대한 사전 정보가 없다.
# * 그룹의 갯수나 특성에 대한 사전 정보가 있는 경우 분류분석(Classification) 사용
# * 요인분석은 유사한 “변수”를 함께 묶어 주고, 군집분석은 “데이터”를 묶어 주는 차이점
# 군집의 갯수나 구조에 대한 가정 없이 각 데이터 간의 거리를 기준으로 군집화 유도
# 
# 
# 2.  유사성 측도
# 
# 1)  연속형 변수
# 
# *   유클리드 거리 (Euclidean distance) : 주로 표준화된 자료에 사용
# *   맨하탄 거리 (Manhattan)
# *   민코우스키 거리 (Minkowski)
# *   마할라노비스 거리 (Mahalanobis) : 공분산 행렬을 고려
# 
# 2)  범주형 변수
# 
# *   자카드 거리(Jaccard)
# 
# 
# 3.  계층적 군집분석 (Hierachical, Agglomerative Clustering)
# 
# 데이터 간의 유사성을 계산해 가장 가까운 객체들부터 차례로 군집화
# 한 번 군집에 속하면 이동 불가능
# dendrogram을 사용해 군집 형성 과정 파악 가능
# 군집 간의 거리 차이에 큰 변화를 보이는 경우를 고려해 군집 갯수 결정
# 
# 1)  최단 연결법 (Single linkage)
# distance matrix 에서 가장 작은 값을 취하여 군집화
# 군집과 군집/데이터 간의 거리 중 최단거리(min) 값을 거리로 산정
# 길게 늘어진 형태의 군집이 형성될 수 있음
# 
# 2)  최장 연결법 (Complete linkage)
# 군집과 군집/데이터 간의 거리 중 최장거리(max) 값을 거리로 산정
# 둥그런 형태의 군집 형성
# 단점 : 이상치에 민감
# 
# 3)  평균 연결법 (Average linkage)
# 군집과 군집/데이터 간의 거리의 평균거리(mean) 값을 거리로 산정
# 
# 4)  Ward 연결법 (Ward’s method)
# 군집 간 정보의 손실을 최소화하는 군집화
# 군집 내 편차들의 제곱합을 고려하여 군집 내 거리(within cluster distance, ESS)를 최소화.
# 비슷한 크기의 군집을 생성하는 경향

#--------------------------------------------------------------
# USArrests data
# Arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973.
# percent of the population living in urban areas.

df = USArrests

dist(df, method = "manhattan") # "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"

mahalanobis(df, colMeans(df), cov(df))   # mahalanobis(x, center : 각 변수의 평균, cov : 공분산행렬)

# ------------------------------------------
# linkage methods

x = data.frame(c(1,3,6,12,20))
rownames(x) = c("a","b","c","d","e")
d = dist(x)

par(mfcol=c(2,2))
hc = hclust(d, method = "single")   # single linkage (최단연결법)
plot(hc)
hc2 = hclust(d, method = "complete")    # complete linkage (최장연결법)
plot(hc2)
hc3 = hclust(d, method = "average")  # average linkage (평균연결법)
plot(hc3)
hc4 = hclust(d, method = "ward.D")  # ward method
plot(hc4)
par(mfcol=c(1,1))

hc$height

# ------------------------------------------
# USArrests

distUS = dist(scale(df))

hc = hclust(distUS, method = "single")
plot(hc)

hc2 = hclust(distUS, method = "complete")
plot(hc2)

hc3 = hclust(distUS, method = "average")
plot(hc3)

hc4 = hclust(distUS, method = "ward.D")
plot(hc4)





# ----------------------------------
# Cluster Plot

h3result = cutree(hc3, k=5) # k : 그룹의 갯수
plot(df$Murder, df$Assault, col=h3result, pch=h3result)
text(df$Murder, df$Assault, labels = rownames(df), col=h3result, pos = 1)

plot(df$Murder, df$UrbanPop, col=h3result, pch=h3result)
text(df$Murder, df$UrbanPop, labels = rownames(df), col=h3result, pos = 1)


df$cluster = h3result

par(mfcol=c(2,2))

for (i in 1:4) {
    boxplot(df[,i] ~ h3result, main = names(df)[i])
}
par(mfcol=c(1,1))


library(psych)
describe(df)
describeBy(df, group = h3result)


#--------------------------------------------------------------
# 비계층적 군집분석 : K-means clustering
#--------------------------------------------------------------

# n개의 객체를 g개의 군집으로 나눌 수 있는 모든 가능한 방법을 점검해 최적화된 군집을 형성
# 
# K-means clustering
# 원하는 군집의 갯수와 초기값 seed를 정해 seed 중심으로 군집을 형성한다.
# 각 객체는 거리가 가장 가까운 seed가 있는 군집으로 분류된다.
# 각 군집의 seed 값을 다시 계산한다.
# 모든 객체를 갱신된 seed와 비교하여 필요시 적절한 군집으로 이동시킨다.
# 더이상 객체의 이동이 없을 때까지 위 과정을 반복한다.
# 
# 1)   장점
# 주어진 데이터의 내부 구조에 대한 사전정보 없이 의미있는 자료 구조를 찾을 수 있다.
# 다양한 형태의 데이터에 적용 가능
# 분석 방법 적용도 쉽다.
# 
# 2)   단점
# 가중치와 거리에 대한 정의가 어렵다.
# 초기 군집수 결정이 어렵다.
# 사전에 주어진 목적이 없기 때문에 결과 해석이 어려울 수 있다.

kdata = iris
kdata$Species = NULL
head(kdata)

# 3개의 군집으로 나누는 경우
kmc = kmeans(kdata, 3)
kmc
table(kmc$cluster, iris$Species)

plot(kdata$Sepal.Length, kdata$Sepal.Width, col = kmc$cluster, xlab = "Sepal.Length", ylab = "Sepal.Width")
points(kmc$centers[, c(1:2)], col = "blue", pch = 4, cex = 2)

plot(kdata$Petal.Length, kdata$Petal.Width, col = kmc$cluster, xlab = "Petal.Length", ylab = "Petal.Width")
points(kmc$centers[, c(3:4)], col = "blue", pch = 4, cex = 2)


# ------------------------------------------
# USArrests

kmUSA = kmeans(df, 3)
kmUSA
plot(df$Murder, df$Assault, type = "n", xlab = "Murder", ylab = "Assault", main = "3 Cluster (K-means)")
text(df$Murder, df$Assault, labels = rownames(df), cex = 0.8, col = kmUSA$cluster)
points(kmUSA$centers[, c(1,2)], col = "blue", pch = 4, cex = 2)

# 클러스터 갯수 결정시 지표
# within_SS : Within cluster sum of squares by cluster: 군집내 거리의 제곱합. 작을수록 좋다. 
# between_SS 군집간 거리의 제곱합. 클수록 좋다.
kmUSA$withinss

wss = c()
for (k in 1:20) {
    km = kmeans(df, k)
    wss[k] = sum(km$withinss)
}
wss

plot(1:20, wss, type = 'l')
points(wss)



#--------------------------------------------------------------
# 모형 기반 군집분석 (Model-based clustering)
#--------------------------------------------------------------

# 확률분포에 대한 정보가 있을 경우 이를 활용하여 군집분석
# k번째 군집에 속한 관측치 x 가 다변량 정규분포인 확률밀도함수 f 를 가진다고 가정
# 각 객체각 각 군집에 속할 사후 확률을 계산하여 가장 확률이 높은 군집으로 할당
# 모형선택방법 중 BIC (Bayesian information criterion)를 사용하여 이 값이 최대가 되는 모형을 선택


# lamda * I : I = identical matrix. 각 dimension의 분산이 같고 각 변수가 서로 독립적이다.
# 모든 모형에 대해 계산을 한 후 BIC를 기준으로 모형을 선택한다.

library(mclust)

mc = Mclust(USArrests)
summary(mc)
# 최종 선택 모형 : Mclust VEI (diagonal, equal shape) model with 3 components:

plot(mc)
# BIC plot을 보고 적당한 클러스터 갯수를 비교하여 선택한다.
# classification plot : 타원 = 공분산의 모양.
# uncertainry : posterior probability (사후 확률) 기준으로 어느 그룹에 속할지 확실하지 않은 관측치.

mc$classification

par(mfcol=c(2,2))
for (i in 1:4) {
    boxplot(USArrests[,1] ~ mc$classification, main = names(USArrests)[i])
}
par(mfcol=c(1,1))

# compare groups 1
USArrests_s = as.data.frame(scale(USArrests))

par(mfcol=c(1,3))
for (i in 1:3) {
    boxplot(USArrests_s[mc$classification == i,], ylim = c(-3,3), main = i)
}
par(mfcol=c(1,1))

# compare groups 2

library(psych)

result = describeBy(USArrests[,1], group = mc$classification, mat = T)  # murder
result

library(ggplot2)

ggplot(result, aes(group1, mean, fill = group1)) +
    geom_bar(position = position_dodge(), stat = "identity") +
    geom_errorbar(aes(ymin = mean-2*se, ymax = mean+2*se), width = 0.1)

# confidence inteval이 서로 겹치지 않기 때문에 그룹간 차이가 명확하게 있다.


# 세 그룹간 비교 --> anova

model = aov(USArrests$Murder ~ factor(mc$classification))
summary(model)

# 사후 검정 : Tukey Test

TukeyHSD(model)
# 각 그룹별 비교시 p-value가 0.05보다 작기 때문에 그룹간 확실한 차이가 있다.


# cluster 갯수 4개로 지정.
mc4 = Mclust(USArrests, 4)  
summary(mc4)
plot(mc4)



#--------------------------------------------------------------
# Practice 4
#--------------------------------------------------------------
# Jet

# 22개 미국 전투기
# -	FFD: 처음 비행 날짜
# -	CAR: 비행기가 항공모함에 착륙 가능여부
# -	SPR: 단위무게 당 출력에 비례하는 특정한 출력
# -	RGF: 비행범위 요인
# -	PLF: 비행기의 총 무게의 일부분으로서의 탑재량
# -	SLF: 일관된 무게 요인

#--------------------------------------------------------------
# 1. 계층적군집분석

# A. FFD와 CAR를 제외한 변수를 표준화 한 후 계층적 군집화를 시행하고 덴드로그램을 그리시오.

jet = read.csv("data/jet.csv", header = T)

rownames(jet) = jet$X
jet = jet[, -c(1,2,7)]   # FFD, CAR 제외.
jet

jet_s = scale(jet)
distJet = dist(jet_s)

par(mfrow=c(2,2))
hc1 = hclust(distJet, method = "single")
plot(hc1, main = "Single")
hc2 = hclust(distJet, method = "complete")
plot(hc2, main = "Complete")
hc3 = hclust(distJet, method = "average")
plot(hc3, main = "Average")
hc4 = hclust(distJet, method = "ward.D")
plot(hc4, main = "Ward.D")
par(mfcol=c(1,1))


# B. A의 결과를 사용해 두 개의 집단으로 관측치를 분류하고 각 집단의 특징을 원변수 관점에서 비교하시오.

# 2개의 군집으로 나누기 위해서는 complete linkage 또는 ward method를 선택하는 것이 좋다.

# (1) complete linkage 사용하는 경우.
result_hc = cutree(hc2, k=2)

par(mfrow=c(2,3))
plot(jet$SPR, jet$RGF, col=result_hc, pch=result_hc, xlab = "SPR", ylab = "RGF")
text(jet$SPR, jet$RGF, labels = rownames(jet), col=result_hc, pos = 1)
abline(v = 3.75)

plot(jet$SPR, jet$PLF, col=result_hc, pch=result_hc, xlab = "SPR", ylab = "PLF")
text(jet$SPR, jet$PLF, labels = rownames(jet), col=result_hc, pos = 1)
abline(v = 3.75)

plot(jet$SPR, jet$SLF, col=result_hc, pch=result_hc, xlab = "SPR", ylab = "SLF")
text(jet$SPR, jet$SLF, labels = rownames(jet), col=result_hc, pos = 1)
abline(v = 3.75)

plot(jet$RGF, jet$PLF, col=result_hc, pch=result_hc, xlab = "RGF", ylab = "PLF")
text(jet$RGF, jet$PLF, labels = rownames(jet), col=result_hc, pos = 1)

plot(jet$RGF, jet$SLF, col=result_hc, pch=result_hc, xlab = "RGF", ylab = "SLF")
text(jet$RGF, jet$SLF, labels = rownames(jet), col=result_hc, pos = 1)

plot(jet$PLF, jet$SLF, col=result_hc, pch=result_hc, xlab = "PLF", ylab = "SLF")
text(jet$PLF, jet$SLF, labels = rownames(jet), col=result_hc, pos = 1)
par(mfcol=c(1,1))

# single linkage 방법을 사용하여 두 개의 군집으로 나눌 경우에는 SPR 변수의 영향을 가장 크게 받았음을 알 수 있다.
# SPR 값이 약 3.7 보다 작은 전투기는 cluster 1, 큰 전투기는 cluster 2로 군집이 형성되었다.

table(result_hc)


# (2) ward method 사용하는 경우
result_hc2 = cutree(hc4, k=2)

par(mfrow=c(2,3))
plot(jet$SPR, jet$RGF, col=result_hc2, pch=result_hc2, xlab = "SPR", ylab = "RGF")
text(jet$SPR, jet$RGF, labels = rownames(jet), col=result_hc2, pos = 1)

plot(jet$SPR, jet$PLF, col=result_hc2, pch=result_hc2, xlab = "SPR", ylab = "PLF")
text(jet$SPR, jet$PLF, labels = rownames(jet), col=result_hc2, pos = 1)

plot(jet$RGF, jet$PLF, col=result_hc2, pch=result_hc2, xlab = "RGF", ylab = "PLF")
text(jet$RGF, jet$PLF, labels = rownames(jet), col=result_hc2, pos = 1)

plot(jet$SLF, jet$SPR, col=result_hc2, pch=result_hc2, xlab = "SLF", ylab = "SPR")
text(jet$SLF, jet$SPR, labels = rownames(jet), col=result_hc2, pos = 1)
abline(v = 1.5)

plot(jet$SLF, jet$RGF, col=result_hc2, pch=result_hc2, xlab = "SLF", ylab = "RGF")
text(jet$SLF, jet$RGF, labels = rownames(jet), col=result_hc2, pos = 1)
abline(v = 1.5)

plot(jet$SLF, jet$PLF, col=result_hc2, pch=result_hc2, xlab = "SLF", ylab = "PLF")
text(jet$SLF, jet$PLF, labels = rownames(jet), col=result_hc2, pos = 1)
abline(v = 1.5)
par(mfcol=c(1,1))

# ward method를 사용하여 두 개의 군집으로 나눌 경우에는 SLF 변수의 분포가 가장 큰 영향을 끼쳤음을 알 수 있다.
# SLF 값이 약 1.5 보다 작은 전투기는 cluster 1, 큰 전투기는 cluster 2로 군집이 형성되었다.

table(result_hc2)


# C. 두 집단을 주성분을 이용해 2차원 산점도로 표현하시오. 
# 즉, 제1 주성분과 제2 주성분을 사용한 산점도에서 두 개의 집단을 서로 다른 마크와 색으로 표현하시오.

# 주성분분석
pca <- prcomp(jet, scale = T)
summary(pca)
# 2개의 주성분으로 전체 변동의 78%를 설명할 수 있다.


library(ggfortify)

jet_c = jet
jet_c$cluster1 = factor(result_hc)   # single linkage
jet_c$cluster2 = factor(result_hc2)  # ward method
jet_c

# single linkage
autoplot(pca, data = jet_c, colour = 'cluster1', shape = F, label = T, loadings = T, 
         label.size = 7, loadings.label.size = 5, main = "PCA (single linkage cluster)",
         loadings.label = T, loadings.colour = "blue", loadings.label.colour = "blue")

# ward method
autoplot(pca, data = jet_c, colour = 'cluster2', shape = F, label = T, loadings = T, 
         label.size = 7, loadings.label.size = 5, main = "PCA (ward method cluster)",
         loadings.label = T, loadings.colour = "blue", loadings.label.colour = "blue")



#--------------------------------------------------------------
# 2. 비계층적군집분석

# A. 군집 개수 1~5까지를 사용해 k-means clustering을 시행하고 
# 얻은 within-group sum of squares를 저장하고 그래프로 표현하여 적절한 군집 개수를 판단하시오.

wss = c()
for (k in 1:5) {
    km = kmeans(jet, k)
    wss[k] = sum(km$withinss)
}
wss

plot(1:5, wss, type = 'l')
points(wss)

# 2에서 팔꿈치 생김
# 군집 3~5개 일 경우 서로 wss 차이가 많지 않음
# ---> 2개의 군집으로 결정


# B. K-means clustering을 이용해 2개의 집단으로 군집화하고 그 결과를 1번의 B, C와 같이 탐색하시오.

result_km = kmeans(jet, 2)
result_km

par(mfcol=c(1,2))

plot(jet$SPR, jet$RGF, type = "n", xlab = "SPR", ylab = "RGF", main = "2 Cluster (K-means)")
text(jet$SPR, jet$RGF, labels = rownames(jet), cex = 0.8, col = result_km$cluster)
points(result_km$centers[, c(1,2)], col = "blue", pch = 4, cex = 2)

plot(jet$SLF, jet$SPR, type = "n", xlab = "SLF", ylab = "SPR", main = "2 Cluster (K-means)")
text(jet$SLF, jet$SPR, labels = rownames(jet), cex = 0.8, col = result_km$cluster)
points(result_km$centers[, c(4,1)], col = "blue", pch = 4, cex = 2)

par(mfcol=c(1,1))



# ---------------------------
# 3. 모형기반 군집화를 통해 최적의 군집 개수를 찾고 그 결과를 1번의 B, C와 같이 탐색하시오.

library(mclust)

result_mc = Mclust(jet)
summary(result_mc)
# 최종 선택 모형 : 군집 3개. VVI model

plot(result_mc)

result_mc$classification

rownames(jet[result_mc$classification == 1, ])
rownames(jet[result_mc$classification == 2, ])
rownames(jet[result_mc$classification == 3, ])


jet[order(result_mc$uncertainty, decreasing = T), ][1, ]


result_km3 = kmeans(jet, 3)

plot(jet$SPR, jet$RGF, type = "n", xlab = "SPR", ylab = "RGF", main = "3 Cluster (K-means)")
text(jet$SPR, jet$RGF, labels = rownames(jet), cex = 0.8, col = result_km3$cluster)
points(result_km3$centers[, c(1,2)], col = "blue", pch = 4, cex = 2)





