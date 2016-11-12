###############################################################
#
# 다변량 통계분석 4 - 군집분석 Cluster Analysis
#
###############################################################

#--------------------------------------------------------------
# 계층적 군집분석
#--------------------------------------------------------------

df = USArrests
distUS = dist(scale(df))

dist(df, method = "manhattan") # "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"

mahalanobis(df, colMeans(df), cov(df))   # mahalanobis(x, center : 각 변수의 평균, cov : 공분산행렬)

# ------------------------------------------
# linkage methods

# single linkage (최단연결법)
x = data.frame(c(1,3,6,12,20))
rownames(x) = c("a","b","c","d","e")
d = dist(x)
hc = hclust(d, method = "single")
plot(hc)
hc$height

# complete linkage (최장연결법)
hc2 = hclust(d, method = "complete")
plot(hc2)

# average linkage (평균연결법)
hc3 = hclust(d, method = "average")
plot(hc3)

# ward method
hc4 = hclust(d, method = "ward.D")
plot(hc4)


# ------------------------------------------
# USArrests
par(mfcol=c(2,2))

hc = hclust(distUS, method = "single")
plot(hc)

hc2 = hclust(distUS, method = "complete")
plot(hc2)

hc3 = hclust(distUS, method = "average")
plot(hc3)

hc4 = hclust(distUS, method = "ward.D")
plot(hc4)

par(mfcol=c(1,1))



# ----------------------------------
# Cluster Plot

h3result = cutree(hc3, k=5) # k : 그룹의 갯수
plot(df$Murder, df$Assault, col=h3result, pch=h3result)
text(df$Murder, df$Assault, labels = rownames(df), col=h3result, pos = 1)

plot(df$Murder, df$UrbanPop, col=h3result, pch=h3result)
text(df$Murder, df$UrbanPop, labels = rownames(df), col=h3result, pos = 1)


df$cluster = h3result

par(mfcol=c(2,2))
# boxplot(df$Murder ~ df$cluster, xlab = "cluster", ylab = "Murder")
# boxplot(df$Rape ~ df$cluster, xlab = "cluster", ylab = "Rape")
# boxplot(df$UrbanPop ~ df$cluster, xlab = "cluster", ylab = "UrbanPop")
# boxplot(df$Assault ~ df$cluster, xlab = "cluster", ylab = "Assault")

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

jet = read.csv("jet.csv", header = T)
jet

rownames(jet) = jet$X
jet = jet[, -c(1,7)]
jet_s = scale(jet)

distJet = dist(jet_s)

par(mfcol=c(2,2))
hc1 = hclust(distJet, method = "single")
plot(hc1)
hc2 = hclust(distJet, method = "complete")
plot(hc2)
hc3 = hclust(distJet, method = "average")
plot(hc3)
hc4 = hclust(distJet, method = "ward.D")
plot(hc4)
par(mfcol=c(1,1))

# complete 선택
result = cutree(hc4, k=2)

# 1
par(mfcol=c(2,3))
for (i in 1:5) {
    boxplot(jet_s[,i] ~ result, main = names(jet_s)[i], ylim = c(-2,3))
}
par(mfcol=c(1,1))

# 2
par(mfcol=c(1,2))
for (i in 1:2) {
    cdata = matrix(jet_s[result==i,],5)
    colnames(cdata) = c("FFD","SPR","RGF","PLF","SLF")
    boxplot(cdata, las = 2, main = paste("Group",i), ylim = c(-2,3))
}
par(mfcol=c(1,1))

# pca
pca <- prcomp(jet, scale = T)
summary(pca)

plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab = "PC2", pch = result, col = result)
text(pca$x[,1], pca$x[,2], labels = names(result), cex = 0.7, pos = 3, col = result)

biplot(pca, col = result)


# ---------------------------
# 

mc = Mclust(jet)
summary(mc)













