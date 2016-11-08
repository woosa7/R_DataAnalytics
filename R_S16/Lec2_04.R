###############################################################
#
# 다변량 통계분석 4 - 군집분석 Cluster Analysis
#
###############################################################

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


# ----------------------------------
# 비계층적 군집분석 : K-means Clustring





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
    cdata = matrix(jet_s[result==i,], ,5)
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




