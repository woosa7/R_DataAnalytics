############################################################
#
# Data Mining 13 - Clustering
#
############################################################

# <a href="https://github.com/woosa7/R_DataAnalytics/blob/master/R_DataMining/Lec/2016_2_DM_MBA_13.pdf">이론설명 자료 링크</a>
    
# Required packages
library(cluster)
library(NbClust)
library(kohonen)
library(ggplot2)
library(gridExtra)
library(scales)

# Read Data
cdata <- read.delim("data/Cluster.txt", stringsAsFactors=FALSE)
head(cdata)

##### K-means Clustering with R

# 군집수를 4로 하는 k-means clustering
km <- kmeans(subset(cdata, select=-c(ID)), centers=4)
str(km)
km

# API 변수를 사용하지 않고, 군집수를 3개로 설정.
km3 <- kmeans(subset(cdata, select=-c(ID, API)), centers=3)
km3

# 군집의 반경과 군집간 관계
clusplot(subset(cdata, select=-c(ID)), km$cluster, main="cluster 4 : All variables")
clusplot(subset(cdata, select=-c(ID, API)), km3$cluster, main="cluster 3 : remove API")


# 군집의 분포
cdata$cluster <- as.factor(km$cluster)
qplot(MONEY, VISIT, colour=cluster, data=cdata)
plot(subset(cdata, select=-c(ID,cluster)), col=km$cluster)

cdata$cluster3 <- as.factor(km3$cluster)
qplot(MONEY, VISIT, colour=cluster3, data=cdata)
plot(subset(cdata, select=-c(ID,cluster,cluster3)), col=km3$cluster)

# 군집별 군집화변수의 밀도 : 방법1
p1 <- qplot(MONEY, fill=cluster, alpha=.5, data=cdata, geom="density") + scale_alpha(guide="none")
p2 <- qplot(VISIT, fill=cluster, alpha=.5, data=cdata, geom="density") + theme(legend.position="none")
p3 <- qplot(CROSS, fill=cluster, alpha=.5, data=cdata, geom="density") + theme(legend.position="none")
p4 <- qplot(API, fill=cluster, alpha=.5, data=cdata, geom="density") + theme(legend.position="none")
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)

# 군집별 군집화변수의 밀도 : 방법2
p1 <- ggplot(cdata, aes(MONEY)) + geom_density(fill='deeppink3', adjust=1) + facet_grid(. ~ cluster) + scale_x_continuous(breaks=NULL) + scale_y_continuous("", breaks=NULL)
p2 <- ggplot(cdata, aes(VISIT)) + geom_density(fill='deeppink3', adjust=1) + facet_grid(. ~ cluster) + scale_x_continuous(breaks=NULL) + scale_y_continuous("", breaks=NULL) + theme(strip.text.x=element_blank())
p3 <- ggplot(cdata, aes(CROSS)) + geom_density(fill='deeppink3', adjust=1) + facet_grid(. ~ cluster) + scale_x_continuous(breaks=NULL) + scale_y_continuous("", breaks=NULL) + theme(strip.text.x=element_blank())
p4 <- ggplot(cdata, aes(API)) + geom_density(fill='deeppink3', adjust=1) + facet_grid(. ~ cluster) + scale_x_continuous(breaks=NULL) + scale_y_continuous("", breaks=NULL) + theme(strip.text.x=element_blank())
grid.arrange(p1, p2, p3, p4, ncol=1, nrow=4)

# 군집의 크기
x <- ggplot(cdata, aes(x=factor(1), fill=cluster))
x + geom_bar(width=1) + coord_polar(theta="y")


# 최적의 군집 수 찾기 : 방법1
sd <- cdata[sample(1:nrow(cdata),100),-1]
d <- dist(sd, method = "euclidean")
fit <- hclust(d, method="complete")
plot(fit)
rect.hclust(fit, k=4, border = "red")

# 최적의 군집 수 찾기 : 방법2
wss <- 0; set.seed(1)
for(i in 1:15) wss[i] <- kmeans(subset(cdata, select=-c(ID,cluster,cluster3)), centers=i)$tot.withinss
plot(1:15, wss, type="b", xlab="# of clusters", ylab="Within group sum of squares")

# 최적의 군집 수 찾기: 방법3. 많은 시간 소요됨.
# nc = NbClust(subset(cdata, select=-c(ID,cluster,cluster3)), min.nc=2, max.nc=15, method='kmeans')
# barplot(table(nc$Best.nc[1,]), xlab="# of clusters", ylab="# of criteria", main="Number of clusters chosen by 26 criteria")




########################################## 
# SOM Clustering with R
##########################################

cdata <- read.delim("data/Cluster.txt", stringsAsFactors=FALSE)

# 데이터 정규화
cdata.n <- scale(subset(cdata, select=-c(ID)))

# SOM clustering 을 3 x 3 으로 설정.
set.seed(1)
som_model <- som(data = cdata.n, grid = somgrid(3, 3, "rectangular"))
str(som_model)

# codes : 각 노드의 weight

# SOM 시각화
plot(som_model, main = "feature distribution")
plot(som_model, type="counts", main = "cluster size")
plot(som_model, type="quality", main = "mapping quality")  # 할당된 element 들의 유사도


# 군집별 변수의 영향도
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

for (i in 1:ncol(som_model$data))
  plot(som_model, type="property", property=som_model$codes[,i], main=dimnames(som_model$data)[[2]][i], palette.name=coolBlueHotRed)

# ggplot2 패키지를 이용하여 SPSS Modeler와 유사한 Grid 시각화
cdata$clusterX <- som_model$grid$pts[som_model$unit.classif,"x"]
cdata$clusterY <- som_model$grid$pts[som_model$unit.classif,"y"]
p <- ggplot(cdata, aes(clusterX, clusterY))
p + geom_jitter(position = position_jitter(width=.2, height=.2))




############################################################
#
# Data Mining 13 - Clustering / SOM
#
############################################################

# K-Means(k=6)을 사용하여 SOM neuron(10x10)들 간의 유사성을 시각화
# <a href="https://www.slideshare.net/shanelynn/2014-0117-dublin-r-selforganising-maps-for-customer-segmentation-shane-lynn">참조 슬라이드 링크</a>

library(kohonen)
library(RColorBrewer)
library(fields)

Hexagon <- function (x, y, unitcell = 1, col = col) {
    polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell, 
              x + unitcell/2), c(y + unitcell * 0.125, 
                                 y + unitcell * 0.875, 
                                 y + unitcell * 1.125, 
                                 y + unitcell * 0.875, 
                                 y + unitcell * 0.125, 
                                 y - unitcell * 0.125), 
            col = col, border=NA)
}

cdata <- read.delim("data/Cluster.txt", stringsAsFactors=FALSE)
cdata.n <- scale(subset(cdata, select=-c(ID)))

som_model2 <- som(data = cdata.n, grid = somgrid(10, 10, "rectangular"))
k = 6
somClusters <- kmeans(som_model2$codes, centers = k)
somClusters

# plotting 1
plot(som_model2, main = "feature distribution")

# plotting 2
plot(0, 0, type = "n", axes = FALSE, xlim = c(0, som_model2$grid$xdim), ylim = c(0, som_model2$grid$ydim), xlab = "", ylab = "", asp = 1)
ColRamp <- rev(designer.colors(n=k, col=brewer.pal(k,"Set1")))
ColorCode <- rep("#FFFFFF", length(somClusters$cluster))

for (i in 1:length(somClusters$cluster))
    ColorCode[i] <- ColRamp[somClusters$cluster[i]]

offset <- 0.5
for (row in 1:som_model2$grid$ydim) {
    for (column in 0:(som_model2$grid$xdim-1))
        Hexagon(column + offset, row - 1, col = ColorCode[row + som_model2$grid$ydim * column])
    offset <- ifelse(offset, 0, 0.5)
}


