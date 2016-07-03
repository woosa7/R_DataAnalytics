setwd("c:/R_Study/R_Summer")

install.packages("reshape")
library(reshape)
attach(tips)
summary(tips)

# size를 범주형 자료로 처리하길 원하는 경우.
tips2 <- tips
tips2$size = factor(tips2$size)
summary(tips2)


# median
summary(tips$tip)

tips2$tip[1]=100
summary(tips2$tip)

quantile(tips$tip)
quantile(tips2$tip)


# 표준편차 / 변동계수
tip <- tips$tip

var(tip)
sd(tip)

sd(tip) / mean(tip)


# IQR
IQR(tip)

boxplot(tip, col="red", horizontal = T, xlab="Tip")
hist(tip, 20, probability = T)

qqnorm(tip)
qqline(tip) # 점들이 선위에 가까이 있을수로 정규분포를 따름.


#-----------------------------
# 질적자료의 요약

tips$day <- factor(tips$day, levels = c("Thur","Fri","Sat","Sun"))
mytable <- table(tips$day)

lbl <- paste(names(mytable), ",", round(mytable/sum(mytable)*100),"%", sep="")

barplot(mytable)

pie(mytable, labels = lbl)


# 두 변수의 요약
mytable2 <- xtabs(~sex+day, tips)
mytable2

barplot(mytable2, legend.text = c("Female", "Male"))

barplot(mytable2, legend.text = c("Female", "Male"), beside = T, ylim = c(0,80))

mosaicplot(mytable2)
mosaicplot(t(mytable2))

boxplot(tip~day, data=tips, ylab="tips", xlab="day")


plot(tip~total_bill, tips)



#--------------------------------------------
# exercise


#-------------------
DF <- read.csv("movie_MBA2.csv",stringsAsFactors = F)
head(DF)
View(DF)
str(DF)
boxplot(DF$total_seen)
unique(DF$rating)
agrating <- aggregate(total_seen~rating,data = DF,mean)
agrating
boxplot(total_seen~rating,data=DF, ylab="tips",xlab="day")

tableOfRating <- table(DF$rating)
tableOfRating
barplot(tableOfRating,ylab="CNT",xlab="Rating")
pie(tableOfRating)
lbl2 <- paste(names(tableOfRating),",",round(prop.table(tableOfRating)* 100,2) ,"%",sep="")
pie(tableOfRating,labels = lbl2)



#-----------------------------------------
movie <- read.csv("movie_MBA.csv")
head(movie)

boxplot(total, horizontal = T, ylim=c(0,13000))
hist(total,20)


par(las=2, mar=c(10,5,5,5))
boxplot(total_seen~rating, movie)

library(plyr)
msales <- ddply(movie, ~rating, summarise, mean_sales=mean(total_sales))
barplot(msales[,2], names.arg = msales[,1])
tab=xtabs(~genre+rating, movie)


par(las=2)
mosaicplot(tab)
mosaicplot(t(tab))

