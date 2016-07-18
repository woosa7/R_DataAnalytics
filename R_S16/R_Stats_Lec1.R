####################################################
##########경영통계 데이터 요약 및 정리##############
####################################################

aggregate(total_sales~rating, data=kmovie, mean)
boxplot(total_sales~rating, data=kmovie, mean, ylab="tips", xlab="rating")

par(las=2,mar=c(10,5,5,5))   #table을 보다 보기 쉽게 정리 (12세 이상 등의 이름이 긴 라벨)
?par
boxplot(total_seen~rating, kmovie)

table(mov$rating)
barplot(table(mov$rating))
pie(table(mov$rating))
lbl2 <- paste(names(table(mov$rating)),",",round(prop.table(table(mov$rating))* 100,2),
              "%",sep="")
pie(table(mov$rating),labels = lbl2)

library(plyr)
msales <- ddply(mov, ~rating, summarize, mean_sales <- mean(total_sales))
barplot(msales[,2], names.arg=msales[,1])
tab <- xtabs(~genre+rating, mov)
tab

mytable3 <- xtabs(~rating+genre, mov)
barplot(mytable3)
barplot(mytable3, legend.text=c("12세이상", "15세이상", "전체", "청소년"), beside=T)
unique(mov$genre)

par(las=2)
mosaicplot(tab)
mosaicplot(t(tab))
