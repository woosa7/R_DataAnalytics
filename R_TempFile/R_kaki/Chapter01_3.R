#----------------------------------------------------------------------------
# 구간 (범위) 데이터 분석 - boxplot
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
# 1.

install.packages("ggplot2")
library(ggplot2)

tempratureData <- read.csv("서울의기온변화.csv", header=T)

# ggplot > boxplot
# aes(x축, y축)
ggplot(tempratureData, aes(factor(Month), MeanTemp)) + geom_boxplot()
ggplot(tempratureData, aes(factor(Month), MeanTemp)) + geom_point()


#----------------------------------------------------------------------------

# p.144



