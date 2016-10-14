#-----------------------------------------------------------
# Microsoft: DAT101x Data Science Orientation
#-----------------------------------------------------------

lemonade <- read.csv("Lemonade2016.csv", header = T)
lemonade$Date <- as.Date(lemonade$Date, format = "%Y.%m.%d")
lemonade

summary(lemonade)
str(lemonade)

sum(lemonade$Revenue)

library(ggplot2)

# Revenue by date
ggplot(lemonade) + geom_line(aes(x = Date, y = Revenue))

# Revenue Histogram
ggplot(lemonade, aes(Revenue)) + geom_histogram(fill = "blue", bins = 10)


# Leaflets vs Sales Scatter-Plot Chart
ggplot(lemonade, aes(Sales, Leaflets)) + geom_point()

# standard deviation of the Leaflets
sd(lemonade$Leaflets)


# correlation
library(psych)
pairs.panels(lemonade[, 6:10])


# Two sample T-test : Lemon and Orange sales

# 등분산검정
# p-value > 0.05 : 귀무가설 인정. 분산이 같다.
var.test(lemonade$Lemon, lemonade$Orange)   # p-value = 0.3673

# t-test
# p-value < 0.05 : 귀무가설 기각. 두 그룹의 평균은 유의수준 5% 하에서 차이가 있다.
t.test(lemonade$Lemon, lemonade$Orange, var.equal = T)   # p-value = 1.879e-07


# Regression - Prediction

head(lemonade)

model <- lm(Sales ~ Temperature + Leaflets + Price, data = lemonade)

newData <- data.frame(Temperature = 80, Leaflets = 110, Price = 0.35)
newData2 <- data.frame(Temperature = 80, Leaflets = 120, Price = 0.35)

round(predict(model, newData, interval = "prediction"))
round(predict(model, newData2, interval = "prediction"))


