#####################################################################
#
# Data Mining 11 & 12 - ARM (Association Rule Mining) 연관규칙탐사
#
#####################################################################

library(arules)
library(dplyr)
library(reshape)

#---------------------------------------------------------
# Import data

data = read.csv("data/lottoData.csv", header = T)
data = data[-8]   # bonus 번호 제외
head(data)
tail(data)

#---------------------------------------------------------
# arules의 transactions format으로 변환
# mdata = melt(data, id = "round")
# head(mdata)
# 
# transLotto = as(split(mdata$value, mdata$round), "transactions")
# transLotto

transLotto = read.transactions("data/lottoData.csv", sep = ",", format = "basket", skip = 1, cols = 1)

inspect(transLotto[1:10])   # transactionID = round

image(transLotto[1:20])
image(sample(transLotto, 100, replace = FALSE), main = "matrix diagram")

itemFrequency(transLotto, type="absolute")
itemFrequency(transLotto, type="relative")
itemFrequency(transLotto)[order(itemFrequency(transLotto), decreasing = TRUE)][1:15]

itemFrequencyPlot(transLotto, support=0.14, cex.names=0.8)
itemFrequencyPlot(transLotto, topN = 15, main = "support top 15 items")

#---------------------------------------------------------
# 연관규칙 도출
rulesL = apriori(transLotto, parameter=list(support=0.003, confidence=0.8))
summary(rulesL)

inspect(sort(rulesL, by = "lift")[1:15])

# frequent itemsets
rulesL2 = apriori(transLotto, parameter=list(support=0.003, target="frequent itemsets"))
summary(rulesL2)

# 출현빈도가 가장 높은 15개의 번호
inspect(sort(rulesL2, by = "support")[1:15])

# 출현빈도 높은 3자리 번호 조합
inspect(sort(rulesL2[size(rulesL2) == 3], by = "support")[1:20])


head(data)

mdata = melt(data, id = "round")
head(mdata)

?apriori

rulesL3 = apriori(transLotto, parameter=list(support=0.003, confidence=0.8, target="rules"))
summary(rulesL3)

inspect(sort(rulesL3, by = "lift")[1:50])

# 16,24,3,19,29,32
# 1,10,41,13,20,28
# 20,22,40,10,19,7
# 12,27,32,21,29,38
# 10,18,31,32,34,27

a1 = c( 3,16,19,24,29,32)
a2 = c( 1,10,13,20,28,41)
a3 = c( 7,10,19,20,22,40)
a4 = c(12,21,29,32,38,45)
a5 = c(10,18,27,31,32,34)

b1 = c( 5,18,21,26,30,44)
b2 = c( 2,16,22,25,29,35)
b3 = c( 5,13,18,25,42,44)
b4 = c( 1, 8,18,25,36,44)
b5 = c( 6,21,23,26,29,42)

c1 = c(30,32,35,36,40,41)
c2 = c( 1,25,29,30,31,39)
c3 = c( 1,11,17,24,27,38)
c4 = c(13,19,31,36,39,43)
c5 = c( 2, 6, 8,11,42,44)

d1 = c(10,26,28,36,39,40)
d2 = c(19,23,24,34,35,37)
d3 = c( 5, 6,10,21,24,28)
d4 = c( 8,10,18,29,36,40)
d5 = c(21,23,29,35,37,41)

y = data[1,2:7]
sum(y == a1)
sum(y == a2)

a1 = data[145,2:7]
a2 = data[722,2:7]

a5 = c( 5,10,13,27,37,41)
c5 = c( 4,10,14,15,18,22)

for(i in 1:nrow(data)) {
    y = data[i,2:7]
    if (sum(y == a1) == 6) cat("a1 : ", i, "\n")
    if (sum(y == a2) == 6) cat("a2 : ", i, "\n")
    if (sum(y == a3) == 6) cat("a3 : ", i, "\n")
    if (sum(y == a4) == 6) cat("a4 : ", i, "\n")
    if (sum(y == a5) == 6) cat("a5 : ", i, "\n")
    
    if (sum(y == b1) == 6) cat("b1 : ", i, "\n")
    if (sum(y == b2) == 6) cat("b2 : ", i, "\n")
    if (sum(y == b3) == 6) cat("b3 : ", i, "\n")
    if (sum(y == b4) == 6) cat("b5 : ", i, "\n")
    if (sum(y == b5) == 6) cat("b5 : ", i, "\n")
    
    if (sum(y == c1) == 6) cat("c1 : ", i, "\n")
    if (sum(y == c2) == 6) cat("c2 : ", i, "\n")
    if (sum(y == c3) == 6) cat("c3 : ", i, "\n")
    if (sum(y == c4) == 6) cat("c4 : ", i, "\n")
    if (sum(y == c5) == 6) cat("c5 : ", i, "\n")
    
    if (sum(y == d1) == 6) cat("d1 : ", i, "\n")
    if (sum(y == d2) == 6) cat("d2 : ", i, "\n")
    if (sum(y == d3) == 6) cat("d3 : ", i, "\n")
    if (sum(y == d4) == 6) cat("d4 : ", i, "\n")
    if (sum(y == d5) == 6) cat("d5 : ", i, "\n")
}



#---------------------------------------------------------
# 시각화

library(arulesViz)

plot(rulesL)
plot(sort(rulesL, by = "lift")[1:20], method = "grouped")
plot(rulesL, method = "graph", control = list(type="items"))

# 출현빈도 높은 3자리 번호 조합
plot(rulesL2)
plot(sort(rulesL2[size(rulesL2) == 3], by = "support")[1:20], method = "graph", control = list(type="items"))








