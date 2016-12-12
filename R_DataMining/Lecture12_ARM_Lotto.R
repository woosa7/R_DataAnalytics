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

inspect(sort(rulesL, by = "lift"))

# frequent itemsets
rulesL2 = apriori(transLotto, parameter=list(support=0.003, target="frequent itemsets"))
summary(rulesL2)

# 출현빈도가 가장 높은 15개의 번호
inspect(sort(rulesL2, by = "support")[1:15])

# 출현빈도 높은 3자리 번호 조합
inspect(sort(rulesL2[size(rulesL2) == 3], by = "support")[1:20])


#---------------------------------------------------------
# 시각화

library(arulesViz)

plot(rulesL)
plot(sort(rulesL, by = "lift")[1:20], method = "grouped")
plot(rulesL, method = "graph", control = list(type="items"))

# 출현빈도 높은 3자리 번호 조합
plot(rulesL2)
plot(sort(rulesL2[size(rulesL2) == 3], by = "support")[1:20], method = "graph", control = list(type="items"))








