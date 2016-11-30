library(arules)
library(dplyr)

#---------------------------------------------------------
# Import data

data = read.csv("lottoData.csv", header = T)
head(data)
tail(data)

rownames(data) = data$round
data = data[-8]


#---------------------------------------------------------
library(reshape)

mdata = melt(data, id = "round")
head(mdata)

transLotto = as(split(mdata$value, mdata$round), "transactions")
transLotto

inspect(transLotto[1:2])   # transactionID = round

image(transLotto[1:50])
image(sample(transLotto, 100, replace = FALSE), main = "matrix diagram")

itemFrequency(transLotto, type="absolute")
itemFrequency(transLotto, type="relative")
itemFrequency(transLotto)[order(itemFrequency(transLotto), decreasing = TRUE)]

itemFrequencyPlot(transLotto, support=0.14, cex.names=0.8)
itemFrequencyPlot(transLotto, topN = 10, main = "support top 10 items")

rulesL = apriori(transLotto, parameter=list(support=0.001, confidence=0.8))
summary(rulesL)

inspect(sort(rulesL, by = "lift")[1:30])

rulesL2 = apriori(transLotto, parameter=list(support=0.1, target="frequent itemsets"))
inspect(sort(rulesL2, by = "support")[1:30])

as(items(rulesL2), "list")



