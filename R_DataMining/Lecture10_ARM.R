##### Mine Associations using arules package

install.packages("arules")
library(arules)
library(dplyr)

tr <- read.delim("dataTransactions.tab", stringsAsFactors=FALSE)
head(tr)

# 고객 관점 : 고객이 1년동안 구매한 것을 하나의 트랜잭션으로 가정함.
tr.filter <- tr %>%
  filter(!(corner %in% c("일반식품","화장품","기타"))) %>%
  distinct(custid, corner)

head(tr.filter)

# arules의 transactions format으로 변환
trans <- as(split(tr.filter$corner, tr.filter$custid), "transactions")
trans

# raw data 바로 사용할 경우
# trans <- read.transactions("dataTransactions.tab", format = "single", sep="\t", cols = c(2,6), skip=1)


inspect(trans[1:2])   # transactionID = custid
transactionInfo(trans[size(trans) > 15])

# 구매 내역이 적으면 지지도를 낮춰야 함.
image(trans[1:5])
image(sample(trans, 100, replace = FALSE), main = "matrix diagram")

itemFrequency(trans, type="absolute")
itemFrequency(trans, type="relative")
itemFrequency(trans)[order(itemFrequency(trans), decreasing = TRUE)]

itemFrequencyPlot(trans, support=0.2, cex.names=0.8)
itemFrequencyPlot(trans, topN = 20, main = "support top 20 items")

rules <- apriori(trans, parameter=list(support=0.2, confidence=0.8))
# rules <- apriori(trans, parameter=list(support=0.2, confidence=0.8), 
#                   appearance=list(rhs="스포츠",default="lhs"))
summary(rules)


inspect(rules)
inspect(sort(rules, by = "lift")[1:30])

rules.target <- subset(rules, rhs %in% "스포츠" & lift > 1.4)
inspect(sort(rules.target, by="confidence"))

rule.interest <- subset(rules, items %in% c("장신구", "섬유"))  # items : lhs, rhs 모두 검색
inspect(rule.interest[1:10])

write(rules.target, file="arules.csv", sep=",", row.name=F)


install.packages("pmml") 
library(pmml)
write.PMML(rules.target, file = "arules.xml")

# rule_df <- as(rules, "data.frame")
# head(rule_df)


#--------------------------------------------------------------------
##### Visualize Association Rules using arulesViz package

install.packages("arulesViz") 
library(arulesViz)

plot(rules)
plot(sort(rules, by = "lift")[1:20], method = "grouped")
plot(rules, method = "graph", control = list(type="items"))


#--------------------------------------------------------------------
##### Exercise
#--------------------------------------------------------------------

data <- read.delim("shoppingmall.txt", stringsAsFactors=FALSE)
head(data)
st <- as.matrix(data[,-1])
trans <- as(st, "transactions")
trans

head(inspect(trans))
transactionInfo(trans)

# 구매 내역이 적으면 지지도를 낮춰야 함.
image(trans[1:5])
image(sample(trans, 100, replace = FALSE), main = "matrix diagram")

itemFrequency(trans, type="absolute")
itemFrequency(trans, type="relative")
itemFrequency(trans)[order(itemFrequency(trans), decreasing = TRUE)]

itemFrequencyPlot(trans, support=0.2, cex.names=0.8)
itemFrequencyPlot(trans, topN = 20, main = "support top 20 items")

rules <- apriori(trans, parameter=list(support=0.05, confidence=0.8))
summary(rules)

inspect(rules)
inspect(sort(rules, by = "lift")[1:30])

plot(rules)
plot(sort(rules, by = "lift"), method = "grouped")
plot(rules, method = "graph", control = list(type="items"))


rules2 <- apriori(trans, parameter=list(support=0.2, target = "frequent itemsets"))
summary(rules2)
inspect(rules2)
