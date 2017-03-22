#####################################################################
#
# Data Mining 11 & 12 - ARM (Association Rule Mining) 연관규칙탐사
#
#####################################################################

# 연관규칙탐사(Association Rule Mining) : 하나의 거래나 사건에 포함되어 있는 항목들의 경향을 파악해서 상호연관성을 발견하는 것
# https://github.com/woosa7/R_DataAnalytics/blob/master/R_DataMining/Lec/2016_2_DM_MBA_11.pdf


#--------------------------------------------------------------------
##### Mine Associations using arules package

library(arules)
library(dplyr)

tr <- read.csv("data/dataTransactions.csv", header = T, stringsAsFactors = T)
head(tr)
summary(tr)

# 고객 관점 : 고객이 1년동안 구매한 것을 하나의 트랜잭션으로 가정함.
# 아주 흔하게 발생하는 카테고리 제외.
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

# 구매 내역이 적으면 지지도를 낮춰야 하므로 전체적인 분포를 파악.
image(trans[1:5])
image(sample(trans, 100, replace = FALSE), main = "matrix diagram")

# 주로 구매가 발생하는 항목
itemFrequency(trans, type="absolute")
itemFrequency(trans, type="relative")
itemFrequency(trans)[order(itemFrequency(trans), decreasing = TRUE)]

itemFrequencyPlot(trans, support=0.2, cex.names=0.8)
itemFrequencyPlot(trans, topN = 20, main = "support top 20 items")


# 연관규칙탐사
rules <- apriori(trans, parameter=list(support=0.2, confidence=0.8))
summary(rules)

inspect(rules)
inspect(sort(rules, by = "lift")[1:30])


# 우측에 특정 카테고리가 있는 것만 탐색할 경우
rules_spo <- apriori(trans, parameter=list(support=0.2, confidence=0.8), appearance=list(rhs="스포츠",default="lhs"))
summary(rules_spo)
inspect(rules_spo)
inspect(sort(rules_spo, by = "lift")[1:10])


# Lift 기준으로 filtering
rules.target <- subset(rules, rhs %in% "스포츠" & lift > 1.4)
inspect(sort(rules.target, by="confidence"))

rule.interest <- subset(rules, items %in% c("장신구", "섬유"))  # items : lhs, rhs 모두 검색
inspect(rule.interest[1:10])

# csv 파일로 저장
# write(rules.target, file="arules.csv", sep=",", row.name=F)

# xml 파일로 저장
# library(pmml)
# write.PMML(rules.target, file = "arules.xml")

rule_df <- as(rules, "data.frame")
head(rule_df)


#--------------------------------------------------------------------
##### Visualize Association Rules using arulesViz package

library(arulesViz)

plot(rules)
plot(sort(rules, by = "lift")[1:20], method = "grouped")
plot(rules, method = "graph", control = list(type="items"))


#--------------------------------------------------------------------
##### Exercise
#--------------------------------------------------------------------

# 여성쇼핑몰 C사 고객 786명의 10가지 구매품목에 대한 거래이력을 기초로 
# 반응률이 높은 교차판매전략을 기획하기 위해 연관규칙탐사 수행.
# 고객id별로 각 품목에 대한 구매여부 0 or 1로 표시

data <- read.delim("data/shoppingmall.txt", stringsAsFactors=FALSE)
head(data)

st <- as.matrix(data[,-1])
trans <- as(st, "transactions")
trans

head(inspect(trans))
transactionInfo(trans)

image(trans[1:10])
image(sample(trans, 100, replace = FALSE), main = "matrix diagram")

itemFrequency(trans, type="absolute")
itemFrequency(trans, type="relative")
itemFrequency(trans)[order(itemFrequency(trans), decreasing = TRUE)]

# 구매 내역이 적으면 지지도를 낮춰야 함.
itemFrequencyPlot(trans, support=0.2, cex.names=0.8)
itemFrequencyPlot(trans, topN = 10, main = "support top 20 items")

rules <- apriori(trans, parameter=list(support=0.07, confidence=0.8))
summary(rules)

inspect(rules)
inspect(sort(rules, by = "lift"))

plot(rules)
plot(sort(rules, by = "lift"), method = "grouped")
plot(rules, method = "graph", control = list(type="items"))

# frequent itemset
rules2 <- apriori(trans, parameter=list(support=0.2, target = "frequent itemsets"))
summary(rules2)
inspect(sort(rules2, by = "support"))
