############################################################
#
# Data Mining 7 - 고객의 성별을 예측하는 의사결정나무(C5.0)
#
############################################################

# Customer Signature를 이용하여 H백화점 고객의 성별을 예측하는 의사결정나무(C5.0) 분석을 수행
# 임의의 고객에 대한 구매정보(HDS_Transactions_MG.tab)는 알고 있지만
# 그 고객이 누구인지 모른다는 가정하에 성별을 예측
# 고객정보(HDS_Customers.tab) 중에서 성별필드만 예측변수로 사용하고 나머지는 독립변수로 사용하지 말것

library(dplyr)
library(lubridate)

tr <- read.delim("HDS_Transactions.tab", stringsAsFactors = F)
cs <- read.delim("HDS_Customers.tab", stringsAsFactors = F)
cs <- cs %>% filter(sex != 0) %>% select(custid, sex)
cs$sex <- factor(cs$sex)            # 1 male, 2 female
summary(cs)

head(tr, 10)
head(cs)


# 총구매상품수
v1 <- tr %>% filter(net_amt > 0) %>%
    distinct(custid, brd_nm) %>% 
    group_by(custid) %>% summarize(buy_brd = n())

custsig <- cs %>% left_join(v1)
head(custsig)

custsig %>% group_by(sex) %>% summarise(t = n(), s = sum(buy_brd), m = mean(buy_brd))


# 내점일수와 평균구매주기(API, average purchasing interval)

v2 <- tr %>% distinct(custid, sales_date) %>% 
    group_by(custid) %>% summarize(visits = n()) %>% 
    mutate(API = as.integer(365/visits))

custsig <- custsig %>% left_join(v2)


# -------------------------------------------------------
head(custsig)

library(C50)
library(caret)

inTrain <- createDataPartition(y = custsig$sex, p = 0.6, list = F)

trainData = custsig[inTrain, ][, -1]
testData = custsig[-inTrain, ][, -1]
dim(trainData)
dim(testData)

options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
model <- C5.0(sex ~ ., data = trainData, control = options)

plot(model)
summary(model)


