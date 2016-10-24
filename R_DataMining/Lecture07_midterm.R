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
cs <- cs %>% select(custid, sex)
cs$sex <- factor(cs$sex)            # 1 male, 2 female
summary(cs)


head(tr, 30)
head(cs)

# 총구매상품수
v1 <- tr %>% distinct(custid, brd_nm) %>% 
    group_by(custid) %>% summarize(buy_brd = n())

head(v1)



