################################################################
#
# Data Mining : EndTerm Exam - QVC Challenge
#
################################################################

#--------------------------------------------------------------
# Import Data
#--------------------------------------------------------------

product = read.csv("QVC/product.csv", header = T, stringsAsFactors = F)
head(product)
dim(product)

order = read.csv("QVC/order.csv", header = T, stringsAsFactors = F)
head(order)
dim(order)

customer = read.csv("QVC/customer.csv", header = T, stringsAsFactors = F)
head(customer)

airtimes = read.csv("QVC/airtime.csv", header = T, stringsAsFactors = F)
head(airtimes)

airtimes %>% group_by(PRODUCT_ID) %>% summarise(airtime_sum = sum(PRODUCT_AIRTIME_MINS))


#--------------------------------------------------------------------
# Sequence rule analysis
#--------------------------------------------------------------------

library(arulesSequences)
library(dplyr)

head(order)
dim(order)

# sequenceID : CUSTOMER_NBR를 기준으로 한 일련번호
# eventID : ORDER_NBR
# SIZE, items : PRODUCT_ID
makeSeqData <- function(x) {
    seqData = NULL
    sequenceID = 1
    
    for (i in 1:nrow(x)) {
        tx = x[i,]

        if (i>1)
        {
            if (tx$CUSTOMER_NBR != x[i-1,]$CUSTOMER_NBR) {
                sequenceID = sequenceID + 1
            }
        }
        
        basket = order %>% filter(CUSTOMER_NBR == tx$CUSTOMER_NBR, ORDER_NBR == tx$ORDER_NBR) %>%
            distinct(PRODUCT_ID)
        items = paste(basket$PRODUCT_ID, collapse = ",")

        custTr = order %>% filter(CUSTOMER_NBR == tx$CUSTOMER_NBR, ORDER_NBR == tx$ORDER_NBR) %>%
            distinct(CUSTOMER_NBR, ORDER_NBR) %>%
            mutate(sequenceID = sequenceID, eventID = ORDER_NBR, SIZE = nrow(basket), items = items) %>%
            select(sequenceID, eventID, SIZE, items)
        seqData <- rbind(seqData, custTr)
    }
    return(seqData)
}

custOrdList = order %>% distinct(CUSTOMER_NBR, ORDER_NBR) %>% arrange(CUSTOMER_NBR, ORDER_NBR)
head(custOrdList)
NROW(custOrdList)

seqData = makeSeqData(custOrdList)
seqData
dim(seqData)
head(seqData, 30)
tail(seqData, 30)

write.table(seqData,"QVC/seqData.txt", sep="\t", row.names=FALSE)



#--------------------------------------------------------------------

x = read_baskets(con  = "QVC/seqData.txt", sep = "\t", info = c("sequenceID","eventID","SIZE"))
head(as(x, "data.frame"), 30)

seq_rule_1 <- cspade(x, parameter = list(support = 0.001), control= list(verbose = TRUE))
summary(seq_rule_1)


data = sort(seq_rule_1[size(seq_rule_1) == 2], by = "support")[1:10]
as(data, "data.frame")
inspect(data)

rulesL2 = apriori(data, parameter=list(support=0.003, target="frequent itemsets"))


#----------------------------------------
rules.target <- subset(seq_rule_1, size(seq_rule_1,"length")==2)
inspect(sort(rules.target, by="support"))

rules.target1 <- subset(seq_rule_1, size(seq_rule_1,"items")==2)
inspect(sort(rules.target1, by="support"))

rules.target2 <- subset(seq_rule_1, size(seq_rule_1,"itemsets")==2)
inspect(sort(rules.target2, by="support"))

# 카테고리
seq_product = c("33649","33687")
seq_product = c("61109","32565")
seq_product = c("10126","10452")
seq_product = c("32612","61109")
seq_product = c("9887","61109")
product %>% filter(PRODUCT_ID %in% seq_product)

#----------------------------------------
install.packages("arulesViz") 
library(arulesViz)

plot(data, method = "graph", control = list(type="items"))

#----------------------------------------
# product_id / cate 집계

order = read.csv("QVC/order.csv", header = T, stringsAsFactors = F)

# 주문내역에 카테고리 추가
product = read.csv("QVC/product.csv", header = T, stringsAsFactors = F)

category = product %>% distinct(PRODUCT_ID, MERCH_DIV_DESC) %>% 
    mutate(CATE = MERCH_DIV_DESC) %>% 
    select(PRODUCT_ID, CATE)

orderC = order %>% left_join(category, by = "PRODUCT_ID")

brands = product %>% distinct(PRODUCT_ID, BRAND_NAME) %>% 
    select(PRODUCT_ID, BRAND_NAME)

orderC = orderC %>% left_join(brands, by = "PRODUCT_ID")

head(orderC)

# 주문내역에 고객 정보 추가
customer = read.csv("QVC/customer.csv", header = T, stringsAsFactors = F)

orderC = orderC %>% left_join(customer, by = "CUSTOMER_NBR")

split <- strsplit(orderC$ORDER_TIME, split = ":")
orderC$TIME = sapply(split, function(x) { x[1] })


# 요일
library(lubridate)
orderC$WDAY = wday(orderC$ORDER_DATE, label = T)

head(orderC)

# 고객별 브랜드 선호도

brandaff = orderC %>% group_by(CUSTOMER_NBR, BRAND_NAME) %>% summarise(N = n()) %>% 
    filter(N > 10, N == max(N)) %>% 
    arrange(desc(N))

brandaff %>% group_by(BRAND_NAME) %>% summarise(customerCnt = n(), buyCnt = sum(N)) %>% 
    arrange(desc(buyCnt), desc(customerCnt))


# STATE 별

# 1. product
rank1 = orderC %>% 
    group_by(STATE, PRODUCT_ID) %>% summarise(N = n()) %>% 
    filter(!is.na(STATE), N > 10, N == max(N)) %>% 
    arrange(desc(N), STATE)

rank1

# 2. category
rank2 = orderC %>% 
    group_by(STATE, CATE) %>% summarise(N = n()) %>% 
    filter(!is.na(STATE), N > 10, N == max(N)) %>% 
    arrange(desc(N), STATE)

rank2


# 시간대별
# 1. product
rank11 = orderC %>% 
    group_by(TIME, PRODUCT_ID) %>% summarise(N = n()) %>% 
    filter(N > 10, N == max(N)) %>% 
    arrange(desc(N), TIME)

rank11

# 2. category
rank12 = orderC %>% 
    group_by(TIME, CATE) %>% summarise(N = n()) %>% 
    filter(N > 10, N == max(N)) %>% 
    arrange(desc(N), TIME)

rank12

product %>% filter(PRODUCT_ID == 33649)



#  SHOPPER_SEGMENT_CODE 별

# 요일별 베스트 타임
# 1. product
rank31 = orderC %>% 
    group_by(WDAY, PRODUCT_ID) %>% summarise(N = n()) %>% 
    filter(N > 10, N == max(N)) %>% 
    arrange(WDAY, desc(N))

rank31

# 2. category
rank32 = orderC %>% 
    group_by(WDAY, CATE) %>% summarise(N = n()) %>% 
    filter(N > 10, N == max(N)) %>% 
    arrange(WDAY, desc(N))

rank32


#----------------------------------------
# home decor 집중 판매 시간

rank01 = orderC %>% 
    filter(CATE == "Home Decor") %>% 
    group_by(TIME) %>% summarise(N = n()) %>% 
    mutate(TIME = as.numeric(TIME)) %>% 
    arrange(TIME, desc(N))

rank02 = orderC %>% 
    filter(CATE == "Apparel") %>% 
    group_by(TIME) %>% summarise(N = n()) %>% 
    mutate(TIME = as.numeric(TIME)) %>% 
    arrange(TIME, desc(N))

rank03 = orderC %>% 
    filter(CATE == "Health/Beauty") %>% 
    group_by(TIME) %>% summarise(N = n()) %>% 
    mutate(TIME = as.numeric(TIME)) %>% 
    arrange(TIME, desc(N))



plot(rank01, type="o", col="red")
par(new=T)
lines(rank02, type="o", pch=21, col="green", lty=2)
lines(rank03, type="o", pch=22, col="blue", lty=2)

head(product)




#--------------------------------------------------------------------
# 
#--------------------------------------------------------------------

head(customer)
head(orderC)

summary(customer)

customer = read.csv("QVC/customer.csv", header = T, stringsAsFactors = F)

# 구매횟수, 주문횟수, 주문당 구매수
buyCount = orderC %>% group_by(CUSTOMER_NBR) %>% summarise(buyCnt = n())
ordCount = orderC %>% distinct(CUSTOMER_NBR, ORDER_NBR) %>% group_by(CUSTOMER_NBR) %>% summarise(ordCnt = n())
brandCount = orderC %>% distinct(CUSTOMER_NBR, BRAND_NAME) %>% group_by(CUSTOMER_NBR) %>% summarise(brdCnt = n())

# 평균구매주기(API)
start_date = ymd(ymd_hm(min(orderC$ORDER_DATE)))
end_date = ymd(ymd_hm(max(orderC$ORDER_DATE)))
cusApi = orderC %>% distinct(CUSTOMER_NBR, ORDER_DATE) %>% 
    group_by(CUSTOMER_NBR) %>% summarize(visits = n()) %>% 
    mutate(API = as.integer((end_date - start_date)/visits)) %>% 
    select(CUSTOMER_NBR, API)

# 주중/주말 -- no patter : 0 / weekday : 1 / weekend : 2
weekPattern = orderC %>%
    mutate(weekDay = ifelse(wday(ORDER_DATE) %in% 2:6, 1, 0),
           weekEnd = ifelse(wday(ORDER_DATE) %in% c(1,7), 1, 0)) %>%
    group_by(CUSTOMER_NBR) %>% summarize_each(funs(sum), weekDay, weekEnd) %>%
    mutate(weekPattern = ifelse(weekDay >= weekEnd * 1.5, 1,
                                ifelse(weekEnd >= weekDay * 1.5, 2, 0))) %>% 
    select(CUSTOMER_NBR, weekPattern)

# Top Category 구매
topCate = orderC %>% group_by(CATE) %>% summarise(total = n()) %>% 
    arrange(desc(total)) %>% 
    head()

topCategory = topCate$CATE
topCategory

buyTopCate = orderC %>% filter(CATE %in% topCategory) %>% 
    group_by(CUSTOMER_NBR) %>% summarize(buyTopCate = n())

customerC = customer %>% 
    left_join(buyCount, by = "CUSTOMER_NBR") %>% 
    left_join(ordCount, by = "CUSTOMER_NBR") %>%
    left_join(brandCount, by = "CUSTOMER_NBR") %>% 
    left_join(cusApi, by = "CUSTOMER_NBR") %>%
    left_join(weekPattern, by = "CUSTOMER_NBR") %>%
    left_join(buyTopCate, by = "CUSTOMER_NBR") %>% 
    mutate(buyCnt = ifelse(is.na(buyCnt), 0, buyCnt), 
           ordCnt = ifelse(is.na(ordCnt), 0, ordCnt),
           buyPerOrd = ifelse(ordCnt == 0, 0, round(buyCnt / ordCnt, 2)),
           brdCnt = ifelse(is.na(brdCnt), 0, brdCnt),
           buyTopCate = ifelse(is.na(buyTopCate), 0, buyTopCate),
           API = ifelse(is.na(API), 999, API),
           weekPattern = ifelse(is.na(weekPattern), 0, weekPattern))

summary(customerC)


#--------------------------------------------------------------------

custSig = customerC %>% select(CUSTOMER_NBR, buyCnt, ordCnt, buyPerOrd, brdCnt, API, weekPattern, buyTopCate)
head(custSig)

km <- kmeans(custSig[,2:8], centers=5)
km

custSig$cluster <- as.factor(km$cluster)
head(custSig)

library(ggplot2)
qplot(buyCnt, brdCnt, colour=cluster, data=custSig)
qplot(buyCnt, API, colour=cluster, data=custSig)
qplot(buyCnt, buyTopCate, colour=cluster, data=custSig)




