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

airtime = read.csv("QVC/airtime.csv", header = T, stringsAsFactors = F)
head(airtime)


a = select(product, PRODUCT_ID, MERCH_DIV_DESC) %>% filter(nchar(MERCH_DIV_DESC) < 30)
a

nrow(a)

#--------------------------------------------------------------------
# ARM
#--------------------------------------------------------------------

library(arules)
library(arulesViz)
library(dplyr)

trans <- read.transactions("QVC/order.csv", format = "single", sep=",", cols = c(3,4), skip=1)

inspect(trans[1:10])   # transactionID = custid

head(itemFrequency(trans)[order(itemFrequency(trans), decreasing = TRUE)], 20)

itemFrequencyPlot(trans, topN = 20, main = "support top 20 items")

rules <- apriori(trans, parameter=list(support=0.002, confidence=0.8))
summary(rules)

inspect(sort(rules, by = "lift"))

rules_1 = sort(rules[size(rules) == 3], by = "lift")
inspect(rules_1)
str(rules_1)

rules_1@rhs@data@i




plot(rules_1)
plot(sort(rules_1, by = "lift"), method = "grouped")
plot(rules_1, method = "graph", control = list(type="items"))


#--------------------------------------------------------------------
# Sequence rule analysis
#--------------------------------------------------------------------

install.packages("arulesSequences")
library(arulesSequences)
library(dplyr)

order = read.csv("QVC/order.csv", header = T, stringsAsFactors = F)
head(order)
dim(order)

# 주문내역에 카테고리 추가
product = read.csv("QVC/product.csv", header = T, stringsAsFactors = F)
head(product)

product2 = product %>% distinct(PRODUCT_ID, MERCH_DIV_DESC) %>% 
    mutate(CATE = MERCH_DIV_DESC) %>% 
    select(PRODUCT_ID, CATE)
head(product2)

order = order %>% left_join(product2, by = "PRODUCT_ID")


# "sequenceID" : CUSTOMER_NBR를 기준으로 한 일련번호
# "eventID" : ORDER_NBR
# "SIZE", PRODUCT_ID = items
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

head(order)
temp = order %>% distinct(CUSTOMER_NBR, ORDER_NBR) %>% 
    arrange(CUSTOMER_NBR, ORDER_NBR)
head(temp)
dim(temp)

seqData = makeSeqData(temp)
seqData
dim(seqData)
head(seqData, 30)

write.table(seqData,"QVC/seqData.txt", sep="\t", row.names=FALSE)



#--------------------------------------------------------------------

x <- read_baskets(con  = "QVC/seqData4.txt", sep = "\t", 
                  info = c("sequenceID","eventID","SIZE"))
head(as(x, "data.frame"), 30)

seq_rule_1 <- cspade(x, parameter = list(support = 0.001), 
                     control= list(verbose = TRUE))
summary(seq_rule_1)


data = sort(seq_rule_1[size(seq_rule_1) == 2], by = "support")
as(data, "data.frame")


#----------------------------------------
rules.target <- subset(seq_rule_1, size(seq_rule_1,"length")==2)
inspect(sort(rules.target, by="support"))

rules.target1 <- subset(seq_rule_1, size(seq_rule_1,"items")==2)
inspect(sort(rules.target1, by="support"))

rules.target2 <- subset(seq_rule_1, size(seq_rule_1,"itemsets")==2)
inspect(sort(rules.target2, by="support"))

# 카테고리
freq_prod = c("33687", "32565", "10452")
a = product2 %>% filter(PRODUCT_ID %in% freq_prod) %>% 
    distinct(CATE)
a

head(product2)


#----------------------------------------
install.packages("arulesViz") 
library(arulesViz)

plot(rules.target2)
plot(sort(rules, by = "lift")[1:20], method = "grouped")
plot(rules, method = "graph", control = list(type="items"))


install.packages("installr")
library(installr)
updateR()


#----------------------------------------
# product_id / cate 집계

order = read.csv("QVC/order.csv", header = T, stringsAsFactors = F)

# 주문내역에 카테고리 추가
product = read.csv("QVC/product.csv", header = T, stringsAsFactors = F)

product2 = product %>% distinct(PRODUCT_ID, MERCH_DIV_DESC) %>% 
    mutate(CATE = MERCH_DIV_DESC) %>% 
    select(PRODUCT_ID, CATE)

order = order %>% left_join(product2, by = "PRODUCT_ID")

# 주문내역에 고객 정보 추가
customer = read.csv("QVC/customer.csv", header = T, stringsAsFactors = F)

orderC = order %>% left_join(customer, by = "CUSTOMER_NBR")

split <- strsplit(orderC$ORDER_TIME, split = ":")
orderC$TIME = sapply(split, function(x) { x[1] })


# 요일
library(lubridate)
orderC$WDAY = wday(orderC$ORDER_DATE, label = T)

head(orderC)


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






