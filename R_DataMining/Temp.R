library(dplyr)
library(lubridate)
library(C50)
library(caret)
library(party)

cs <- read.delim("HDS_Customers.tab", stringsAsFactors = F)
custsig <- cs %>% filter(sex != 0) %>% select(custid, sex)
custsig$sex = factor(custsig$sex)

summary(custsig)

tr <- read.delim("HDS_Transactions1.tab", stringsAsFactors = F)
tr2 <- read.delim("HDS_Transactions2.tab", stringsAsFactors = F)
tr <- rbind(tr, tr2)
rm(tr2)
rm(cs)


# --------------------------------------------------------------------------------
# 총구매상품수
v1 <- tr %>% filter(net_amt > 0) %>%
    distinct(custid, brd_nm) %>% 
    group_by(custid) %>% summarize(buy_brd = n())

custsig <- custsig %>% left_join(v1)

# --------------------------------------------------------------------------------
# 내점일수와 평균구매주기(API, average purchasing interval)
v2 <- tr %>% distinct(custid, sales_date) %>% 
    group_by(custid) %>% summarize(visits = n()) %>% 
    mutate(API = as.integer(365/visits))

custsig <- custsig %>% left_join(v2)

# --------------------------------------------------------------------------------
# 내점당 구매상품수
v3 <- tr %>% 
    group_by(custid, sales_date) %>% summarize(itemCount = n()) %>%
    group_by(custid) %>% summarize(visits = n(), itemCount = sum(itemCount), NPPV = round(itemCount/visits,2)) %>%
    select(custid, NPPV)

custsig <- custsig %>% left_join(v3)

# --------------------------------------------------------------------------------
# 환불
v4 <- tr %>% 
    filter(net_amt < 0) %>% 
    group_by(custid) %>% summarize(rf_amt = sum(net_amt), rf_cnt = n())

custsig <- left_join(custsig, refund) %>%
    mutate(rf_amt = ifelse(is.na(rf_amt), 0, rf_amt),
           rf_cnt = ifelse(is.na(rf_cnt), 0, rf_cnt))

# --------------------------------------------------------------------------------
# 요일
v5 <- tr %>%
    mutate(wk_amt = ifelse(wday(sales_date) %in% 2:6, net_amt, 0),
           we_amt = ifelse(wday(sales_date) %in% c(1,7), net_amt, 0)) %>%
    group_by(custid) %>% summarize_each(funs(sum), wk_amt, we_amt)
    
custsig <- left_join(custsig, v5)

# --------------------------------------------------------------------------------
# 
end_date <- ymd(ymd_hms(max(tr$sales_date)))

start_date <- ymd('20010501') - months(12)   # 특정 일자 기준으로 12개월 전 날짜
cs.v7.12 <- tr %>%
    filter(sales_date >= start_date & sales_date <= end_date) %>%
    group_by(custid) %>% summarise(amt12 = sum(net_amt), nop12 = n())

start_date <- ymd('20010501') - months(6)
cs.v7.6 <- tr %>%
    filter(sales_date >= start_date & sales_date <= end_date) %>%
    group_by(custid) %>% summarise(amt6 = sum(net_amt), nop6 = n())

start_date <- ymd('20010501') - months(3)
cs.v7.3 <- tr %>%
    filter(sales_date >= start_date & sales_date <= end_date) %>%
    group_by(custid) %>% summarise(amt3 = sum(net_amt), nop3 = n())

v7 <- left_join(cs.v7.12, cs.v7.6) %>% left_join(cs.v7.3) %>% 
    mutate(amt6 = ifelse(is.na(amt6), 0, amt6),
           nop6 = ifelse(is.na(nop6), 0, nop6),
           amt3 = ifelse(is.na(amt3), 0, amt3),
           nop3 = ifelse(is.na(nop3), 0, nop3))

custsig <- left_join(custsig, v7)


# --------------------------------------------------------------------------------
# 이용 코너
funcCornerCheck <- function(x) {
    vPick = NULL
    for (i in x) {
        if (as.integer(i)%%100 == 0)
            print(i)
        
        clist <- tr %>% filter(custid == i) %>% distinct(corner_nm)
        
        pick1 <- clist %>% filter(corner_nm == '핸드백')
        pick2 <- clist %>% filter(corner_nm == '스타킹')
        pick3 <- clist %>% filter(corner_nm == '색조화장품')
        pick4 <- clist %>% filter(corner_nm == '여성구두')
        
        df <- data.frame(custid = i, 
                         pickItem1 = ifelse(nrow(pick1) == 0, 0, 1), pickItem2 = ifelse(nrow(pick2) == 0, 0, 1),
                         pickItem3 = ifelse(nrow(pick3) == 0, 0, 1), pickItem4 = ifelse(nrow(pick4) == 0, 0, 1))
        
        vPick <- rbind(vPick, df)
    }
    return(vPick)
}

###
a <- custsig %>% filter(sex == 1) %>% sample_n(5000) 
b <- custsig %>% filter(sex == 2) %>% sample_n(5000) 

tempcs <- rbind(a, b)
head(tempcs)
dim(tempcs)

tempcs %>% group_by(sex) %>% summarise(cnt = n())

v8 <- funcCornerCheck(tempcs$custid)
tempcs <- left_join(tempcs, v8)
trainData = tempcs[, -1]

head(trainData);tail(trainData)

tree_model <- ctree(sex ~ ., data = trainData)
plot(tree_model)
trainData$pred <- predict(tree_model, newdata = trainData)
confusionMatrix(trainData$pred, trainData$sex)




# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
head(custsig)

inTrain <- createDataPartition(y = custsig$sex, p = 0.6, list = F)

trainData = custsig[inTrain, ][, -1]
testData = custsig[-inTrain, ][, -1]

trainData = as.data.frame(custsig[, -1])
dim(trainData)
head(trainData)
summary(trainData)


# --------------------------------------------------------------------------------
# C50

options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
# options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE, CF = 0.7)
# options <- C5.0Control(winnow = TRUE, noGlobalPruning = FALSE)
# options <- C5.0Control(winnow = FALSE, noGlobalPruning = TRUE)

# model <- C5.0(sex ~ ., data = trainData, control = options)
model <- C5.0(sex ~ ., data = trainData, control = options)
summary(model)
plot(model)

trainData$pred <- predict(model, trainData, type = "class")
confusionMatrix(trainData$pred, trainData$sex)

# --------------------------------------------------------------------------------
# ctree

tree_model <- ctree(sex ~ ., data = trainData)
plot(tree_model)
trainData$pred <- predict(tree_model, newdata = trainData)
confusionMatrix(trainData$pred, trainData$sex)

# --------------------------------------------------------------------------------




# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# 이용코너

vm <- tr %>% filter(sex == 1) %>% filter(brd_nm != '식품') %>%
    group_by(corner_nm) %>% summarise(cnt = n()) %>%
    arrange(desc(cnt)) %>%
    head(30)
vm

vf <- tr %>% filter(sex == 2) %>% filter(brd_nm != '식품') %>%
    group_by(corner_nm) %>% summarise(cnt = n()) %>%
    arrange(desc(cnt)) %>%
    head(30)
vf

funcCornerCheck <- function(x) {
    vPick = NULL
    for (i in x) {
        if (as.integer(i)%%100 == 0)
            print(i)
        
        pick <- tr %>% filter(custid == i) %>% distinct(corner_nm) %>% 
            filter(corner_nm == '핸드백' | corner_nm == '스타킹' | corner_nm == '색조화장품' | corner_nm == '여성구두')
        
        df <- data.frame(custid = i, fmItem = ifelse(nrow(pick) == 0, 0, 1))
        
        vPick <- rbind(vPick, df)
    }
    return(vPick)
}

v5 <- funcCornerCheck(cs$custid)

custsig <- custsig %>% left_join(v5)

a = tr %>% filter(custid == '4559') %>% distinct(corner_nm) %>% filter(corner_nm == '핸드백' | corner_nm == '스타킹')
nrow(a)

tr %>% filter(corner_nm == '스타킹')

head(custsig)


tr %>% filter(sex == 1) %>% 
    group_by(custid, sales_date, corner_nm) %>% summarise(cnt = n()) %>%
    group_by(custid) %>% summarise(cornerCnt = max(cnt)) %>%
    arrange(desc(cornerCnt))

summary(a)

head(tr)

head(cs)

cs %>% group_by(sex) %>% summarise(cnt = n())

tr %>% distinct(custid, sex) %>% group_by(sex) %>% summarise(cnt = n())


v3 <- tr %>% 
    group_by(custid, sales_date) %>% summarize(itemCount = n()) %>%
    group_by(custid) %>% summarize(visits = n(), itemCount = sum(itemCount), NPPV = round(itemCount/visits,2)) %>%
    select(custid, itemCount, NPPV)

# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------



# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------

