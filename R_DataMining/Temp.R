
library(dplyr)
library(lubridate)
library(C50)
library(caret)

cs <- read.delim("HDS_Customers.tab", stringsAsFactors = F)
custsig <- cs %>% filter(sex != 0) %>% select(custid, sex)
custsig$sex = factor(custsig$sex)

summary(custsig)

tr <- read.delim("HDS_Transactions1.tab", stringsAsFactors = F)
tr2 <- read.delim("HDS_Transactions2.tab", stringsAsFactors = F)
tr <- rbind(tr, tr2)
rm(tr2)


# --------------------------------------------------------------------------------

# 총구매상품수
v1 <- tr %>% filter(net_amt > 0) %>%
    distinct(custid, brd_nm) %>% 
    group_by(custid) %>% summarize(buy_brd = n())

custsig <- custsig %>% left_join(v1)


# 내점일수와 평균구매주기(API, average purchasing interval)
v2 <- tr %>% distinct(custid, sales_date) %>% 
    group_by(custid) %>% summarize(visits = n()) %>% 
    mutate(API = as.integer(365/visits))

custsig <- custsig %>% left_join(v2)


# 3
v3 <- tr %>% 
    group_by(custid, sales_date) %>% summarize(itemCount = n()) %>%
    group_by(custid) %>% summarize(visits = n(), itemCount = sum(itemCount), NPPV = round(itemCount/visits,2)) %>%
    select(custid, itemCount, NPPV)

custsig <- custsig %>% left_join(v3)


# 4 성별 이용코너

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
inTrain <- createDataPartition(y = custsig$sex, p = 0.6, list = F)

trainData = custsig[inTrain, ][, -1]
testData = custsig[-inTrain, ][, -1]

trainData = trainData[, -5]

dim(trainData)
head(trainData)



# --------------------------------------------------------------------------------
library(party)

tree_model <- ctree(sex ~ ., data = trainData)
plot(tree_model)

testData$pred <- predict(tree_model, newdata = testData)
confusionMatrix(testData$pred, testData$sex)


# --------------------------------------------------------------------------------
# options1 <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
# model1 <- C5.0(sex ~ ., data = trainData, control = options1)
# 
# plot(model1)
# summary(model1)
# 
# testData$pred <- predict(model1, testData, type = "class")
# confusionMatrix(testData$pred, testData$sex)
# 

