############################################################
#
# Data Mining 3 - Customer Signature
#
############################################################

library(dplyr)
library(lubridate)
library(ggplot2)

tr <- read.delim("HDS_Transactions.tab", stringsAsFactors = F)

cs <- read.delim("HDS_Customers.tab", stringsAsFactors = F)
cs <- select(cs , -birth_flg, -income_flg, -car_stype, -rel_type, -dmnot_flg, -tmnot_flg, 
             -pur_date1, -pur_date2, -pur_date3, -card_flg2, -mail_flg, -billnot_flg);

save(cs, file = "cs.RData")


#-------------------------------------------------------------------
# 1. 고객의 환불형태(금액, 건수) : 환불 = net_amt <0

cs.v1 <- cs %>% select(custid)

refund <- tr %>% 
    filter(net_amt < 0) %>% 
    group_by(custid) %>% summarize(rf_amt = sum(net_amt), rf_cnt = n())

cs.v1 <- left_join(cs.v1, refund) %>%
    mutate(rf_amt = ifelse(is.na(rf_amt), 0, rf_amt),
           rf_cnt = ifelse(is.na(rf_cnt), 0, rf_cnt))

cs.v1[order(cs.v1$rf_amt), ]
cs.v1[order(cs.v1$rf_cnt, decreasing = T), ]

save(cs.v1, file = "cs_v1.RData")


#-------------------------------------------------------------------
# 2. 고객의 구매상품 다양성

cs.v2 <- tr %>% distinct(custid, brd_nm) %>% 
    group_by(custid) %>% summarize(buy_brd = n())

cs.v2[order(cs.v2$buy_brd, decreasing = T), ]

save(cs.v2, file = "cs_v2.RData")


#-------------------------------------------------------------------
# 3. 고객의 내점일수와 평균구매주기(API, average purchasing interval)

start_date = ymd(ymd_hms(min(tr$sales_date)))   # 2000-05-03 00:00:00
end_date = ymd(ymd_hms(max(tr$sales_date)))

cs.v3 <- tr %>% distinct(custid, sales_date) %>% 
    group_by(custid) %>% summarize(visits = n()) %>% 
    mutate(API = as.integer((end_date - start_date)/visits))

max(cs.v3$visits)
filter(cs.v3, visits >= 100, API < 10 )

save(cs.v3, file = "cs_v3.RData")


#-------------------------------------------------------------------
# 4. 내점당구매건수(Number of Purchases Per Visit)

tmp <- tr %>% group_by(custid) %>% summarise(PCount = n())
tmp

cs.v4 <- inner_join(cs.v3, tmp) %>%
    mutate(NPPV = round(PCount / visits, 2)) %>%
    select(custid, NPPV)

cs.v4

save(cs.v4, file = "cs_v4.RData")


#-------------------------------------------------------------------
# 5. 고객의 주중/주말 구매패턴
# wday : 1 = sunday

cs.v5 <- tr %>%
    mutate(wk_amt = ifelse(wday(sales_date) %in% 2:6, net_amt, 0),
           we_amt = ifelse(wday(sales_date) %in% c(1,7), net_amt, 0)) %>%
    group_by(custid) %>% summarize_each(funs(sum), wk_amt, we_amt) %>%
    mutate(wk_pat = ifelse(wk_amt >= we_amt * 1.5, "주중형",
                           ifelse(we_amt >= wk_amt * 1.5, "주말형", "유형없음")))

cs.v5

ggplot(cs.v5, aes(wk_pat)) + geom_bar(aes(fill = wk_pat))

save(cs.v5, file = "cs_v5.RData")


#-------------------------------------------------------------------
# 6. 고객의 생일로부터 특정시점의 나이와 연령대를 계산
# age == NA 이면 평균 연령으로

cs.v6 <- cs[c("custid", "birth")] %>%
    mutate(age = year('2001-05-01') - year(ymd_hms(birth))) %>%
    mutate(age = ifelse(age < 10 | age > 100, NA, age)) %>%
    mutate(age = ifelse(is.na(age), round(mean(age, na.rm = T)), age)) %>%
    mutate(agegrp = cut(age, c(0, 19, 29, 39, 49, 59, 69, 100), labels = F) * 10) %>%
    select(custid, age, agegrp)

ggplot(cs.v6, aes(agegrp)) + geom_bar(aes(fill = agegrp))

cs.v6 %>% group_by(agegrp) %>% summarise(n = n())

save(cs.v6, file = "cs_v6.RData")


#-------------------------------------------------------------------
# 7. 기간별(최근 12개월, 6개월, 3개월) 구매금액 및 구매횟수

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

cs.v7 <- left_join(cs.v7.12, cs.v7.6) %>% left_join(cs.v7.3) %>% 
    mutate(amt6 = ifelse(is.na(amt6), 0, amt6),
           nop6 = ifelse(is.na(nop6), 0, nop6),
           amt3 = ifelse(is.na(amt3), 0, amt3),
           nop3 = ifelse(is.na(nop3), 0, nop3))

cs.v7

save(cs.v7, file = "cs_v7.RData")


#-----------------------------------------------------------------
# 11. 주 구매상품 변수

# 고객의 브랜드별 구매내역 집계 중 구매금액 및 구매횟수 비중이 가장 큰 브랜드
# 브랜드점수(brdScore) = 구매금액점수(amtScore) + 구매횟수점수(qtyScore)

funcBrdPick <- function(x) {
    vPick = NULL
    for (i in x) {
        if (as.integer(i)%%100 == 0)   # 진행상황 체크
            print(i)
        
        brd_pick <- tr %>% filter(custid == i) %>% 
            group_by(custid, brd_nm) %>% summarise(brd_amt = sum(net_amt), brd_qty = n()) %>% 
            mutate(amtScore = as.numeric(round(scale(brd_amt), 2)),
                   qtyScore = as.numeric(round(scale(brd_qty), 2)),
                   qtyScore = ifelse(is.na(qtyScore), 0, qtyScore),
                   brdScore = amtScore + qtyScore) %>% 
            arrange(desc(brdScore)) %>%
            head(1) %>%
            select(custid, brd_nm, brdScore)
        
        vPick <- rbind(vPick, brd_pick)
    }
    return(vPick)
}

cs.v11 <- funcBrdPick(cs$custid)

head(cs.v11);tail(cs.v11)

save(cs.v11, file = "cs_v11.RData")


#-----------------------------------------------------------------
# 12. 시즌 선호도 변수

# 계절별 구매금액 집계와 총구매액에 대한 비율

cs.v12 <- tr %>% 
    mutate(spring_buy = ifelse(month(sales_date) %in% 3:5, net_amt, 0), 
           summer_buy = ifelse(month(sales_date) %in% 6:8, net_amt, 0), 
           fall_buy = ifelse(month(sales_date) %in% 9:11, net_amt, 0), 
           winter_buy = ifelse(month(sales_date) %in% c(12,1,2), net_amt, 0)) %>%
    group_by(custid) %>% summarize_each(funs(sum), spring_buy, summer_buy, fall_buy, winter_buy, net_amt) %>%
    mutate(spring_ratio = round(spring_buy / net_amt, 2),
           summer_ratio = round(summer_buy / net_amt, 2),
           fall_ratio = round(fall_buy / net_amt, 2),
           winter_ratio = round(winter_buy / net_amt, 2)) %>%
    select(-net_amt)

head(cs.v12);tail(cs.v12)

save(cs.v12, file = "cs_v12.RData")


#-----------------------------------------------------------------
# 13. 가격 선호도 변수

# PAPV : 내점당 구매금액 (Purchase Amount Per Visit). 총구매금액 < 0 인 내역은 제외.

# P_group : 내점당 구매금액(PAPV) 및 평균구매주기(API)에 따른 고객 구분
# 1 : 한번 방문당 구매금액이 높고 자주 방문하는 고객
# 2 : 한번 방문당 구매금액이 높지만 자주 오지 않는 고객
# 3 : 한번 방문당 구매금액이 낮지만 자주 방문하는 고객
# 4 : 한번 방문당 구매금액도 낮고 자주 오지 않는 고객

# price_group : 선호 가격대 그룹 (1만원대, 10만원대, .....)

totalAmt <- tr %>% 
    group_by(custid) %>% summarise(PAmount = sum(net_amt)) %>% 
    mutate(PAmount = ifelse(PAmount < 0, 0, PAmount))

cutRange <- c(-1, 100000, 200000, 300000, 400000, 500000, 1000000, 10000000)
cutLabel <- c(1, 10, 20, 30, 40, 50, 100)

cs.v13 <- inner_join(cs.v3, totalAmt) %>% 
    mutate(PAPV = PAmount / visits) %>%
    mutate(P_group = ifelse(PAPV >= mean(PAPV) & API <= mean(API), 1, 99)) %>%
    mutate(P_group = ifelse(PAPV >= mean(PAPV) & API > mean(API), 2, P_group)) %>%
    mutate(P_group = ifelse(PAPV < mean(PAPV) & API <= mean(API), 3, P_group)) %>%
    mutate(P_group = ifelse(PAPV < mean(PAPV) & API > mean(API), 4, P_group)) %>%
    mutate(PAPV = floor(PAmount / visits / 10000) * 10000) %>%                      # 10,000원 단위
    mutate(price_group = cut(PAPV, cutRange, labels = cutLabel)) %>%
    select(custid, PAPV, P_group, price_group)

cs.v13

# P_group
data <- cs.v13 %>% group_by(P_group) %>% summarise(Count = n())
f_ratio <- round(data$Count / sum(data$Count) * 100, 2)
f_labels <- paste(data$P_group, "\n", f_ratio, "%")
pie(data$Count, main = "P_group", init.angle = 90, col = rainbow(length(data$Count)), 
    radius = 1, cex = 1.0, labels = f_labels)

# price_group
data2 <- cs.v13 %>% group_by(price_group) %>% summarise(Count = n())
data2
ggplot(data2, aes(x = price_group, y = Count)) + geom_bar(stat = "identity", aes(fill = price_group)) +
    geom_text(aes(y = Count, label = Count), color="black", size=4)

save(cs.v13, file = "cs_v13.RData")


#-----------------------------------------------------------------
# 14. 구매추세 패턴 변수 & 휴면/이탈 가망 변수

# 평균구매주기(API) : 작을수록 자주 방문 및 구매
# 상반기/하반기 6개월간의 평균구매주기(API)를 비교하여 
# 상승, 하강, 유지, 신규, 휴면, 이탈, 충성고객으로 구분

# 상반기 API / 1.5 > 하반기 API : 상승 (하반기 더 자주 방문)
# 하반기 API / 1.5 > 상반기 API : 하강 (상반기 더 자주 방문)
# 상반기 API = 999 (NA) : 신규 (상반기 기록 없음, 하반기 구매)
# 하반기 API = 999 (NA) : 휴면 (상반기 방문/구매 했으나, 하반기 기록 없음)
# 하반기 API = 999 (NA) & 상반기 API > 60 : 이탈 (상반기 구매주기 60일 이상, 하반기 방문/구매 없음)
# 상반기 API & 하반기 API < 10 : 충성고객 (상반기, 하반기 모두 10일 이내 재방문)
# 나머지 : 유지

cs.v14 <- cs %>% select(custid)

# 상반기 API
start_date <- ymd(ymd_hms(min(tr$sales_date)))
end_date = start_date + months(6)

temp1 <- tr %>% 
    filter(sales_date >= start_date & sales_date < end_date) %>% 
    distinct(custid, sales_date) %>% 
    group_by(custid) %>% summarize(visits1 = n()) %>% 
    mutate(api1 = as.integer((end_date - start_date) / visits1)) %>%
    select(custid, api1)

# 하반기 API
start_date <- end_date
end_date = start_date + months(6)

temp2 <- tr %>% 
    filter(sales_date >= start_date & sales_date < end_date) %>% 
    distinct(custid, sales_date) %>% 
    group_by(custid) %>% summarize(visits2 = n()) %>% 
    mutate(api2 = as.integer((end_date - start_date) / visits2)) %>%
    select(custid, api2)

summary(temp1$api1)
summary(temp2$api2)

cs.v14 <- left_join(cs.v14, temp1) %>% left_join(temp2) %>%
    mutate(api1 = ifelse(is.na(api1), 999, api1),
           api2 = ifelse(is.na(api2), 999, api2)) %>%
    mutate(p_trend = ifelse((api1 / 1.5) > api2, "up", "keep"),   
           p_trend = ifelse((api2 / 1.5) > api1, "down", p_trend),
           p_trend = ifelse(api1 == 999, "new", p_trend),
           p_trend = ifelse(api2 == 999, "sleep", p_trend),
           p_trend = ifelse(api2 == 999 & api1 > 60, "leave", p_trend),
           p_trend = ifelse(api1 < 10 & api2 < 10, "loyalty", p_trend)) %>%
    select(custid, p_trend, api1, api2)

cs.v14$p_trend <- factor(cs.v14$p_trend, levels = c("loyalty", "up", "down", "keep", "new", "sleep", "leave"))

data <- cs.v14 %>% group_by(p_trend) %>% summarise(Count = n())
data
ggplot(data, aes(x = p_trend, y = Count)) + geom_bar(stat = "identity", aes(fill = p_trend)) +
    geom_text(aes(y = Count, label = Count), color = "black", size = 4)

save(cs.v14, file = "cs_v14.RData")


#-----------------------------------------------------------------
# 15. 상품별 구매순서 변수

funcBrdTimeline <- function(x) {
    vPick = NULL
    for (i in x) {
        if (as.integer(i)%%100 == 0)
            print(i)
        
        a <- tr %>% filter(custid == i) %>% distinct(brd_nm)
        brd_pick <- paste(a$brd_nm, collapse = "/")
        vPick <- c(vPick, brd_pick)
    }
    return(vPick)
}

cs.v15 <- cs %>% select(custid)

cs.v15$brd_timeline <- funcBrdTimeline(cs$custid)

head(cs.v15)

save(cs.v15, file = "cs_v15.RData")


#-----------------------------------------------------------------
# 16. 상품별 구매금액 및 횟수 변수

# 구매금액과 구매횟수의 비중이 가장 큰 2개 브랜드
# 브랜드점수(brdScore) = 구매금액점수(amtScore) + 구매횟수점수(qtyScore)

funcLikeBrand <- function(x) {
    vPick = NULL
    for (i in x) {
        if (as.integer(i)%%100 == 0)
            print(i)
        
        pick <- tr %>% filter(custid == i) %>% 
            group_by(custid, brd_nm) %>% summarise(brd_amt = sum(net_amt), brd_qty = n()) %>% 
            mutate(amtScore = as.numeric(round(scale(brd_amt), 2)),
                   qtyScore = as.numeric(round(scale(brd_qty), 2)),
                   qtyScore = ifelse(is.na(qtyScore), 0, qtyScore),
                   brdScore = amtScore + qtyScore) %>% 
            arrange(desc(brdScore)) %>%
            head(2)
        
        df <- data.frame(custid = i, 
                         brd_nm1 = pick[1,]$brd_nm, brd_amt1 = pick[1,]$brd_amt, brd_qty1 = pick[1,]$brd_qty, 
                         brd_nm2 = pick[2,]$brd_nm, brd_amt2 = pick[2,]$brd_amt, brd_qty2 = pick[2,]$brd_qty)
        
        vPick <- rbind(vPick, df)
    }
    return(vPick)
}

cs.v16 <- funcLikeBrand(cs$custid)

head(cs.v16)

save(cs.v16, file = "cs_v16.RData")


#-----------------------------------------------------------------
# 17. 고객별 할부 구매횟수, 평균 할부기간 및 할부구매비율 변수

# buyCount : 총구매횟수
# instCnt : 할부구매 횟수
# instRatio : 전체 구매건에 대한 할부구매 비율
# instMonth :  평균 할부 기간

cs.v17 <- tr %>% group_by(custid) %>% summarise(buyCount = n())

installment <- tr %>% filter(inst_mon > 1) %>% 
    group_by(custid) %>% summarise(instCnt = n(), instMonth = round(mean(inst_mon),1))

cs.v17 <- left_join(cs.v17, installment) %>% 
    mutate(instCnt = ifelse(is.na(instCnt), 0, instCnt),
           instRatio = ifelse(instCnt == 0, 0, round(instCnt / buyCount, 2)),
           instMonth = ifelse(is.na(instMonth), 0, instMonth))

save(cs.v17, file = "cs_v17.RData")


#-----------------------------------------------------------------
# 18. 고객별 이용 매장 및 매장별 이용 코너 내역 변수

# 매장번호 - s1 : 신촌점, s2 : 본점, s3 : 무역점, s4 : 천호점

funcVisitStore <- function(x) {
    vPick = NULL
    store <- tr %>% distinct(str_nm)
    storeV <- store$str_nm           
    
    for (i in x) {
        if (as.integer(i)%%100 == 0)
            print(i)
        
        # 고객별 구매내역
        temp <- tr %>% filter(custid == i)
        
        # 이용 매장 및 갯수
        data1 <- temp %>% distinct(str_nm)
        store_nms <- paste(data1$str_nm, collapse = "/")
        store_cnt <- nrow(data1)
        
        # 매장별 이용 코너
        data2 <- temp %>% distinct(str_nm, corner_nm)
        
        s1_corner <- " "
        s2_corner <- " "
        s3_corner <- " "
        s4_corner <- " "

        for (k in 1:nrow(data2)) {
            sname <- data2[k, ]$str_nm
            cname <- data2[k, ]$corner_nm
            
            if (sname == storeV[1]) {
                s1_corner <- paste(s1_corner, cname, sep = "/")
            }
            if (sname == storeV[2]) {
                s2_corner <- paste(s2_corner, cname, sep = "/")
            }
            if (sname == storeV[3]) {
                s3_corner <- paste(s3_corner, cname, sep = "/")
            }
            if (sname == storeV[4]) {
                s4_corner <- paste(s4_corner, cname, sep = "/")
            }
        }
        
        df <- data.frame(custid = i, store_nms, store_cnt, s1_corner, s2_corner, s3_corner, s4_corner)
        
        vPick <- rbind(vPick, df)
    }
    return(vPick)
}

cs.v18 <- funcVisitStore(cs$custid)

head(cs.v18, 10)

save(cs.v18, file = "cs_v18.RData")


#-----------------------------------------------------------------
# 최종결과 종합

rm(list=ls())

load("cs.RData")

load("cs_v1.RData")
load("cs_v2.RData")
load("cs_v3.RData")
load("cs_v4.RData")
load("cs_v5.RData")
load("cs_v6.RData")
load("cs_v7.RData")

load("cs_v11.RData")
load("cs_v12.RData")
load("cs_v13.RData")
load("cs_v14.RData")
load("cs_v15.RData")
load("cs_v16.RData")
load("cs_v17.RData")
load("cs_v18.RData")

custsig <- cs %>% left_join(cs.v1) %>% left_join(cs.v2) %>% 
    left_join(cs.v3) %>% left_join(cs.v4) %>% left_join(cs.v5) %>% 
    left_join(cs.v6) %>% left_join(cs.v7) %>% 
    left_join(cs.v11) %>% left_join(cs.v12) %>% left_join(cs.v13) %>% 
    left_join(cs.v14) %>% left_join(cs.v15) %>% left_join(cs.v16) %>% 
    left_join(cs.v17) %>% left_join(cs.v18)

head(custsig)

write.csv(custsig, file = "custsig.csv")
