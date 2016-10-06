############################################################
#
# Data Mining 3 - Customer Signature
#
############################################################

library(dplyr)
library(lubridate)
library(ggplot2)

# windows
tr <- read.delim("HDS_Transactions.tab", stringsAsFactors = F)

# Mac
# tr <- read.delim("HDS_Transactions1.tab", stringsAsFactors = F)
# tr2 <- read.delim("HDS_Transactions2.tab", stringsAsFactors = F)
# tr <- rbind(tr, tr2)
# rm(tr2)
head(tr)

cs <- read.delim("HDS_Customers.tab", stringsAsFactors = F)
cs <- select(cs , -birth_flg, -income_flg, -car_stype, -rel_type, -dmnot_flg, -tmnot_flg, 
             -pur_date1, -pur_date2, -pur_date3, -card_flg2, -mail_flg, -billnot_flg);
head(cs)

card <- read.delim("HDS_Cards.tab", stringsAsFactors = F)
job <- read.delim("HDS_Jobs.tab", stringsAsFactors = F)


#-----------------------------------------------------------------
# 주 구매상품 변수
# 고객의 브랜드별 구매내역 집계 중 구매금액 및 구매횟수 비중이 가장 큰 브랜드

funcBrdPick <- function(x) {
    vPick = NULL
    for (i in x) {
        if (as.integer(i)%%100 == 0)
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


#-----------------------------------------------------------------
# 시즌 선호도 변수
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


#-----------------------------------------------------------------
# 가격 선호도 변수

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
pie(data$Count, main = "P_group", init.angle = 90, col = rainbow(length(counts)), 
    radius = 1, cex = 1.0, labels = f_labels)

# price_group
data2 <- cs.v13 %>% group_by(price_group) %>% summarise(Count = n())
data2
ggplot(data2, aes(x = price_group, y = Count)) + geom_bar(stat = "identity", aes(fill = price_group)) +
    geom_text(aes(y = Count, label = Count), color="black", size=4)

cs.v13 %>% filter(price_group == 10)


#-----------------------------------------------------------------
# 구매추세 패턴 변수
# 휴면/이탈 가망 변수

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

temp1
summary(temp1$api1)

# 하반기 API
start_date <- end_date
end_date = start_date + months(6)

temp2 <- tr %>% 
    filter(sales_date >= start_date & sales_date < end_date) %>% 
    distinct(custid, sales_date) %>% 
    group_by(custid) %>% summarize(visits2 = n()) %>% 
    mutate(api2 = as.integer((end_date - start_date) / visits2)) %>%
    select(custid, api2)

temp2
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

cs.v14 %>% filter(p_trend == "leave")

cs.v14$p_trend <- factor(cs.v14$p_trend, levels = c("loyalty", "up", "down", "keep", "new", "sleep", "leave"))
head(cs.v14)

data <- cs.v14 %>% group_by(p_trend) %>% summarise(Count = n())
data
ggplot(data, aes(x = p_trend, y = Count)) + geom_bar(stat = "identity", aes(fill = p_trend)) +
    geom_text(aes(y = Count, label = Count), color = "black", size = 4)


#-----------------------------------------------------------------
# 상품별 구매순서

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


#-----------------------------------------------------------------
# 상품별 구매금액 및 횟수
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

cs.v16


#-----------------------------------------------------------------
# 



















