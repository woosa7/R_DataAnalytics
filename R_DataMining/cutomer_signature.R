############################################################
#
# Data Mining 3 - Customer Signature
#
############################################################

library(dplyr)
library(lubridate)
library(ggplot2)

# windows
# tr <- read.delim("HDS_Transactions.tab", stringsAsFactors = F)

# Mac
tr <- read.delim("HDS_Transactions1.tab", stringsAsFactors = F)
tr2 <- read.delim("HDS_Transactions2.tab", stringsAsFactors = F)
tr <- rbind(tr, tr2)
rm(tr2)
head(tr)

cs <- read.delim("HDS_Customers.tab", stringsAsFactors = F)
cs <- select(cs , -birth_flg, -income_flg, -car_stype, -rel_type, -dmnot_flg, -tmnot_flg, 
             -pur_date1, -pur_date2, -pur_date3, -card_flg2, -mail_flg, -billnot_flg);
head(cs)

card <- read.delim("HDS_Cards.tab", stringsAsFactors = F)
job <- read.delim("HDS_Jobs.tab", stringsAsFactors = F)


#-----------------------------------------------------------------
# 주 구매상품

funcBrdPick <- function(x) {
    vPick = NULL
    for (i in x) {
        if (as.integer(i)%%100 == 0)
            print(i)
        
        # 고객의 브랜드별 구매내역 집계 중 가장 구매금액이 큰 브랜드
        brd_pick <- tr %>% filter(custid == i) %>% 
            group_by(custid, brd_nm) %>% summarise(brd_amt = sum(net_amt), brd_qty = n()) %>% 
            arrange(desc(brd_amt)) %>% head(1)
        
        vPick <- rbind(vPick, brd_pick)
    }
    return(vPick)
}

cs.v11 <- funcBrdPick(cs$custid)

head(cs.v11);tail(cs.v11)


#-----------------------------------------------------------------
# 시즌 선호도
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
           winter_ratio = round(winter_buy / net_amt, 2))

head(cs.v12);tail(cs.v12)


#-----------------------------------------------------------------
# 가격 선호도

# 내점당 구매금액 (PAPV, Purchase Amount Per Visit)
# 총구매금액 < 0 인 내역은 제외

# P_group : 내점당 구매금액(PAPV) 및 평균구매주기(API)에 따른 고객 구분
# 1 : 한번 방문당 구매금액이 높고 자주 방문하는 고객
# 2 : 한번 방문당 구매금액이 높지만 자주 오지 않는 고객
# 3 : 한번 방문당 구매금액이 낮지만 자주 방문하는 고객
# 4 : 한번 방문당 구매금액도 낮고 자주 오지 않는 고객

totalAmt <- tr %>% 
    group_by(custid) %>% summarise(PAmount = sum(net_amt)) %>% 
    mutate(PAmount = ifelse(PAmount < 0, 0, PAmount))

cs.v13 <- inner_join(cs.v3, totalAmt) %>% mutate(PAPV = PAmount / visits) %>%
    mutate(P_group = ifelse(PAPV >= mean(PAPV) & API <= mean(API), 1, 99)) %>%
    mutate(P_group = ifelse(PAPV >= mean(PAPV) & API > mean(API), 2, P_group)) %>%
    mutate(P_group = ifelse(PAPV < mean(PAPV) & API <= mean(API), 3, P_group)) %>%
    mutate(P_group = ifelse(PAPV < mean(PAPV) & API > mean(API), 4, P_group)) %>%
    mutate(PAPV = floor(PAmount / visits / 10000) * 10000) %>%                      # 10,000원 단위
    select(custid, PAPV, API)

cs.v13

cs.v13 %>% group_by(P_group) %>% summarise(p = n())


#-----------------------------------------------------------------
# 구매추세 패턴

#-----------------------------------------------------------------
# 상품별 구매금액 및 횟수

#-----------------------------------------------------------------
# 상품별 구매순서


#-----------------------------------------------------------------
# 휴면/이탈 가망 변수











