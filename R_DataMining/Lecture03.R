############################################################
#
# Data Mining 3 - Customer Signature : 파생변수 만들기
#
############################################################

# H 백화점 2000.0501 ~ 2001.04.29 : 고객정보, 구매정보 데이터

library(dplyr)
library(lubridate)
library(ggplot2)

cs <- read.delim("HDS_Customers.tab", stringsAsFactors = F)
tr <- read.delim("HDS_Transactions_MG.tab", stringsAsFactors = F)
card <- read.delim("HDS_Cards.tab", stringsAsFactors = F)
job <- read.delim("HDS_Jobs.tab", stringsAsFactors = F)


#-----------------------------------------------------------
# 데이터 탐색

cs %>% group_by(sex) %>% summarise(Count = n()) 
cs %>% group_by(h_type1) %>% summarise(Count = n()) %>% arrange(desc(Count))

jobtype <- cs %>% group_by(job_stype) %>% summarise(Count = n()) %>% arrange(desc(Count))
jobtype
jobtype %>% left_join(job) %>% 
    group_by(job_nm_gr) %>% summarise(Count = sum(Count)) %>% arrange(desc(Count))

distinct(card, card_sflg_nm)

tr %>% group_by(hour = as.integer(sales_time/100)) %>% summarise(Count = n()) 

tr %>% group_by(str_nm) %>% 
    summarise(Count = n(), Total = sum(as.numeric(net_amt))/1000000) %>% 
    arrange(desc(Total))


#-------------------------------------------------------------------
# 1. 고객의 환불형태(금액, 건수) : 환불 = net_amt <0

cs.v1 <- tr %>% filter(net_amt < 0) %>% 
    group_by(custid) %>% summarize(rf_amt = sum(net_amt), rf_cnt = n())

cs.v1[order(cs.v1$rf_amt), ]
cs.v1[order(cs.v1$rf_cnt, decreasing = T), ]


# 2. 고객의 구매상품 다양성

cs.v2 <- tr %>% distinct(custid, brd_nm) %>% 
    group_by(custid) %>% summarize(buy_brd = n())

cs.v2[order(cs.v2$buy_brd, decreasing = T), ]


# 3. 고객의 내점일수와 평균구매주기(API, average purchasing interval)
# lubridate package : blog.naver.com/dfdf4912/220623488198

start_date = ymd(ymd_hms(min(tr$sales_date)))   # 2000-05-03 00:00:00
end_date = ymd(ymd_hms(max(tr$sales_date)))

cs.v3 <- tr %>% distinct(custid, sales_date) %>% 
    group_by(custid) %>% summarize(visits = n()) %>% 
    mutate(API = as.integer((end_date - start_date)/visits))

max(cs.v3$visits)
filter(cs.v3, visits >= 100, API < 10 )


# 4. 내점당구매건수(Number of Purchases Per Visit)

tmp <- tr %>% group_by(custid) %>% summarise(PCount = n())
tmp

cs.v4 <- inner_join(cs.v3, tmp) %>%
    mutate(NPPV = PCount / visits) %>%
    select(custid, NPPV)

cs.v4


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


# 6. 고객의 생일로부터 특정시점의 나이와 연령대를 계산
# age == NA 이면 평균 연령으로

# case 1.
cs.v6 <- cs[c("custid", "birth")] %>%
    mutate(age = year('2001-05-01') - year(ymd_hms(birth))) %>%
    mutate(age = ifelse(age < 10 | age > 100, NA, age)) %>%
    mutate(age = ifelse(is.na(age), round(mean(age, na.rm = T)), age)) %>%
    mutate(agegrp = cut(age, c(0, 19, 29, 39, 49, 59, 69, 100), labels = F) * 10) %>%
    select(custid, age, agegrp)

head(cs.v6)

# case 2.
cs.v6 <- cs %>%
    mutate(age = year('2001-05-01') - year(ymd_hms(birth)),
           #age = ifelse(age < 10 | age > 100, NA, age),
           #age = ifelse(is.na(age), round(mean(age, na.rm = T)), age),
           agegrp = cut(age, c(0, 19, 29, 39, 49, 59, 69, 100), labels = F)*10) %>%
    select(custid, age, agegrp)

ggplot(cs.v6, aes(agegrp)) + geom_bar(aes(fill = agegrp))


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

