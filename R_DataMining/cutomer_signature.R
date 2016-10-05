library(dplyr)
library(lubridate)
library(ggplot2)

cs <- read.delim("HDS_Customers.tab", stringsAsFactors = F)
tr <- read.delim("HDS_Transactions.tab", stringsAsFactors = F)

card <- read.delim("HDS_Cards.tab", stringsAsFactors = F)
job <- read.delim("HDS_Jobs.tab", stringsAsFactors = F)

cs <- select(cs , -birth_flg, -income_flg, -car_stype, -rel_type, -dmnot_flg, -tmnot_flg, 
             -pur_date1, -pur_date2, -pur_date3, -card_flg2, -mail_flg, -billnot_flg);

head(cs)
head(tr)

#-----------------------------------------------------------------
# 주 구매상품

funcBrdPick <- function(x) {
    vPick = NULL
    for (i in x) {
        if (as.integer(i)%%100 == 0)
            print(i)
        
        brd_pick <- tr %>% filter(custid == i) %>% 
            group_by(custid, brd_nm) %>% summarise(brd_amt = sum(net_amt), brd_qty = n()) %>% 
            arrange(desc(brd_amt)) %>% head(1)
        
        vPick <- rbind(vPick, brd_pick)
    }
    return(vPick)
}

cs.v11 <- funcBrdPick(cs$custid)


#-----------------------------------------------------------------
# 시즌 선호도



#-----------------------------------------------------------------
# 가격 선호도



#-----------------------------------------------------------------
# 구매추세 패턴

#-----------------------------------------------------------------
# 상품별 구매금액 및 횟수

#-----------------------------------------------------------------
# 상품별 구매순서


#-----------------------------------------------------------------
# 휴면/이탈 가망 변수











