product = read.csv("QVC/product.csv", header = T, stringsAsFactors = F)
order = read.csv("QVC/order.csv", header = T, stringsAsFactors = F)
customer = read.csv("QVC/customer.csv", header = T, stringsAsFactors = F)

library(arulesSequences)
library(dplyr)

x <- read_baskets(con  = "QVC/seqData.txt", sep = "\t", info = c("sequenceID","eventID","SIZE"))

head(as(x, "data.frame"), 30)

seq_rule_1 <- cspade(x, parameter = list(support = 0.001), control= list(verbose = TRUE))

summary(seq_rule_1)


#----------------------------------------
# 1. next product a customer will buy in next month

rules.target <- subset(seq_rule_1, size(seq_rule_1,"itemsets")==2)

inspect(sort(rules.target, by="support"))

seq <- as(rules.target,"data.frame")
seq <- seq[order(-seq$support),]
str(seq)
head(seq,10)

head(order)
head(product)

pre_prod = c("33649","61109","10126","32612","9887","9887","10126","10811","11916","9887")
next_prod = c("33687", "32565", "10452","61109","61109","32565","11916","61109","32612","10811") 

prod_buy = product %>% filter(PRODUCT_ID %in% pre_prod)
prod_buy

prod_buy_next = product %>% filter(PRODUCT_ID %in% next_prod)
prod_buy_next



# 2. 카테고리


product2 = product %>% distinct(PRODUCT_ID, MERCH_DIV_DESC) %>% 
    mutate(CATE = MERCH_DIV_DESC) %>% 
    select(PRODUCT_ID, CATE)

head(product2)

freq_prod = c("33687", "32565", "10452","61109","61109","32565","11916","61109","32612","10811")

prod_cate = product2 %>% filter(PRODUCT_ID %in% freq_prod) %>% 
    distinct(CATE)

prod_cate


#####################################
# 3. state별/time zone/ segment ->  product_id / cate 집계

# 주문내역에 카테고리 추가

order = order %>% left_join(product2, by = "PRODUCT_ID")

head(order)


# 주문내역에 고객 정보 추가

head(customer)

orderC = order %>% left_join(customer, by = "CUSTOMER_NBR")

head(orderC)


library(lubridate)

orderC$Time = hour(hms(orderC$ORDER_TIME))

# 요일

orderC$WDAY = wday(orderC$ORDER_DATE, label = T)

head(orderC)

# 3.1 STATE 별

# 3.1.1. product

rank311 = orderC %>% 
    group_by(STATE, PRODUCT_ID) %>% summarise(N = n()) %>% 
    arrange(desc(N), STATE) %>% 
    mutate(rank = row_number()) %>% 
    filter(!is.na(STATE), N == max(N), rank == 1)

rank311 <- rank311 %>% left_join(product) %>% select(STATE, PRODUCT_ID, PACKING_SLIP_DESC, N)

rank311 = rank311 %>% mutate(TITLE = paste(STATE, as.character(PRODUCT_ID), sep = "\n"))

library(treemap)

treemap(rank311, index="title", vSize="N", type = "index")


# 3.1.2. category

rank312 = orderC %>% 
    group_by(STATE, CATE) %>% summarise(N = n()) %>% 
    filter(!is.na(STATE), N > 10, N == max(N)) %>% 
    arrange(desc(N), STATE)

rank312


# 3.2 timezone

# timezone

timezone <- read.csv("QVC/Timezone.csv", stringsAsFactors = F)
timezone <- timezone[,-1]
names(timezone) <- c("STATE","TimeZone")
head(timezone)

orderC = orderC %>% left_join(timezone, by = "STATE")
head(orderC)

# 3.2.1. product

rank321 = orderC %>% 
    group_by(TimeZone, PRODUCT_ID) %>% summarise(N = n()) %>% 
    filter(!is.na(TimeZone), N > 10, N == max(N)) %>% 
    arrange(desc(N), TimeZone)

rank321 <- rank321 %>% left_join(product) %>% select(TimeZone, PRODUCT_ID, PACKING_SLIP_DESC, N)
rank321


# 3.2.2. category

rank322 = orderC %>% 
    group_by(TimeZone, CATE) %>% summarise(N = n()) %>% 
    filter(!is.na(TimeZone), N > 10, N == max(N)) %>% 
    arrange(desc(N), TimeZone)

rank322


#3.3  SHOPPER_SEGMENT_CODE 별

# 3.3.1. product

head(orderC)

rank331 = orderC %>% 
    
    group_by(SHOPPER_SEGMENT_CODE, PRODUCT_ID) %>% summarise(N = n()) %>% 
    
    filter(!is.na(SHOPPER_SEGMENT_CODE), N == max(N)) %>% 
    
    arrange(desc(N), SHOPPER_SEGMENT_CODE)



rank331 <- rank331 %>% left_join(product) %>% select(SHOPPER_SEGMENT_CODE, PRODUCT_ID, PACKING_SLIP_DESC, N)
rank331

# 3.3.2. category

rank332 = orderC %>% 
    
    group_by(SHOPPER_SEGMENT_CODE, CATE) %>% summarise(N = n()) %>% 
    
    filter(!is.na(SHOPPER_SEGMENT_CODE), N > 10, N == max(N)) %>% 
    
    arrange(desc(N), SHOPPER_SEGMENT_CODE)


rank332


# 4.  Is there a best time of day to sell a particular product or product category

# 4.1. product

rank41 = orderC %>% 
    
    group_by(Time, PRODUCT_ID) %>% summarise(N = n()) %>% 
    
    filter(N > 10, N == max(N)) %>% 
    
    arrange(desc(N), Time)



rank41 <- rank41 %>% left_join(product) %>% select(Time, PRODUCT_ID, PACKING_SLIP_DESC, N)
rank41


# 4.2. category

rank42 = orderC %>% 
    
    group_by(Time, CATE) %>% summarise(N = n()) %>% 
    
    filter(N > 10, N == max(N)) %>% 
    
    arrange(desc(N), Time)


rank42


# --------------------------------------------------------------------
# brand affinity (personal connection with the brands QVC sells)


product3 = product %>% select(PRODUCT_ID, BRAND_NAME)

orderC = orderC %>% left_join(product3, by = "PRODUCT_ID")

orderC %>% 
    filter(!is.na(BRAND_NAME)) %>% 
    group_by(CUSTOMER_NBR, BRAND_NAME) %>% summarise(N = n()) %>% 
    filter(N == max(N)) %>% 
    arrange(desc(N))





# --------------------------------------------------------------------
# 4.1. Product

detach("package:plyr", unload=TRUE) 

head(orderC)
str(orderC)


freq_item1 = orderC %>% 
    filter(PRODUCT_ID == 11916) %>% 
    group_by(PRODUCT_ID, Time) %>% summarise(Item = n()) %>% 
    mutate(Time = as.numeric(Time)) %>% 
    arrange(Time, desc(Item))
freq_item1

freq_item2 = orderC %>% 
    filter(PRODUCT_ID == 32565) %>% 
    group_by(PRODUCT_ID, Time) %>% summarise(Item = n()) %>% 
    mutate(Time = as.numeric(Time)) %>% 
    arrange(Time, desc(Item))
freq_item2

freq_item3 = orderC %>% 
    filter(PRODUCT_ID == 61109) %>% 
    group_by(PRODUCT_ID, Time) %>% summarise(Item = n()) %>% 
    mutate(Time = as.numeric(Time)) %>% 
    arrange(Time, desc(Item))
freq_item3

freq_item4 = orderC %>% 
    filter(PRODUCT_ID == 10811) %>% 
    group_by(PRODUCT_ID, Time) %>% summarise(Item = n()) %>% 
    mutate(Time = as.numeric(Time)) %>% 
    arrange(Time, desc(Item))
freq_item4

freq_item5 = orderC %>% 
    filter(PRODUCT_ID == 10452) %>% 
    group_by(PRODUCT_ID, Time) %>% summarise(Item = n()) %>% 
    mutate(Time = as.numeric(Time)) %>% 
    arrange(Time, desc(Item))
freq_item5

freq_item = freq_item1 %>% rbind(freq_item2) %>% rbind(freq_item3) %>% rbind(freq_item4) %>% rbind(freq_item5)

freq_item$PRODUCT_ID = factor(freq_item$PRODUCT_ID)

freq_item

ggplot(freq_item, aes(Time, Item, colour = PRODUCT_ID)) + geom_line() + geom_point()






