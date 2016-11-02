############################################################
#
# Data Mining 3 - Data Munging with R
#
############################################################


library(dplyr)

#-----------------------------------------------------------
# 0. Data Setting

cs <- read.csv("dataCustomers.csv", header = T, stringsAsFactors = T)
head(cs)
str(cs)

cs$custid <- as.character(cs$custid)

tr <- read.csv("dataTransactions.csv", header = T, stringsAsFactors = T)
head(tr)
str(tr)

tr$datetime <- as.character(tr$datetime)
tr$custid <- as.character(tr$custid)
tr$import <- as.factor(tr$import)
tr$installment <- as.factor(tr$installment)
tr$product <- as.character(format(tr$product, digits = 13))

summary(tr)


#-----------------------------------------------------------
# 1. Selects a subset

filter(cs, age > 60 & gender == "여")

filter(tr, installment == 10, amount > mean(amount))

x1 <- select(tr, custid:amount)         # range fields
head(x1)

x2 <- select(tr, -datetime, -import)    # remove fields
head(x2)

x3 <- select(cs, custid, job, gender:residence)     # reorder fields
head(x3)

x4 <- rename(cs, juso = residence, upjong = job)    # rename fields
head(x4)


#-----------------------------------------------------------
# 2. Select random sample

a <- sample_n(cs, 30)       # fixed number
nrow(a)

a <- sample_frac(cs, 0.1)   # fixed fraction
nrow(a)

a <- cs[as.logical((1:nrow(cs)%%20 == 1)), ]   # 1-in-n sampling
head(a, 10)


#-----------------------------------------------------------
# 3. Summarize information

# R_KMU / end_term.R 참조

cs %>% group_by(gender) %>% summarise(Total = n())

cs %>% group_by(gender, marriage) %>% summarise(Count = n())

tr %>% group_by(store) %>% 
    summarise(Total = sum(amount), Mean = mean(amount))

tr %>% group_by(store) %>% 
    summarise_each(funs(sum, min, max), amount)


#-----------------------------------------------------------
# 4. Reorder records

tr %>% group_by(custid, store) %>% 
    summarise(Total = sum(amount), Count = n()) %>%
    arrange(desc(Total), desc(Count))

cs %>% group_by(residence) %>% 
    summarise(Count = n()) %>%
    arrange(desc(Count))


#-----------------------------------------------------------
# 5. Distinct values in specified fields

filter(cs, !duplicated(custid))

distinct(cs, custid)


#-----------------------------------------------------------
# 6. Derive new fields based on existing fields

score <- read.csv("score.csv", header = T)
score

score <- mutate(score, ExamSum = exam1 + exam2, ExamMean = ExamSum / 2)
score


#-----------------------------------------------------------
# 7. Replace with new values

score$extra <- c(1, 1, NA, NA, 2)
score

score$extra[is.na(score$extra)] <- 0
score

#-----------------------------------------------------------
# 8. Merge records

score2 <- read.csv("score2.csv", header = T)
score2

merge(score, score2, by.x = "ID", by.y = "CID")


#-----------------------------------------------------------
# 9. Melt & Cast

library(reshape)

id <- c(1, 1, 1, 1, 2, 2, 2)
site <- c("a", "b", "c", "a", "a", "b", "b")
pageview <- c(1, 2, 3, 4, 5, 6, 7)
dwelltime <- c(7, 6, 5, 4, 3, 2, 1)

mydata <- data.frame(id, site, pageview, dwelltime)
mydata

tx <- melt(mydata, id = c("id", "site"))
tx

cast(tx, id ~ site, sum)
cast(tx, id ~ site, sum, subset = variable == "pageview")

cast(tx, id + site ~ variable, sum)
cast(tx, id ~ variable, mean, subset = variable == "pageview")
cast(tx, id ~ variable, range)


#-----------------------------------------------------------
# 10. Binning : Converts numeric fields into discrete pieces

cust <- tr %>% group_by(custid) %>% summarise(visits = n())
head(cust)
summary(cust)

cust$level <- cut(cust$visits, breaks = 10, labels = F)
head(cust);tail(cust)

cust$level <- cut(cust$visits, c(0, 50, 100, 150, 200, 300), labels = F)
head(cust);tail(cust)


#-----------------------------------------------------------
# Practice

# 지점별 수입/국산품 판매건수
tr$importGubun <- ifelse(tr$import == 1, "수입", "국산")
table(tr$store, tr$importGubun)

# 남녀별 건당 구매액 최소, 중앙, 최대값
mg <- merge(cs, tr)   # tr 수만큼 custid로 merge
mg %>% group_by(gender) %>% summarise_each(funs(min, median, max), amount)

# 총구매액 상위 10명

tr %>% group_by(custid) %>% summarise(Total = sum(amount)) %>%
    arrange(desc(Total)) %>%
    head(10)



#-----------------------------------------------------------
library(ggplot2)

head(diamonds)

# 1
aggregate(price ~ cut, diamonds, mean)

diamonds %>% group_by(cut) %>% summarise(price = mean(price))


# 2
aggregate(price ~ cut + color, diamonds, mean)

diamonds %>% group_by(cut, color) %>% summarise(price = mean(price))


# 3
aggregate(cbind(price, carat) ~ cut, diamonds, mean)

diamonds %>% group_by(cut) %>% summarise_each(funs(mean), price, carat)















