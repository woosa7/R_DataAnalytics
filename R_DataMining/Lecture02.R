############################################################
#
# Data Mining 2 - Data Mungingwith R
#
############################################################


library(dplyr)

#-----------------------------------------------------------
# 0. Data Setting

cs <- read.table("dataCustomers.tab", sep="\t", header = T, stringsAsFactors = T)
head(cs)
str(cs)

cs$custid <- as.character(cs$custid)

tr <- read.table("dataTransactions.tab", sep="\t", header = T, stringsAsFactors = T)
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

filter(cs, age > 60 & gender == '여')

filter(tr, installment == 10, amount > mean(amount))


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



head(cs)
head(tr)
str(tr)





