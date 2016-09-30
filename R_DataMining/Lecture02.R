############################################################
#
# Data Mining 1 - Data Mungingwith R
#
############################################################


library(dplyr)

cs <- read.table("dataCustomers.tab", sep="\t", header = T, stringsAsFactors = T)
tr <- read.table("dataTransactions.tab", sep="\t", header = T, stringsAsFactors = T)

head(cs)
summary(cs)
cs$custid <- as.character(cs$custid)

head(tr)
summary(tr)
tr$custid <- as.character(tr$custid)
tr$product <- as.character(tr$product)

# 1. Selects a subset




filter(cs, age >= 50 & age <=59 ) %>% tail()

head(cs)
head(tr)

summary(tr)
