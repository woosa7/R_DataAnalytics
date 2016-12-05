################################################################
#
# Data Mining : EndTerm Exam - QVC Challenge
#
################################################################

#--------------------------------------------------------------
# Import Data
#--------------------------------------------------------------

customer = read.csv("QVC/customer.csv", header = T, stringsAsFactors = F)
product = read.csv("QVC/product.csv", header = T, stringsAsFactors = F)
airtime = read.csv("QVC/airtime.csv", header = T, stringsAsFactors = F)
order = read.csv("QVC/order.csv", header = T, stringsAsFactors = F)

head(customer)
head(product)
head(airtime)
head(order)
summary(customer)

a = select(product, PRODUCT_ID, MERCH_DIV_DESC) %>% filter(nchar(MERCH_DIV_DESC) < 30)
a

nrow(a)

#--------------------------------------------------------------------
# ARM
#--------------------------------------------------------------------

library(arules)
library(dplyr)

trans <- read.transactions("QVC/order.csv", format = "single", sep=",", cols = c(3,4), skip=1)

inspect(trans[1:10])   # transactionID = custid

head(itemFrequency(trans)[order(itemFrequency(trans), decreasing = TRUE)], 20)

itemFrequencyPlot(trans, topN = 20, main = "support top 20 items")

rules <- apriori(trans, parameter=list(support=0.002, confidence=0.8))
summary(rules)

inspect(rules)
inspect(sort(rules, by = "lift"))


# rules <- apriori(trans, parameter=list(support=0.2, confidence=0.8), 
#                   appearance=list(rhs="스포츠",default="lhs"))




#--------------------------------------------------------------------
# Sequence rule analysis
#--------------------------------------------------------------------

# install.packages("arulesSequences")
# library(arulesSequences)
# library(dplyr)
# 
# head(order)
# dim(order)
# 
# # ORDER_DATE = "sequenceID", CUSTOMER_NBR = "eventID", "SIZE", PRODUCT_ID = items
# makeSeqData <- function(x) {
#     seqData = NULL
#     sequenceID = 1
#     for (i in 1:nrow(x)) {
#         tx = x[i,]
#         basket = order %>% filter(CUSTOMER_NBR == tx$CUSTOMER_NBR, ORDER_NBR == tx$ORDER_NBR) %>% 
#             distinct(PRODUCT_ID)
#         items = paste(basket$PRODUCT_ID, collapse = ",")
#         
#         custTr = order %>% filter(CUSTOMER_NBR == tx$CUSTOMER_NBR, ORDER_NBR == tx$ORDER_NBR) %>% 
#             distinct(CUSTOMER_NBR, ORDER_NBR) %>% 
#             mutate(sequenceID = sequenceID, eventID = CUSTOMER_NBR, SIZE = nrow(basket), items = items) %>% 
#             select(sequenceID, eventID, SIZE, items)
#         seqData <- rbind(seqData, custTr)
#         
#         sequenceID = sequenceID + 1
#     }
#     return(seqData)
# }
# 
# temp = order %>% distinct(CUSTOMER_NBR, ORDER_NBR)
# temp = temp[1:30, ]
# temp
# #filter(CUSTOMER_NBR == 355768, ORDER_NBR == 210) %>% 
# seqData = makeSeqData(temp)
# seqData
# 
# head(seqData, 30)
# 
# write.csv(seqData, "QVC/seqData.csv", row.names = F)
# 
# write.table(seqData,"QVC/seqData.txt", sep="\t", row.names=FALSE)
# 
# seqData %>% filter(sequenceID == 23)
# 
# 
# #--------------------------------------------------------------------
# 
# x <- read_baskets(con  = "QVC/seqData.txt", sep = "\t", info = c("sequenceID","eventID","SIZE"))
# head(as(x, "data.frame"), 30)
# 
# seq_rule_1 <- cspade(x, parameter = list(support = 0.3, maxsize = 5, maxlen = 4), control= list(verbose = TRUE))
# summary(seq_rule_1)
# 
# as(zaki, "data.frame")
# 
# data(zaki)
# 
# 
# #--------------------------------------------------------------------