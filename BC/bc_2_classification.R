library(C50)
library(caret)
library(ROCR)

cust = read.csv('cust_data.csv')

summary(cust)
dim(cust)

inTrain <- createDataPartition(y = cust$TARGET, p = 0.9, list = F)
cust_train = cust[inTrain, ]

dim(cust_train)
summary(cust_train)
