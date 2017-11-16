library(C50)
library(caret)
library(ROCR)
library(dplyr)

cust = read.csv('cust_data.csv', row.names=NULL)

cust = select(cust, -CUST_ID)

# factor 처리
cust$TARGET <- factor(cust$TARGET)
cust$AGE <- factor(cust$AGE)
cust$STRT_CRDT_GRAD <- factor(cust$STRT_CRDT_GRAD)
cust$LTST_CRDT_GRAD <- factor(cust$LTST_CRDT_GRAD)
cust$CPT_LNIF_BIG <- factor(cust$CPT_LNIF_BIG)
cust$TEL_CNTT_QTR <- factor(cust$TEL_CNTT_QTR)
cust$MIN_CNTT_DATE <- factor(cust$MIN_CNTT_DATE)

levels(cust$PAYM_METD)[1] <- "N"
levels(cust$TEL_MBSP_GRAD)[1] <- "N"

summary(cust)
dim(cust)


# DecisionTree
inTrain <- createDataPartition(y = cust$TARGET, p = 0.9, list = F)
cust_train = cust[inTrain, ]
cust_test  = cust[-inTrain, ]

dim(cust_train)
dim(cust_test)
head(cust_train)

# Modeling
options1 <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
model1 <- C5.0(TARGET ~ ., data = cust_train, control = options1)

summary(model1)

plot(model1, type = 'simple')

# Predict
cb.test$pred1 <- predict(model1, cb.test, type = "class")
cb.test$pred2 <- predict(model2, cb.test, type = "class")
cb.test$pred3 <- predict(model3, cb.test, type = "class")
cb.test$pred4 <- predict(model4, cb.test, type = "class")
cb.test$pred5 <- predict(model5, cb.test, type = "class")

# Accuracy of Models
confusionMatrix(cb.test$pred1, cb.test$반품여부)["overall"]$overall["Accuracy"]

