############################################################
#
# Data Mining 7 - 고객의 성별을 예측하는 의사결정나무(C5.0)
#
############################################################

library(dplyr)
library(C50)
library(caret)

# Import Data
custsig <- read.csv("custsig.csv", header = T)
head(custsig)

custsig <- custsig %>% filter(sex != 0) %>%
    select(sex, buy_brd, visits, API, NPPV,  wk_amt,  we_amt, 
           spring_buy, summer_buy, fall_buy, p_trend, instCnt)

custsig$sex <- factor(custsig$sex)
summary(custsig)


# Split Data
inTrain <- createDataPartition(y = custsig$sex, p = 0.7, list = F)

trainData = custsig[inTrain, ]
testData = custsig[-inTrain, ]

dim(trainData)
dim(testData)


# Modeling 1
options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
model <- C5.0(sex ~ ., data = trainData, control = options)
plot(model)
summary(model)

testData$pred <- predict(model, testData, type = "class")
confusionMatrix(testData$pred, testData$sex)


# Modeling 2
options2 <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE, CF = 0.7)
model2 <- C5.0(sex ~ ., data = trainData, control = options2)
plot(model2)
summary(model2)

testData$pred <- predict(model2, testData, type = "class")
confusionMatrix(testData$pred, testData$sex)


# Modeling 3
options3 <- C5.0Control(winnow = FALSE, noGlobalPruning = TRUE)
model3 <- C5.0(sex ~ ., data = trainData, control = options3)
plot(model3)
summary(model3)

testData$pred <- predict(model3, testData, type = "class")
confusionMatrix(testData$pred, testData$sex)

