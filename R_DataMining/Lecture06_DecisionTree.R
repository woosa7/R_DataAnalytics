############################################################
#
# Data Mining 6 & 7 - Classification / Decision Tree
#
############################################################

# 반품고객 예측

library(C50)
library(caret)
library(ROCR)

cb = read.table("Hshopping.txt", header = T)
summary(cb)

cb$반품여부 <- factor(cb$반품여부)
cb$성별 <- factor(cb$성별)
cb$출연자 <- factor(cb$출연자)

inTrain <- createDataPartition(y = cb$반품여부, p = 0.6, list = F)

cb.train = cb[inTrain, ][, -1]
cb.test = cb[-inTrain, ][, -1]

dim(cb.train)
dim(cb.test)

# C50 Features

# Boosting
# 분류가 잘못된 데이터에 가중치를 주고 다시 표본추출하여 분류 생성.
# C5.0(...... trials = 10) : boosting 반복 횟수 지정

# Winnowing
# 입력 필드가 유용한지 측정한 다음 유용하지 않는 경우 배제하고 모델링. 입력필드가 많을 경우 유용.

# Global Pruning
# 전역적 가지치기 여부를 결정. 전체적인 Tree 구조에서 강도가 약한 sub-tree 자체를 삭제.

# Pruning severity
# 지역적 가지치기의 강도를 조정. CF 파라미터를 0에서 1사이의 값으로 설정(default = 0.25)
# 값이 작을수록 강도가 강해져서 Over-fitting의 가능성이 적어지지만 대신 정확도가 떨어질 수 있음.
# C5.0Control(...... CF = 0.7)
    
# Modeling    
options1 <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
model1 <- C5.0(반품여부 ~ ., data = cb.train, control = options1)
model2 <- C5.0(반품여부 ~ ., data = cb.train, control = options1, trials = 100)

options3 <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE, CF = 0.7)
model3 <- C5.0(반품여부 ~ ., data = cb.train, control = options3)

options4 <- C5.0Control(winnow = TRUE, noGlobalPruning = FALSE)
model4 <- C5.0(반품여부 ~ ., data = cb.train, control = options4)

options5 <- C5.0Control(winnow = FALSE, noGlobalPruning = TRUE)
model5 <- C5.0(반품여부 ~ ., data = cb.train, control = options5)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

plot(model1, type = 'simple')
plot(model2, type = 'simple')
plot(model3, type = 'simple')
plot(model4, type = 'simple')
plot(model5, type = 'simple')

# Predict
cb.test$pred1 <- predict(model1, cb.test, type = "class")
cb.test$pred2 <- predict(model2, cb.test, type = "class")
cb.test$pred3 <- predict(model3, cb.test, type = "class")
cb.test$pred4 <- predict(model4, cb.test, type = "class")
cb.test$pred5 <- predict(model5, cb.test, type = "class")

# Accuracy of Models
confusionMatrix(cb.test$pred1, cb.test$반품여부)["overall"]$overall["Accuracy"]
confusionMatrix(cb.test$pred2, cb.test$반품여부)["overall"]$overall["Accuracy"]
confusionMatrix(cb.test$pred3, cb.test$반품여부)["overall"]$overall["Accuracy"]
confusionMatrix(cb.test$pred4, cb.test$반품여부)["overall"]$overall["Accuracy"]
confusionMatrix(cb.test$pred5, cb.test$반품여부)["overall"]$overall["Accuracy"]

# Accuracy 가장 높은 모델로 적용
cb.test$pred <- predict(model4, cb.test, type = "class")       # 예측결과
cb.test$pred_prob <- predict(model4, cb.test, type = "prob")   # 확률
confusionMatrix(cb.test$pred, cb.test$반품여부)

c5_pred <- prediction(cb.test$pred_prob[,2], cb.test$반품여부)
c5_perf <- performance(c5_pred, "tpr", "fpr")   
c5_lift <- performance(c5_pred, "lift", "rpp")   

plot(c5_perf, colorize = T)
plot(c5_lift, colorize = T)


# Result
# funcResultValue <- function(x) {
#     predVec <- ifelse(x == 1, 1, 0)
#     sumVal <- apply(predVec, 1, sum)
#     result <- ifelse(sumVal >= 3, 1, 0)
#     return(result)
# }
# 
# cb.test$result <- funcResultValue(cb.test[ , c("pred1","pred2","pred3","pred4","pred5")])
# confusionMatrix(cb.tpredest$result, cb.test$반품여부)


