############################################################
#
# Data Mining 1 & 2 - Classification
#
############################################################

# Data Mining Process Demo
# PEP: Personal Equity Plan, 연금보험 구매 여부

library(caret)
library(ROCR)
library(C50)
library(e1071)

#-----------------------------------------------------------
# 1. Data Preparation

# Train Set
train <- read.csv("data/pepTrainSet.csv", stringsAsFactors = T)
head(train)
summary(train)

# Test Set
test <- read.csv("data/pepTestSet.csv", stringsAsFactors = T)
head(test)
summary(test)

train <- subset(train, select=-c(id))   # id 제거
test <- subset(test, select=-c(id))

# New Data
newdata <- read.csv("data/pepNewCustomers.csv", stringsAsFactors = T)
head(newdata)
summary(newdata)

newdata$id <- as.character(newdata$id)   # id 변수 factor --> character로 변환


#-----------------------------------------------------------
# 2. Modeling

# 1st candidate model : decision tree
c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
c5_model <- C5.0(pep ~ ., data = train, control = c5_options, rules = FALSE)
plot(c5_model)

pred_train <- predict(c5_model, train, type = "class")
confusionMatrix(pred_train, train$pep)                  # Accuracy : 0.9367

# 2nd candidate model : logistic regression
lm_model <- glm(pep ~ ., data = train, family = binomial)
summary(lm_model)


#-----------------------------------------------------------
# 3. Model Evaluation with Test Data by Confusion Matrix 

# (1) C5 decision tree
test$c5_pred <- predict(c5_model, test, type = "class")      # 예측결과
test$c5_pred_prob <- predict(c5_model, test, type = "prob")  # 확률
confusionMatrix(test$c5_pred, test$pep)                      # Accuracy : 0.89
head(test)

# (2) logistic regression
test$lm_pred <- ifelse(predict(lm_model, test, type = "response") > 0.5, "YES", "NO")
test$lm_pred_prob <- predict(lm_model, test, type = "response")
confusionMatrix(test$lm_pred, test$pep)     # Accuracy : 0.5867


#-----------------------------------------------------------
# 4. Model Evaluation by ROC chart

c5_pred <- prediction(test$c5_pred_prob[, "YES"], test$pep)
c5_model.perf <- performance(c5_pred, "tpr", "fpr")   # True positive rate, False positive rate

lm_pred <- prediction(test$lm_pred_prob, test$pep)
lm_model.perf <- performance(lm_pred, "tpr", "fpr")

plot(c5_model.perf, col = "red")
plot(lm_model.perf, col = "blue", add=T)
legend(0.7, 0.7, c("C5 ","LM "), cex = 0.9, col = c("red", "blue"), lty = 1)


#-----------------------------------------------------------
# 5. Deployment - 신규 데이터에 모델 적용

newdata$c5_pred <- predict(c5_model, newdata, type = "class")
newdata$c5_pred_prob <- predict(c5_model, newdata, type = "prob")

target <- subset(newdata, c5_pred == "YES" & c5_pred_prob[ ,"YES"] > 0.8)
target

selectedTarget <- target[order(target$c5_pred_prob[,"YES"], decreasing = T), ]
head(selectedTarget)

#write.csv(target[order(target$c5_pred_prob[,"YES"], decreasing=T), ], "dm_target.csv", row.names=FALSE)



######################################################################
# Bagging - 위 데이터에 ensemble 기법 적용
######################################################################

train <- read.csv("data/pepTrainSet.csv", stringsAsFactors = T)
train <- subset(train, select=-c(id))

test <- read.csv("data/pepTestSet.csv", stringsAsFactors = T)
test <- subset(test, select=-c(id))

summary(train)

# booting data 생성
data_boot1 <- train[sample(1:nrow(train), replace = T), ]
data_boot2 <- train[sample(1:nrow(train), replace = T), ]
data_boot3 <- train[sample(1:nrow(train), replace = T), ]
data_boot4 <- train[sample(1:nrow(train), replace = T), ]
data_boot5 <- train[sample(1:nrow(train), replace = T), ]
data_boot6 <- train[sample(1:nrow(train), replace = T), ]
data_boot7 <- train[sample(1:nrow(train), replace = T), ]

summary(data_boot5)
head(data_boot5);tail(data_boot5)

# Modeling
c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)

model1 <- C5.0(pep ~ ., data = data_boot1, control = c5_options, rules = FALSE)
model2 <- C5.0(pep ~ ., data = data_boot2, control = c5_options, rules = FALSE)
model3 <- C5.0(pep ~ ., data = data_boot3, control = c5_options, rules = FALSE)
model4 <- C5.0(pep ~ ., data = data_boot4, control = c5_options, rules = FALSE)
model5 <- C5.0(pep ~ ., data = data_boot5, control = c5_options, rules = FALSE)
model6 <- C5.0(pep ~ ., data = data_boot6, control = c5_options, rules = FALSE)
model7 <- C5.0(pep ~ ., data = data_boot7, control = c5_options, rules = FALSE)

test$pred1 <- predict(model1, test, type = "class")
test$pred2 <- predict(model2, test, type = "class")
test$pred3 <- predict(model3, test, type = "class")
test$pred4 <- predict(model4, test, type = "class")
test$pred5 <- predict(model5, test, type = "class")
test$pred6 <- predict(model6, test, type = "class")
test$pred7 <- predict(model7, test, type = "class")

head(test)
head(test[, 11:18])

funcResultValue <- function(x) {
    predVec <- ifelse(x == "YES", 1, 0)
    sumVal <- apply(predVec, 1, sum)
    result <- ifelse(sumVal > 3, "YES", "NO")
    return(result)
}

test$result <- funcResultValue(test[ , 12:18])
confusionMatrix(test$result, test$pep)




