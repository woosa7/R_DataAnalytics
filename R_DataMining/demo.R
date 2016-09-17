# PEP: Personal Equity Plan, 연금보험 구매 여부

library(caret)
library(ROCR)
library(C50)
library(e1071)

# Data Preparation

# Train Set
train <- read.csv("pepTrainSet.csv", stringsAsFactors = T)
head(train)
summary(train)

train <- subset(train, select=-c(id))

# Test Set
test <- read.csv("pepTestSet.csv", stringsAsFactors = T)
head(test)
summary(test)

test <- subset(test, select=-c(id))

# New Data
newdata <- read.csv("pepNewCustomers.csv", stringsAsFactors = T)
head(newdata)
summary(newdata)

newdata$id <- as.character(newdata$id)


# Modeling

# 1st candidate model : decision tree
c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
c5_model <- C5.0(pep ~ ., data = train, control = c5_options, rules = FALSE)
plot(c5_model)

pred_train <- predict(c5_model, train, type = "class")
confusionMatrix(pred_train, train$pep)  # Accuracy = 0.9367

# 2nd candidate model : logistic regression
lm_model <- glm(pep ~ ., data = train, family = binomial)
summary(lm_model)


# Model Evaluation by Confusion Matrix

# 1
test$c5_pred <- predict(c5_model, test, type = "class")      # 예측
test$c5_pred_prob <- predict(c5_model, test, type = "prob")  # 확률
confusionMatrix(test$c5_pred, test$pep)                      # Accuracy = 0.89
head(test)

# 2
test$lm_pred <- ifelse(predict(lm_model, test, type = "response") > 0.5, "YES", "NO")
test$lm_pred_prob <- predict(lm_model, test, type = "response")
confusionMatrix(test$lm_pred, test$pep)


# Model Evaluation by ROC chart

c5_pred <- prediction(test$c5_pred_prob[, "YES"], test$pep)
c5_model.perf <- performance(c5_pred, "tpr", "fpr")

lm_pred <- prediction(test$lm_pred_prob, test$pep)
lm_model.perf <- performance(lm_pred, "tpr", "fpr")

plot(c5_model.perf, col = "red")
plot(lm_model.perf, col = "blue", add=T)
legend(0.7, 0.7, c("C5","LM"), cex=0.9, col=c("red", "blue"), lty=1)


# Deployment - 신규 데이터에 모델 적용

newdata$c5_pred <- predict(c5_model, newdata, type = "class")
newdata$c5_pred_prob <- predict(c5_model, newdata, type="prob")

target <- subset(newdata, c5_pred == "YES" & c5_pred_prob[ ,"YES"] > 0.8)
target

selectedTarget <- target[order(target$c5_pred_prob[,"YES"], decreasing = T), ]
head(selectedTarget)

#write.csv(target[order(target$c5_pred_prob[,"YES"], decreasing=T), ], "dm_target.csv", row.names=FALSE)


#------------------------------------------
# randomForest

test_rf <- read.csv("pepTestSet.csv", stringsAsFactors = T)
test_rf <- subset(test_rf, select=-c(id))

library(randomForest)

rf_model <- randomForest(pep ~ ., data = train, ntree = 100, proximity = T)
plot(rf_model)
varImpPlot(rf_model)

test_rf$rf_pred <- predict(rf_model, test_rf, type = "class")      # 예측
test_rf$rf_pred_prob <- predict(rf_model, test_rf, type = "prob")  # 확률
confusionMatrix(test_rf$rf_pred, test_rf$pep)
head(test_rf)



######################################################################
#
# 1개 패키지에 train dataset을 여러개 만들어 모델링
#
######################################################################

seedData <- read.csv("pepTrainSet.csv", stringsAsFactors = T)
seedData <- subset(seedData, select=-c(id))
head(seedData)

set.seed(1000)
idx1 <- sample(2, nrow(seedData), replace = T, prob = c(0.7, 0.3))
table(idx1)

set.seed(2000)
idx2 <- sample(2, nrow(seedData), replace = T, prob = c(0.7, 0.3))
table(idx2)

set.seed(3000)
idx3 <- sample(2, nrow(seedData), replace = T, prob = c(0.7, 0.3))
table(idx3)

train1 <- seedData[idx1 == 1, ]
train2 <- seedData[idx2 == 1, ]
train3 <- seedData[idx3 == 1, ]

nrow(train1)
nrow(train2)
nrow(train3)

c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)

model1 <- C5.0(pep ~ ., data = train1, control = c5_options, rules = FALSE)
plot(model1)

model2 <- C5.0(pep ~ ., data = train2, control = c5_options, rules = FALSE)
plot(model2)

model3 <- C5.0(pep ~ ., data = train3, control = c5_options, rules = FALSE)
plot(model3)


test <- read.csv("pepTestSet.csv", stringsAsFactors = T)
test <- subset(test, select=-c(id))

test$pred1 <- predict(model1, test, type = "class")
confusionMatrix(test$pred1, test$pep)   # 0.87

test$pred2 <- predict(model2, test, type = "class")
confusionMatrix(test$pred2, test$pep)   # 0.8933

test$pred3 <- predict(model3, test, type = "class")
confusionMatrix(test$pred3, test$pep)   # 0.8767

funcSumPred <- function(x) {
    y1 <- ifelse(x$pred1 == "YES", 1, 0)
    y2 <- ifelse(x$pred2 == "YES", 1, 0)
    y3 <- ifelse(x$pred3 == "YES", 1, 0)
    
    return(y1 + y2 + y3)
}

test$result <- funcSumPred(test[ , 12:14])
test$resultPep <- ifelse(test$result == 3, "YES", "NO")

head(test)
test[test$result == 1, c("pep", "resultPep")]
test[test$result == 2, c("pep", "resultPep")]

confusionMatrix(test$resultPep, test$pep)

head(test[test$pep != test$resultPep, ])



######################################################################
#
# 3개 패키지에 train dataset을  모델링 (party, C50, RandomForest)
#
######################################################################

train <- read.csv("pepTrainSet.csv", stringsAsFactors = T)
train <- subset(train, select=-c(id))

test <- read.csv("pepTestSet.csv", stringsAsFactors = T)
test <- subset(test, select=-c(id))

library(caret)
library(C50)

?C5.0Control

c5_options <- C5.0Control(winnow = F, noGlobalPruning = F)
model <- C5.0(pep ~ ., data = train, control = c5_options, rules = FALSE)
plot(model, type = "simple")

test$pred <- predict(model, newdata = test, type = "class")
confusionMatrix(test$pred, test$pep)   # 0.89

odds <- test[test$pep != test$pred, ]
head(odds)
tail(odds)
summary(odds)

