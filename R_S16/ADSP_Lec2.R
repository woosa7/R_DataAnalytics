################################################################
#
# ADSP Lecture
#
################################################################

###############################################################
# 정형 데이터 마이닝
###############################################################

#--------------------------------------------------------------
# 분류분석 (Classification) - Decision Tree
#--------------------------------------------------------------

head(iris)

#--------------------------------------------------------------
# party package 이용한 Decision Tree

library(party)
library(caret)

# train / test data 분리 (6:4 or 7:3)
idx <- sample(2, nrow(iris), replace = T, prob = c(0.6, 0.4))
table(idx)
train_1 <- iris[idx == 1, ]
test_1 <- iris[idx == 2, ]

# train data 이용한 모델 
tree_model <- ctree(Species ~ ., data = train_1)
tree_model
plot(tree_model)
plot(tree_model, type = "simple")

# 예측된 데이터와 실제 데이터 비교
table(train_1$Species)                        # real data
train_1$pred <- predict(tree_model)
confusionMatrix(train_1$pred, train_1$Species)  # Accuracy : 0.9762

# Test Data로 검증
test_1$pred <- predict(tree_model, newdata = test_1)
confusionMatrix(test_1$pred, test_1$Species)    # Accuracy : 0.9394


#--------------------------------------------------------------
# C50 패키지 이용한 Decision Tree

library(C50)

# train / test data 분리 (6:4)
idx <- sample(2, nrow(iris), replace = T, prob = c(0.6, 0.4))
table(idx)
train_2 <- iris[idx == 1, ]
test_2 <- iris[idx == 2, ]

# modeling
c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)

c5_model <- C5.0(Species ~ ., data = train_2, control=c5_options, rules=FALSE)
summary(c5_model)
plot(c5_model)

# validation
train_2$pred <- predict(c5_model, newdata = train_2)
confusionMatrix(train_2$pred, train_2$Species)      # Accuracy : 0.9885

test_2$pred <- predict(c5_model, newdata = test_2)
confusionMatrix(test_2$pred, test_2$Species)        # Accuracy : 0.9048



#--------------------------------------------------------------
# Decision Tree - Ensemble - Bagging
#--------------------------------------------------------------

library(party)
library(caret)

head(iris)

# bootstrap data 생성
data_boot1 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot2 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot3 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot4 <- iris[sample(1:nrow(iris), replace = T), ]
data_boot5 <- iris[sample(1:nrow(iris), replace = T), ]

# Modeling
tree1 <- ctree(Species ~ ., data_boot1)
tree2 <- ctree(Species ~ ., data_boot2)
tree3 <- ctree(Species ~ ., data_boot3)
tree4 <- ctree(Species ~ ., data_boot4)
tree5 <- ctree(Species ~ ., data_boot5)

plot(tree1)
plot(tree2)
plot(tree3)
plot(tree4)
plot(tree5)

pred1 <- predict(tree1, iris)
pred2 <- predict(tree2, iris)
pred3 <- predict(tree3, iris)
pred4 <- predict(tree4, iris)
pred5 <- predict(tree5, iris)

# 각각의 예측 결과를 취합
test <- data.frame(Species = iris$Species, pred1, pred2, pred3, pred4, pred5)
head(test)

# 5개 분류기의 결과를 취합하여 최종 결과를 voting
funcResultValue <- function(x) {
    result <- NULL
    for (i in 1:nrow(x)) {
        xtab <- table(t(x[i, ]))
        rvalue <- names(sort(xtab, decreasing = T)[1])
        result <- c(result, rvalue)
    }
    return(result)
}

test$result <- funcResultValue(test[ , 2:6])
confusionMatrix(test$result, test$Species)


#--------------------------------------------------------------
# Decision Tree - Ensemble - Boosting
#--------------------------------------------------------------

library(tree)

data(kyphosis, package = "rpart")
data <- kyphosis
head(data)

totalCount <- nrow(data)
totalCount

boost <- function(k, compare) {
    # 첫번째 표본 추출 확률을 모두 동일하게 설정
    pr <- rep(1/totalCount, totalCount)
    
    # 결과에 대한 확률 및 모델의 정확도를 저장할 객체
    result <- matrix(0, k, 3) # k row 3 col
    
    # k개 만큼 tree model 생성
    for (j in 1:k) {
        # 배깅과 달리 각 인덱스에 설정된 확률로 샘플링
        data.boost <- data[sample(1:totalCount, prob = pr, replace = T), ]
        
        # 샘플링 데이터에 대한 tree 생성
        data.tree <- tree(Kyphosis ~ ., data.boost)
        
        # 각 row에 대한 예측을 저장할 객체  
        pred <- matrix(0, totalCount, 1)
        
        for (i in 1:totalCount) {
            # predict - absent / present 확률
            if (predict(data.tree, data[i, ])[ , 1] > 0.5) {
                pred[i, 1] <- "absent"
            } else {
                pred[i, 1] <- "present"
            }
        }
        
        # test data (compare) 한 개에 대한 예측 확률
        result[j, 1] <- predict(data.tree, compare)[ , 1]
        result[j, 2] <- predict(data.tree, compare)[ , 2]
        result[j, 3] <- length(which(as.matrix(data)[ , 1] == pred)) / totalCount # 정확도
        
        pr <- rep(1/totalCount, totalCount)
        # 오분류 표본의 확률을 2배로 설정하여 2번째 loop 수행
        pr[as.matrix(data)[ , 1] != pred] <- 2/totalCount   
    }
    return(result)
}

# 80번째 데이터로 10회 반복해서 측정
boost.result <- boost(10, data[80, ])
boost.result

a <- t(boost.result[,1])%*%(boost.result[,3])    # absent 확률
b <- t(boost.result[,2])%*%(boost.result[,3])    # present 확률
a;b

# b가 a 보다 확률이 높기 때문에 80번째 데이터는 present로 최종 예측



#--------------------------------------------------------------
# Decision Tree - Ensemble - Random Forest
#--------------------------------------------------------------

head(iris)

idx <- sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))
trainData <- iris[idx == 1, ]
testData <- iris[idx == 2, ]

library(randomForest)

# ntree = 100 : 100 개의 tree 만듬.
# proximity = T : 다양한 트리 분할 시도
model <- randomForest(Species ~ ., data = trainData, ntree = 100, proximity = T)
model

table(trainData$Species, predict(model))

importance(model)  
# 지니계수. 값이 높은 변수가 클래스를 분류하는데 가장 큰 영향을 줌.

plot(model, main = "randomForest model of iris")
# tree가 40개 이상일 경우 오차가 안정적으로 나타난다.

varImpPlot(model)
# 변수의 상대적 중요도를 표시

pred <- predict(model, newdata = testData)
table(testData$Species, pred)

plot(margin(model, testData$Species))


#--------------------------------------------------------------
# ROCR
#--------------------------------------------------------------

# http://blog.naver.com/woosa7/220840338896

library(C50)
library(ROCR)

data(churn)     # C50 dataset. 서비스 제공자를 바꾸는 고객.
summary(churnTrain)

# Modeling
c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
model <- C5.0(churn ~ ., data = churnTrain, control = c5_options, rules = FALSE)
summary(model)

# 가지가 너무 많아서 Attribute usage 가 작은 변수 제거하고 다시 모델링
drops <- c("total_day_charge", "account_length", "total_eve_calls", "total_day_calls", "total_eve_minutes")
churnTrain2 <- churnTrain[, !(names(churnTrain) %in% drops)]

model2 <- C5.0(churn ~ ., data = churnTrain2, control = c5_options, rules = FALSE)
summary(model2)
plot(model2, type = "simple")

pred_train <- predict(model2, churnTrain2, type = "class")
confusionMatrix(pred_train, churnTrain2$churn)   # Accuracy : 0.9583

# Test
head(churnTest)

churnTest$pred <- predict(model2, churnTest, type = "class")        # 예측결과
churnTest$pred_prob <- predict(model2, churnTest, type = "prob")    # 확률
confusionMatrix(churnTest$pred, churnTest$churn)                    # Accuracy : 0.9478

# Model Evaluation by ROCR chart
head(churnTest$pred_prob)
c5_pred <- prediction(churnTest$pred_prob[, "yes"], churnTest$churn)
c5_model.perf <- performance(c5_pred, "tpr", "fpr")
# True positive rate (tpr) = Sensitivity
# False positive rate (fpr) = 1 - Specificity

c5_model.perf

plot(c5_model.perf, col = "red")

AUROC <- performance(c5_pred, "auc")
AUROC@y.values
# 0.8804094 : Good

c5_model.lift <- performance(c5_pred, "lift", "rpp")  # rpp : Rate of positive predictions
plot(c5_model.lift, col = "red")




#--------------------------------------------------------------
# 인공신경망 (Artificial Neural Network)
#--------------------------------------------------------------

# 인간의 뇌는 비선형적이며 병렬적인 정보처리를 한다.
# 뉴런은 가중치가 있는 링크들로 연결되어 있다.
# 뉴런은 여러 입력 신호를 받지만 출력 신호는 오직 하나만 생성한다.

# 인공신경망 학습
# 가중치를 반복적으로 조정하며 학습
# 뉴런의 각 링크에는 그와 연관된 수치적인 가중치가 있음.
# 가중치는 장기 기억을 위한 기본적인 수단으로 각 뉴런 입력 강도 즉, 중요도를 표현
# 신경망의 구조를 먼저 선택하고, 어떤 알고리즘을 사용할지 결정한 후 신경망을 훈련시킨다
# 훈련 데이터를 통해 가중치를 갱신
# 역전파 알고리즘(Backpropagation)을 활용하여 비선형성을 극복

# Deep Learning
# Unsupervised learning을 이용한 전처리 과정을 다층신경망에 추가하는 방식을 통해 
# 다층망을 쌓아도 정확도를 해치지 않는 방식.
# 신경망을 여러 층 쌓아올려 모델을 구축하는 머신 러닝 기법이면 모두 딥 러닝이라고 할 수 있다.

# ---> 추후 수업 후 다시 정리할 것 !!!!

install.packages("nnet")
library(nnet)














