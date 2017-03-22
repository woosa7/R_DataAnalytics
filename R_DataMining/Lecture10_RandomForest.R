############################################################
#
# Data Mining 10 - Random Forest / SVM / Cross Validation
#
############################################################

# https://github.com/woosa7/R_DataAnalytics/blob/master/R_DataMining/Lec/2016_2_DM_MBA_10.pdf

##### 1. Random Forest

# Randomization
# - Bootstrap samples (Bagging)
# - Random selection of K <= p split variables (Random input selection)

# 장단점
# - 숲의 크기(나무의 수)가 커질수록 일반화오류가 특정값으로 수렴하게 되어 over-fitting을 피할 수 있음
# - 전체 학습용 데이터에서 무작위로 복원추출된 데이터를 사용함으로써 잡음이나 outlier로부터 크게 영향을 받지 않음
# - 분석가가 입력변수 선정으로부터 자유로울 수 있음
# - Class의 빈도가 불균형일 경우 타기법에 비해 우수한 예측력을 보임
# - 최종결과에 대한 해석이 어려움


library(randomForest)
library(caret)
library(ROCR)

# 홈쇼핑 반품 고객 예측

cb <- read.delim("data/Hshopping.txt", stringsAsFactors=FALSE)
cb$반품여부 <- factor(cb$반품여부)

set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

# 모델링
# mtry : Number of variables randomly sampled as candidates at each split.
# ntree : Number of trees to grow.

set.seed(123)
rf_model <- randomForest(반품여부 ~ .-ID, data=cb.train, ntree=50, mtry=2)

# OOB estimate : out-of-bag 샘플을 사용하여검증
rf_model

plot(rf_model, main="random Forest model")
legend("topright", c("worst","overall","best"), fill = c("green", "black", "red"))

# 변수의 중요도
importance(rf_model)
varImpPlot(rf_model)

cb.test$rf_pred <- predict(rf_model, cb.test, type="response")
confusionMatrix(cb.test$rf_pred, cb.test$반품여부)

cb.test$rf_pred_prob <- predict(rf_model, cb.test, type="prob")
rf_pred <- prediction(cb.test$rf_pred_prob[,2],cb.test$반품여부)

rf_model.perf1 <- performance(rf_pred, "tpr", "fpr") 
plot(rf_model.perf1, colorize=TRUE); abline(a=0, b=1, lty=3)  # ROC-chart

rf_model.perf2 <- performance(rf_pred, "lift", "rpp") 
plot(rf_model.perf2, colorize=TRUE); abline(v=0.4, lty=3)     # Lift chart

performance(rf_pred, "auc")@y.values[[1]] 



##### 2. Support Vector Machine (SVM)

# https://github.com/woosa7/PythonDataAnalytics/blob/master/KMU/Lec/svm.pdf
# http://blog.naver.com/nieuwendyk/220820706749

# 두 카테고리 중 어느 하나에 속한 데이터의 집합이 주어졌을 때, SVM 알고리즘은 주어진 데이터 집합을 바탕으로 새로운 데이터가 어느 카테고리에 속할지 판단하는 비확률적 이진 선형 분류모델을 만든다.
# 만들어진 분류모델은 데이터가 사상된 공간에서 경계로 표현되는데 SVM 알고리즘은 그 중 가장 큰 폭을 가진 경계를 찾는 알고리즘이다.
# SVM은 선형분류와 더불어 비선형 분류에서도 사용될 수 있다.
# 비선형분류를 하기 위해서 주어진 데이터를 고차원 특징 공간으로 사상하는 작업이 필요한데, 이를 효율적으로 하기 위해 '커널트릭'을 사용하기도 한다.
# Support Vector = 의사결정 경계에서 가장 가까

# SVM의 특징
# 기존의 지도학습 모형과 같이 예측 부분에서 활용될 수 있으며 기계학습 부분에서 다른 모델에 비해 예측률이 높다고 알려져 있다.
# 넓은 형태의 데이터셋(많은 예측변수를 가지고 있는)에 적합하다.
# 모델을 생성할 때는 기본적인 설정사항을 이용해 비교적 빨리 모형을 생성할 수 있다.
# 실제 응용에 있어서 인공신경망보다 높은 성과를 내고 명백한 이론적 근거에 기반하므로 결과해석이 상대적으로 용이하다.

library(e1071)

# svm 주요 옵션
# cost : Complexity parameter (C). 
# gamma : gamma parameter. 커질수록 가우시안 커브가 좁아진다. 즉, 튀어나온 봉우리들이 많아진다.
# probability : 확률적 예측 허용.

svm_model <- svm(반품여부~성별+나이+구매금액+출연자, data=cb.train, cost=100, gamma=1, probability = TRUE)
summary(svm_model)

plot(svm_model, data=cb.train, 구매금액~나이)
legend(50, 1.7, "x : support vector")


# + : support vector
plot(cmdscale(dist(cb.train[,2:5])), col=cb.train$반품여부, pch=c("o","+")[1:nrow(cb.train) %in% svm_model$index+1])


cb.test$svm_pred <- predict(svm_model, cb.test)
confusionMatrix(cb.test$rf_pred, cb.test$반품여부)
postResample(cb.test$svm_pred, cb.test$반품여부)


cb.test$svm_pred_prob <- attr(predict(svm_model, cb.test, probability = TRUE), "probabilities")[,2] # 1이 될 확률

svm_pred <- prediction(cb.test$svm_pred_prob, cb.test$반품여부)

svm_model.perf1 <- performance(svm_pred, "tpr", "fpr") # ROC-chart
plot(svm_model.perf1, colorize=TRUE); abline(a=0, b=1, lty=3)

svm_model.perf2 <- performance(svm_pred, "lift", "rpp") 
plot(svm_model.perf2, colorize=TRUE); abline(v=0.4, lty=3)

performance(svm_pred, "auc")@y.values[[1]]


# the best values to use for the parameters gamma and cost
set.seed(123)
tune.svm(반품여부~성별+나이+구매금액+출연자, data=cb.train, gamma=seq(.5, .9, by=.1), cost=seq(100,1000, by=100))



##### 3. K-fold Cross Validation

# Create a 5-fold partition using the caret package
set.seed(1)
flds <- createFolds(cb$반품여부, k=5, list=TRUE, returnTrain=FALSE)
str(flds)

# Perform 5 experiments
experiment <- function(train, test, m) {
  rf <- randomForest(반품여부 ~ .-ID, data=train, ntree=50)
  rf_pred <- predict(rf, test, type="response")
  m$acc = c(m$acc, confusionMatrix(rf_pred, test$반품여부)$overall[1])   # 정확도
  rf_pred_prob <- predict(rf, test, type="prob")
  rf_pred <- prediction(rf_pred_prob[,2], cb.test$반품여부)    # 예측
  m$auc = c(m$auc, performance(rf_pred, "auc")@y.values[[1]])  # AUC
  return(m) 
}

measure = list()
for(i in 1:5){
  inTest <- flds[[i]]
  cb.test <- cb[inTest, ]
  cb.train <- cb[-inTest, ]
  measure = experiment(cb.train, cb.test, measure) 
}

measure 
mean(measure$acc); sd(measure$acc)
mean(measure$auc); sd(measure$auc)
