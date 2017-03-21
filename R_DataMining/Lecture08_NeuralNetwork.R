############################################################
#
# Data Mining 8 & 9 - Neural Network
#
############################################################

# Neural Network, 인공신경망 이론 (링크)
# https://github.com/woosa7/R_DataAnalytics/blob/master/R_DataMining/Lec/2016_2_DM_MBA_08.pdf

# 홈쇼핑에서 반품 고객의 특성을 파악하고자 함.


##### 1. Neural Network Analysis using "nnet" package

library(nnet)
library(caret)
library(ROCR)

cb <- read.delim("data/Hshopping.txt", stringsAsFactors=FALSE)
head(cb)
summary(cb$반품여부)

cb$반품여부 <- factor(cb$반품여부)	# 명목형 값 예측일 경우 factor로 변환.
summary(cb$반품여부)

# 데이터 분할
set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

# 모델링
# nnet의 주요 옵션들
# size : hidden node 수 
# maxit : 반복횟수
# decay : overfitting을 피하기 위해 사용하는 weight decay parameter
# rang : Initial random weights on [-rang, rang]. default 0.5

set.seed(123)

nn_model1 <- nnet(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train, size=3, maxit=1000)
nn_model2 <- nnet(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train, size=5, maxit=1000, decay = 0.0005, rang = 0.1)	

summary(nn_model1)
summary(nn_model2)

library(devtools)

source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(nn_model1)
plot.nnet(nn_model2)


# 위 인공신경망 모델에서 각 변수의 중요도 확인
library(NeuralNetTools)

garson(nn_model1)
garson(nn_model2)

# 두 모델의 테스트 데이터셋에 대한 예측력/성능 비교
confusionMatrix(predict(nn_model1, newdata=cb.test, type="class"), cb.test$반품여부)
confusionMatrix(predict(nn_model2, newdata=cb.test, type="class"), cb.test$반품여부)

# ROCR::prediction - ROCR 패키지에 있는 prediction 함수 사용.

# model1
nn_pred1 <- ROCR::prediction(predict(nn_model1, newdata=cb.test, type="raw"), cb.test$반품여부)
nn_model1.roc <- performance(nn_pred1, "tpr", "fpr")   # ROC-chart
plot(nn_model1.roc, colorize=TRUE)
nn_model1.lift <- performance(nn_pred1, "lift", "rpp")  # Lift chart
plot(nn_model1.lift, colorize=TRUE)

# model2
nn_pred2 <- ROCR::prediction(predict(nn_model2, newdata=cb.test, type="raw"), cb.test$반품여부)
nn_model2.roc <- performance(nn_pred2, "tpr", "fpr")   # ROC-chart
plot(nn_model2.roc, colorize=TRUE)
nn_model2.lift <- performance(nn_pred2, "lift", "rpp")  # Lift chart
plot(nn_model2.lift, colorize=TRUE)



##### 2. Neural Network Analysis using "neuralnet" package

library(neuralnet)

cb <- read.delim("data/Hshopping.txt", stringsAsFactors=FALSE) # neuralnet 패키지는 목표변수가 numeric.

# 데이터 분리
set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

# 모델링
set.seed(123)
nnet_model1 <- neuralnet(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train, hidden=3, threshold=0.01)
nnet_model2 <- neuralnet(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train, hidden=c(2,2), threshold=0.01)

# threshold : 에러의 감소분이 threshold 값보다 작으면 stop
# hidden : hidden node 수. 
# hidden=c(2,2) : hidden layer 2개가 각각 hidden node 2개를 가짐
# linear.output: 활성함수('logistic' or 'tanh')가 출력 뉴런에 적용되지 않아야 하는 경우(즉, 회귀) TRUE로 설정(default)
# stepmax: 훈련 수행 최대 횟수

plot(nnet_model1)
plot(nnet_model2)

# 모델 내에서 각 변수의 영향도(일반화 가중치)
# 나이 : 분산이 0에 가까움. 결과에 미치는 영향이 미미하다.
par(mfrow=c(2,2))
gwplot(nnet_model1, selected.covariate = "성별", min=-3,max=6)
gwplot(nnet_model1, selected.covariate = "나이", min=-3,max=6) 
gwplot(nnet_model1, selected.covariate = "구매금액", min=-3,max=6)
gwplot(nnet_model1, selected.covariate = "출연자", min=-3,max=6)
par(mfrow=c(1,1))


# 테스트 데이터에서 필요한 필드만 지정!!!
# nnet_model1
cb.test$nnet1_pred_prob <- compute(nnet_model1, covariate=cb.test[, c(2:5)])$net.result  
cb.test$nnet1_pred <- ifelse(cb.test$nnet1_pred_prob > 0.5, 1, 0)
confusionMatrix(cb.test$nnet1_pred, cb.test$반품여부)

# nnet_model2
cb.test$nnet2_pred_prob <- compute(nnet_model2, covariate=cb.test[, c(2:5)])$net.result  
cb.test$nnet2_pred <- ifelse(cb.test$nnet2_pred_prob > 0.5, 1, 0)
confusionMatrix(cb.test$nnet2_pred, cb.test$반품여부)


# ROC & Lift chart
nnet1_pred <- ROCR::prediction(cb.test$nnet1_pred_prob, cb.test$반품여부)
nnet_model1.roc <- performance(nnet1_pred, "tpr", "fpr") # ROC-chart
plot(nnet_model1.roc, colorize=TRUE)
nnet_model1.lift <- performance(nnet1_pred, "lift", "rpp") # Lift chart
plot(nnet_model1.lift, colorize=TRUE)

nnet2_pred <- ROCR::prediction(cb.test$nnet2_pred_prob, cb.test$반품여부)
nnet_model2.roc <- performance(nnet2_pred, "tpr", "fpr") # ROC-chart


##### Input Normalization in Neural Networks

# 입력값 정규화
# 값을 0 ~ 1 사이의 값으로 변환
normalize <- function (x) {
    normalized = (x - min(x)) / (max(x) - min(x))
    return(normalized)
}

cb <- read.delim("data/Hshopping.txt", stringsAsFactors=FALSE)

# 나이와 구매금액을 정규화
cb$나이 <- normalize(cb$나이)
cb$구매금액 <- normalize(cb$구매금액)

head(cb)

set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

set.seed(123)
nnet_model3 <- neuralnet(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train, hidden=3, threshold=0.01)

par(mfrow=c(2,2))
gwplot(nnet_model3, selected.covariate = "성별", min=-3, max=6)
gwplot(nnet_model3, selected.covariate = "나이", min=-3, max=6)
gwplot(nnet_model3, selected.covariate = "구매금액", min=-3, max=6)
gwplot(nnet_model3, selected.covariate = "출연자", min=-3, max=6)
par(mfrow=c(1,1))

garson(nnet_model3)

cb.test$nnet3_pred_prob <- compute(nnet_model3, covariate=cb.test[, c(2:5)])$net.result  
cb.test$nnet3_pred <- ifelse(cb.test$nnet3_pred_prob > 0.5, 1, 0)
confusionMatrix(cb.test$nnet3_pred, cb.test$반품여부)



# Model Comparison

nnet3_pred <- ROCR::prediction(cb.test$nnet3_pred_prob, cb.test$반품여부)
nnet_model3.roc <- performance(nnet3_pred, "tpr", "fpr") # ROC-chart

plot(nnet_model1.roc, col="red")
plot(nnet_model2.roc, col="green", add=T)
plot(nnet_model3.roc, col="blue", add=T)

legend(0.6,0.7,c("nnet_model1","nnet_model2","nnet_model3"),cex=0.9,col=c("red","green","blue"),lty=1)

performance(nnet1_pred, "auc")@y.values[[1]]
performance(nnet2_pred, "auc")@y.values[[1]] 
performance(nnet3_pred, "auc")@y.values[[1]]



##### 3. Multinomial Classification using "neuralnet" : 

# iris 데이터 다항 분류
data(iris)
summary(iris)

# neuralnet은 식에서 '.'을 지원하지 않기 때문에 아래와 같이 식을 문자열로 생성.
formula <- as.formula(paste('Species ~ ', paste(names(iris)[-length(iris)], collapse='+')))
formula

# neuralnet does not support the '.' notation in the formula.
# multi_model <- neuralnet(formula, iris, hidden=3, linear.output=FALSE)

# fails !
# Species가 factor : neuralnet 패키지는 target으로 factor를 사용할 수 없다.
# Species를 3개의 binary 변수로 펼쳐야 한다.

formula <- as.formula(paste('setosa + versicolor + virginica ~ ', paste(names(iris)[-length(iris)], collapse='+')))
formula

trainData <- cbind(iris[, 1:4], class.ind(iris$Species))
head(trainData)

multi_model <- neuralnet(formula, trainData, hidden=3)
plot(multi_model)

species_prob = compute(multi_model, iris[, 1:4])$net.result
head(species_prob)



