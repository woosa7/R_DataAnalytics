############################################################
#
# Data Mining 8 & 9 - Neural Network
#
############################################################

##### 1. Neural Network Analysis using nnet package

install.packages("nnet")
library(nnet); library(caret); library(ROCR)

cb <- read.delim("data/Hshopping.txt", stringsAsFactors=FALSE)
cb$반품여부 <- factor(cb$반품여부)	# 명목형 값 예측일 경우

set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

set.seed(123)
# size: # of hidden nodes
# maxit : 반복횟수
nn_model <- nnet(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train, size=3, maxit=1000)	
?nnet
nn_model$value

nn_model2 <- nnet(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train, size=5, maxit=1000, 
                      decay = 0.0005, rang = 0.1)	

summary(nn_model)
summary(nn_model2)

install.packages("devtools")
library(devtools)

source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r') 
plot.nnet(nn_model)
plot.nnet(nn_model2)

#-------------------------------------------
install.packages("NeuralNetTools")
library(NeuralNetTools)

garson(nn_model)

confusionMatrix(predict(nn_model, newdata=cb.test, type="class"), cb.test$반품여부)
confusionMatrix(predict(nn_model2, newdata=cb.test, type="class"), cb.test$반품여부)


# ROCR::prediction - ROCR 패키지에 있는 prediction 함수 사용.
nn_pred <- ROCR::prediction(predict(nn_model, newdata=cb.test, type="raw"), cb.test$반품여부)
nn_model.perf1 <- performance(nn_pred, "tpr", "fpr") # ROC-chart
nn_model.perf2 <- performance(nn_pred, "lift", "rpp") # Lift chart
plot(nn_model.perf1, colorize=TRUE); plot(nn_model.perf2, colorize=TRUE)




##### 2. Neural Network Analysis using neuralnet package

install.packages("neuralnet")
library(neuralnet)

cb <- read.delim("data/Hshopping.txt", stringsAsFactors=FALSE)
# neuralnet 패키지는 목표변수가 numeric이어야 함.

set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

set.seed(123)
nn2_model <- neuralnet(반품여부 ~ 성별+나이+구매금액+출연자, 
                           data=cb.train, hidden=3, threshold=0.01)

nn3_model <- neuralnet(반품여부 ~ 성별+나이+구매금액+출연자, 
                           data=cb.train, hidden=c(2,2), threshold=0.01)

# threshold : 에러의 감소분이 threshold 값보다 작으면 stop
# hidden=c(2,2) : hidden layer 2개

?neuralnet

plot(nn2_model)
plot(nn3_model)

par(mfrow=c(2,2))
gwplot(nn2_model, selected.covariate = "성별", min=-3,max=6)
gwplot(nn2_model, selected.covariate = "나이", min=-3,max=6)  # 나이에 상관없이 0 지점에 분포. output에 미치는 영향이 미미하다.
gwplot(nn2_model, selected.covariate = "구매금액", min=-3,max=6)
gwplot(nn2_model, selected.covariate = "출연자", min=-3,max=6)
par(mfrow=c(1,1))

# 테스트 데이터에서 필요한 필드만 지정!!!
cb.test$nn2_pred_prob <- compute(nn2_model, covariate=cb.test[, c(2:5)])$net.result  
cb.test$nn2_pred <- ifelse(cb.test$nn2_pred_prob > 0.5, 1, 0)

confusionMatrix(cb.test$nn2_pred, cb.test$반품여부)


cb.test$nn3_pred_prob <- compute(nn3_model, covariate=cb.test[, c(2:5)])$net.result  
cb.test$nn3_pred <- ifelse(cb.test$nn3_pred_prob > 0.5, 1, 0)

confusionMatrix(cb.test$nn3_pred, cb.test$반품여부)



nn2_pred <- ROCR::prediction(cb.test$nn2_pred_prob, cb.test$반품여부)

nn2_model.perf1 <- performance(nn2_pred, "tpr", "fpr") # ROC-chart
nn2_model.perf2 <- performance(nn2_pred, "lift", "rpp") # Lift chart
plot(nn2_model.perf1, colorize=TRUE)
plot(nn2_model.perf2, colorize=TRUE)


##### 3. Multinomial Classification using neuralnet

data(iris)
formula <- as.formula(paste('Species ~ ', paste(names(iris)[-length(iris)], 
                                                collapse='+')))		

# neuralnet does not support the '.' notation in the formula.
m2 <- neuralnet(formula, iris, hidden=3, linear.output=FALSE)
# fails !

trainData <- cbind(iris[, 1:4], class.ind(iris$Species))
head(trainData)

m2 <- neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, trainData, hidden=3)
plot(m2)
compute(m2, iris[, 1:4])$net.result



##### 4. Input Normalization in Neural Networks

# 값을 0 ~ 1 사이의 값으로 변환
normalize <- function (x) {
  normalized = (x - min(x)) / (max(x) - min(x))
  return(normalized)
}

cb <- read.delim("data/Hshopping.txt", stringsAsFactors=FALSE)

cb$나이 <- normalize(cb$나이)
cb$구매금액 <- normalize(cb$구매금액)

head(cb)

set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

set.seed(123)
nn3_model <- neuralnet(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train, hidden=3, threshold=0.01)

par(mfrow=c(2,2))
gwplot(nn3_model, selected.covariate = "성별", min=-3, max=6)
gwplot(nn3_model, selected.covariate = "나이", min=-3, max=6)
gwplot(nn3_model, selected.covariate = "구매금액", min=-3, max=6)
gwplot(nn3_model, selected.covariate = "출연자", min=-3, max=6)
par(mfrow=c(1,1))

garson(nn3_model)

cb.test$nn3_pred_prob <- compute(nn3_model, covariate=cb.test[, c(2:5)])$net.result
cb.test$nn3_pred <- ifelse(cb.test$nn3_pred_prob > 0.5, 1, 0)

confusionMatrix(cb.test$nn3_pred, cb.test$반품여부)


##### 5. Model Comparison

nn3_pred <- ROCR::prediction(cb.test$nn3_pred_prob, cb.test$반품여부)
nn3_model.perf1 <- performance(nn3_pred, "tpr", "fpr") # ROC-chart

plot(nn_model.perf1, col="red")
plot(nn2_model.perf1, col="green", add=T)
plot(nn3_model.perf1, col="blue", add=T)

legend(0.6,0.7,c("N1","N2","N3"),cex=0.9,col=c("red","green","blue"),lty=1)
