###########################
# KMU : Lecture
###########################

library(dplyr)
library(reshape2)


### data loading -----
cust <- read.csv("data/customerDb.csv", stringsAsFactors=F)
str(cust)
head(cust)

basket <- read.csv("data/basketData.csv", stringsAsFactors=F)
str(basket)
head(basket);tail(basket)

table(cust$city)
table(cust$sex)

cust$ageGroup <- floor(cust$age/10)*10
table(cust$ageGroup)

custAge <- cust %>%
    group_by(sex, ageGroup) %>%
    summarise(N=n(), avg_age=mean(age))

custAge

dcast(custAge, sex ~ ageGroup, value.var = "N", fun.aggregate = sum)


#------------------------------------------------------------------

### 기본 EDA--------------

#Who are top 10 in purchase amount?
basket %>%
  group_by(custId) %>%
  summarize(total_amt=sum(amount)) %>%
  arrange(desc(total_amt))

#Who are top 10 in visit frequency?
basket %>%
  group_by(custId, receiptNum) %>%
  summarize(visit=n()) %>%
    group_by(custId) %>%
    summarise(realvisit=n()) %>%
    arrange(desc(realvisit))


#Who are top 10 in average purchase amount?
basket %>%
  group_by(custId) %>%
  summarize(avg_amt=mean(amount)) %>%
  arrange(desc(avg_amt))

#what time is biggest sales hour?
basket %>%
  group_by(time) %>%
  summarize(amt=mean(amount)) %>%
  arrange(desc(amt))

#what time is biggest sales hours of each branch?
dummy <- basket %>%
  group_by(time, branchId) %>%
  summarize(amt=mean(amount))

dummy

dummy1 <- dcast(dummy, branchId ~ time, fun.aggregate=sum, value.var="amt")
dummy1

dummy1 <- dcast(dummy, time ~ branchId, fun.aggregate=sum, value.var="amt")
dummy1


args(dcast)
branchHour <- as.data.frame(dummy1)
round(dummy1,0)
branchHour$best <- apply(branchHour[,-1], 1,which.max)
branchHour

dummy1

dummy2 <- melt(dummy1, id.var="time")
dummy2


##Continue RFM
head(basket)
head(cust)
basket$date <- as.Date(as.character(basket$date), format="%Y%m%d")
basket$date_num <- as.numeric(basket$date)
date_dum <- as.Date(basket$date_num, origin="1970-01-01")



##RFM의 의미 이해 필요
library(dplyr)

rfm_dummy <- basket %>%
  group_by(custId, date) %>%
  summarize(N=n())
rfm_dummy

userF <- basket %>%
  group_by(custId, date) %>%
  summarize(N=n()) %>%
  group_by(custId) %>%
  summarize(freq=n())
userF


userRFM <- basket %>%
           group_by(custId) %>%
           summarize(minRecency=min(date_num),
                     recency=max(date_num),
                     monetary=sum(amount),
                     period=max(date_num)-min(date_num))

userRFM <- basket %>%
  group_by(custId) %>%
  summarize(minRecency=min(date),
            recency=max(date),
            monetary=sum(amount),
            period=as.numeric(max(date)-min(date)))


userRFM
userRFM <- left_join(userRFM, userF)

head(userRFM);tail(userRFM)


##RFM연습 다시 시작-------

hist(userRFM$recency, breaks=10)

str(userRFM)
length(unique(userRFM$maxDate))
hist(userRFM$monetary, breaks=100)
range(userRFM$monetary)

plot(table(userRFM$recency), main="Guests Recency")

plot(table(userRFM$freq), main="Guests Frequency")


# 분위수를 따로 구한다.

quantile(userRFM$monetary, c(0.2,0.4,0.6,0.8))

quantile(as.numeric(userRFM$recency), c(0.2,0.4,0.6,0.8)) # be careful about as.numeric


# RFM별로 상위 20%가 차지하는 총 매출액 대비 비중을 구한다.

sumM <- sum(userRFM$monetary[userRFM$monetary > quantile(userRFM$monetary, 0.8)])
sumM/sum(userRFM$monetary) # 65%

sumF <- sum(userRFM$monetary[userRFM$freq > quantile(userRFM$freq, 0.8)])
sumF/sum(userRFM$monetary) # 58%

as.Date(quantile(as.numeric(userRFM$recency),0.8), origin="1970-01-01")
head(as.numeric(userRFM$recency))
str(userRFM)

sumR <- sum(userRFM$monetary[as.numeric(userRFM$recency) > quantile(as.numeric(userRFM$recency), 0.8)])
sumR/sum(userRFM$monetary) # 42%

(weightR <- sumR/(sumR + sumF + sumM))
(weightF <- sumF/(sumR + sumF + sumM))
(weightM <- sumM/(sumR + sumF + sumM))

# RFM지수 = weightR * Recency 점수 + weightF * Frequency점수 + weightM * Monetary 점수

quantM <- quantile(userRFM$monetary,c(0,0.2,0.4,0.6,0.8,1))
quantM
quantR <- as.Date(quantile(as.numeric(userRFM$recency),c(0,0.2,0.4,0.6,0.8,1)),origin="1970-01-01")
quantR
quantF <- quantile(userRFM$freq,c(0,0.2,0.4,0.6,0.8,1))
quantF



# parse 함수 활용방법
columnName <- paste0("userRFM","$","frequency")
eval(parse(text=columnName))[2] # 문자열 조합으로 데이터프레임의 열을 찾는 방법
# ?parse


head(userRFM$frequency)


intervalGrade <- function(mainData, fileName, rfmName, quantileData) {
  
  forLength <- dim(mainData)[1]
  
  results <- rep(0, forLength)
  
  
  for (i in 1:forLength) {
    
    data <- eval(parse(text=paste0(fileName,"$",rfmName)))[i]
    
    if (data >= quantileData[1] && data < quantileData[2] ) {
      results[i] <- 1
    } else if (data >= quantileData[2] && data < quantileData[3]) {
      results[i] <- 2
    } else if (data >= quantileData[3] && data < quantileData[4]) {
      results[i] <- 3
    } else if (data >= quantileData[4] && data < quantileData[5]) {
      results[i] <- 4
    } else { results[i] <- 5 }
  }
  
  return(results)
}


userRFM$R <- intervalGrade(userRFM, "userRFM", "recency", quantR )
userRFM$F <- intervalGrade(userRFM, "userRFM", "freq", quantF )
userRFM$M <- intervalGrade(userRFM, "userRFM", "monetary", quantM )

head(userRFM)

userRFM$score <- (weightR * userRFM$R + weightF * userRFM$F + weightM * userRFM$M)*100/5

hist(userRFM$score)


dim(userRFM)

(quantS <- quantile(userRFM$score,c(0,0.2,0.4,0.6,0.8,1)))

finalGrade <- function(mainData, fileName, rfmName, quantileData) {
  
  forLength <- dim(mainData)[1]
  
  results <- rep(0, forLength)
  
  
  for (i in 1:forLength) {
    
    data <- eval(parse(text=paste0(fileName,"$",rfmName)))[i]
    
    if (data >= quantileData[1] && data < quantileData[2] ) {
      results[i] <- "E"
    } else if (data >= quantileData[2] && data < quantileData[3]) {
      results[i] <- "D"
    } else if (data >= quantileData[3] && data < quantileData[4]) {
      results[i] <- "C"
    } else if (data >= quantileData[4] && data < quantileData[5]) {
      results[i] <- "B"
    } else { results[i] <- "A" }
  }
  
  return(results)
}

userRFM$grade <- finalGrade(userRFM, "userRFM", "score", quantS )
head(userRFM)
str(userRFM)


