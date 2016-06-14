########################################
# 기말 과제
########################################
# 우리나라를 방문한 해외 관광객의 로밍통화 log 데이터
# 데이터의 한행은 통화나 SMS가 한 통화 수신 혹은 발신.


########################################
# 데이터 읽기

#setwd("/Volumes/MacHDD/workspace/R_Study/R_KMU")

df_cdr <- read.csv("trainData_CDR.csv", header = T, stringsAsFactors = F)
head(df_cdr)

library(dplyr)
library(reshape2)



########################################
# 1

# touristID가 중복되어 있으므로 group_by 한 후에 nation 별로 다시 group_by
data_Q1 <- df_cdr %>%
    group_by(touristID, nation) %>%
    summarize(N=n()) %>%
    group_by(nation) %>%
    summarize(Count=n()) %>%
    arrange(desc(Count))

# 우리나라를 가장 많이 방문한 나라의 이름은? = 1위 China, 2위 Japan
data_Q1[1:2,]                                     

# 1등과 2등 국가의 방문객수 차이는? = 9312
data_Q1$Count[1] - data_Q1$Count[2]



########################################
# 2

data_Q2 <- df_cdr
tail(data_Q2)

# dateChar를 날짜 형식으로 변경
data_Q2$date <- strptime(data_Q2$dateChar, format = "%Y/%m%d")

# 해당 날짜의 요일. 숫자 factor를 문자로 변경
data_Q2$wday <- data_Q2$date$wday     # week : 0 = Sunday

data_Q2$wday <- factor(data_Q2$wday, 
                       levels = c(0:6), labels = c("일","월","화","수","목","금","토"), 
                       ordered = T)
tail(data_Q2)

# 요일별 통화건수 집계 후 정렬
# 해외여행객들의 통화건수가 가장 많은 요일은? = 금요일
sort(table(data_Q2$wday), decreasing = T) 



########################################
# 3

# timeChar 첫 두글자가 시간
data_Q2$callTime <- substr(data_Q2$timeChar, 1, 2)

# 시간대별 집계 후 정렬
# 해외여행객들의 통화건수가 가장 많은 시간대는? = 17시
sort(table(data_Q2$callTime), decreasing = T) 



########################################
# 4

# 행정구역별, 국가별 집계
dummy <- df_cdr %>%
    group_by(city, nation) %>%
    summarize(Count=n())

dummy

# reshape2의 dcast 사용. 행은 city, 열은 nation으로 집계
data_Q4 <- dcast(dummy, city ~ nation, fun.aggregate=sum, value.var="Count")
data_Q4

# 각 city별로 합계와 비중. 
data_Q4$Total <- apply(data_Q4[-1], 1, sum)

totalsum <- sum(data_Q4$Total)
data_Q4$Ratio <- round(data_Q4$Total / totalsum * 100, 2)

# 행정구역별로 해외방문객의 국가별 통화건수 비중을 데이터프레임으로 작성.
data_Q4



########################################
# 5

# totalCall : 총통화건수
dummy1 <- df_cdr %>%
    group_by(touristID) %>%
    summarize(totalCall=n())

str(dummy1)

# callDays : 총통화발생일수 (여러번의 통화가 있더라도 같은 날이면 하루로 환산)
dummy2 <- df_cdr %>%
    group_by(touristID, dateChar) %>%
    summarize(N=n())  %>%
    group_by(touristID) %>%
    summarize(callDays=n())

str(dummy2)

# visitAreas : 국내 방문지역 수 (행정구역 기준)
dummy3 <- df_cdr %>%
    group_by(touristID, city) %>%
    summarize(N=n())  %>%
    group_by(touristID) %>%
    summarize(visitAreas=n())

str(dummy3)

# group_by를 통해 각각의 필드에 해당하는 데이터를 추출한 후,  join으로 DB를 구성.
library(plyr)

resultData <- join(dummy1, dummy2)
resultData <- join(resultData, dummy3)

head(resultData)
nrow(resultData)

# 국내를 방문한 해외 관광객 DB 생성
# touristID, totalCall, callDays, visitAreas
write.csv(resultData, "visitorData.csv")

