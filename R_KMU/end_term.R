# 우리나라를 방문한 해외 관광객의 로밍통화 log 데이터(Call Data Records, Call Detail Records)
# 데이터의 한행은 통화나 SMS가 한 통화 수신 혹은 발신되었음을 의미.

#----------------------------------------------------------------
# 0. load data

setwd("/Volumes/MacHDD/workspace/R_Study/R_KMU")

cdrData <- read.csv("trainData_CDR.csv", header = T, stringsAsFactors = F)
summary(cdrData)
head(cdrData)

require(dplyr)
require(reshape2)


#----------------------------------------------------------------
# 1. 우리나라를 가장 많이 방문한 나라의 이름은? 1등과 2등 국가의 방문객수 차이는?

# touristID 를 기준으로 묶은 후, nation 별로 집계
sumByNation <- cdrData %>%
    group_by(touristID, nation) %>%
    summarize(N=n()) %>%
        group_by(nation) %>%
        summarize(Total=n()) %>%
        arrange(desc(Total))

# Answer
sumByNation                                     # 1위 China, 2위 Japan
sumByNation$Total[1] - sumByNation$Total[2]     # 1,2위 방문객수 차이 = 9,312


#----------------------------------------------------------------
# 2.해외여행객들의 통화건수가 가장 많은 요일은?

# ***** 방법 1 *****
cdrData$weekDay <- weekdays.Date(strptime(cdrData$dateChar, format = "%Y/%m%d"))
head(cdrData)

cdrData %>%
    group_by(weekDay) %>%
    summarize(Total=n())  %>%
    arrange(desc(Total))

# Answer : 금요일 32,217건


# ***** 방법 2 *****
data_Q2 <- df_cdr
head(data_Q2)

# dateChar를 date 형식으로 변경. date 형식은 dplyr 적용 안됨!!!
# 해당 날짜의 요일. 숫자 factor를 문자로 변경
data_Q2$date <- strptime(data_Q2$dateChar, format = "%Y/%m%d")
data_Q2$wday <- data_Q2$date$wday     # week : 0 = Sunday
data_Q2$wday <- factor(data_Q2$wday, 
                       levels = c(0:6), labels = c("일","월","화","수","목","금","토"), 
                       ordered = T)

# 요일별 통화건수 집계 후 정렬
sort(table(data_Q2$wday), decreasing = T) 


#----------------------------------------------------------------
# 3.해외여행객들의 통화건수가 가장 많은 시간대는? 

# ***** 방법 1 *****
cdrData %>%
    group_by(Time=substr(timeChar,1,2)) %>%
    summarize(Total=n())  %>%
    arrange(desc(Total))

# Answer : 17시 18,248건


# ***** 방법 2 *****
# timeChar 첫 두글자가 시간
data_Q2$callTime <- substr(data_Q2$timeChar, 1, 2)

# 시간대별 집계 후 정렬
sort(table(data_Q2$callTime), decreasing = T) 


#----------------------------------------------------------------
# 4.국내행정구역별로 해외방문객의 국가별 통화건수 비중을 데이터프레임으로 작성.

summary1 <-cdrData %>%
    group_by(city, nation) %>%
    summarize(Total=n())

summary1

# 행은 city, 열은 nation
summary2 <- dcast(summary1, city ~ nation, fun.aggregate=sum, value.var="Total")

# 각 city별로 합계와 비중. 
summary2$Total <- apply(summary2[-1], 1, sum)
totalCount <- sum(summary2$Total)
summary2$Ratio <- round(summary2$Total / totalCount * 100,2)
summary2

# Answer : 비중을 기준으로 정렬
df_AreaNation <- summary2[order(-summary2$Total),]
df_AreaNation


#----------------------------------------------------------------
# 5.국내를 방문한 해외 관광객 DB 생성. 해외 방문 고객 DB를 만드는 코드와 그 결과물 제출.
# - touristID : 해외방문객의 unique한 touristID
# - totalCall : 총통화건수
# - callDays : 총통화발생일수 (여러번의 통화가 있더라도 같은 날이면 하루로 환산)
# - visitAreas : 국내 방문지역 수 (행정구역 기준)
# 
# dplyr 패키지를 적절한 group_by를 통해 각각의 필드에 해당하는 데이터를 추출한 후,  join으로 DB를 구성.

# 5-1. totalCall
callCount <- cdrData %>%
    group_by(touristID) %>%
    summarize(totalCall=n())

head(callCount);nrow(callCount)

# 5-2. callDays
dayCount <- cdrData %>%
    group_by(touristID, dateChar) %>%
    summarize(N=n())  %>%
        group_by(touristID) %>%
        summarize(callDays=n())

head(dayCount);nrow(dayCount)

# 5-3. visitAreas
areaCount <- cdrData %>%
    group_by(touristID, city) %>%
    summarize(N=n())  %>%
    group_by(touristID) %>%
    summarize(visitAreas=n())

head(areaCount);nrow(areaCount)

# 5-4. 위 데이터 join
require(plyr)
touristsData <- join(callCount, dayCount)
touristsData <- join(touristsData, areaCount)

head(touristsData)
nrow(touristsData)

# Answer
write.csv(touristsData, "touristsData_callSummary.csv")

