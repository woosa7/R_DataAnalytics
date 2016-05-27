#-------------------------------------------------------------------------
# Capital Bike Share
#-------------------------------------------------------------------------

setwd("c:/R_Study/Capital_Bike_Share")

#-------------------------------------------------------------------------
# 1. csv 파일 목록

dummy <- dir()
dummyCsv <- which(substr(dummy, nchar(dummy)-2, nchar(dummy)) == "csv")
loadlist <- dummy[dummyCsv]
loadlist


#-------------------------------------------------------------------------
# 2. csv 파일 내용을 dataframe에 넣기

for (i in seq_along(loadlist)) {
    assign(paste0("bikedata_",i), read.csv(loadlist[i], stringsAsFactors = F))
}


#-------------------------------------------------------------------------
# 3. data 구조 비교

for (i in 2:length(loadlist)) {
    boolEqual <- all.equal(names(get("bikedata_1")), names(get(paste0("bikedata_",i))))
    if (boolEqual == TRUE) {
        cat("bikedata", i, " --- all equal", rep("\n",1))
    } else {
        cat("bikedata", i, " : different", rep("\n",1)) 
    }
}

# bikedata_1 colnames : Duration / Start.date / End.date / Start.station / End.station / Bike / Member.Type

totalCount <- 0
for (i in 1:length(loadlist)) {
    cat("bikedata_", i, " : ", colnames(get(paste0("bikedata_",i))), rep("\n",1))
    totalCount <- totalCount + nrow(get(paste0("bikedata_",i)))
}
totalCount


#-------------------------------------------------------------------------
# 4. 각 dataframe 컬럼 순서와 갯수를 bikedata_1과 동일하게 만들기

bikedata_13 <- bikedata_13[c(1,2,4,3,5,6,7)]    # 13 ~ 19 : End.date <-> Start.Station 위치 변경
bikedata_14 <- bikedata_14[c(1,2,4,3,5,6,7)]        
bikedata_15 <- bikedata_15[c(1,2,4,3,5,6,7)]
bikedata_16 <- bikedata_16[c(1,2,4,3,5,6,7)]
bikedata_17 <- bikedata_17[c(1,2,4,3,5,6,7)]
bikedata_18 <- bikedata_18[c(1,2,4,3,5,6,7)]
bikedata_19 <- bikedata_19[c(1,2,4,3,5,6,7)]

bikedata_20 <- bikedata_20[c(1,2,3,5,7,8,9)]    # 20 ~ 22 : Start.station.number & End.station.number 컬럼 제거
bikedata_21 <- bikedata_21[c(1,2,3,5,7,8,9)]
bikedata_22 <- bikedata_22[c(1,2,3,5,7,8,9)]

tail(bikedata_19)
tail(bikedata_22)


#-------------------------------------------------------------------------
# 5. 각 data 컬럼명 통일

commonColnames <- c("Duration", "Start.date", "End.date", "Start.station", "End.station", "Bike", "Member.Type")

colnames(bikedata_1) <- commonColnames
colnames(bikedata_2) <- commonColnames
colnames(bikedata_3) <- commonColnames
colnames(bikedata_4) <- commonColnames
colnames(bikedata_5) <- commonColnames
colnames(bikedata_6) <- commonColnames
colnames(bikedata_7) <- commonColnames
colnames(bikedata_8) <- commonColnames
colnames(bikedata_9) <- commonColnames
colnames(bikedata_10) <- commonColnames
colnames(bikedata_11) <- commonColnames
colnames(bikedata_12) <- commonColnames
colnames(bikedata_13) <- commonColnames
colnames(bikedata_14) <- commonColnames
colnames(bikedata_15) <- commonColnames
colnames(bikedata_16) <- commonColnames
colnames(bikedata_17) <- commonColnames
colnames(bikedata_18) <- commonColnames
colnames(bikedata_19) <- commonColnames
colnames(bikedata_20) <- commonColnames
colnames(bikedata_21) <- commonColnames
colnames(bikedata_22) <- commonColnames

tail(bikedata_19)
tail(bikedata_22)


#-------------------------------------------------------------------------
# 6. 날짜 포맷 변경 후 Duration 계산

funcTransDateTime <- function(x) {
    x$Start.date <- strptime(x$Start.date, format = "%m/%d/%Y %H:%M")       # 시작일시
    x$End.date   <- strptime(x$End.date, format = "%m/%d/%Y %H:%M")         # 종료일시
    x$Duration   <- as.numeric(x$End.date - x$Start.date, unit = "mins")    # 이용시간(분)
    
    return(x)   
}

bikedata_1 <- funcTransDateTime(bikedata_1)
bikedata_2 <- funcTransDateTime(bikedata_2)
bikedata_3 <- funcTransDateTime(bikedata_3)
bikedata_4 <- funcTransDateTime(bikedata_4)
bikedata_5 <- funcTransDateTime(bikedata_5)
bikedata_6 <- funcTransDateTime(bikedata_6)
bikedata_7 <- funcTransDateTime(bikedata_7)
bikedata_8 <- funcTransDateTime(bikedata_8)
bikedata_9 <- funcTransDateTime(bikedata_9)
bikedata_10 <- funcTransDateTime(bikedata_10)
bikedata_11 <- funcTransDateTime(bikedata_11)
bikedata_12 <- funcTransDateTime(bikedata_12)
bikedata_13 <- funcTransDateTime(bikedata_13)
bikedata_14 <- funcTransDateTime(bikedata_14)
bikedata_15 <- funcTransDateTime(bikedata_15)
bikedata_16 <- funcTransDateTime(bikedata_16)
bikedata_18 <- funcTransDateTime(bikedata_18)
bikedata_19 <- funcTransDateTime(bikedata_19)
bikedata_20 <- funcTransDateTime(bikedata_20)
bikedata_21 <- funcTransDateTime(bikedata_21)
bikedata_22 <- funcTransDateTime(bikedata_22)

# bikedata_17 날짜 포맷 다름
bikedata_17$Start.date <- strptime(bikedata_17$Start.date, format = "%Y-%m-%d %H:%M")       # 시작일시
bikedata_17$End.date   <- strptime(bikedata_17$End.date, format = "%Y-%m-%d %H:%M")         # 종료일시
bikedata_17$Duration   <- as.numeric(bikedata_17$End.date - bikedata_17$Start.date, unit = "mins")    # 이용시간(분)

tail(bikedata_17)
tail(bikedata_22)

#-------------------------------------------------------------------------
# 7. 년도_분기 컬럼 만들기

# 파일 하나가 한 분기이므로 첫 row만 체크
funcExtractQuarter <- function(x) {
    yearStr <- as.character( as.numeric(format(x[1,]$Start.date, '%Y')))
    
    mon <- as.numeric(format(x[1,]$Start.date, '%m'))
    monthStr <- NULL
    if (mon>=1 && mon<=3)  monthStr <- "Q1"
    else if (mon>=4 && mon<=6)  monthStr <- "Q2"
    else if (mon>=7 && mon<=9)  monthStr <- "Q3"
    else  monthStr <- "Q4"
    
    Quarter <- paste(yearStr,"-",monthStr, sep="")
    
    return(Quarter)   
}

bikedata_1$Quarter <- funcExtractQuarter(bikedata_1)
bikedata_2$Quarter <- funcExtractQuarter(bikedata_2)
bikedata_3$Quarter <- funcExtractQuarter(bikedata_3)
bikedata_4$Quarter <- funcExtractQuarter(bikedata_4)
bikedata_5$Quarter <- funcExtractQuarter(bikedata_5)
bikedata_6$Quarter <- funcExtractQuarter(bikedata_6)
bikedata_7$Quarter <- funcExtractQuarter(bikedata_7)
bikedata_8$Quarter <- funcExtractQuarter(bikedata_8)
bikedata_9$Quarter <- funcExtractQuarter(bikedata_9)
bikedata_10$Quarter <- funcExtractQuarter(bikedata_10)
bikedata_11$Quarter <- funcExtractQuarter(bikedata_11)
bikedata_12$Quarter <- funcExtractQuarter(bikedata_12)
bikedata_13$Quarter <- funcExtractQuarter(bikedata_13)
bikedata_14$Quarter <- funcExtractQuarter(bikedata_14)
bikedata_15$Quarter <- funcExtractQuarter(bikedata_15)
bikedata_16$Quarter <- funcExtractQuarter(bikedata_16)
bikedata_17$Quarter <- funcExtractQuarter(bikedata_17)
bikedata_18$Quarter <- funcExtractQuarter(bikedata_18)
bikedata_19$Quarter <- funcExtractQuarter(bikedata_19)
bikedata_20$Quarter <- funcExtractQuarter(bikedata_20)
bikedata_21$Quarter <- funcExtractQuarter(bikedata_21)
bikedata_22$Quarter <- funcExtractQuarter(bikedata_22)

tail(bikedata_17)
tail(bikedata_22)


#-------------------------------------------------------------------------
# 8. 다시 전체 dataframe 구조 비교

for (i in 2:22) {
    boolEqual <- all.equal(names(get("bikedata_1")), names(get(paste0("bikedata_",i))))
    if (boolEqual == TRUE) {
        cat("bikedata", i, " --- all equal", rep("\n",1))
    } else {
        cat("bikedata", i, " : different", rep("\n",1)) 
    }
}


#-------------------------------------------------------------------------
# 9. 전체 데이터 합치기

bike_df <- rbind(bikedata_1,bikedata_2,bikedata_3,bikedata_4,bikedata_5,bikedata_6,bikedata_7,
                 bikedata_8,bikedata_9,bikedata_10,bikedata_11,bikedata_12,bikedata_13,bikedata_14,
                 bikedata_15,bikedata_16,bikedata_17,bikedata_18,bikedata_19,bikedata_20,bikedata_21,bikedata_22)
nrow(bike_df)

# Subscriber / Member --> Registered
bike_df$Member <- ifelse( bike_df$Member.Type == "Subscriber", "Registered", 
                          ifelse(bike_df$Member.Type == "Member", "Registered", bike_df$Member.Type) )

save(bike_df, file="BikeShareTotal.RData")

rm(list=ls())                 # 메모리의 모든 객체 삭제


#---------------------------------------------------------------------------------------------
# 10. 데이터 분석

load("BikeShareTotal.RData")
head(bike_df)
tail(bike_df)


windows(800, 600, pointsize = 12)   # 별도의 윈도우 열기


# 10-1. 분기별 사용시간 (Line Chart)

Quarter_data <- aggregate(Duration ~ Quarter, data = bike_df, FUN = sum)
labels <- Quarter_data$Quarter
xvalues <- Quarter_data$Duration/60
max(xvalues)

plot(xvalues, xlab="", ylab="", ylim=c(0,350000), axes=FALSE, type="o", col="red", 
     main="Usage per Quarter (hour)")
axis(1, at=1:length(labels), lab=labels, las=2) # x
axis(2, las=1)                                  # y


# 10-2. 분기별 회원타입별 사용시간 (Line Chart)

df_Register <- bike_df[which(bike_df$Member == "Registered"),]
head(df_Register)

df_Casual <- bike_df[which(bike_df$Member == "Casual"),]
head(df_Casual)

data_R <- aggregate(Duration ~ Quarter, data = df_Register, FUN = sum)
x_R <- data_R$Duration/60
max(x_R)

data_C <- aggregate(Duration ~ Quarter, data = df_Casual, FUN = sum)
x_C <- data_C$Duration/60
max(x_C)

labels <- data_R$Quarter

plot(x_R, xlab="", ylab="", ylim=c(0,200000), axes=FALSE, type="o", col="red", 
     main="Usage per Quarter by MemberType (hour)")
axis(1, at=1:length(labels), lab=labels, las=2) # x
axis(2, las=1)                                  # y

lines(x_C, col="blue", type="o")       
colors <- c("red","blue")
legend(5, 200000, c("Registered","Casual"), cex=0.8, col=colors, lty=1, lwd=3)


# 10-3. 회원타입별 이용시간 / 이용횟수 비율

# 이용시간
usageHour_R <- sum(df_Register$Duration) / 60
usageHour_C <- sum(df_Casual$Duration) / 60
data03 <- c(usageHour_R, usageHour_C)

f_members <- c("Registered","Casual")
f_ratio <- round(data03/sum(data03)*100, 1)
f_labels <- paste(f_members, "\n", f_ratio, "%")

pie(data03, init.angle = 90, col=c("red","green"), radius = 1, labels = f_labels, 
    main = "Usage (In = Count / Out = Hour )")


# 이용횟수
par(new = T)   # 그래프 위에 다른 그래프를 겹쳐 그린다.

usageCount_R <- nrow(df_Register) / 10000
usageCount_C <- nrow(df_Casual) / 10000
data04 <- c(usageCount_R, usageCount_C)

f_ratio <- round(data04/sum(data04)*100, 1)
f_labels <- paste(f_members, "\n", f_ratio, "%")

pie(data04, init.angle = 90, col=c("pink","yellow"), radius = 0.5, labels = f_labels)

