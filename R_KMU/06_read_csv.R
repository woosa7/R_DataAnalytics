###########################
# KMU : Lecture
###########################

dummy <- read.csv("data/2016-Q1-Trips-History-Data.csv", stringsAsFactor=F, nrows=1000)
dim(dummy)
str(dummy)
head(dummy)


Q1_16 <- read.csv("data/2016-Q1-Trips-History-Data.csv", stringsAsFactor=F)
summary(Q1_16)

object.size(Q1_16)/1024^2

head(Q1_16[,1:5])
tail(Q1_16)
Q1_16[300000,]


# 가장 오랜 시간 이동
x <- which.max(Q1_16$Duration..ms.)    
Q1_16[x,]


# best bike number
Q1_16$cnt <- 1
x <- tapply(Q1_16$cnt, Q1_16$Bike.number, sum)
which.max(x)
hist(x)

# average use time
x <- tapply(Q1_16$Duration..ms./1000/60, Q1_16$Bike.number, mean)
head(x)

# member type
x <- tapply(Q1_16[,1]/1000/60, Q1_16$Member.Type, mean)
x

save(Q1_16, file = "Q1_16_2016_bike.RData")
save(x, file = "bikeTimeMembertype.RData")

Q1_16[Q1_16$Member.Type == "Registered",]

write.csv(Q1_16[Q1_16$Member.Type == "Registered",], file = "registered_member.csv", row.names = F)


# 중요.... !!!!!!!!
#------------------------------------------------------------
aggregate(Duration..ms. ~ Member.Type, data=Q1_16, FUN=mean)

aggregate((Duration..ms./1000/60) ~ Member.Type, data=Q1_16, FUN=mean)

aggregate((Duration..ms./1000/60) ~ Member.Type, data=Q1_16, function(x) c(
  mean = mean(x),
  length = length(x)))
#------------------------------------------------------------




#------------------------------------------------------------
# 데이터 정렬
#------------------------------------------------------------

# sort 순서를 바꿈
# rank 순위를 번호로 지정 (1 최상위)
# order 각 값의 "위치"를 순위대로 리턴

sort(Q1_16$rideTime)
sort(Q1_16$rideTime, decreasing = T)

order(Q1_16$rideTime)
order(Q1_16$rideTime, decreasing = T)

age <- c(25, 60, 45, 19, 48, 27)
sort(age)
sort(age, decreasing = T)
rank(age)
order(age)

order(age, decreasing = T)




#------------------------------------------------------------
# 날짜
#------------------------------------------------------------
Q1_16 <- read.csv("data/2016-Q1-Trips-History-Data.csv", stringsAsFactor=F)
head(Q1_16)

Q1_16 <- Q1_16[1:10,]

Q1_16$Start.ptime <- strptime(Q1_16$Start.date, format="%m/%d/%Y %H:%M")

Q1_16$End.ptime <- strptime(Q1_16$End.date, format="%m/%d/%Y %H:%M")

# week : 0 = Sunday
Q1_16$wday <- Q1_16$Start.ptime$wday

Q1_16$wday <- factor(Q1_16$wday, 
                     levels = c(1:6,0), labels = c("월","화","수","목","금","토","일"), 
                     ordered = T)


Q1_16$hour <- Q1_16$Start.ptime$hour
Q1_16$dummy <- 1
bikehour <- tapply(Q1_16$dummy, Q1_16$hour, sum)
bikehour
plot(bikehour)

Q1_reg <- Q1_16[Q1_16$Member.Type == "Registered",]
Q1_casual <- Q1_16[Q1_16$Member.Type == "Casual",]

bikehourReg <- tapply(Q1_reg$dummy, Q1_reg$hour, sum)
bikehourCas <- tapply(Q1_casual$dummy, Q1_casual$hour, sum)

bikehourReg
bikehourCas

Q1_16



# -----------------------------------------------
# date time

as.numeric(Q1_16$dateEnd[1])

epochkor <- as.POSIXct(as.numeric(Q1_16$dateEnd[1]), origin="1970-01-01", tz="Asia/Seoul")
epochkor
as.numeric(epochkor)
epochusa <- as.POSIXlt(as.numeric(epochkor), origin="1970-01-01", tz="America/Dawson")
epochusa

pdate <- strftime(epochkor, format="%d")
pdate
pyear <- strftime(epochkor, format="%Y")
pmonth <- strftime(epochkor, format="%m")
pweek <- strftime(epochkor, format="%W")



Q1_16$dateStart <- strptime(Q1_16$Start.date,format="%m/%d/%Y %H:%M")
Q1_16$dateEnd <- strptime(Q1_16$End.date,format="%m/%d/%Y %H:%M")
Q1_16$rideTime <- (as.numeric(Q1_16$dateEnd) - as.numeric(Q1_16$dateStart))
head(Q1_16)

hist(Q1_16$rideTime)
hist(Q1_16$rideTime, breaks = 200)
fivenum(Q1_16$rideTime)

tapply(Q1_16$rideTime, Q1_16$Member.Type, sum)
names(Q1_16)
bikeAvgTime <- tapply(Q1_16$rideTime, Q1_16$Bike.number, mean)

hist(bikeAvgTime)
hist(bikeAvgTime, breaks=200)

