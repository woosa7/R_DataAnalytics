###########################
# KMU : Lecture
###########################

## Data Sampling

csvList <- dir("./dataAll/")
csvList

for(i in csvList) {
  filename <- paste0("bike",substr(i,3,4), substr(i,6,7)) 
  smplDummy <- read.csv(paste0("./dataAll/",i), stringsAsFactors=FALSE)
  

  smplIndex <- sample(1:dim(smplDummy)[1] , 50000, replace=FALSE)
  smplData <- smplDummy[smplIndex,]
  assign(paste0(filename,"_smpl"), smplData)
  
  save(list=paste0(filename,"_smpl"), file=paste0("./data_smpl/",paste0(filename,"_smpl"),".RData"))
  rm(list=paste0(filename,"_smpl"))
}


## Data Handling

csvList <- dir("./dataAll/")
csvList


## practicing first lines of csv files
# i <- csvList[1]
for(i in csvList) {
  filename <- paste0("bike",substr(i,3,4), substr(i,6,7)) 
  assign(filename, read.csv(paste0("./dataAll/",i), stringsAsFactors=FALSE, nrows=6))
  
  cat("\n",filename,"\n","\n")
  print(names(get(filename)))
  
  save(list=filename, file=paste0("./RData/",filename,".RData"))
  
}

head(bike12Q2)
head(bike14Q4)
head(bike15Q1)
head(bike15Q3)
head(bike16Q1)


# rm(smplDummy)
rdir <- dir("./RData/")
length(rdir)
obj_vol <- NULL
for (i in seq_along(rdir)) {
  obj_vol[i] <- object.size(get(substr(rdir[i],1,8)))
}
sum(obj_vol)/1024^2

rdir[19:21]
for (i in 19:21) {
  dummy <- get(substr(rdir[i],1,8))
  assign(substr(rdir[i],1,8), dummy[,c(1,2,3,5,7:9)]) 
}

rdir[12:18]
for (i in 12:18) {
  dummy <- get(substr(rdir[i],1,8))
  assign(substr(rdir[i],1,8), dummy[,c(1,2,4,3,5:7)]) 
}

for (i in seq_along(rdir)) {
  print(head(get(substr(rdir[i],1,8)),1))
}

for (i in seq_along(rdir)) {
  dummy <- get(substr(rdir[i],1,8)) 
  names(dummy) <- c("duration","start_date","end_date","start_stn","end_stn","bike_no","subs_type")
  assign(substr(rdir[i],1,8), dummy)
}

bike_data <- NULL
for (i in seq_along(rdir)) {
  bike_data <- rbind(bike_data, get(substr(rdir[i],1,8)))
}

dim(bike_data)



## Dlong full lines of csv files
# i <- csvList[1]
for(i in csvList) {
  filename <- paste0("bike",substr(i,3,4), substr(i,6,7)) 
  assign(filename, read.csv(paste0("./dataAll/",i), stringsAsFactors=FALSE))
  
  cat("\n",filename,"\n","\n")
  print(names(get(filename)))
  
  save(list=filename, file=paste0("./RData/",filename,".RData"))
  
}

head(bike12Q2)
head(bike14Q4)
head(bike15Q1)
head(bike15Q3)
head(bike16Q1)


# rm(smplDummy)
rdir <- dir("./RData/")
length(rdir)
obj_vol <- NULL
for (i in seq_along(rdir)) {
  obj_vol[i] <- object.size(get(substr(rdir[i],1,8)))
}
sum(obj_vol)/1024^2

rdir[19:21]
for (i in 19:21) {
  dummy <- get(substr(rdir[i],1,8))
  assign(substr(rdir[i],1,8), dummy[,c(1,2,3,5,7:9)]) 
}

rdir[12:18]
for (i in 12:18) {
  dummy <- get(substr(rdir[i],1,8))
  assign(substr(rdir[i],1,8), dummy[,c(1,2,4,3,5:7)]) 
}

for (i in seq_along(rdir)) {
  print(head(get(substr(rdir[i],1,8)),1))
}

for (i in seq_along(rdir)) {
  dummy <- get(substr(rdir[i],1,8)) 
  names(dummy) <- c("duration","start_date","end_date","start_stn","end_stn","bike_no","subs_type")
  assign(substr(rdir[i],1,8), dummy)
}

bike_data <- NULL
for (i in seq_along(rdir)) {
  bike_data <- rbind(bike_data, get(substr(rdir[i],1,8)))
}

dim(bike_data)


bike_data$start_date <- strptime(bike_data$start_date, format="%m/%d/%Y %H:%M")
sum(is.na(bike_data$start_date))
dummy <- is.na(bike_data$start_date)
tail(bike_data[dummy,])

head(bike14Q4)
startdummy <- strptime(bike14Q4$start_date, format="%Y-%m-%d %H:%M")
enddummy <- strptime(bike14Q4$end_date, format="%Y-%m-%d %H:%M")
startformat <- strftime(startdummy, format="%m/%d/%Y %H:%M")
head(startformat)
endformat <- strftime(enddummy, format="%m/%d/%Y %H:%M")
head(endformat)
bike14Q4$start_date <- startformat
bike14Q4$end_date <- endformat

bike_data <- NULL
for (i in seq_along(rdir)) {
  bike_data <- rbind(bike_data, get(substr(rdir[i],1,8)))
}

dim(bike_data)

bike_data$start_date <- strptime(bike_data$start_date, format="%m/%d/%Y %H:%M")
sum(is.na(bike_data$start_date))
bike_data$end_date <- strptime(bike_data$end_date, format="%m/%d/%Y %H:%M")

bike_data$mon <- strftime(bike_data$start_date, format="%b") # 월

bike_data$wday <- strftime(bike_data$start_date, format="%a") # 요일


bike_data$year <- strftime(bike_data$start_date, format="%Y") # 년도

head(bike_data)
bike_data$dummy <- 1
bike_data$duration <- as.numeric(bike_data$end_date - bike_data$start_date)

head(bike_data)
tail(bike_data)

tapply(bike_data$dummy, bike_data$subs_type, sum)
tapply(bike_data$dummy, bike_data$subs_type, sum)/sum(tapply(bike_data$dummy, bike_data$subs_type, sum))


###############
# dplyr
###############

# Data Categorization / subset --- group_by / filter
# 변수 선택 --- select
# Data Sorting --- arrange
# 수치계산 & 요약 --- summarize
# Data Transformation --- 


library(dplyr)

bike_data[,-c(2:3)] %>%
group_by(year, wday) %>%
summarize(N=n())

bike_data$wday <- factor(bike_data$wday, levels=c("월","화","수","목","금","토","일"), ordered=T) #월 숫자

bike_data[,-c(2:3)] %>%
group_by(year, wday) %>%
summarize(N=n())

wdaystat <- bike_data[,-c(2:3)] %>%
group_by(wday) %>%
summarize(N=n())

plot(wdaystat)

plot(bike_data[,-c(2:3)] %>%
filter(year == 2011) %>%
group_by(wday) %>%
summarize(N=n())
)

plot(bike_data[,-c(2:3)] %>%
filter(year %in% 2011:2015) %>%
group_by(year) %>%
summarize(N=n())
)

