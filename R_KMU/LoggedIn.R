#-----------------------------------------------------------
# apply, lapply, sapply, by, tapply, aggregate

# apply     : 결과를 vector로 반환 (1 행 / 2 열)
# lapply    : 결과를 list로 반환
# sapply    : 결과를 vector 또는 matrix로 반환
#-----------------------------------------------------------

s1 <- c(91, 87, 95, 96, 89, 87, 86, 85, 92, 93)
s2 <- c(89, 86, 85, 92, 93, 91, 90, 95, 87, 89)
s3 <- c(89, 86, 78, 99, 95, 87, 89, 86, 85, 92)


# list
score <- list(korean = s1, english = s2, math = s3)
score

lapply(score, mean)   # ---> list
sapply(score, mean)   # ---> vector
sapply(score, range)

sapply(score, t.test)


# matrix
score <- c(s1, s2, s3)
dim(score) <- c(3, 10)
colnames(score) <- c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10")
rownames(score) <- c("K", "L", "M")
score

apply(score, 1, mean)
apply(score, 2, max)


# dataframe

# Case 1.
data <- as.data.frame(score)
data$name <- c("K", "L", "M")
data

apply(data, 1, mean)   # error : 각 열의 데이터타입이 다름

df <- data[-11]
df

apply(df, 1, mean)
apply(df[3:5], 2, mean)

lapply(df, mean)
sapply(df, mean)


# Case 2.
df2 <- data.frame(score=c(s1, s2, s3))
df2$name <- c("K", "L", "M")
df2

by(df2$score, df2$name, mean)

tapply(df2$score, df2$name, mean)

aggregate(score ~ name, data = df2, mean)



#------------------------------------------------------------
# 데이터 정렬

# sort 순서를 바꿈
# rank 순위를 번호로 지정 (1 최상위)
# order 각 값의 "위치"를 순위대로 리턴
#------------------------------------------------------------

mtcars
str(mtcars)
head(mtcars)

aggregate(mpg ~ cyl, data = mtcars, mean)

mtcars[which(mtcars$cyl == 4), ]

# Transmission (am) / Number of forward gears (gear) 별 연비 평균은?

# cyl == 4 인 것들을 mpg 가 큰 것부터 정렬
selectCar <- mtcars[mtcars$cyl == 4, ]
selectCar <- selectCar[order(selectCar$mpg, decreasing = T), ]
selectCar



#------------------------------------------------------------
# subset : dataframe 에서 조건에 맞는 데이터를를 dataframe으로 추출
#------------------------------------------------------------

subset(mtcars, cyl == 4)
subset(mtcars, wt > 5.0)
subset(mtcars, select = c(mpg, cyl, wt), subset = gear > 4)
subset(mtcars, select = -mpg)

library(MASS)
str(Cars93)

subset(Cars93, select = c(Model, Type, Price), MPG.city > 30)

subset(Cars93, select = c(Manufacturer, Model, Type, Price, Make), 
       MPG.highway > median(MPG.highway) & Manufacturer == "Subaru")



#------------------------------------------------------------
# Quiz_9
#------------------------------------------------------------


#------------------------------------------------------------
# 09_RFM
#------------------------------------------------------------

#------------------------------------------------------------
# end_term
#------------------------------------------------------------

#------------------------------------------------------------
# plot - R_JS 폴더 참조
#------------------------------------------------------------

