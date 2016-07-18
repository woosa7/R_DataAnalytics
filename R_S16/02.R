################################################################
#
# R을 활용한 통계분석 : 정여진 교수 (2016 여름특강)
#
# 2. 평균에 대한 추론
#
################################################################

t.test(tips$total_bill)

# 95 percent confidence interval:  95% 신뢰구간
#     18.66333 20.90855
# sample estimates: mean of x 점추정

t.test(tips$total_bill, conf.level = 0.99)


#-------------------------------
# 귀무가설, 대립가설 설정
# 가정 체크
# 검정통계량, p-value 계산
# 결론

# 2006년 한국인 1인 1일 알콜 섭취량 : 8.1g
# 2008년 10명 무작위 조사
x <- c(15.5, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.5, 0, 4.97)
x
summary(x)

# 검정통계량 t-statistics
t.test(x) # default mu = 0

t.test(x, mu=8.1)   # t = 0.653, p-value = 0.5301

# p-value = 0.53
# 모평균이 8.1 인 것이 사실일 때, 표본평균이 9.18이 나올 가능성은 0.53 이므로
# 귀무가설이 사실일 가능성이 크다.
# 95% 신뢰구간 5.436 ~ 12.926

# = : 양측검정 / >, < : 단측검정
t.test(x, mu=8.1, alternative = "greater")



#-------------------------------
# exercise

movie <- read.csv("movie_MBA.csv")
head(movie)

df <- movie[movie$rating == "15세이상관람가",]
hist(df$total_seen, 20)

t.test(df$total_seen, mu=1500000) # 양측 검정
t.test(df$total_seen, mu=1500000, alternative = "greater") # 단측 검정
# p-value = 0.02183 : 0.05보다 작으므로 귀무가설 기각

movie3 <- movie[movie$rating == "15세이상관람가" | movie$rating == "12세이상관람가",]
movie3$rating <- factor(movie3$rating, labels = c("12세", "15세"))
boxplot(log(total_seen)~rating, movie3)

var.test(total_seen~rating, movie3)
t.test(total_seen~rating, movie3, alternative = "less") # 12세 - 15세

var.test(log(total_seen)~rating, movie3)
t.test(log(total_seen)~rating, movie3, var.equal=T, alternative = "less") # 12세 - 15세


#-------------------------------
dental <- read.csv("dental.csv")
dental

boxplot(log(resp)~treatment, dental, horizontal=T)

boxplot(resp~treatment,data=dental)
boxplot(log(resp)~treatment,data=dental)

var.test(log(resp)~treatment,data=dental)


t.test(log(resp)~treatment,var.equal=T,data=dental)

var.test(resp~treatment,data=dental)

FT=read.csv("FT.csv")
with(FT,boxplot(Postwt-Prewt))
with(FT,hist(Postwt-Prewt))

with(FT,t.test(Postwt-Prewt,alternative="greater"))


#-------------------------------
# exercise

earning <- read.csv("earnings.csv")
attach(earning)

boxplot(Actual-Predicted)
t.test(Actual-Predicted)





