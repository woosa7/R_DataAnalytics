################################################################
#
# R을 활용한 통계분석 : 정여진 교수 (2016 여름특강)
#
# 2. 평균에 대한 추론
#
################################################################

# 추정(estimation) : 표본을 통해 모집단 특성이 어떠한가 추측
# 가설검정(testing hypothesis) : 모집단 실제 값이 얼마나 되는가 하는 주장과 관련해
#                            표본이 가지고 있는 정보를 이용해 가설이 올바른지 판정

#---------------------------------------------------------------
# 구간 추정
#---------------------------------------------------------------

# 평균에 대한 추정 : 관심이 되는 변수가 '양적변수'인 경우

library(reshape)
attach(tips)
summary(tips)
str(tips)

# --- 한 레스토랑에 오는 손님들의 평균 결제금액은 얼마인가? = 19.79
# --- 팁은 평균적으로 얼마나 주나? = 2.998
# --- 평균적으로 몇 명이 한 팀으로 식사를 하는가? = 2.57

# 레스토랑의 손님 244개 팀의 평균 결제금액은 $19.79 (점추정)
# --- 전체 손님의 평균도 19.79일까? 다른 표본이 추출되면?

# 구간추정 = 점추정치를 기준으로 일정구간을 만들어 그 구간안에 모수가 포함될 가능성을 높이는 것.
# --- 모분산이 알려져 있는 경우 : 표준정규분포를 따르는 Z 통계량 이용
# --- 모분산을 모르는 경우 : 자유도가 n-1인 t-분포를 따르는 T 통계량 이용

t.test(tips$total_bill)

# 95 percent confidence interval:
#     18.66333 20.90855             --> 95% 신뢰구간 = 18.66 ~ 20.91
# sample estimates: mean of x 
#     19.78594                      --> 점추정값
# t = 34.717, df = 243, p-value < 2.2e-16 --> p-value < 0.05 이기 때문에 추정값이 유의미하다


#데이터 개수가 많아지면 conf.lev이 작아져도 나쁘지 않음
t.test(tips$total_bill, conf.level = 0.99)   # 99% 신뢰구간


#---------------------------------------------------------------
# 단일 집단 모평균에 대한 검정 (One sample t-test)
#---------------------------------------------------------------

# One sample t-test : 모집단의 평균이 어떤 특정한 값과 같은지를 검증

# (1) 귀무가설, 대립가설 설정
# (2) 가정 체크
# (3) 검정통계량과 p-value 계산 (t-검정 / t-test)
# (4) 결론

# 2006년 한국인 1인 1일 평균 알콜 섭취량 : 8.1g
# 2008년 무작위 조사. 1일 평균 알콜 섭취량이 늘어났는가?

# Case 1.
x <- c(15.5, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.5, 0, 4.97)
summary(x)

# Case 2.
xx <- c(15.5, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.5, 0, 4.97, 10.55, 20.1, 9.99, 17.97, 8.05, 13.13, 15.78)
summary(xx)

# (1) 귀무가설 : 2008년 평균 알콜 섭취량은 8.1g 이다. (늘지 않았다)

# (2) 가정 체크
#     --- 자료가 정규분포를 따르는지.
#     --- 심하게 편중되거나 극단치가 있는 경우 표본수가 50개 (또는 30개) 이상인지.

boxplot(x)   # 극단치 없음
hist(x)
qqnorm(x)
qqline(x)    # 어느 정도 정규분포를 따른다.

boxplot(xx)
hist(x)
qqnorm(xx)
qqline(xx)

# Shapiro-Wilk normality test
# P-value > 0.05 : 데이터가 정규분포를 따른다고 결정 --> t-test 진행
shapiro.test(x)
shapiro.test(xx)

# (3) T-검정

t.test(x, mu=8.1)
# t = 0.653, df = 9, p-value = 0.5301
# mean of x : 9.181 

t.test(xx, mu=8.1)   
# t = 2.276, df = 16, p-value = 0.03695
# mean of x : 11.02235 

# (4)결론

# Case 1.
# 모평균이 8.1일 때, 표본평균이 9.18이 나올 가능성은 0.53
# p-value > 0.05 이므로 귀무가설 기각할 수 없음.
# 95% 신뢰구간 : 5.436 ~ 12.926

# Case 2.
# p-value < 0.05 이므로 귀무가설 기각 --> 2008년 평균 알콜 섭취량이 증가했다

# --------------------------------
# = : 양측검정 / > or < : 단측검정
t.test(x, mu=8.1, alternative = "greater")
t.test(x, mu=8.1, alternative = "less")


#---------------------------------------------------------------
# p-value / 유의수준(a)
#---------------------------------------------------------------

# p-value

# 귀무가설이 옳다면, 이런 데이터 패턴이 관찰될 가능성
# p-value가 작으면 귀무가설이 틀렸을 거라고 합리적으로 의심.

# ??? 위약을 투약한 환자의 49%, 신약을 투약한 환자의 91%가 호전
#   : 신약이 심장병에 효과가 없는데(귀무가설) 위의 결과를 얻을 확률은?
# ??? 자폐증 어린이 59명의 뇌가 그렇지 않은 어린이 38명에 비해 최대 10% 이상 큼
#   : 두 집단의 뇌크기에 실제로 차이가 없는데 표본집단에서 뇌크기의 차이가 관찰될 확률은?





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





