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
# 한 그룹의 평균        : One sample T-test
# 두 그룹의 평균        : Two sample T-test
# 세 그룹 이상의 평균   : ANOVA (분산분석)

# 한 그룹의 두 변수     : Paired T-test
# 연관성 높은 두 그룹   : Paired T-test

# 한 그룹의 비율        : prop.test (Z-검정)
# 두 그룹의 비율        : prop.test

# --- 비모수적 방법 (자료가 정규분포를 따르지 않는 경우)
# One sample / Paired T-test    : Wilcoxon Signed-Rank Test
# Two sample T-test             : Wilcoxon Rank-Sum Test
# ANOVA                         : Kurskal-Wallis Test

# 정규성 검정           : shapiro.test
#---------------------------------------------------------------


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
# 한 그룹의 평균에 대한 T-검정 (One sample T-test)
#---------------------------------------------------------------

# 한 모집단의 평균이 어떤 특정한 값과 같은지 검증

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


# (1) 귀무가설 : mu = 8.1 (늘지 않았다)
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

# Shapiro-Wilk normality test (정규성 검정)
# P-value > 0.05 : 귀무가설(정규분포를 따른다) 기각 못함 --> t-test 진행
shapiro.test(x)
shapiro.test(xx)


# (3) T-검정

t.test(x, mu = 8.1)
    # t = 0.653, df = 9, p-value = 0.5301
    # mean of x : 9.181 

t.test(xx, mu = 8.1)   
    # t = 2.276, df = 16, p-value = 0.03695
    # mean of x : 11.02235 


# (4) 결론

# Case 1.
# 모평균이 8.1일 때, 표본평균이 9.18이 나올 가능성은 0.53
# p-value > 0.05 이므로 귀무가설 기각할 수 없음.
# 95% 신뢰구간 : 5.436 ~ 12.926

# Case 2.
# p-value < 0.05 이므로 귀무가설 기각 --> 2008년 평균 알콜 섭취량이 증가했다


#--------------------------------
t.test(x, mu = 8.1)                             # Ha : mu <> 8.1
t.test(x, mu = 8.1, alternative = "greater")    # Ha : mu > 8.1
t.test(x, mu = 8.1, alternative = "less")       # Ha : mu < 8.1


#--------------------------------
# t-test 결과물

out <- t.test(x, mu = 8.1)
names(out)

out$statistic
out$p.value


#---------------------------------------------------------------
# p-value

# 귀무가설이 옳다면, 이런 데이터 패턴이 관찰될 가능성
# p-value가 작으면 귀무가설이 틀렸을 거라고 합리적으로 의심.

# 위약을 투약한 환자의 49%, 신약을 투약한 환자의 91%가 호전
#   : 신약이 심장병에 효과가 없는데(귀무가설) 위의 결과를 얻을 확률은?
# 자폐증 어린이 59명의 뇌가 그렇지 않은 어린이 38명에 비해 최대 10% 이상 큼
#   : 두 집단의 뇌크기에 실제로 차이가 없는데 표본집단에서 뇌크기의 차이가 관찰될 확률은?

#---------------------------------------------------------------
# 유의수준(a)

# 귀무가설이 옳은데 기각하는 오류(1종 오류)를 범할 확률
# 귀무가설을 기각하기 위한 p-value의 임계치로 사용

# 1종 오류와 2종 오류의 상충관계
# (귀무가설 = 효과가 없다 일 경우)
# a가 너무 크면 효과가 없는 약을 효과가 있다고 잘못 판단 (1종 오류)
# a가 너무 작으면 효과가 있는 약을 승인하지 않을 수 있음 (2종 오류)
# 상황에 따라 어느 오류를 줄이는 것이 중요한가 (유의수준 조정) 판단 필요



#---------------------------------------------------------------
# 두 그룹의 평균에 대한 독립표본 T-검정 (Two sample T-test)
#---------------------------------------------------------------

# 줄기세포 이용한 배양치아 제작
# 다른 두 조건(control, test)에서 배양된 뼈세포수(resp) 측정 후 비교
# 범주형 변수와 양적 변수

# (1) 귀무가설 H0 : mu1 = mu2

# (2) 가정 체크
# --- 두 집단 모두 정규분포를 따른다.
# --- 정규분포를 따르지 않아도 관측치가 충분히 많다면(n1 + n2 > 30) 독립표본 T-검정 사용 가능.

dental <- read.csv("dental.csv")
dental

library(psych)
describeBy(dental, group = dental$treatment)

# test군의 분산이 control군의 분산보다 크고, 두 그룹 모두 편향되어(skew) 있다.
# log 변환을 통해 분산 차이를 좁히고, 편향도를 낮춘다.
# 많은 경우 실제 데이터는 편향된 분포를 가진다.
# --> 여러 통계기법들은 정규분포를 가정하므로 분석 전 변수 변환이 필요한지 체크한다.

par(mfcol=c(1,2))
boxplot(resp ~ treatment, data = dental, col = "red", ylab = "resp")
boxplot(log(resp) ~ treatment, data = dental, col = "blue", ylab = "log(resp)")
par(mfcol=c(1,1))

# T-test
# One Sample T-test : T = 자료의 평균 x_bar가 모집단 평균 mu로부터 떨어진 상대적인 거리.
# Two Sample T-test : T = (x1_bar - x2_bar) / root(Var(x1_bar - x2_bar)). 두 자료의 평균이 서로 떨어진 상대적인 거리.

# 등분산검정 : var.test
# 두 집단의 분산이 같으면 t.test( , var.equal = T) --> 귀무가설
# 두 집단의 분산이 다르면 t.test()

var.test(log(resp) ~ treatment, data = dental)   # p-value > 0.05 : 귀무가설 인정. 분산이 같다.
var.test(resp ~ treatment, data = dental)        # p-value < 0.05 : 귀무가설 기각.


# (3) T-검정

t.test(log(resp) ~ treatment, var.equal = T, data = dental)
# Case 1
# p-value = 0.03571 < 0.05 : 귀무가설 기각.
# 두 그룹의 평균은 유의수준 5% 하에서 차이가 있다.

t.test(resp ~ treatment, data = dental)
# Case 2
# log 변환 전 자료로 가설검정시
# p-value = 0.08988 > 0.05 : 귀무가설 기각할 수 없다.
# 두 그룹의 평균은 유의수준 5% 하에서 차이가 없다.
# 95% 신뢰구간에 0 이 포함됨 (-122.239 ~ 12.639)



#---------------------------------------------------------------
# 두 그룹의 평균에 대한 쌍체표본 T-검정 (Paired T-test)
#---------------------------------------------------------------

# 쌍을 이룬 두 변수 (matched sample)의 차이를 보는 검정
# 한 집단을 대상으로 약의 복용 전후, 치료 전후, 교육방법 도입 등
# 두 집단이더라도 쌍둥이 또는 부부처럼 변수들 간의 상관관계가 존재할 때
# http://math7.tistory.com/107

# 15명의 개인, 케이블 TV 시청시간과 라디오 청취시간에 대한 자료를 비교.
# 과거 3개월간 DM 발송 유무에 따른 평균 구매금액 차이 (A/B Test)
# 디자인 변경 전/후 상품 구매자 수 증가 유무
# ---> 쌍체표본 T-검정


# Case
# 거식증 치료제 FT 복용 전후 체중변화 --> FT가 "체중 증가"에 영향이 있는지 검증
# 귀무가설 : postwt - prewt = 0
# 대립가설 : postwt - prewt > 0

ft <- read.csv("FT.csv")
ft

attach(ft)
boxplot(Postwt-Prewt)
hist(Postwt-Prewt)
qqnorm(Postwt-Prewt)
qqline(Postwt-Prewt)

shapiro.test(Postwt-Prewt)  # p-value = 0.5156 > 0.05 : 정규분포를 따른다

t.test(Postwt-Prewt, alternative = "greater")
    # p-value = 0.0003501 < 0.05 : 귀무가설 기각. 유의한 체중 중가가 있다.



#---------------------------------------------------------------
# practice 1
#---------------------------------------------------------------

# 2012 - 2013 년 국내 개봉 영화
kmovie <- read.csv("movie_utf8.csv")
View(kmovie)
str(kmovie)

unique(kmovie$rating)

# (1) 15세이상 관람가 영화의 평균 관객수를 95% 신뢰구간을 통해 추정
df <- kmovie[kmovie$rating == "15세이상관람가", ]
summary(df$total_seen)

t.test(df$total_seen)   # 95% 신뢰구간 : 1,517,279 ~ 2,674,186


# (2) 한 영화사에서 15세이상 관람가 영화의 평균 관객수가 1,500,000 보다 크다고 주장
# ---> One sample T-test

# 귀무가설 : mu = 1,500,000
# 대립가설 : mu > 1,500,000
boxplot(df$total_seen)
qqnorm(df$total_seen)
qqline(df$total_seen)

shapiro.test(df$total_seen)   # p-value = 8.782e-13 : 정규분포 따르지 않는다. but 데이터가 충분히 크므로 t-test 진행.

t.test(df$total_seen, mu = 1500000, alternative = "greater")   # p-value = 0.02183 < 0.05 : 귀무가설 기각


# (3) 한 영화사에서 15세이상 관람가 영화의 평균 관객수가 12세이상 관람가 영화의 평균관객수보다 많다고 주장
# ---> Two sample T-test

# 귀무가설 : 12세 - 15세 = 0
# 대립가설 : 12세 - 15세 < 0
df2 <- kmovie[kmovie$rating == "15세이상관람가" | kmovie$rating == "12세이상관람가", c("rating", "total_seen")]
df2$rating <- factor(df2$rating, labels = c("12세", "15세"))
df2

boxplot(total_seen ~ rating, df2)       # 이상치 많고, 편향되어 있으므로 변수 변환
boxplot(log(total_seen) ~ rating, df2)

var.test(log(total_seen) ~ rating, df2) # 등분산검정 : p-value > 0.05 분산이 같다.

t.test(log(total_seen) ~ rating, df2, var.equal=T, alternative = "less") # 12세 - 15세
# p-value = 0.3881 > 0.05 : 귀무가설을 기각할 수 없다.
# 15세이상 관람가 영화의 평균 관객수가 12세 관람가 영화보다 많다고 할 수 없다.



#---------------------------------------------------------------
# practice 2
#---------------------------------------------------------------

# 대기업의 2002년 주당이익 자료.
# 2002년 이전에 애널리스트들이 이들 대기업에 대한 주당이익을 예측

earning <- read.csv("earnings.csv")
earning

attach(earning)

# (1) 실제 평균 주당이익과 추정 평균 주당이익에 대한 기술 통계
library(psych)
describe(earning[ , c(2,3)])
summary(earning)

# (2) 실제 모집단 평균 주당이익과 추정 모집단 평균 주당이익 간의 차이에 대하여 가설검정을 a = 0.05에서 수행
# --> Paired T-test

# 귀무가설 : Actual - Predicted = 0

boxplot(Actual - Predicted)
qqnorm(Actual - Predicted)
qqline(Actual - Predicted)

shapiro.test(Actual - Predicted)  # p-value = 0.6816

t.test(Actual - Predicted)   # p-value = 0.5602 : 귀무가설 기각할 수 없음. 실제와 추정의 차이가 크지 않다.

# 두 평균의 차이에 대한 점 추정치 : mean of x = -0.103 
# 해당 기업의 주당 이익에 대해 약간 과대평가.



#---------------------------------------------------------------
# Wilcoxon Signed-Rank Test
#---------------------------------------------------------------

# One sample / Paired T-test 에 해당하는 비모수적 방법








