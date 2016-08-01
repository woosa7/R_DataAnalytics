################################################################
#
# R을 활용한 통계분석 : 정여진 교수 (2016 여름특강)
#
# 6. 분산분석 (ANOVA)
#
################################################################

# 한 집단의 평균과 특정한 수 비교   : One Sample T-test
# 독립적인 두 집단의 평균 비교      : Two Sample T-test 
# 세 그룹 이상의 평균 비교          : ANOVA

#---------------------------------------------------------------
# 분산분석 (ANOVA) = 설명변수가 "범주형"인 회귀분석
#---------------------------------------------------------------

# 분산분석 in R
# --- 회귀분석과 마찬가지로 lm 명령어를 사용
# --- 설명(독립)변수가 그룹을 의미하는 범주형 변수 (Factor함수 사용하여 정의)

# y = b0 + b1*x + e   :   x가 0 또는 1을 가지는 범주형 변수라면?
# x = 0 이면 y = b0 + e
# x = 1 이면 y = b0 + b1 + e
# b1 = 0 이면 x의 두 그룹은 평균이 같다.
# 즉, H0 : mu1 = mu2  <-->  H0 : b1 = 0

# 그룹이 3개 이상이라면? x가 3개 그룹을 정의하는 질적변수라면? (서울, 부산, 제주)
# 더미 변수 (k-1) 개를 만든다.
# y = b0 + b1*x1 + b2*x2 + e

#           x1      x2  
# 서울      1       0
# 부산      0       1
# 제주      0       0

#---------------------------------------------------------------
# 영화 등급이 총관객수에 영향을 미치는가?

movie <- read.csv("movie_utf8.csv", header = T)
summary(movie)

# 독립변수 = factor
levels(movie$rating)
levels(movie$rating) <- c("12years", "15years", "All", "AdultOnly")

library(psych)
describeBy(movie$total_seen, group = movie$rating, mat = T)

# 종속변수
hist(movie$total_seen)      # 데이터가 한쪽으로 몰려있으므로 log 변환을 해준다.
hist(log(movie$total_seen))

# 회귀분석
out0 <- lm(total_seen ~ rating, movie)
plot(out0)

out <- lm(log(total_seen) ~ rating, movie)
plot(out)                   # 종속변수 변환 전보다 후에 잔차도가 안정됨

summary(out)
#--------------------------
# Result

# F-검정 (H0 : mu1 = mu2 = mu3 = mu4)
# p-value: 0.001601 < 0.05 : 등급별 총관객수는 서로 유의미한 '차이'가 있다.

# (Intercept)                   --- (b0) 12years 평균
# rating15years     0.75375     --- (b1) 15years 평균 - 12years 평균 : 차이 없음
# ratingAll         0.00628 **  --- (b2) All 평균 - 12years 평균 : 전체관람가와 12세관람가는 유의한 차이가 있다.
# ratingAdultOnly   0.09320     --- (b3) Adult - 12 years

# But, 어떤 등급끼리 차이가 있는지 알 수 없다.
# 각 변수 95% 신뢰구간 ---> 0.95 * 0.95 * 0.95 : 신뢰구간이 작아진다.
# 실제로 유의하지 않은데 유의하게 결론이 나올 수 있다.
# 그래서 t-test 대신에 Dunnett 또는 Tukey 방법론 사용 !!!
#--------------------------

# 다중비교
# Dunnett Method : reference level(기준)과 각 범주의 평균 차이 검정 
# Tukey Method : 가능한 모든 범주 쌍의 평균 차이 검정

install.packages("multcomp")
library(multcomp)

out <- lm(log(total_seen) ~ rating, movie)

# Dunnett Method
dunnet <- glht(out, linfct = mcp(rating = "Dunnett"))
summary(dunnet)

# Tukey Method
tukey <- glht(out, linfct = mcp(rating = "Tukey"))
summary(tukey)

# 추정된 회귀식
# y = 13.70 + 0.068*x1 - 0.682*x2 - 0.431*x3
# 13.70 : 12세관람가 영화의 평균 log(총관객수)
# 13.70 + 0.068 : 15세 관람가 영화의 평균 log(총관객수)



#---------------------------------------------------------------
# 특정 범주를 하나로 합쳐서 분석하고자 할 경우

movie$rating2 <- movie$rating
levels(movie$rating2)

levels(movie$rating2) <- c(2,2,1,3)     # 12세 + 15세
summary(movie$rating2)

# 가장 앞에 있는 level "2"가 reference level로 자동 설정
out2 <- lm(log(total_seen) ~ rating2, movie)
summary(out2)

# reference level을 1 (All)로 설정
movie$rating2 <- relevel(movie$rating2, ref = "1")
out2 <- lm(log(total_seen) ~ rating2, movie)
summary(out2)

# 1 (All)과 2 (12세+15세)는 평균 총관객수에 유의한 차이가 있다.
# 1 (All)과 3 (AdultOnly)은 차이가 없다.



#---------------------------------------------------------------
# 공분산분석 (ANCOVA)
#---------------------------------------------------------------

# 공분산분석 = 분산분석 + 회귀분석
# 종속변수의 변동을 설명하는데 그룹변수 이외의 다른 변인이 있을때 그 효과를 통제
# 설명(독립)변수가 질적변수와 양적변수가 함께 있는 경우

#---------------------------------------------------------------
# Case.
# 거식증 치료제 : CBT, FT, Control 3가지 치료방법
# 종속변수 : 치료 전/후 몸무게 차이 (postwt - prewt)
# 설명변수 : 치료 전 몸무게 (공변량, 통제할 변수), 치료방법 (주요 관심 설명변수)

# 분산분석 : 치료전후 몸무게 변화가 치료방법 간에 차이가 있는가 검증
# 공분산분석 : 치료 전 몸무게가 무거울수록 치료 후 몸무게 변화가 크지 않을까? 
#              이것이 치료방법 간 차이를 보는데 방해가 될 수 있는지 검증

df <- read.csv("anorexia.csv")
str(df)
summary(df)

# dummy 변수 생성시 Control 그룹을 레퍼런스로 지정하기 위해 relevel
df$Treat <- relevel(df$Treat, ref = "Cont")
summary(df)

boxplot(Prewt ~ Treat, df)
boxplot(Postwt - Prewt ~ Treat, df)

# 분산분석 : 이전 몸무게 관심 없음
out <- lm(Postwt - Prewt ~ Treat, df)
summary(out)

# 공분산 분석 : 이전 몸무게가 동일한 사람들을 기준으로 분석.
out <- lm(Postwt-Prewt ~ Prewt + Treat, df)
anova(out)
    # Prewt 통제시 Treat 변수가 설명해 주는 y의 변동성에 대한 F-test. 
    # p-value < 0.05 --> Treat 3개 그룹간 평균의 차이가 유의하다.
summary(out)
    # Prewt가 클수록 Postwt-Prewt 변화율이 적다 (음의 상관관계)
    # TreatCBT & TreatFT 모두 유의한 차이를 가짐
    # 각 범주간 평균의 차이 검증 --> Dunnett Method
plot(out)

dunnet <- glht(out, linfct = mcp(Treat = "Dunnett"))
summary(dunnet)
    # CBT보다는 FT가 더 유의미한 차이를 보여준다.


# Result
#---------------------------
# y = 45.674 - 0.565 * prewt + 4.097 * CBT + 8.660 * FT
# --- control : y = 45.674 - 0.565 * prewt
# --- CBT     : y = 45.674 + 4.097 - 0.565 * prewt
#               Prewt가 평균이었던 사람에 대해 CBT는 control 그룹보다 4.097만큼 더 몸무게 변화를 주었다.
# --- FT      : y = 45.674 + 8.660 - 0.565 * prewt
#               Prewt가 평균이었던 사람에 대해 FT는 control 그룹보다 8.66만큼 더 몸무게 변화를 주었다.
#---------------------------

gubun <- as.numeric(df$Treat)
plot(Postwt - Prewt ~ Prewt, df, col = gubun, pch = gubun)
legend("topright", levels(df$Treat), col = 1:3, pch = 1:3, lty = 1)
abline(45.674, - 0.565, col = 1)
abline(45.674 + 4.097, - 0.565, col = 2)
abline(45.674 + 8.660, - 0.565, col = 3)



#---------------------------------------------------------------
# 더미변수 포함한 회귀분석
#---------------------------------------------------------------

# Case.
# 수축기혈압(Systolic blood pressure; SBP)과 연령 (age)을 남자 40명, 여자 29명으로부터 기록
# 연령이 높을수록 수축기혈압이 높은 경향. 연령과 혈압 사이의 관계가 남녀간에 차이가 있는가?

# y = b0 + b1*x + b2*z + b3*x*z + e : x = age, z = sex (1 female, 0 male)

sbp <- read.csv("SBP.csv")
head(sbp)
summary(sbp)

model <- lm(SBP ~ AGE + SEX, sbp)
summary(model)

# 두 회귀선은 평행한가? --> H0 : b3 = 0
model1 <- lm(SBP ~ AGE + SEX + AGE*SEX, sbp)
summary(model1)

# 두 회귀선이 동일한가? --> H0 : b2 = b3 = 0
model2 <- lm(SBP ~ AGE, sbp)
summary(model2)
anova(model2)

anova(model2, model1)



#---------------------------------------------------------------
# 공분산분석 (ANCOVA)  vs.  더미변수 포함한 회귀분석
#---------------------------------------------------------------

# ANCOVA
# --- 주요 관심사는 집단간 종속변수의 평균 차이
# --- 공변량의 효과를 통제한 후 집단간 차이를 파악하는 것이 목적
# --- 공변량이 전체 평균인 수준에서 종속변수의 평균치를 비교
# --- 각 집단의 회귀식이 평행하지 않으면 의미 없음
# --- 회귀선들이 평행한지 검정 후 귀무가설(회귀선이 평행하다)이 기각되지 않으면 ANCOVA 실시

# 더미변수를 포함한 회귀분석
# --- 범주형 변수 뿐만 아니라 공변량도 관심대상
# --- 만일 회귀선이 평행하지 않다면 해당 설명변수들 간에 interaction effect 존재
# --- 예) 나이가 어릴때는 여자의 혈압이 더 높지만 나이가 들면 남자의 혈압이 높다.



#---------------------------------------------------------------
# practice 6
#---------------------------------------------------------------

df <- read.csv("Forbes500.csv")
str(df)
summary(df)
summary(df$sector)

df$sector <- relevel(df$sector, ref = "HiTech")

#-----------------------------------
# Case 1. Sales ~ sector : 분산분석

# boxplot을 통해 sector간 비교.
boxplot(Sales ~ sector, df)
boxplot(log(Sales) ~ sector, df)   # log변환한 데이터가 분석에 더 적합

# Sales가 sector 간에 서로 다른지 판단하기 위한 분산분석
# 구체적으로 어느 sector간 통계적으로 유의한 차이가 있는지 Dunnett test
model <- lm(log(Sales) ~ sector, df)
summary(model)
    # F-statistic: 5.288 on 8 and 70 DF, p-value: 3.387e-05 < 0.05 : sector간 차이가 있다.

dunnet <- glht(model, linfct = mcp(sector = "Dunnett"))
summary(dunnet)
    # Hi-tech를 기준으로, Energy, Finance, Medical가 유의한 차이를 보인다.


#-----------------------------------
# Case 2. Sales ~ Assets + sector : 공분산분석

# 산점도
plot(df$Assets, log(df$Sales))
plot(log(df$Assets), log(df$Sales))   # Sales와 Assets 모두 log 변환 후 분석

# 공분산분석
model <- lm(log(Sales) ~ log(Assets) + sector, df)
summary(model)
dunnet <- glht(model, linfct = mcp(sector = "Dunnett"))
summary(dunnet)

# --> log(Assets) 이 평균수준으로 동일할 때 HiTech와 Energy, HiTech와 Finance sector 간에는 log(Sales)에 유의한 차이가 있다.


# Asset이 3000 million이고 finance sector에 있는 회사의 sales를 예측.
# log(Sales) = 2.184 - 2.146 + 0.767 * log(Assets)

y = exp(2.184 - 2.146 + 0.767 * log(3000))
y

