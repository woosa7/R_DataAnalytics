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
# --- 단, 설명변수가 그룹을 정의하는 범주형 변수
# --- Factor함수를 사용하여 설명변수가 범주형 변수라고 정의

# y = b0 + b1*x + e
# x가 0 또는 1을 가지는 범주형 변수라면?
# x = 0 : y = b0 + e
# x = 1 : y = b0 + b1 + e
# b1 = 0 이면 위 두 그룹 사이의 평균이 같다.
# 즉, H0 : mu1 = mu2  <-->  H0 : b1 = 0

# 그룹이 3개 이상이라면? x가 3개 그룹을 정의하는 질적변수라면? (서울, 부산, 제주)
# 더미 변수 (k-1) 개를 만든다.
# y = b0 + b1*x1 + b2*x2 + e

#           x1      x2  
# 서울      1       0
# 부산      0       1
# 제주      0       0






movie <- read.csv("movie_MBA.csv", header = T)
summary(movie)
levels(movie$rating)

out <- lm(log(total_seen)~rating, movie)
summary(out)

install.packages("multcomp")
library(multcomp)


dunnet <- glht(out, linfct = mcp(rating="Dunnett"))
summary(dunnet)

tukey <- glht(out, linfct = mcp(rating="Tukey"))
summary(tukey)


#-------------------
movie$rating2 <- movie$rating
levels(movie$rating2)

levels(movie$rating2) <- c(2,2,1,3) #12세 15세 합치기
summary(movie$rating2)

out2 <- lm(log(total_seen)~rating2, movie)
summary(out2)

movie$rating2 <- relevel(movie$rating2, ref="1")
out2 <- lm(log(total_seen)~rating2, movie)
summary(out2)


#-------------------------
# 공분산분석

df <- read.csv("anorexia.csv")
str(df)

boxplot(Prewt~Treat, df)

out <- lm(Postwt-Prewt ~ Treat, df)
summary(out)

# 이전 몸무게가 동일한 사람들을 기준으로...
out <- lm(Postwt-Prewt ~ Prewt+ Treat, df)
summary(out)
anova(out)


#---------------------------
# exercise : forbes

df <- read.csv("Forbes500.csv")
str(df)

boxplot(Sales~sector, df)
boxplot(log(Sales)~sector, df)

model <- lm(log(Sales)~sector, df)
summary(model)


model <- lm(log(Sales)~log(Assets)+sector, df)
summary(model)
dunnet <- glht(model, linfct = mcp(sector="Dunnett"))
summary(dunnet)


df$sector <- factor(df$sector)
df$sector <- relevel(df$sector, ref = "HiTech")

model2 <- lm(log(Sales)~Assets+sector, df)
summary(model2)
dunnet2 <- glht(model2, linfct = mcp(sector="Dunnett"))
summary(dunnet2)


