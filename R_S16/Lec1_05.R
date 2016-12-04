################################################################
#
# R을 활용한 통계분석 : 정여진 교수 (2016 여름특강)
#
# 5. 다중회귀분석
#
################################################################

#---------------------------------------------------------------
# 다중회귀분석 (Multiple Regression Model)
#---------------------------------------------------------------

# programmer 20명
# salary가 experience(경력년수), score (직무적성검사성적)과 연관성을 갖는지 검증.
df <- read.csv("data/salary.csv")
head(df)
summary(df)

library(psych)
pairs.panels(df)    # salary ~ experience 상관계수 0.86

# 단순회귀 : 경력 증가시 연봉 증가 상관관계
model <- lm(salary ~ experience, data = df)
summary(model)

# 다중회귀 : 경력 증가시 적성검사 점수 증가로 인한 연봉 증가까지 포함된 관계
# experience ~ score 의 cor() = 0.34
# model <- lm(salary ~ ., data = df) 
model <- lm(salary ~ experience + score, data = df)
summary(model)

# 추정된 회귀식
# salary = 3.174 + 1.404 * experience + 0.251 * score

# b1 : b2(score)가 일정하다고 할 때, experience가 1년 증가하면 salary가 $1,404 증가할 것으로 기대된다.
# b2 : b1(experience)가 일정하다고 할 때, score가 1점 증가하면 salary가 $251 증가할 것으로 기대된다.


#---------------------------------------------------------------
# 다중회귀분석 결과 해석

# (1) Adjusted R-squared
# R-squared: 0.83 --> experience와 score가 salary 변동량의 83%를 설명한다.
# But, 설명변수 갯수가 증가하면 결정계수도 증가
# --> 설명변수 갯수에 대한 패널티 적용한 결정계수 = Adjusted R-squared

# (2) F-test
# H0 : b1 = b2 = ...... = bk = 0
# 종속변수와 모든 독립(설명)변수 집합간에 유의한 관계가 존재하는지 검정
# b0 는 큰 의미가 없다.

# (3) T-test
# H0 : bi = 0
# 각 개별 독립변수의 유의성 검정

# (4) 잔차분석 --> Residuals plot / Normal Q-Q plot / Leverage plot


#---------------------------------------------------------------
# 영향점이 있는 경우

plot(model)   # --> Leverage plot에서 2번째 자료가 이상치 & 영향점

dcolor <- rep(1, length(df$salary))
dcolor[2] = 2
pairs(df, col = dcolor, pch = dcolor)     # 2번 자료만 다르게 표시

# 영향점 제거는 주관적으로 판단하는 수밖에 없다.

df2 <- df[-2, ]         # 영향점 제거할 경우
pairs.panels(df2)       # salary ~ experience 상관계수 높아짐(0.91). 다른 상관계수는 낮아짐.
model2 <- lm(salary ~ experience + score, data = df2)
summary(model2)         # score 회귀계수가 유의하지 않다.


#---------------------------------------------------------------
# 추정과 예측

# 경력 5년, 적성검사성적 80점인 사람과 경력 10년, 성적 70점인 사람의 연봉 예측

# 평균 연봉의 95% 신뢰구간
predict(model, data.frame("experience" = c(5,10), "score" = c(80,70)), 
        interval = "confidence")

# 새로운 한 명에 대한 95% 예측구간
predict(model, data.frame("experience" = c(5,10), "score" = c(80,70)), 
        interval = "prediction")


#---------------------------------------------------------------
# 다중공선성 (Multicollinearity)
#---------------------------------------------------------------

# 독립변수들이 서로 높은 상관관계를 가지면 회귀계수의 정확한 추정이 어렵다.
# ---> 모형 선택 방법론을 적용하여 가장 적절한 변수를 선택할 수 있다.

# 30개 부서에서 부서당 35명의 직원 설문조사
# 데이터 숫자는 해당 질문에 긍정한 직원의 비율
attitude
round(cor(attitude),3)

pairs.panels(attitude)

# cor : complaints + learning = 0.597
# cor : complaints + raises = 0.669

plot(attitude[ , c("rating", "complaints", "learning")])

a <- lm(rating ~ complaints + learning, data = attitude)
summary(a)

# learning의 t-test p-value 값을 보면 유의하지 않다. 
# 하지만 rating과 상관관계가 없는 것이 아니다. 
# complaints 와의 상관관계도 있기 때문에 rating 변수에 대한 역할이 작아보일 뿐이다.


#---------------------------------------------------------------
# 모형 선택법 (Model Selection) = 설명변수 선택
#---------------------------------------------------------------

# *** 해당 업무분야에서 반드시 들어가야 하는 변수는 고정 !!!
# (1) Forward selection
# --- 가장 유의한 변수부터 하나씩 추가 (R-sq 기준)
# --- 변수값의 작은 변동에도 결과가 크게 달라져 안정성 부족

# (2) Backward selection
# --- 모든 변수를 넣고 가장 기여도가 낮은 것부터 하나씩 제거
# --- 전체 변수 정보를 이용하는 장점
# --- 변수의 갯수가 많은 경우 사용 어려움. 안정성 부족.

# (3) Stepwise selection
# --- Forward selection과 backward selection을 조합
# --- 새로운 변수 추가 후에 기존 변수의 중요도가 약화되면 그 변수 제거

# (4) All Subsets Regression
# --- 모든 가능한 모형을 비교하여 최적의 모형선택
# --- 여러 모형 중 최소 AIC, BIC, Mallow’s Cp 또는 최대 adjusted R-sq를 갖는 모형을 선택
# --- 모형의 복잡도에 벌점을 주는 방법. AIC (Akaike information criterion), BIC (Bayesian ...)


#---------------------------------------------------------------
# Backward selection

out <- lm(rating ~ ., attitude)
summary(out) 
anova(out)      # 각 회귀계수 t검정 p-value 기준 선별. critical 제거.

out2 <- lm(rating ~ . - critical, data = attitude)
summary(out2)
anova(out2)     # raises 제거

# Backward selection 자동화
backward <- step(out, direction = "backward", trace = T)

backward <- step(out, direction = "backward", trace = F)
backward        # 최종 선택된 회귀모형 : rating ~ complaints + learning
backward$anova  # critical, raises, privileges, advance 순으로 제거됨


#---------------------------------------------------------------
# Stepwise selection

both <- step(out, direction = "both", trace = F)
both
both$anova


#---------------------------------------------------------------
# All Subsets Regression

library(leaps)

leap <- regsubsets(rating ~ ., attitude, nbest = 5)   # size당 5개의 최적 모형 저장
summary(leap)

plot(leap)
plot(leap, scale = "adjr2") # adjusted r-squred 기준



#---------------------------------------------------------------
# practice 5
#---------------------------------------------------------------

# hotel margin prediction

data <- read.csv("data/laquinta.csv")
summary(data)
str(data)

# 자료의 산점도 확인
round( cor(data), 3)
pairs.panels(data)   # 설명변수간의 correlation도 낮다. 종속변수와도 낮다.

# 회귀모형
model <- lm(Margin ~ ., data)
summary(model)      # F-test 유의함. R-squared: 0.525. Distance, Enrollment 제외한 회귀계수 유의함.
plot(model)         # 잔차도 이상 없음

backward <- step(model, direction = "backward", trace = F)
backward

both <- step(model, direction = "both", trace = F)
both

# 최종 회귀모형 : Margin ~ Number + Nearest + Office.Space + Enrollment + Income
# Coefficients:
#     (Intercept)        Number       Nearest  Office.Space    Enrollment        Income  
#       37.128891     -0.007742      1.586923      0.019576      0.196385      0.421411 


# 다음 조건을 가진 한 지역의 Margin을 95% 신뢰구간으로 예측
new <- data.frame("Number" = 3815, "Nearest" = 0.9, "Office.Space" = 476, 
                  "Enrollment" = 24.5, "Income" = 35, "Distance" = 11.2)
new
predict(model, new, interval = "prediction")


# BIC 값을 최소로 하는 설명변수의 조합을 찾아 회귀식을 추정
regsub <- regsubsets(Margin ~ ., data, nbest = 5)
plot(regsub)   # 최종 회귀모형 : Margin ~ Number + Nearest + Office.Space + Income
plot(regsub, scale="adjr2")

