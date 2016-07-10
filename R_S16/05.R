#------------------------------------
# 다중회귀분석

df <- read.csv("salary.csv")
head(df)
str(df)
summary(df)

pairs.panels(df)

df2 <- df[-2,]       # 영향점 1개 제거할 경우
pairs.panels(df2)
model2 <- lm(salary ~ ., data = df2)
summary(model2)


# 단순회귀 : 경력 증가시 연봉 증가 상관관계
model0 <- lm(salary ~ experience, data = df)
summary(model0)

# 다중회귀 : 경력 증가시 적성검사 점수 증가로 인한 연봉 증가까지 포함된 관계
# experience ~ score 의 cor() = 0.34
#model <- lm(salary ~ experience + score, data = df)
model <- lm(salary ~ ., data = df)
summary(model)
plot(model)

predict(model, data.frame("experience" = c(5,10), "score" = c(80,70)), 
        interval = "confidence")

predict(model, data.frame("experience" = c(5,10), "score" = c(80,70)), 
        interval = "prediction")


#----------------------------------------
# 다중공신성

# attitude
attitude
round(cor(attitude),3)

pairs.panels(attitude)
# cor : complaints + learning = 0.597

a <- lm(rating~complaints+learning, data = attitude)
summary(a)

b <- lm(rating~complaints+raises, data = attitude)
summary(b)


# 모형선택방법 : 설명변수 선택
out <- lm(rating~., attitude)
summary(out) # Pr(>|t|) : t검정 p-value 기준 선별
anova(out)

# 위 결과 중 가장 유의하지 않은 변수 제거
out2 <- lm(rating~. - critical, data = attitude)
summary(out2)
anova(out2)

# 모형선택 자동화
backward <- step(out, direction = "backward", trace = T)

backward <- step(out, direction = "backward", trace = F)
backward
backward$anova


# stepwise selection
both <- step(out, direction = "both", trace = F)
both
both$anova


#
out3 <- lm(rating~complaints+learning+advance, data = attitude)
summary(out3)



# all subsets regression

library(leaps)

leap <- regsubsets(rating~., attitude, nbest = 5)
summary(leap)

plot(leap)
plot(leap, scale = "adjr2") # adjusted r-squred 기준
plot(leap, scale = "cp")


#---------------------------------
# excercise : hotel margin prediction

data <- read.csv("laquinta.csv")
summary(data)

pairs.panels(data)

model <- lm(Margin~., data)
summary(model)
plot(model)

# Enrollment, Distance - 유의성 떨어짐

new <- data.frame("Number"=3815, "Nearest"=0.9, "Office.Space"=476, 
                  "Enrollment"=24.5, "Income"=35, "Distance"=11.2)
new
predict(model, new, interval = "prediction")


regsub <- regsubsets(Margin~., data, nbest = 5)
plot(regsub) # Enrollment, Distance  변수 제거




