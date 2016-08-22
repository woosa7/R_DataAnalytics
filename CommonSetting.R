
windows(800, 600, pointsize = 12)   # 별도의 윈도우 열기
savePlot("aa.png", type="png")      # 결과물을 그림으로 저장
dev.off()                           # 윈도우 닫기

install.packages("treemap")

colors()   # color name list

par(family="NanumGothic")   # Mac Plot 한글 표시
a <- c("서울","부산","제주","제주","서울","대전","부산","서울")
fa <- factor(a)
plot(fa)


##################################
# sapply / tapply 를 통한 간단한 회귀분석

# cors <- sapply(dataframe, cor, y = targetVariables)
# mask <- (rank(-abs(cors)) <= 10)
# bestprediction <- dataframe[ , mask]
# lm(targetVariables ~ bestprediction)


# 특정 컬럼을 기준으로 회귀분석을 한꺼번에 진행.
library(MASS)
attach(Cars93)
head(Cars93)

tapply(Weight, Origin, mean)
tapply(Weight, Origin, length)

# Origin 기준으로 
model <- by(Cars93, Origin, function(df) lm(MPG.highway ~ Weight + EngineSize + Horsepower, data = df))
model
summary(model$USA)
summary(model$`non-USA`)

# Type 기준으로
model <- by(Cars93, Type, function(df) lm(MPG.city ~ Weight + EngineSize + Horsepower, data = df))
model
summary(model$Small)
summary(model$Large)


##################################
