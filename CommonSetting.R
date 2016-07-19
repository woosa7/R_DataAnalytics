
setwd("c:/R_Study/R_KMU")

setwd("/Volumes/MacHDD/workspace/R_Study/R_JS")

windows(800, 600, pointsize = 12)   # 별도의 윈도우 열기
savePlot("aa.png", type="png")    # 결과물을 그림으로 저장
dev.off()                           # 윈도우 닫기

install.packages("readxl")


##################################
# sapply / tapply 를 통한 간단한 회귀분석

# cors <- sapply(dataframe, cor, y = targetVariables)
# mask <- (rank(-abs(cors)) <= 10)
# bestprediction <- dataframe[ , mask]
# lm(targetVariables ~ bestprediction)

library(MASS)
attach(Cars93)

tapply(Weight, Origin, mean)
tapply(Weight, Origin, length)

model <- by(Cars93, Origin, function(df) lm(Price~Weight+EngineSize+Horsepower, data = df))
model

summary(model$USA)
summary(model$`non-USA`)

##################################
