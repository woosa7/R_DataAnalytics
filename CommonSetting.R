#--------------------------------------------------------------
# <-  : option + -
# %>% : cmd + shift + m
# comment : cmd + shift + c
# run all : cmd + shift + enter
#--------------------------------------------------------------

windows(800, 600, pointsize = 12)   # 별도의 윈도우 열기
savePlot("aa.png", type="png")      # 결과물을 그림으로 저장
dev.off()                           # 윈도우 닫기

install.packages("lubridate")

colors()   # color name list


ddply()
aggregate()
dcast()

basket %>%
    group_by(custId) %>%
    summarize(avg_amt=mean(amount)) %>%
    arrange(desc(avg_amt))


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



install.packages("psych", repo="https://cran.cnr.Berkeley.edu/", type="source")

