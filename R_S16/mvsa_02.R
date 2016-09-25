

#------------------------------------
crime <- read.csv("crime.csv")
head(crime)

rownames(crime) <- crime$state
crime <- crime[ , -c(1,9)]

# star & nightingale - dataframe
a <- stars(crime)
stars(crime, key.loc = c(12, 2), cex = 0.8)
stars(crime, key.loc = c(12, 2), cex = 0.8, draw.segments = T)

# heatmap - matrix
crime <- as.matrix(crime)

heatmap(crime, scale = "column", Colv = NA) 



#------------------------------------------------------------
# 주성분 분석
#------------------------------------------------------------

# 상관계수 행렬을 이용한 선형 결합

df <- read.csv("open_closed.csv")
head(df)

library(psych)
pairs.panels(df)















