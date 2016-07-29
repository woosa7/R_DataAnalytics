################################################################
#
# ADSP
#
################################################################

#--------------------------------------------------------------
# 상관계수 correlation coefficient

data("mtcars")
mtcars
str(mtcars)

attach(mtcars)

# disp 배기량 / drat 후방차축비율 / wt 차량 무게
plot(disp, drat)
plot(disp, wt)

cov(disp, wt)   # 공분산
cor(disp, wt)   # 상관계수

cov(mtcars)
cor(mtcars)  # 모든 변수간의 상관계수 테이블 표시

library(psych)
pairs.panels(mtcars[, c("disp", "wt", "drat")])

# 피어슨 상관계수 - 일반적인 경우. 연속형 변수.
# 스피어만 상관계수 - 서열척도(순서형 변수)인 경우 사용.

install.packages("Hmisc")
library(Hmisc)

rcorr(as.matrix(mtcars), type = "pearson")$r

rcorr(disp, wt, type = "pearson")   # P : 유의확률. 0.05 이하 값 유의미 
rcorr(disp, wt, type = "spearman")  


# 순서형변수 샘플 데이터

sno <- seq(1001,1050)
Korean <- sample(1:50, replace = F)
English <- sample(1:50, replace = F)
Math <- sample(1:50, replace = F)

rank <- cbind(sno, Korean, English, Math)
rownames(rank) <- seq(1,50)
rank    
    
rcorr(as.matrix(rank), type = "spearman")    


#--------------------------------------------------------------
# 











