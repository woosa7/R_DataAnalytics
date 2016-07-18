################################################################
#
# R을 활용한 통계분석 : 정여진 교수 (2016 여름특강)
#
# 3. 모비율에 대한 검정
#
################################################################

prop.test(67, 120, p=0.5)

prop.test(c(60,120), c(150,250), alternative = "less")


#------------
# 독립성 테스트

d <- matrix(c(33,28,5,67,122,45), 3, 2)
d

chisq.test(d)

# p-value = 0.002222 : 소득수준과 우울증이 독립이다라는 귀무가설 기각.
# 소득수준과 우울증은 관련이 있다.


#--------------------------------
# 분할표

ny <- read.csv("NYReform.csv")
head(ny)

tab1 <- xtabs(~Pay.Cut+Party, data=ny)
tab1

chisq.test(tab1)

mosaicplot(tab1)
mosaicplot(t(tab1))


tab2 <- xtabs(~Lobbyists+Party, data=ny)
tab2

chisq.test(tab2)

par(mfcol=c(1,2), las=1)
mosaicplot(tab2)
mosaicplot(t(tab2))


tab3 <- xtabs(~Term.Limits+Party, data=ny)
tab3

prop.test(c(17,32), c(36,45), alternative = "less")

prop.test(21, 36)

