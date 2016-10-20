###############################################################
#
# 다변량 통계분석 2 (정여진 교수) - 탐색적인자분석
#
###############################################################

# 인자분석 = 요인분석
# f : 공통인자 common factor
# u : 특정인자 
# f, u 는 측정불가능

# l : 인자적재값 facor loading - 각 변수가 공통인자를 얼마나 반영하는지에 대한 계수
# li^2 공통인자에 의해 설명되는 부분
# var(ui) 공통인자에 의해 설명되지 않는 부분. 특정인자의 분산.

# 2-factor
# var(x1) = l11^2 + l12^2 + psi1^2

app <- read.table("Applicant.TXT", header = T)
app

app <- app[,-1]

library(psych)
describe(app)

pairs.panels(app)

app_s <- scale(app)

fa1 <- factanal(app_s, 4)        # cor
print(fa1, digits = 2, sort = T)

fa2 <- factanal(app, 4)          # cov
print(fa2, digits = 2, sort = T)


# X13까지 factor1에 대한 로딩값이 크다.
# X1,9,15는 factor2에 대한 로딩값이 크다.

# x14 = 0.42*f1 + 0.39*f2 + 0.55 * f3 - 0.60 * f4

# communality of x5 = 0.92^2 + 0.14^2 = 0.866 (loading matrix 가로 제곱합)
# x5의 특정요인의 분산 (uniqueness) = 1 - 0.866 = 0.134
# uniqueness : 0에 가까울수로 common factor 에 대한 설명력이 높다.

# loading matrix 세로 제곱합 - 각 factor의 설명력 (SS loadings) 총분산에서 factor가 설명해주는 양
# Factor1 = 5.57 / 15 (변수갯수) = 0.37


# Test of the hypothesis that 4 factors are sufficient. = H0 (귀무가설)
# p-value is 0.00247 < 0.05  귀무가설 기각 --> 충분하지 않다.
# factor 갯수 선택시 이 방법에 의존하지는 말 것.

# 요인분석은 factor에 대한 해석이 가장 중요하다.

load = fa1$loadings
plot(load, type = "n")
text(load, labels = colnames(app_s), cex = 0.7)



# -----------------------------------------------------------
# Practice 3

stock <- read.csv("stock_price.csv", header = T)
stock <- stock[, -1]
stock

fa2 <- factanal(stock, 2)
print(fa2, digits = 2, sort = T)

fa3 <- factanal(stock, 3)

# https://prezi.com/cuefyeoq15le/09/







