###############################################################
#
# 다변량 통계분석 4 - 탐색적 인자분석(Exploratory Factor Analysis)
#
###############################################################

# Factor Analysis : 인자분석 = 요인분석

# 심리학, 행동과학 등 많은 분야에서 직접 측정이 불가한 주된 관심의 개념을 간접적으로 측정
# 잠재변수 (latent variable) : 직접 측정이 불가하지만 간접적으로 측정할 수 있는 변수
# (예) 시험점수 --> 지능

# 인자분석 : 측정변수와 잠재변수 사이의 관계를 밝히는 것.
# 탐색적 인자분석 : 어떤 측정변수가 어떤 인자에 관련된다는 특정한 가정 없이 조사
# 확인적 인자분석 : 사전에 가정된 특정한 인자 모형에 대해 측정하여 변수 사이의 공분산 또는 상관관계가 적합한지 검정.

# f : 공통인자 common factor
# u : 각 변수의 특정인자 specific factor. 공통인자를 완벽히 측정하지 못하는데서 오는 오차로 가정.
# f, u 는 측정불가능
# 인자분석에서 추정하는 것은 분산 (공통인자의 분산 + 특정인자의 분산)
# 가정 : u 들간의 상호독립성, u와 f의 독립성

# 1 factor
# X1 = L1*f1 + U1
# var(X1) = L1**2 + var(U1) = 1

# l : 인자적재값 (facor loading) - X와 f의 상관계수. 각 변수가 공통인자를 얼마나 반영하는지에 대한 계수.
# li**2 : 공통인자에 의해 발생하는 분산의 비율 = Communality.
# Communality 가 1에 가까우면 변수 Xi가 공통인자를 잘 반영한 것.
# var(ui) 공통인자에 의해 설명되지 않는 부분. 특정인자의 분산.

# 2 factor
# X1 = L11*f1 + L12*f2 + U1
# var(x1) = L11**2 + L12^2 + var(U1)


# ----------------------------------------------------------------------
# Example

app <- read.table("data/Applicant.TXT", header = T)
head(app)
app <- app[,-1]   # ID 제거

library(psych)
describe(app)
pairs.panels(app)

app_s <- scale(app)

fa1 <- factanal(app_s, 4)        # cor
print(fa1, digits = 2, sort = T)

fa2 <- factanal(app, 4)          # cov
print(fa2, digits = 2, sort = T)

# X6.LC 의 Communality = 0.84**2 + 0.11**2 + 0.29**2 = 0.80 (4개 factor에 의해 80% 설명됨)
# X6.LC 의 Uniquenesse = 1 - Communality = 0.20
# 즉, Uniquenesse 값이 작은 변수가 공통인자에 대한 설명력이 높다.

1 - fa1$uniquenesses # = Communality

# X5에서 X13까지 factor1에 대한 로딩값이 크다.
# X1,9,15는 factor2에 대한 로딩값이 크다.

# x14 = 0.42*f1 + 0.39*f2 + 0.55 * f3 - 0.60 * f4

# loading matrix 세로 제곱합 : 각 factor의 설명력 (SS loadings) 총분산에서 factor가 설명해주는 양
# Factor1 = 5.57 / 15 (변수갯수) = 0.37

# Cumulative Var = 0.74 : 4개의 factor에 의해 원변수 변동량의 74%가 설명된다.

# Test of the hypothesis that 4 factors are sufficient. = H0 (귀무가설)
# p-value is 0.00247 < 0.05  귀무가설 기각 --> 4개로는 충분하지 않다.
# factor 갯수 선택시 이 방법에 의존하지는 말 것.
# 요인분석은 factor에 대한 해석이 가장 중요하다.


# ----------------------------------------------------------------------
# factor loading 값의 산점도

load = fa1$loadings
plot(load, type = "n")
text(load, labels = colnames(app_s), cex = 0.7)


# ----------------------------------------------------------------------
# fator rotation (인자 회전)

# 인자적재값의 구별이 쉬운 고유벡터를 찾아 회전

# (1) 직교 회전 (orthogonal ratation) : 회전된 인자들이 서로 상관되지 않도록 제약
# varimax : 한 공통인자에 대해 각 변수가 가지는 인자적재값 제곱의 분산이 최대가 되도록 변환
#           loadings matrix 각 열의 분산을 최대화 (가로 방향)
# quartimax : 한 변수가 각각의 공통인자에서 차지하는 비중의 제곱에 대한 분산을 최대화
#             loadings matrix 각 행의 분산을 최대화 (세로 방향)

# (2) 사각 회전 (oblique rotation) : 상관된 인자들을 허용
# oblimin : 인자들 사이의 상관성 정도를 제어
# promax : 회전에 의해 적재값을 어떤 승수로 올리는 방법. 인자들 사이에 낮은 상관성을 갖도록 함.


fa1 = factanal(app, 4)   # default : varimax
fa2 = factanal(app, 4, rotation = "none")
print(fa2, digits = 2, sort = T)


library(psych)
library(GPArotation)

fa3 = fa(app, 4, rotate = "quartimax")
print(fa3, digits = 2, sort = T)

# MR : factor loadings
# h2 : communality
# u2 : uniquiness. specific factor 분산

fa.diagram(fa3)



# -----------------------------------------------------------
# Practice 3

# https://prezi.com/cuefyeoq15le/09/

stock <- read.csv("data/stock_price.csv", header = T)
stock <- stock[, -1]
head(stock)


# 1. 적절한 공통인자의 수를 구하시오

fa01 <- factanal(stock, 2)
print(fa01, digits = 2, sort = T)


# fa02 <- factanal(stock, 3)  # 3 factors are too many for 5 variables

    # common factor 갯수 = 2


# 2. 다양한 Rotation을 적용한 것과 하지 않은 것의 인자적재값을 비교하고 
#    적절하다고 판단되는 결과를 고르시오.

library(GPArotation)

fa_varimax = fa(stock, 2, rotate = "varimax")
fa_quartimax = fa(stock, 2, rotate = "quartimax")

print(fa_varimax, digits = 2, sort = T)
print(fa_quartimax, digits = 2, sort = T)

bank = apply(stock[, 1:3], 1, mean)
oil = apply(stock[, 4:5], 1, mean)

plot(bank, type = "l", ylim = c(-0.1,0.1))
lines(oil, col = "red")
legend("topright", c("bank", "oil"), lty = 1, col = 1:2)


