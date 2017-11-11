library(zoo)

closale = read.csv('closale.csv')
head(closale)
summary(closale)

closale$date = as.yearmon(closale$date)

plot(closale, type='l', lty=4, main="미국 의류 판매", xlab="연도", ylab="판매량")
lines(closale$date, lowess(closale$sales)$y, col=2)


# 이동평균

mov.avr = rollmean(closale$sales, 5, na.pad=T)

plot(closale, type='l', lty=4, main="미국 의류 판매", xlab="연도", ylab="판매량")
lines(closale$date, mov.avr, col='red', lwd=3)


# 이동중간값

mov.med = rollmedian(closale$sales, 5, na.pad=T)

plot(closale, type='l', lty=4, main="미국 의류 판매", xlab="연도", ylab="판매량")
lines(closale$date, mov.med, col='red', lwd=3)


# 국소 가중 회귀

loc.reg = lowess(closale$sales)

plot(closale, type='l', lty=4, main="미국 의류 판매", xlab="연도", ylab="판매량")
lines(closale$date, loc.reg$y, col='red', lwd=3)


# ACF

acf(closale$sales)


# ----------------------------------------
library(zoo)

bvr = read.csv('beverage.csv')
head(bvr)
summary(bvr)

bvr$date = as.yearmon(bvr$date)
summary(bvr)

plot(bvr, type='l')
lines(bvr$date, lowess(bvr$shipment)$y, col=2)   # 추세

acf(bvr$shipment, 36)


# 차분

d1 = diff(bvr$shipment, 1)

plot(d1, type='l')
lines(lowess(d1), col=2)

acf(d1, 36)   # 12개월 주기의 계절성


# 계절성 차분

d12 = diff(bvr$shipment, 12)

plot(d1, type='l')
lines(lowess(d12), col=2)

acf(d12)


# 계절성과 추세를 차분 - 추세도 제거.

d121 = diff(d12, 1)

plot(d1, type='l')
lines(lowess(d121), col=2)

acf(d121)  # 상관계수가 마이너스. 지난 달 많이 팔리면 이번 달 적게 팔림.


# ------------------------------------------------------
# Viscosity reading 데이터

vis = read.csv('viscosity.csv')
head(vis)

plot(vis, typ='l')
lines(vis$time, lowess(vis$reading)$y, col=2)   # 추세


# MA(q) 모형 - 이전 시점의 오차가 현재 시점에 영향을 준다.

acf(vis$reading)
vis.ma = arima(vis$reading, c(0,0,5))
vis.ma


# AR(p) 모형 - 이전 시점의 값 자체가 현재 시점에 영향을 준다.

# PACF : 종교시설이 많은 도시는 범죄율도 높다. 
# 하지만 두 변수 모두 인구와 관련되어 있다.
# 종교시설수와 범죄율의 상관관계는 각각 인구가 설명하는 부분을 제외하고 상관성을 고려해야 한다.

pacf(vis$reading)
vis.ar = arima(vis$reading, c(1, 0, 0))
vis.ar


# ARIMA(p, q) 모형

vis.arma = arima(vis$reading, c(1, 0, 5))
vis.arma


# ---> ACF 가 완만하게 감소하므로 PACF 사용. 즉 AR 모형이 적합 !!!

# 모형 비교 : 값이 작은 것 선택.

AIC(vis.ma)
AIC(vis.ar)
AIC(vis.arma)

BIC(vis.ma)
BIC(vis.ar)
BIC(vis.arma)


library(forecast)

vis_ts = ts(vis$reading)
vis_ts

vis_arima <- arima(vis_ts, order = c(1, 0, 0))  # AR 모델
vis_fcast <- forecast.Arima(vis_arima, h = 5)
plot.forecast(vis_fcast)



