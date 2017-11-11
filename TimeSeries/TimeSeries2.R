beverage = read.csv('beverage.csv')
head(beverage)

# ARIMA

# p = 1, d= 1, q = 0
arima(beverage$shipment, c(1,1,0))  

# 12개월 단위 MA(1)
m.a = arima(beverage$shipment, c(1,1,0), seasonal = list(order = c(0, 0, 1), period = 12)) 
m.a


################################################
# Prophet - Facebook
################################################

library(dynlm)   # as.yearmon

library(prophet)

d = data.frame(ds = as.Date(as.yearmon(beverage$date)),
               y = beverage$shipment)

head(d)

m = prophet(d)   # n_changepoints : 기본 25개.

# 예측

future = make_future_dataframe(m, period=12, freq='month')

forecast = predict(m, future)

plot(m, forecast)


# 추세와 계절 효과로 나눠 보기

prophet_plot_components(m, forecast)


# 변화점(change point)

m$params$k  # 기본 성장률

m$changepoints  # 변화점

m$params$delta  # 변화점에서 성장률 변화

plot(m$changepoints, m$params$delta)  # 96, 97년 성장률의 변화량 감소




