# Chapter 3
library(forecast); library(tidyverse)
library(fpp2)
  
# fpp2
# Y = a0 + a1*t + a2*t^2
uspop=as.numeric(uspop)
time=1:length(uspop)
fit=lm(uspop~time+I(timeˆ2))
plot(uspop)
lines(predict(fit)~time)


# 트렌드는 따르지만, seasonal behavior은 모른다
# time + time^2
library(forecast)
time=time(AirPassengers)
fit=tslm(AirPassengers~time+I(time^2))
ts.plot(AirPassengers)
lines(fitted(fit),col=2)

# residual 함수와 acf
library(fpp2)
checkresiduals(fit)

# cycle function => 계절성을 표현함
# month data가 들어갔기 때문임
# raw data
AirPassengers
# momth 변수가 만들어짐

cycle(AirPassengers)

# tslm = time series linear model
# y = trend + season
# 그러나, fitted value가 variance를 따르지 않는다. 원 데이터와 차이 존재

library(tidyverse)
month = AirPassengers %>% cycle %>% as.factor
fit=tslm(AirPassengers~time+month)
ts.plot(AirPassengers)
lines(fitted(fit),col=2)

# 같은 거를 좀더 편하게
library(forecast)
fit=tslm(AirPassengers ~ trend+season)
ts.plot(AirPassengers)
lines(fitted(fit),col=2)

# 더하기 대신 곱하기로, 상호작용항을 추가함
# small variance와 large variance를 잡을 수 있다.
# multiplicative model
# y = trend + season + trend*season

fit=tslm(AirPassengers ~ trend*season)
ts.plot(AirPassengers)
lines(fitted(fit),col=2)

# three 변수 = 트렌드, 트렌드 + 시즌, 트렌드 * 시즌

# taking log transformation
# 분산이 시간에 대해 constant - accerelation stablized
par(mfrow=c(1,2))
ts.plot(AirPassengers)
ts.plot(log(AirPassengers))

# Power transformation
# Box - Cox transformation
# 박스콕스lambda = lamda로 최적의 람다를 만들어내줌 - 
# 오른쪽이 훨씬 낫다
fit1=tslm(AirPassengers ~ trend*season)
lambda = BoxCox.lambda(AirPassengers)
fit2=tslm(AirPassengers ~ trend*season, lamabda = lambda)
par(mfrow=c(1,2))
ts.plot(AirPassengers)
lines(fitted(fit1),col=2)
ts.plot(AirPassengers)
lines(fitted(fit2),col=2)

# exponential decaying
# linear regression이 좋지 않다.
# 선형회귀는 뭐 설명할 때나 써라
checkresiduals(fit1)
checkresiduals(fit2)

library(fpp2)
autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

tslm(Consumption ~ Income, data=uschange) %>% summary
# 소비 증가 => 경제 증가 => 소득증가 uschane는 데이터
# Income이 input
# p-value가 significant하다 => 상관관계가 존재한다.
# model p-value도 significant하다
# 소득과 소비가 관계가 있었다.

install.packages("ggplot2")
library(ggplot2)
uschange %>% as.data.frame %>% GGally::ggpairs()

fit.consMR <- tslm(
  Consumption ~ Income + Production + Unemployment + Savings, data=uschange)
summary(fit.consMR)

# p-value > 0.05 귀무가설 기각 불가, aCf가 일정 초과
checkresiduals(fit.consMR)

dw(fit2)












