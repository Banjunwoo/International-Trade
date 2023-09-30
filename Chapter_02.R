#2023_09_14
install.packages("tidyverse")
library(tidyverse)
AirPassengers %>% log %>% decompose %>% plot 

# log transformation => data의 variance가 Stablize 된다. => Variance가 keep increasing 방지

# %>% = pipeline = plug in = Take the log
# notation
#

# White noise = stochastic process = stationary

# Weak stationarity
# mean value, Variance가 t에 대해 독립
# 공분산이 시차 k에는 의존하지만 t에 대해 독립이다.

# Strict stationarity
# 결합확률분포가 주어져있는 것임, 그리고 그 것이 시간에 따라 일정하다고 가정

# Gaussian processes

acf(rnorm(100))

#acf of White noise
# ACF means Autocorrelation Function
# remaining value quites small number
# we can see blue dot curve

# Stationary Process
# 자기상관함수의 모습

# Yt = 2 + Et + Et-1
# EPS = Error term
# No trend, weakly stationary process
# acf 함수 = correlation, X축 = k = 시차 // # k = 0 => 2*Sigma제곱, 점점 작아짐

y = eps = rnorm(100)
for (i in 2:100) y[i] = 2 + eps[i] + eps[i-1]
par(mfrow=c(1,2))
ts.plot(y, col=4); acf(y,main="")


# EX2
# Xt = u + Et - Et-1
# Xt is stationary process.
# k = 1에서 음수 값을 보인다.
# Sign이 음수로 바뀌었다. 전항이랑 비교하기
# negative situation => Accerallation
# 좀더 가속화된 경향을 보인다. 빠르게 올라갔다가, 내려갔다가 함

y = eps = rnorm(100)
for (i in 2:100) y[i] = 2 + eps[i] + eps[i-1]
par(mfrow=c(1,2))
ts.plot(y, col=4); acf(y,main="")


# Non-Stationary process
# Random work 랜덤 워크 프로세스는 Not weakly stationary
# rnorm 숫자 넣을때마다 심각하게 바뀐다.
# e간의 상관계수가 매우 높다.
# 좋은 지표가 아니다. detrending, deseasonalisng을 해야 한다.
# 그래서, 우리는 difference를 했더니 => Weakly stationary가 되었다.

# 분산이 굉장히 크다
y = cumsum(rnorm(200)) # RW
par(mfrow=c(1,3))
ts.plot(y, col=4); acf(y,main=""); ts.plot(diff(y), col=4)

# 수업시간
# rnorm 숫자 = t가 커지면, acf가 거의 1의 값과 가깝다.
# 즉, 높은 자기상관관계를 보여준다.
# ACF = t/ Root(t(t+k))

y = cumsum(rnorm(10000))
ts.plot(y)
acf(y)


# deterministic trends
# 시간이 지남에 따라 0.5씩 항상증가
# 분산도 점차 증가 10을 곱했기 때문
# y2
# stochastic trends
# 계속 돌려보면, 증가할 수도 있고, 감소할 수도 있다.

time = 1:100
y1 = 0.5 * time + 10*rnorm(100) # deterministic trend
y2 = cumsum(rnorm(100)) # stochastic trend
par(mfrow=c(1,2))
ts.plot(y1, col=4); ts.plot(y2, col=4)


# 잔차= y1이라는 타임시리즈에서 time effect를 제거하는 것
# detrending time series
# linear regressiong을 사용하여, detrending을 할 수 있다. = eps1
# eps2 는 differencing을 사용하여, detrending
# 결국 stationary process를 만들었다.

eps1 = lm(y ~ time)$residual
eps2 = diff(y2)
par(mfrow=c(1,2))
ts.plot(eps1, col=4); ts.plot(eps2, col=4)

# S&P 500
# checkresiduals 함수 => 3 plot 제공 (ACF, histogram)
library(MASS); library(forecast)
checkresiduals(SP500)






