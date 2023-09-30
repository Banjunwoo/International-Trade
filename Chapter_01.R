# Chapter 01 - Intro

# Graph
ts.plot(AirPassengers)

# 시계열데이터를 볼 때 봐야할것
# 1. trend 2. seasonality 3. Outliers 4. long-run cycle 5. constant variance

# trend : 증가 추세 (체계적인 변화 X)
# seasonal variation : 계절적 변동

AP=AirPassengers
par(mfrow=c(1,2))
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

# Time series decomposition
plot(decompose(AirPassengers, type="additive"))
plot(decompose(AirPassengers, type="multiplicative"))