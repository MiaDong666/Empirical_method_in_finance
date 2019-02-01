library(MTS)
library(readxl)
library(stats)
library(timeSeries)
library(data.table)
library(xts)
library(forecast)

#Q1
#take phi(0)=0, x1=0.1, x2=0.2
#a)
phi0=0
phi1=1.1
phi2=-0.25
sigma=1

n=10000
r=1:10000
r[1]=-1.5
r[2]=1

for(i in 3:10000){
  r[i]=phi0+phi1*r[i-1]+phi2*r[i-2]+sigma*rnorm(1)
}

acf(r,lag.max = 20)

plot(r,type="l")

#b)
X1=(phi1+sqrt(phi1*phi1+4*phi2))/(-2*phi2)
X2=(phi1-sqrt(phi1*phi1+4*phi2))/(-2*phi2)
X1
X2
#|x1|,|x2| bigger than 1, this process is stationary

#c) phi1^6+5*phi1^4*phi2+6*phi1^2*phi2^2+phi2^3
phi1^6+5*phi1^4*phi2+6*phi1^2*phi2^2+phi2^3

#d)
phi1=0.9
phi2=0.8
phi1^6+5*phi1^4*phi2+6*phi1^2*phi2^2+phi2^3
#no,X2<1
X1=(phi1+sqrt(phi1*phi1+4*phi2))/(-2*phi2)
X2=(phi1-sqrt(phi1*phi1+4*phi2))/(-2*phi2)


#Q2
#1)
PPI=read_excel("PPIFGS.xls")
par(mfrow=c(2,2))
plot(PPI$VALUE,type="l",main="a) PPI in levels")
plot(diff(PPI$VALUE),type = "l",main="b) delta PPI")
plot(log(PPI$VALUE),type="l",main="c) log(PPI)")
plot(diff(log(PPI$VALUE)),type="l",main="delta(log(PPI))")

#2)diff(log(PPI))
#3)
yt=diff(log(PPI$VALUE))
acf(yt)
#acf converges quickly it's covariance stationary

#4)
pacf(yt)
#lag1, lag2, lag3, lag11 may be more effective

#5)
#a)
AR3=arima(yt,c(3,0,0))
AR311=lm(yt~shift(yt,1)+shift(yt, 2)+shift(yt, 3)+shift(yt, 11))
summary(AR311)
acf(AR3$residuals)
acf(AR311$residuals)
#b)
plot(AR3$residuals)
plot(AR311$residuals,type="l")
#c)

Box.test(AR3$residuals, lag = 8, type = "Ljung-Box")
Box.test(AR311$residuals,lag=8,type="Ljung-Box")
Box.test(AR3$residuals, lag = 12, type = "Ljung-Box")
Box.test(AR311$residuals,lag=12,type="Ljung-Box")

AIC(AR3,k=2)
BIC(AR3)
AIC(AR311,k=2)
BIC(AR311)

#Q6
rawdata=xts(log(PPI$VALUE),PPI$DATE)
colnames(rawdata)="PPI_index"
data1=diff(rawdata)
traindata=data1['1947-04-01/2005-12-31']
testdata=data1['2005-12-31/2015-07-01']


AR3=arima(traindata$PPI_index,c(3,0,0))
n=length(testdata$PPI_index)
AR_fore=forecast(AR3,h=n)
AR_fore1=AR_fore$mean
e1=as.numeric(testdata$PPI_index)-as.numeric(AR_fore1)
MSPE1=sum(e1*e1)/n

AR311=arima(traindata$PPI_index,c(11,0,0),fixed=c(NA,NA,NA,0,0,0,0,0,0,0,NA,NA),transform.pars = FALSE)
AR_fore=forecast(AR311,h=n)
AR_fore2=AR_fore$mean
e2=as.numeric(testdata$PPI_index)-as.numeric(AR_fore2)
MSPE2=sum(e2*e2)/n

A=0
sd=sqrt(var(traindata$PPI_index[-1,])) 
y0=0.005025136
y1=1:n
for(j in 1:10000){
  y1[1]=y0+sd*rnorm(1)
  for(i in 2:n){
     y1[i]=y1[i-1]+sd*rnorm(1)
  } 
  e3=testdata$PPI_index-y1
  MSPE3=sum(e3*e3)/n
  A=A+MSPE3
}
MSPE3=A/10000
cbind(MSPE1,MSPE2,A/10000)









  
