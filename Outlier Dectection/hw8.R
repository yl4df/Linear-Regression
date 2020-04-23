getwd()
setwd("/Users/yunluli/Desktop/stat5120/hw8")
data<-read.table("Marketshare.txt", header=TRUE, sep="")
attach(data)
result<-lm(Share~price+discount+promo)
summary(result)

res<-result$residuals

standard.res<-rstandard(result)

student.res<-rstudent(result)

plot(result$fitted.values,res,xlab='Fitted Value', ylab="Residuals", main="Residuals")
plot(result$fitted.values,standard.res,xlab='Fitted Value', ylab="Residuals", main="Standardized Residuals")
plot(result$fitted.values,student.res,xlab='Fitted Value', ylab="Residuals", main="Studentized Residuals")

sort(student.res)

qt(1-0.05/72, 31)

lev<-lm.influence(result)$hat

plot(lev, res, ylab="Residuals", xlab="Leverages", main="Residuals against Leverages")
abline(v=2*4/36)

lev[lev>2*4/36]

data2<-data[-36,]

result2<-lm(Share~price+discount+promo,data=data2)

summary(result2)

result$fitted.values[36]

res.PRESS<-res/(1-lev) 
yhat.i<-Share-res.PRESS 
yhat.i[36]

DFFITS<-dffits(result)
DFFITS[abs(DFFITS)>2*sqrt(4/36)]

COOK<-cooks.distance(result)
F<-qf(0.5,4,32)
COOK[COOK>F]
