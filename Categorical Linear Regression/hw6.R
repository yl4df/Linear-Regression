getwd()
setwd("/Users/yunluli/Desktop/stat5120/hw6")

data <- read.table("Valuation.txt", header=TRUE ,sep="")
attach(data)


noncorner<-subset(data,Lot=="0") 
corner<-subset(data,Lot=="1")

reg1<-lm(Price~Value,data=noncorner)
reg2<-lm(Price~Value,data=corner)

plot(Value,Price, main="Plot of Price against Value by Lot")
points(noncorner$Value,noncorner$Price, pch=20, col="red") ##generates 1 type of points for mutual firms
points(corner$Value,corner$Price, pch=20, col="blue") ##generates another type of points for stock firms
abline(reg1,lty=1, col="red") ##overlay the respective fitted lines
abline(reg2,lty=2, col="blue") ##overlay the fitted line, with different line type
legend("topleft", c("Non Corner","Corner"), lty=c(1,2), pch=20, col=c("red","blue")) ##add legend, first argument is placement of the legend. 


result<-lm(Price~Value*Lot)
summary(result)


plot(result$fitted.values,result$residuals, main="Residual Plot")
abline(h=0, col="red")

acf(result$residuals, main="ACF of Residuals")

qqnorm(result$residuals)
qqline(result$residuals, col="red")

library(lawstat)
levene.test(Price, Lot)
