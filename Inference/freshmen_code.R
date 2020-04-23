getwd()
setwd("/Users/yunluli/Desktop/stat5120/hw2")
data<-read.table("Freshmen.txt", header=TRUE, sep="")
attach(data)
plot(RtSpan, Fastest, xlab="Student's Stretched Right Hand Span", ylab="Fastest Speed", 
     main="Plot of Fastest against RtSpan")
result=lm(Fastest~RtSpan)
summary(result)
cor(Fastest, RtSpan)

data.1<-subset(data, Sex=="M")
data.2<-subset(data, Sex=="F")
result.1=lm(data.1$Fastest~data.1$RtSpan)
cor(data.1$Fastest, data.1$RtSpan)
summary(result.1)
result.2=lm(data.2$Fastest~data.2$RtSpan)
summary(result.2)

col.list<-rep(0,length(Sex))
col.list[Sex=="M"]<-1
col.list[Sex=="F"]<-2
colors<-c("blue","red")
plot(RtSpan, Fastest, col=c(colors[col.list]), xlab="Student's Stretched Right Hand Span", ylab="Fastest Speed", 
     main="Plot of Fastest against RtSpan")
legend("topleft", c("Male","Female"),
       col = c("blue","red"), pch=c(1,1))

confint(result,level = 0.95)
anova(result)
newdata<-data.frame(Serviced=5)
predict.lm(result, newdata, interval="confidence", level=0.95)

4-qt(0.975,8)*0.469
4+qt(0.975,8)*0.469
(10.2-9)/sqrt(3.3)

qt(0.975,8)
18.2+2.306004*1.483*sqrt(1.1)
