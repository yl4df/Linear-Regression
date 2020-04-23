getwd()
setwd("/Users/yunluli/Desktop/stat5120/hw9")
data<-read.table("insects.txt")
colnames(data)<-c("logdose","size","died")
attach(data)
p<-died/size
plot(logdose, log(p/(1-p)), main="Log odds against logdose")
result<-glm(p~logdose, family = binomial, weights=size)
summary(result)
linpred<- -2.64369+0.67399*2
odds<-exp(linpred)
odds

prob<-exp(linpred)/(1+exp(linpred))
prob

1-pchisq(381.6204,1)

pearson<-residuals(result,type="pearson")
X2<-sum(pearson^2)
X2
1-pchisq(X2,6-2)

1-pchisq(summary(result)$deviance,6-2)
