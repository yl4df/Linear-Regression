getwd()
setwd("/Users/yunluli/Desktop/stat5120/hw7")

data<-read.table("Marketshare.txt", header=TRUE, sep="")
attach(data)

x<-cbind(Const=1, price, nielsen, discount, promo, time)
y<-Share
predictor_names<-c("price","nielsen","discount","promo","time") 
number_predictor<-5 
n<-nrow(data)

logic.list<-vector("list", number_predictor)
for( i in 1:number_predictor){
  logic.list[[i]]<-c(F,T)
}
names(logic.list)<-predictor_names
models<-as.matrix(expand.grid(logic.list))

results<-matrix(NA,2^number_predictor,7) 
dimnames(results)<-list(NULL, c("p","R2","R2.adj","PRESS","AIC","BIC","Cp")) 

tmp<-lsfit(x,y,intercept=F)
MSE.max<-sum(tmp$res^2)/(n-number_predictor-1)
for (i in 1:(2^number_predictor)){ 
  
  which<-c(T, models[i,]) 
  tmp<-lsfit(x[,which], y, intercept=F) 
  p<-sum(which) 
  SSTo<-(n-1)*var(y)
  MSTo<-var(y)
  SSE<-sum(tmp$res^2)
  MSE<-SSE/(n-p)
  R2<-1-(SSE/SSTo)
  R2.adj<-1-(MSE/MSTo)
  hi<-ls.diag(tmp)$hat 
  res.PRESS<-tmp$res/(1-hi)
  PRESS<-sum(res.PRESS^2)
  AIC<-n*log(SSE/n)+2*p
  BIC<-n*log(SSE/n)+p*log(n)
  Cp<-(SSE/MSE.max)-n+2*p
  
  results[i,1]<-p
  results[i,2]<-R2
  results[i,3]<-R2.adj
  results[i,4]<-PRESS
  results[i,5]<-AIC
  results[i,6]<-BIC
  results[i,7]<-Cp
  
  print(paste("run",i))
}
p<-results[,1]
R2<-results[,2]
R2.adj<-results[,3]
PRESS<-results[,4]
AIC<-results[,5]
BIC<-results[,6]
Cp<-results[,7]

i1<-which.max(R2.adj) ##which model has the max R2.adj
models[i1,]

i2<-which.min(Cp) ##which model has the min Cp
models[i2,]

i3<-which.min(AIC) ##which model has the min AIC
models[i3,]

i4<-which.min(BIC) ##which model has the min AIC
models[i4,]

i5<-which.min(PRESS) ##which model has the min PRESS
models[i5,]

reg<-lm(Share~price+discount+promo)
plot(reg$fitted.values,reg$residuals,xlab="Fitted Values",ylab='Residuals',main="residual plot")
abline(h=0, col='red')
summary(reg)

plot(p,AIC)
identify(p,AIC)
models[13,]

plot(p,PRESS)
identify(p,PRESS)
models[6,]


result13<-lm(Share~discount+promo)
result6<-lm(Share~price+discount)

library(DAAG)
press(result13)
s13<-summary(result13)$sigma
mse13<-s13^2
sse13<-mse13*summary(result13)$df[2]
sse13

press(result6)
s6<-summary(result6)$sigma
mse6<-s6^2
sse6<-mse6*summary(result6)$df[2]
sse6

start<-lm(Share~1, data=data)
end<-lm(Share~.,data=data)
result.f<-step(start, scope=list(lower=start,upper=end), direction="forward")
summary(result.f)
 
result.b<-step(end, direction="backward")
summary(result.b)

library(leaps)

b<-regsubsets(Share~.,data=data,force.in=c(2),nbest=2^5)
plot(b,scale="adjr2", main="adj R2 criterion",nbest=2^5)
