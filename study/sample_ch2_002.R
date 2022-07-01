install.packages("installr",dep=T)
library(installr)
updateR()


data(cars)

install.packages("ggplot2",dep=T)
library(ggplot2)

#2-20
mu<-3000
sigma<-80
x1<-2948
x2<-3080
pnorm(x2,mean = mu, sd=sigma)-pnorm(x1,mean = mu,sd=sigma)

z1<-(x1-mu)/sigma
z2<-(x2-mu)/sigma
pnorm(z2)-pnorm(z1)


dnorm_range<-function(x, a=checkX[1], b=checkX[2]){
  # print("*")
  # print(x)
  y<-dnorm(x)
  y[ x< a | x > b ] <-NA
  return(y)
}
pnorm_range<-function(x, a=-5, b=5){
  # print("*")
  # print(x)
  y<-pnorm(x)
  y[ x< a | x > b ] <-NA
  return(y)
}
#checkX<-c(-Inf,+Inf)
checkX<-c(-5,5)
floorK<-z1
ceilK<-z2
belowNotation=-0.015
#dnorm
ggplot(data.frame(x=checkX), aes(x=x)) + 
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=dnorm_range, args=list(a=floorK,b=ceilK),geom="area", fill="grey", alpha=0.5) +
  geom_vline(xintercept=0.0, colour="grey", linetype="dashed", size=1) +
  annotate("text",x=floorK,y=round(dnorm_range(floorK),3),label=round(dnorm_range(floorK),3),size=3, col="red") + 
  annotate("text",x=ceilK,y=round(dnorm_range(ceilK),3),label=round(dnorm_range(ceilK),3),size=3, col="red") + 
  annotate("text",x=floorK,y=belowNotation, label=floorK, size=3, col="red" ) +
  annotate("text",x=ceilK,y=belowNotation, label=ceilK, size=3, col="red" ) 
#pnorm
ggplot(data.frame(x=checkX), aes(x=x)) + 
  stat_function(fun=pnorm, colour="blue", size=1) +
  stat_function(fun=pnorm_range, args=list(a=floorK,b=ceilK),geom="area", fill="grey", alpha=0.5) +
  geom_vline(xintercept=floorK, colour="grey", linetype="dashed", size=1) +
  geom_vline(xintercept=ceilK, colour="grey", linetype="dashed", size=1) +
  annotate("text",x=floorK-0.75,y=round(pnorm_range(floorK),3),label=round(pnorm_range(floorK),3),size=3, col="red") + 
  annotate("text",x=ceilK+0.75,y=round(pnorm_range(ceilK),3),label=round(pnorm_range(ceilK),3),size=3, col="red") + 
  annotate("text",x=floorK,y=belowNotation, label=floorK, size=3, col="red" ) +
  annotate("text",x=ceilK,y=belowNotation, label=ceilK, size=3, col="red" ) 

#2-30 / exam 
mu<-5.3
sigma<-2.1
x1<-5
x2<-10
pnorm(10,mean=mu, sd=sigma)-pnorm(5,mean=mu, sd=sigma)
1-pnorm(2,mean=mu, sd=sigma)
z1<-(x1-mu)/sigma
z2<-(x2-mu)/sigma
pnorm(z2)-pnorm(z1)

dnorm_range<-function(x, a=checkX[1], b=checkX[2]){
  # print("*")
  # print(x)
  y<-dnorm(x)
  y[ x< a | x > b ] <-NA
  return(y)
}
pnorm_range<-function(x, a=checkX[1], b=checkX[2]){
  # print("*")
  # print(x)
  y<-pnorm(x)
  y[ x< a | x > b ] <-NA
  return(y)
}
#checkX<-c(-Inf,+Inf)
checkX<-c(-5,5)
floorK<-z1
ceilK<-z2
belowNotation=-0.015
#dnorm
ggplot(data.frame(x=checkX), aes(x=x)) + 
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=dnorm_range, args=list(a=floorK,b=ceilK),geom="area", fill="grey", alpha=0.5) +
  geom_vline(xintercept=0.0, colour="grey", linetype="dashed", size=1) +
  annotate("text",x=floorK,y=round(dnorm_range(floorK),3),label=round(dnorm_range(floorK),3),size=3, col="red") + 
  annotate("text",x=ceilK,y=round(dnorm_range(ceilK),3),label=round(dnorm_range(ceilK),3),size=3, col="red") + 
  annotate("text",x=floorK,y=belowNotation, label=round(floorK,3), size=3, col="red" ) +
  annotate("text",x=ceilK,y=belowNotation, label=round(ceilK,3), size=3, col="red" ) 
#pnorm
ggplot(data.frame(x=checkX), aes(x=x)) + 
  stat_function(fun=pnorm, colour="blue", size=1) +
  stat_function(fun=pnorm_range, args=list(a=floorK,b=ceilK),geom="area", fill="grey", alpha=0.5) +
  geom_vline(xintercept=floorK, colour="grey", linetype="dashed", size=1) +
  geom_vline(xintercept=ceilK, colour="grey", linetype="dashed", size=1) +
  annotate("text",x=floorK-0.75,y=round(pnorm_range(floorK),3),label=round(pnorm_range(floorK),3),size=3, col="red") + 
  annotate("text",x=ceilK+0.75,y=round(pnorm_range(ceilK),3),label=round(pnorm_range(ceilK),3),size=3, col="red") + 
  annotate("text",x=floorK,y=belowNotation, label=round(floorK,3), size=3, col="red" ) +
  annotate("text",x=ceilK,y=belowNotation, label=round(ceilK,3), size=3, col="red" ) 

#2-32 / exam
mu<-100
sigma<-15
x1<-120
p1<-0.98
1-pnorm(x1,mean=100,sd=sigma)
qnorm(p1,mean=100,sd=sigma)
z1<-(x1-mu)/sigma
z2<-5 #default value = 5
#checkX<-c(-Inf,+Inf)
checkX<-c(-5,5)
floorK<-z1
ceilK<-z2
belowNotation=-0.015
#dnorm
ggplot(data.frame(x=checkX), aes(x=x)) + 
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=dnorm_range, args=list(a=floorK,b=ceilK),geom="area", fill="grey", alpha=0.5) +
  geom_vline(xintercept=0.0, colour="grey", linetype="dashed", size=1) +
  annotate("text",x=floorK,y=round(dnorm_range(floorK),3),label=round(dnorm_range(floorK),3),size=3, col="red") + 
  annotate("text",x=ceilK,y=round(dnorm_range(ceilK),3),label=round(dnorm_range(ceilK),3),size=3, col="red") + 
  annotate("text",x=floorK,y=belowNotation, label=round(floorK,3), size=3, col="red" ) +
  annotate("text",x=ceilK,y=belowNotation, label=round(ceilK,3), size=3, col="red" ) 
#pnorm
ggplot(data.frame(x=checkX), aes(x=x)) + 
  stat_function(fun=pnorm, colour="blue", size=1) +
  stat_function(fun=pnorm_range, args=list(a=floorK,b=ceilK),geom="area", fill="grey", alpha=0.5) +
  geom_vline(xintercept=floorK, colour="grey", linetype="dashed", size=1) +
  geom_vline(xintercept=ceilK, colour="grey", linetype="dashed", size=1) +
  annotate("text",x=floorK-0.75,y=round(pnorm_range(floorK),3),label=round(pnorm_range(floorK),3),size=3, col="red") + 
  annotate("text",x=ceilK+0.75,y=round(pnorm_range(ceilK),3),label=round(pnorm_range(ceilK),3),size=3, col="red") + 
  annotate("text",x=floorK,y=belowNotation, label=round(floorK,3), size=3, col="red" ) +
  annotate("text",x=ceilK,y=belowNotation, label=round(ceilK,3), size=3, col="red" ) 

#2-35 / excercise
mu<-36
sigma<-10
x1<-25
x2<-40
p1<-0.25
(z1<-(x1-mu)/sigma)
z2<-(x2-mu)/sigma
1-pnorm(z2)
qnorm(p1,mean=mu, sd=sigma)

#checkX<-c(-Inf,+Inf)
checkX<-c(-5,5)
floorK<-z1
ceilK<-z2
belowNotation=-0.015
#dnorm
ggplot(data.frame(x=checkX), aes(x=x)) + 
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=dnorm_range, args=list(a=floorK,b=ceilK),geom="area", fill="grey", alpha=0.5) +
  geom_vline(xintercept=0.0, colour="grey", linetype="dashed", size=1) +
  annotate("text",x=floorK,y=round(dnorm_range(floorK),3),label=round(dnorm_range(floorK),3),size=3, col="red") + 
  annotate("text",x=ceilK,y=round(dnorm_range(ceilK),3),label=round(dnorm_range(ceilK),3),size=3, col="red") + 
  annotate("text",x=floorK,y=belowNotation, label=round(floorK,3), size=3, col="red" ) +
  annotate("text",x=ceilK,y=belowNotation, label=round(ceilK,3), size=3, col="red" ) 
#pnorm
ggplot(data.frame(x=checkX), aes(x=x)) + 
  stat_function(fun=pnorm, colour="blue", size=1) +
  stat_function(fun=pnorm_range, args=list(a=floorK,b=ceilK),geom="area", fill="grey", alpha=0.5) +
  geom_vline(xintercept=floorK, colour="grey", linetype="dashed", size=1) +
  geom_vline(xintercept=ceilK, colour="grey", linetype="dashed", size=1) +
  annotate("text",x=floorK-0.75,y=round(pnorm_range(floorK),3),label=round(pnorm_range(floorK),3),size=3, col="red") + 
  annotate("text",x=ceilK+0.75,y=round(pnorm_range(ceilK),3),label=round(pnorm_range(ceilK),3),size=3, col="red") + 
  annotate("text",x=floorK,y=belowNotation, label=round(floorK,3), size=3, col="red" ) +
  annotate("text",x=ceilK,y=belowNotation, label=round(ceilK,3), size=3, col="red" ) 

