install.packages("installr",dep=T)
library(installr)
updateR()

#chap1.????- 1-11page
blood = c("B","A","B","A","A","B","O","A","A","A","O","B","AB","B","AB","AB","A","A","O","AB","O","A","B","O","B","B","A","A","O","A","A","AB","B","B","O","B","B","B","A","AB","A","A","B","O","B","B","O","B","O","B","A","A","AB","A","A")
cnt = sort(table(blood),decreasing = T) 
prop = prop.table(cnt) 
tbl_blood<-(cbind(cnt,prop) )
par(mfrow=c(2,1))
slices=c("red","blue","yellow","green")
pie(cnt,col = slices, radius = 1, main="blood type")
barplot(cnt,col=slices, main="blood type")
par(mfrow=c(1,1))



#chap1-???? ?ڷ??? ???? 1-15page
noise = c(55.9,63.8,57.2,59.8,65.7,62.7,60.8,51.3,61.8,56.0,66.9,56.8,66.2,64.6,59.5,63.1,60.6,62.0,59.4,67.2,63.6,60.5,66.8,61.8,64.8,55.8,55.7,77.1,62.1,61.0,58.9,60.0,66.9,61.7,60.3,51.5,67.0,60.2,56.2,59.4,67.9,64.9,55.7,61.4,62.6,56.4,56.4,69.4,57.6,63.8)
which(noise==63.8)
noise<-noise[1:49]
noise[which(noise==51.3)]<-45.7
noise[which(noise==45.7)]
noise = c(55.9,63.8,57.2,59.8,65.7,62.7,60.8,45.7,61.8,56.0,66.9,56.8,66.2,64.6,59.5,63.1,60.6,62.0,59.4,67.2,63.6,60.5,66.8,61.8,64.8,55.8,55.7,77.1,62.1,61.0,58.9,60.0,66.9,61.7,60.3,51.5,67.0,60.2,56.2,59.4,67.9,64.9,55.7,61.4,62.6,56.4,56.4,69.4,57.6)
length(noise)
mean(noise,na.rm = T, )
median(noise)
# https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
v<-c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
result<-getmode(v)
getmode(noise)
uniz<-unique(noise)
length(noise)
length(uniz)
tabulate(match(noise,uniz))
uniz
which.max(table(noise))

var(noise) # s^2
sd(noise)  # s

quantile(noise,type=7) #사분위수
(IQR<-quantile(noise,type = 7)[[4]]-quantile(noise,type = 7)[[2]])
(rangeIQR<-IQR*1.5)
(upperOutlier<-quantile(noise,type = 7)[[4]]+rangeIQR)
(bottomOutlier<-quantile(noise,type = 7)[[2]]-rangeIQR)
(noise[which(upperOutlier < noise)])
(noise[which(bottomOutlier > noise)])
par(mfrow=c(2,1))
hist(noise,freq=FALSE, breaks = 10, main="Noise")
rug(jitter(noise))
lines(density(noise),col="blue",lty=3)
boxplot(noise,horizontal=T,col = "yellow")
rug(jitter(noise))
par(mfrow=c(1,1))
write.csv(noise,file="noise.csv",quote = FALSE, row.names = FALSE)
getwd()

# chap1.??�� 1-20page
install.packages("MASS",dep=T)
library(MASS)
data(survey)
str(survey)
smoke<-sort(table(survey$Smoke),decreasing = T)
prop<-prop.table(smoke)
tbl_smoke<-cbind(smoke,prop)
par(mfrow=c(2,1))
slices=c("red","blue","yellow","green")
barplot(smoke,col=slices,main="Smoke")
pie(smoke,col=slices, main="Smoke")
par(mfrow=c(1,1))

#chap1.????3 1-22page
weight=c(101,177,178,184,185,185,185,185,188,190,200,205,
         205,206,210,210,210,212,212,215,215,220,223,228,
         230,232,241,241,242,245,247,250,250,259,260,260,
         265,265,270,272,273,275,276,278,280,280,285,285,
         286,290,290,295,302)
length(weight)
mean(weight,na.rm = T)
min(weight, na.rm=T)
median(weight, na.rm = T)
max(weight, na.rm=T)
var(weight) 
sd(weight) 
quantile(weight,type=7)
which.max(table(weight))
getmode(weight)
(IQR<-quantile(weight,type = 7)[[4]]-quantile(weight,type = 7)[[2]])
(rangeIQR<-IQR*1.5)
(upperOutlier<-quantile(weight,type = 7)[[4]]+rangeIQR)
(bottomOutlier<-quantile(weight,type = 7)[[2]]-rangeIQR)
(weight[which(upperOutlier < weight)])
(weight[which(bottomOutlier > weight)])
par(mfrow=c(2,1))
hist(weight,freq=FALSE, breaks = 10, main="Weight")
rug(jitter(weight))
lines(density(weight),col="blue",lty=3)
boxplot(weight,horizontal=T,col = "blue")
rug(jitter(weight))
par(mfrow=c(1,1))



#chap1.????3 1-28page
height <- c(170,178,171,168,173,178,171,174,170,170,175,
           170,169,166,162,170,171,175,175,171,171,170,
           172,179,164,170,181,178,180,177,166,169,168,
           165,163,175,166,178,165,168,167,177,168,177,
           174,174,176,179,169,173,167,170,173,170,162)

cut <- c(161.5,165.5,169.5,173.5,177.5,181.5)
hist(height,breaks=cut,probability=T)
lines(density(height),col="red",lty=1)

getwd()
write.csv(height,file="height.csv",quote = FALSE, row.names = FALSE)
write.csv(weight,file = "weight.csv",quote=F,row.names=F)


# exercise3
weight = c(101, 177, 178, 184, 185, 185, 185, 188, 190, 200, 205,
           205, 206, 210, 210, 210, 212, 212, 215, 220, 223, 228,
           230, 232, 241, 241, 242, 245, 247, 250, 250, 259, 260, 260,
           265, 265, 270, 272, 273, 275, 276, 278, 280, 280, 285, 285,
           286, 290, 290, 295, 302)
length(weight)
quantile(weight, type = 7)
which.max(table(weight))
(IQR<-quantile(weight, type = 7)[[4]] - quantile(weight, type = 7)[[2]])
(rangeIQR<-IQR*1.5)
(upperOutlier<-quantile(weight, type = 7)[[4]] + rangeIQR)
(bottomOutlier<-quantile(weight, type = 7)[[2]] - rangeIQR)
(weight[which(upperOutlier < weight)])
(weight[which(bottomOutlier < weight)])

hist(weight,probability=T)
lines(density(weight),col="red",lty=1)
