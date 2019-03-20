Github username: vohuong1004
public repository: 890THQ1
file name: 45515840-THQ1
#Question 1
Coupon=C
Face value=F
e=2.71828
The number of coupon payments=n
# C is paid every 6 months
tn=n/2
y=c(y(0.5),y(1),y(1.5),...,y(tn))
tj=seq(0.5,tn,by=0.5)
P=sum(C*e^-(y*tj))+F*e^-(y(tn)*tn)
#Question 3
(a)
dataset=read.csv(file.choose())
dataset
(b)
dataset=na.omit(dataset)
dataset
(c)
plot(dataset$time,dataset$gdp,main = "Singapore GDP growth",xlab = "Time",ylab = "GDP(%)")
(d)
x1<- mean(subset(dataset,period==1)$gdp, trim=0.10)
y1<-sd(subset(dataset,period==1)$gdp)
x2<- mean(subset(dataset,period==2)$gdp, trim=0.10)
y2<-sd(subset(dataset,period==2)$gdp)
x3<- mean(subset(dataset,period==3)$gdp, trim=0.10)
y3<-sd(subset(dataset,period==3)$gdp)
stat.table<-matrix(c(x1,y1,x2,y2,x3,y3),3,2)
colnames(stat.table)<-c("mean","standard deviation")
rownames(stat.table)<-c("per1","per2","per3")
stat.table
(e)
pairs(dataset[,-(1:2)])
(f)
lm(dataset$exp~dataset$gdp)
summary(lm(dataset$exp~dataset$gdp))
According to the result which represents the relationship between export growth rate and GDP,  we have the simple linear regression:
  predicted GDP = -0.9336+1.5092 x (x:export growth rate)
In addition F(1, 108) = 43.66, p = 1.524e-09 and exp assumes 28.13% of the variability in GDP
(g)
G=lm(dataset$exp+dataset$epg+dataset$hpr+dataset$gdpus+dataset$oil+dataset$crd~dataset$gdp)
summary(G)
The result indicates that GDP could be predicted based on many variables as exp, epg, hpr, gdpus, oil, crd . 
F(1, 108) = 1.141, p = 0.2879 and 6 variables assume 0.1287% the variability in GDP.
(h)
q=quantile(dataset$gdp,0.05)
state=rep("crisis",nrow(dataset))
state[dataset$gdp>q]="normal"
state=as.factor(state)
state
dataset=data.frame(dataset,state)
glm(dataset$bci[1:72]~dataset$state[1:72])
