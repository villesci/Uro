library(gridExtra)
library(tidyr)
library(ggplot2)

P1<-read.csv(here::here('data/heat.bar.ramping.P1.csv'))
P1<-na.omit(P1)
P1$acc<-as.factor(P1$acc)
P1[54,5] = 24.8
#View(P1)
summary(P1)


##wide to long format
P1_long<-gather(P1,time,temp,t0:t5)
P1_long[114,4] = 't1'
#View(P1_long)
summary(P1_long)

P120<-subset(P1_long, acc == "20")

#View(P120)
##PLot of a sample P1 at 20 and 24
ggplot(P1_long,aes(x=time,y=temp,group=interaction(col,acc), color=acc)) + geom_line()+labs(x="Time (hrs)",y="Temperature (?C)",color="acclimation temperature")

P2<-read.csv(here::here('data/heat.bar.ramping.P2.csv'))
P2<-na.omit(P2)       
P2$acc<-as.factor(P2$acc)
summary(P2)

##wide to long format
P2_long<-gather(P2,time,temp,t0:t5)
P2_long[62,5] = 25.5
P2_long[152,5] = 29.3
#View(P2_long)

ggplot(P2_long,aes(x=time,y=temp,group=interaction(col,acc),color=acc))+geom_line()+labs(x="Time (hrs)",y="Temperature (?C)",color="acclimation temperature")

##both!

P1P2<-read.csv(here::here('data/heat.bar.ramping.P1P2.csv'))
P1P2<-na.omit(P1P2)
P1P2$acc<-as.factor(P1P2$acc)
P1P2_long<-gather(P1P2,time,temp,t0:t5)
summary(P1P2_long)
#View(P1P2_long)
P1P2_long[144,6] = 24.8
P1P2_long[144,5] = 't1'
P1P2_long[242,6]= 25.5
P1P2_long[512,6]= 29.3
ggplot(P1P2_long,aes(x=time,y=temp,group=interaction(col,acc,protocol),color=protocol))+geom_line()+labs(x="Time (hrs)",y="Temperature (?C)",color="Protocol")+facet_wrap(acc~.)
##no 24
df<-filter(P1P2_long, acc!="24")
ggplot(df,aes(x=time,y=temp,group=interaction(col,protocol),color=protocol))+geom_line(size=1)+labs(x="Time (hrs)",y="Temperature (?C)",color="acclimation temperature")
 
##let's model 
#Filter by protocol
p1<-filter(df,protocol=="P1")
p2<-filter(df,protocol=="P2")
#filter by time
p1<-filter(p1,time>"t2"&time<="t5")
p2<-filter(p2,time>"t2"&time<="t5")
#filter by 4 runs around the CTmax (37-38C)
p1<-filter(p1,col>="25"&col<="29")
p2<-filter(p2,col>="20"&col<="24")
#construct linear models
m.p1<-lm(temp~time,p1)
m.p2<-lm(temp~time,p2)

all<-glm(temp~time+protocol,filtered,family="gaussian")
summary(all)
hist(resid(all))

filtered<-rbind(p1,p2)

ggplot(filtered,aes(x=time,y=temp,group=interaction(col,protocol),color=protocol))+geom_point()+geom_smooth(method="lm",aes(group=protocol))

### Figures for paper
P1_long$prot<-"1"
P2_long$prot<-"2"
prots<-rbind(P1_long,P2_long)

protss<-c("Protocol 1","Protocol 2")
names(protss)<-c("1","2")

#protocol 1 and 2
ggplot(prots,aes(x=time,y=temp,group=interaction(col,acc),color=acc))+facet_wrap(prot~.,labeller=labeller(prot=protss))+geom_line()+labs(x="Time (hrs)",y="Temperature (?C)",color="acclimation temperature")+theme_classic()+
  theme(text=element_text(family="arial",size=22))+scale_color_manual(name=expression(atop(paste("Acclimation"),"Temperature")),labels=c("20?C","24?C"),values=c("blue","red"))+
  scale_x_discrete(labels=c("0","1","2","3","4","5"))

#just protocol 1
ggplot(P1_long,aes(x=time,y=temp,group=interaction(col,acc),color=acc))+geom_line(size=1)+labs(x="Time (hrs)",y="Temperature (°C)",color="acclimation temperature")+theme_classic()+
  theme(text=element_text(family="arial",size=22))+scale_color_manual(name=expression(atop(paste("Acclimation"),"Temperature")),labels=c("20°C","24°C"),values=c("blue","red"))+
  scale_x_discrete(labels=c("0","1","2","3","4","5"))

