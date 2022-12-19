library(ggplot2)
library(dplyr)
library(TSdist)

wp15<-read.csv(here::here('data/environmental_data/rawPacific/pac/wp_test_15.csv'))
wp18<-read.csv(here::here('data/environmental_data/rawPacific/pac/wp_test_18.csv'))
hm15<-read.csv(here::here('data/environmental_data/rawPacific/pac/hum_test_15.csv'))
hm18<-read.csv(here::here('data/environmental_data/rawPacific/pac/hum_test_18.csv'))

wp<-rbind(wp15,wp18)
wp<-filter(wp,WTMP!=999)

summary(aov(YY~WTMP,wp))
ggplot(wp,aes(x=YY,y=WTMP,group=YY))+geom_boxplot()


hm<-rbind(hm15,hm18)
hm<-filter(hm,WTMP!=999)

summary(aov(YY~WTMP,hm))
ggplot(hm,aes(x=YY,y=WTMP,group=YY))+geom_boxplot()

ACFDistance(wp15$WTMP,wp18$WTMP)
ACFDistance(hm15$WTMP,hm18$WTMP)
