rm(list=ls())

library(extrafont)
library(devtools)
library(rstan)
library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)
library(extrafont)
library(extrafontdb)
library(Rttf2pt1)
library(ggbeeswarm)
library(gridExtra)
library(cowplot)
library(readr)
library(gtable)
library(lemon)
library(Cairo)
library(mlogit)
library(sjstats)
library(svglite)

x1<-c(0,.75,1)
x2<-c(0,.025,.5,.975,1)
x3<-c(0,.475,.5,.525,1)
f1<-c(0,.5,1)
f2<-c(0,.25,.5,.75,1)
y11<-(.66666*x1)+(.33333*f1)
y12<-(.33333*x1)+(.66666*f1)
y21<-(.66666*x2)+(.3333*f2)
y22<-(.33333*x2)+(.6666*f2)
y31<-(.66666*x3)+(.3333*f2)
y32<-(.33333*x3)+(.6666*f2)
conds<-c()


data1<-data.frame(cbind(x1,f1,y11,y12))
data2<-data.frame(cbind(x2,f2,y21,y22)) %>% gather(index, y, c('y21','y22')) %>%
  mutate(cond=ifelse(index=='y21','Single-Attribute (w = 0.66)','Multi-Attribute  (w  =0.33)')) 
data3<-data.frame(cbind(x3,f2,y31,y32))%>% gather(index, y, c('y31','y32')) %>%
  mutate(cond=ifelse(index=='y31','Single-Attribute (w = 0.66)','Multi-Attribute  (w = 0.33)'))


#plots
p2<-ggplot(data=data2, aes(x=x2, y=y, linetype=cond))+
  ylab("Value")+ xlab("Attribute Level")+
  xlim(0,1)+ ylim(0,1)+
  theme(text=element_text(family="Times New Roman", size=12),panel.background = element_blank(), 
        legend.title=element_blank(), legend.position=c(0.45,0.25))+
  geom_line()

p3<-ggplot(data=data3, aes(x=x3, y=y, linetype=cond))+
  ylab("Value")+ xlab("Attribute Level")+
  xlim(0,1)+ ylim(0,1)+
  theme(text=element_text(family="Times New Roman", size=12),panel.background = element_blank(), 
        legend.title=element_blank(), legend.position=c(0.45,0.25))+
  geom_line()
figure5<-grid.arrange(p2,p3, nrow=1)

ggsave("Figure 5.svg", width = 10, height=4.5, figure5)

