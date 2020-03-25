rm(list=ls())

#These are my default packages. I don't always use all of them
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


#Importing dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read_csv("Study3.csv")

df2<-df %>% mutate(choose_donate=ifelse(donate==1,1,0),
                   #mean-centering predictor variables to facilitate interpretation of coefficients
                   labels_num=ifelse(labels=="no",-.5,.5),
                   spacing_num=ifelse(spacing=="accel",-.5,.5))

#one participant indicated their year of birth (1996) rather than age (~24)
df2$age<-ifelse(df2$age==1996,24,df2$age)
mean(df2$age, na.rm=T)
sd(df2$age, na.rm=T)
table(df2$gender)

means<-df2 %>% 
  group_by(spacing, labels) %>%
  summarise(mean=mean(choose_donate, na.rm=T), sd=sd(choose_donate, na.rm=T), n=length(choose_donate))



m<-glm(choose_donate~ labels_num + spacing_num +labels_num*spacing_num, data=df2, family="binomial")  
summary(m)     
confint(m)
exp(cbind(coef(m)))


#if someone is interested in simple-effects tests
  
subset_acc<-subset(df2, spacing=='accel')
subset_dec<-subset(df2, spacing=='decel')
m_accel<-glm(choose_donate~ labels , data=subset_acc, family="binomial")  
summary(m_accel)     
m_decel<-glm(choose_donate~ labels , data=subset_dec, family="binomial")  
summary(m_decel) 
