#To whomever is looking at this code. This is old code. While correct, it isn't pretty. My coding skills have improved greatly since this was written. My apologies :)
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
mydata<-read_csv("StudyS2.csv")

#ALIGNING COLUMNS. BACK WHEN THIS WAS COLLECTED, WE WERE PROGRAMMING CONDITIONS IN QUALTRICS INTO BLOCKS. THIS PUTS THE DV'S
#INTO COMBINED COLUMNS AND CREATES A COLUMN INDICATING CONDITION
mydata$cond <- ifelse(!is.na(mydata$a_wtp3), "acc", 
                      ifelse(!is.na(mydata$b_wtp3), "uni",
                             ifelse(!is.na(mydata$c_wtp3), "dec",NA)))


mydata$wtp3 <- as.numeric(ifelse(!is.na(mydata$a_wtp3), mydata$a_wtp3, 
                      ifelse(!is.na(mydata$b_wtp3), mydata$b_wtp3,
                             ifelse(!is.na(mydata$c_wtp3), mydata$c_wtp3 ,NA))))
mydata$wtp2 <-  as.numeric(ifelse(!is.na(mydata$a_wtp2), mydata$a_wtp2, 
                      ifelse(!is.na(mydata$b_wtp2), mydata$b_wtp2,
                             ifelse(!is.na(mydata$c_wtp2), mydata$c_wtp2 ,NA))))
mydata$wtp1 <-  as.numeric(ifelse(!is.na(mydata$a_wtp1), mydata$a_wtp1, 
                      ifelse(!is.na(mydata$b_wtp1), mydata$b_wtp1,
                             ifelse(!is.na(mydata$c_wtp1), mydata$c_wtp1 ,NA))))

mydata$qual3 <- as.numeric(ifelse(!is.na(mydata$a_qual3), mydata$a_qual3, 
                                 ifelse(!is.na(mydata$b_qual3), mydata$b_qual3,
                                        ifelse(!is.na(mydata$c_qual3), mydata$c_qual3 ,NA))))
mydata$qual2 <-  as.numeric(ifelse(!is.na(mydata$a_qual2), mydata$a_qual2, 
                                  ifelse(!is.na(mydata$b_qual2), mydata$b_qual2,
                                         ifelse(!is.na(mydata$c_qual2), mydata$c_qual2 ,NA))))
mydata$qual1 <-  as.numeric(ifelse(!is.na(mydata$a_qual1), mydata$a_qual1, 
                                  ifelse(!is.na(mydata$b_qual1), mydata$b_qual1,
                                         ifelse(!is.na(mydata$c_qual1), mydata$c_qual1 ,NA))))
mydata$level <-as.numeric(ifelse(!is.na(mydata$a_qual2), 0.2, 
                                 ifelse(!is.na(mydata$b_qual2), 0.5,
                                        ifelse(!is.na(mydata$c_qual2), 0.8 ,NA))))
mydata$ppnr<-1:length(mydata$cond)+1000

#HERE WE ARE SIMPLY SPECIFYING THE RESCALED ATTRIBUTE LEVELS
mydata$r.level1<-0
mydata$r.level2<-(mydata$level)
mydata$r.level3<-1

#BELOW IS A SCALING FUNCTION FOR RESCALING THE WTP JUDGMENTS BETWEEN 0 AND 1
scaling_func <- function(x, x1, x3)(x-x1)/(x3-x1)  

######################################################################################
######################For those interested in the pattern of nonmonotic responses########
#####################################################################################

#violations of nonmonotonicity by condition
d_mon<-mydata %>% mutate(nonmon_wtp= ifelse(wtp3<=wtp1 | wtp2<wtp1 |wtp2>wtp3, 1,0),
                         nonmon_wtp_low=ifelse(wtp3<wtp2 & wtp3>=wtp1,1,0),
                         nonmon_wtp_high=ifelse(wtp1>wtp2 & wtp3>=wtp1,1,0),
                         nonmon_wtp_other=ifelse(wtp3<=wtp1,1,0),
                         nonmon_qual= ifelse(qual3<=qual1 | qual2<qual1 |qual2>qual3, 1,0),
                         nonmon_qual_low=ifelse(qual3<qual2 & qual3>=qual1,1,0),
                         nonmon_qual_high=ifelse(qual1>qual2 & qual3>=qual1,1,0),
                         nonmon_qual_other=ifelse(qual3<=qual1,1,0))
table(d_mon$cond, d_mon$nonmon_wtp)   
fisher.test(table(d_mon$cond, d_mon$nonmon_wtp_low))  
fisher.test(table(d_mon$cond, d_mon$nonmon_wtp_high))
fisher.test(table(d_mon$cond, d_mon$nonmon_wtp_other))
fisher.test(table(d_mon$cond, d_mon$nonmon_wtp))

fisher.test(table(d_mon$cond, d_mon$nonmon_qual)) 
fisher.test(table(d_mon$cond, d_mon$nonmon_qual_low))   
fisher.test(table(d_mon$cond, d_mon$nonmon_qual_high)) 
fisher.test(table(d_mon$cond, d_mon$nonmon_qual_other))
chisq.test(table(d_mon$cond, d_mon$nonmon_qual))



mydata2<- mydata %>%  
  #STRICT MONOTONICITY BETWEEN THE 15 AND 5 MP CAMERAS AND WEAK MONOTONICITY FOR THE INTERMEDIATE ALTERNATIVE.
  filter(wtp3>=wtp2 & wtp1<=wtp2 & wtp3>wtp1) %>% 
        mutate(r.wtp1 = 0,
                           r.wtp2 = scaling_func(wtp2, wtp1, wtp3),
                           r.wtp3 = 1,
                           rangewtp=abs(wtp3-wtp1),
                           log_rangewtp = log(rangewtp),
                           range_center=rangewtp - mean(rangewtp),
                           log_range_center=log_rangewtp - mean(log_rangewtp))

#DESCRIPTIVES FOR THE RESCALED INTERMEDIATE ALTERNATIVE BY CONDITION
aggregate(r.wtp2~cond, data=mydata2, mean)                         
aggregate(r.wtp2~cond, data=mydata2, sd)

#T-TEST OF RELATIVE WTP MINUS RELATIVE ATTRIBUTE LEVEL
acc<-lm(r.wtp2-level~ 1 , data=subset(mydata2, cond=="acc"))
uni<-lm(r.wtp2-level~ 1 ,  data=subset(mydata2, cond=="uni"))
dec<-lm(r.wtp2-level~ 1 , data=subset(mydata2, cond=="dec"))
summary(acc)
summary(uni)
summary(dec)



#DOING THE SAME THING FOR THE QUALITY RATINGS. SINCE WE ARE CONDITIONING MONOTONICITY WITHIN A JUDGMENT CONTEXT, THIS WILL BE A
#SLIGHTLY DIFFERENT DATASET
mydata3<- mydata %>% mutate(r.qual1 = 0,
                            r.qual2 = scaling_func(qual2, qual1, qual3),
                            r.qual3 = 1,
                            rangequal=abs(qual3-qual1),
                            range_qualcenter= rangequal-mean(rangequal))%>%
  filter(qual2 >= qual1 & qual2 <= qual3 & qual3>qual1) 

hist(mydata2$r.wtp2[mydata2$cond=="acc"])
hist(mydata2$r.wtp2[mydata2$cond=="dec"])



aggregate(r.qual2~cond, data=mydata3, mean)
aggregate(r.qual2~cond, data=mydata3,sd)

acc<-lm(r.qual2-level~ 1  , data=subset(mydata3, cond=="acc"))
uni<-lm(r.qual2-level~ 1, data=subset(mydata3, cond=="uni"))
dec<-lm(r.qual2-level~ 1, data=subset(mydata3, cond=="dec"))
summary(acc)
summary(uni)
summary(dec)

acc_cont<-lm(r.qual2-level~ 1 +rangequal , data=subset(mydata3, cond=="acc"))
uni_cont<-lm(r.qual2-level~ 1 +rangequal ,  data=subset(mydata3, cond=="uni"))
dec_cont<-lm(r.qual2-level~ 1 +qual3 , data=subset(mydata3, cond=="dec"))
totalmodel_cont<-lm(r.qual2-level~1+ relevel(as.factor(cond), ref= "uni")*range_qualcenter, data=mydata3)
summary(acc_cont)
summary(uni_cont)
summary(dec_cont)
summary(totalmodel_cont)

######################################################################################
### FIGURE S2
#####################################################################################
r.ldata1<- mydata2 %>% 
  gather(index, r.wtp, r.wtp1:r.wtp3)
r.ldata2<- mydata2 %>% 
  gather(index, r.level, r.level1:r.level3)
r.ldata_wtp<- r.ldata1 %>% bind_cols(r.ldata2 %>% select(r.level)) 
r.ldata_wtp$spacing_figure<-ifelse(r.ldata_wtp$cond=="acc", "Accelerated",
                               ifelse(r.ldata_wtp$cond=="uni", "Uniform",
                                      ifelse(r.ldata_wtp$cond=="dec", "Decelerated",NA)))
r.ldata_wtp$spacing_figure <- factor(r.ldata_wtp$spacing_figure, levels = c("Accelerated", "Uniform", "Decelerated"))



r.ldata3<- mydata3 %>%  
  gather(index, r.qual, r.qual1:r.qual3)
r.ldata4<- mydata3 %>% 
  gather(index, r.level, r.level1:r.level3)
r.ldata_qual<- r.ldata3 %>% bind_cols(r.ldata4 %>% select(r.level)) 
r.ldata_qual$spacing_figure<-ifelse(r.ldata_qual$cond=="acc", "Accelerated",
                               ifelse(r.ldata_qual$cond=="uni", "Uniform",
                                      ifelse(r.ldata_qual$cond=="dec", "Decelerated",NA)))
r.ldata_qual$spacing_figure <- factor(r.ldata_qual$spacing_figure, levels = c("Accelerated", "Uniform", "Decelerated"))




wtpplot<-
  ggplot(r.ldata_wtp, aes(x=r.level, y =r.wtp, linetype=spacing_figure)) +
  ylim(0,1)+ ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Attribute Level")+
  geom_jitter(data=subset(r.ldata_wtp, r.level>0 & r.level<1), width=0.03, height=0.0, color="grey")+
 stat_summary(fun.y=mean, geom="point") +
 stat_summary(fun.y=mean, geom="line", size=.25)+
    scale_linetype_manual(name= "spacing_figure" , 
                          values=c("dashed", "solid", "dotted"),  
                          labels=c("Accelerated","Uniform", "Decelerated"))+
    theme(text=element_text(family="Times New Roman", size=12),
          legend.title = element_blank(),
          legend.position = c(.7,.2))+ 
    #guides(linetype=FALSE)+
    scale_x_continuous(breaks = c(0,.20, 0.5,.8,1), 
                       labels=c("5\n(0.00)","7\n(0.20)","10\n(0.50)","13\n(0.80)","15\n(1.00)"))
  
  

qualplot <- ggplot(r.ldata_qual, aes(x=r.level, y =r.qual, linetype=spacing_figure)) +
  ylim(0,1)+ ylab("Perceived Quality")+
  xlim(0,1)+ xlab("Attribute Level")+
  geom_jitter(data=subset(r.ldata_qual, r.level>0 & r.level<1), width=0.03, height=0.0, color="grey")+
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line", size=.25)+
  scale_linetype_manual(name= "spacing_figure" , 
                        values=c("dashed", "solid", "dotted"),  
                        labels=c("Accelerated","Uniform", "Decelerated"))+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.title = element_blank(),
        legend.position = c(.7,.2))+ 
  #guides(linetype=FALSE)+
  scale_x_continuous(breaks = c(0,.20, 0.5,.8,1), 
                     labels=c("5\n(0.00)","7\n(0.20)","10\n(0.50)","13\n(0.80)","15\n(1.00)"))


figures2<-grid.arrange(wtpplot,qualplot, nrow=1)
ggsave("Figure S2.svg", width = 12, height=6, figures2)



######################################################################################
### Analysis of Raw judgments in Online Appendix
#####################################################################################
aggregate(log(rangewtp)~cond, data=mydata2, mean)
aggregate(log(meanwtp)~cond, data=mydata2, mean)
summary(aov(log(rangewtp)~cond, data=mydata2))
summary(aov(log(meanwtp)~cond, data=mydata2))


aggregate(wtp1~cond, data=mydata2, mean)
aggregate(wtp3~cond, data=mydata2, mean)

aggregate(wtp1~cond, data=mydata2, median)
aggregate(wtp3~cond, data=mydata2, median)

aggregate(qual1~cond, data=mydata3, mean)
aggregate(qual3~cond, data=mydata3, mean)

aggregate(qual1~cond, data=mydata3, median)
aggregate(qual3~cond, data=mydata3, median)

aggregate(wtp2~cond, data=mydata2, mean)
aggregate(wtp2~cond, data=mydata2, median)
aggregate(qual2~cond, data=mydata3, mean)
aggregate(qual2~cond, data=mydata3, median)



#ANOVAS and KRUSKAL-WALLIS tests
summary(aov(wtp1~cond, data=mydata2))
kruskal.test(mydata2$wtp1~as.factor(mydata2$cond))
summary(aov(wtp3~cond, data=mydata2))
kruskal.test(mydata2$wtp3~as.factor(mydata2$cond))


summary(aov(qual1~cond, data=mydata3))
kruskal.test(mydata3$qual1~as.factor(mydata3$cond))
summary(aov(qual3~cond, data=mydata3))
kruskal.test(mydata3$qual3~as.factor(mydata3$cond))

summary(aov(wtp2~cond, data=mydata2))
kruskal.test(mydata2$wtp2~as.factor(mydata2$cond))
summary(aov(qual2~cond, data=mydata3))
kruskal.test(mydata3$qual2~as.factor(mydata3$cond))

######################################################################################
### FIGURE S10
#####################################################################################

ldata1<- mydata %>% 
  gather(index, wtp, wtp1:wtp3)
ldata2<- mydata %>% 
  gather(index, qual, qual1:qual3)
ldata3<- mydata %>% 
  mutate(level1=5,
         level2=(10*level)+5,
         level3=15) %>%
  gather(index, level_var, level1:level3)
ldata<- ldata1 %>% bind_cols(ldata2 %>% select(qual)) %>% bind_cols(ldata3 %>% select(level_var))
ldata$spacing_figure<-ifelse(ldata$cond=="acc", "Accelerated",
                                    ifelse(ldata$cond=="uni", "Uniform",
                                           ifelse(ldata$cond=="dec", "Decelerated",NA)))
ldata$spacing_figure <- factor(ldata$spacing_figure, levels = c("Accelerated", "Uniform", "Decelerated"))

#geom_mean function  
gm_mean <-function(x){exp(mean(log(x)))}



wtpplot<-
  ggplot(ldata, aes(x=level_var, y =wtp, linetype=spacing_figure)) +
  ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Attribute Level")+
  stat_summary(fun.y=gm_mean, geom="point") +
  stat_summary(fun.y=gm_mean, geom="line", size=.25)+
  scale_linetype_manual(name= "spacing_figure" , 
                        values=c("dashed", "solid", "dotted"),  
                        labels=c("Accelerated","Uniform", "Decelerated"))+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.title = element_blank(),
        legend.position = c(.7,.2))+ 
  #guides(linetype=FALSE)+
  scale_x_continuous(breaks = c(5,7, 10,13,15), 
                     labels=c("5\n(0.00)","7\n(0.20)","10\n(0.50)","13\n(0.80)","15\n(1.00)"))



qualplot <- ggplot(ldata, aes(x=level_var, y =qual, linetype=spacing_figure)) +
  ylab("Perceived Quality")+
  xlim(0,1)+ xlab("Attribute Level")+
  stat_summary(fun.y=gm_mean, geom="point") +
  stat_summary(fun.y=gm_mean, geom="line", size=.25)+
  scale_linetype_manual(name= "spacing_figure" , 
                        values=c("dashed", "solid", "dotted"),  
                        labels=c("Accelerated","Uniform", "Decelerated"))+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.title = element_blank(),
        legend.position = c(.7,.2))+ 
  #guides(linetype=FALSE)+
  scale_x_continuous(breaks = c(5,7, 10,13,15), 
                     labels=c("5\n(0.00)","7\n(0.20)","10\n(0.50)","13\n(0.80)","15\n(1.00)"))


figures10<-grid.arrange(wtpplot,qualplot, nrow=1)

ggsave("Figure S10.svg", width = 12, height=6, figures10)

