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
mydata <- read_csv("Study4.csv")
#######AGE GENDER######
table(mydata$gender)
mean(mydata$age, na.rm=T)
sd(mydata$age, na.rm=T)


# Structuring data
scaling_func <- function(x, x1, x3)(x-x1)/(x3-x1)  
#these are the correct answers to the CRT
crt1_ans<-c(47)
crt2_ans<-c(5)
crt3_ans<-c(.05)

#These are the systematically incorrect answers to the CRT in the online appendix
ord_crt1_ans<-c(24)
ord_crt2_ans<-c(100)
ord_crt3_ans<-c(.10)

mydata2<- mydata %>%
  mutate(
    wta1=wta_1,
    wta2=wta_2,
    wta3=wta_3,
    r.wta1 = scaling_func(wta1, wta1, wta3),
    r.wta2 = scaling_func(wta2, wta1, wta3),
    r.wta3 = scaling_func(wta3, wta1, wta3),
    level1 = 0,
    level2 = ifelse(mid==10, .20, .80),
    level3 = 1,
    #identifying non-monotonicity
    nonmon = ifelse(wta2<wta1,1, ifelse(wta3<wta2, 1, ifelse(wta3<=wta1, 1, 0))),
    logdif= log(wta3-wta1),
    avgjudg = log(wta3+wta2+wta1/3),
    crt1= ifelse(CRT1 %in% crt1_ans, 1, 0),
    crt2= ifelse(CRT2 %in% crt2_ans, 1, 0),
    crt3= ifelse(CRT3 %in% crt3_ans, 1, 0),
    ordcrt1= ifelse(CRT1 %in% ord_crt1_ans, 1, 0),
    ordcrt2= ifelse(CRT2 %in% ord_crt2_ans, 1, 0),
    ordcrt3= ifelse(CRT3 %in% ord_crt3_ans, 1, 0),
    ordcrt=ordcrt1+ordcrt2+ordcrt3,
    crt=crt1+crt2+crt3,
    absdev=abs(r.wta2-level2),
    crt_split=ifelse(crt<=1,"low","high"),
    cutoff1= ifelse(wta1<=.20, 1, 0),
    cutoff2= ifelse(wta2<=.25 & mid==10, 1, ifelse(wta2<=.40 & mid==16,1,0)), 
    cutoff3= ifelse(wta3<=.45, 1, 0),
    cond=ifelse(mid==10,"acc",ifelse(mid==16,"dec",NA))
    )


#number of failed comprehension check
table(mydata2$compcheck)
#identifying nonmonotonicity
table( mydata2$nonmon)

table( mydata2$gender)
sd(mydata2$age, na.rm=T)



#removing nonmonotic observations not fixing the dollar/cent issue and removing those who indicated that they were not interested in further surveys
#This code removes both nonmonotic responses and those who chose not to give offers as well as those who did not give a response to each option
filtereddata<-mydata2 %>% filter(nonmon==0)   %>%
  mutate(condcode=ifelse(mid==10, .2,.8),
    condcenter=condcode-.5,
    crt_center=crt-mean(crt,na.rm=T),
    ord_center=ordcrt-mean(ordcrt,na.rm=T),
    mturkcode=1:length(unique(ResponseId)))


########################################################################################################
#######################model in paper###################################################################
########################################################################################################
papermodel<-lm(r.wta2~1+ condcenter +crt_center +condcenter*crt_center , data=filtereddata)
summary(papermodel)
confint(papermodel)
eta_sq(papermodel)



#putting data in long for for later
r.ldata1<- filtereddata %>% 
  gather(index, r.wta, r.wta1:r.wta3)
r.ldata2<- filtereddata %>% 
  gather(index, r.level, level1:level3) %>%
  mutate(level=(r.level*10)+8)
r.ldata3<- filtereddata %>% 
  gather(index, cutoff, cutoff1:cutoff3)
r.ldata4<- filtereddata %>% 
  gather(index, wta, wta1:wta3) %>% mutate(logwta=log(wta+1))
r.ldata_wta<- r.ldata1 %>% bind_cols(r.ldata2 %>% dplyr::select(r.level))%>% 
  bind_cols(r.ldata3 %>% dplyr::select(cutoff)) %>%
  bind_cols(r.ldata4 %>% dplyr::select(wta))


########################################################################################################
###################Analysis of Economic Consequences####################################################
########################################################################################################
gm_mean <-function(x){exp(mean(log(x)))}
detach(package:plyr) 
econ_data<-filtereddata %>% group_by(level2) %>%
  summarize(geommean1=gm_mean(wta1),
            geommean2=gm_mean(wta2),
            geommean3=gm_mean(wta3)) %>%
  mutate(marq1=geommean1/8,
         marq2=ifelse(level2==.2, geommean2/10, ifelse(level2==.8,geommean2/16,NA)),
         marq3=geommean3/18,
         impliedrate=(geommean3-geommean1)/10,
         implied2=ifelse(level2==.2, geommean1+(2*impliedrate),
                         ifelse(level2==.8, geommean1+(8*impliedrate),NA)),
         mimplied2=ifelse(level2==.2,implied2/10, ifelse(level2==.8,implied2/16,NA)),
         difmid=geommean2-implied2,
         difperc=(difmid/implied2)*100)
aggregate(wta1 ~ mid, data=filtereddata, gm_mean)
aggregate(wta2 ~ mid , data=filtereddata, gm_mean)
aggregate(wta3 ~ mid, data=filtereddata, gm_mean)




########################################################################################################
#######################Figure 2#########################################################################
########################################################################################################

figure2<-ggplot(data=r.ldata_wta, aes(x=r.level, y =r.wta)) +
  ylim(0,1)+ ylab("Willingness to Accept")+
  xlim(0,1)+ xlab("Multiple-Choice Questions")+
  geom_jitter(data=subset(r.ldata_wta, r.level>0 & r.level<1), 
              width=.03,height=0.0, colour="grey",shape=8)+
  stat_summary(fun.y=mean, geom="point", aes(shape=as.factor(crt)), size=3) +
  stat_summary(fun.y=mean, geom="line", aes(colour=as.factor(crt) ,linetype=cond))+
  scale_shape_manual(values=c(16,17,18,15))+
  scale_linetype_manual(labels=c("Accelerated", "Decelerated"),values=c("dashed", "dotted"))+
  scale_colour_manual(values=c("black","black","black","black"))+
  guides(colour=FALSE, linetype=guide_legend(order=1), shape=guide_legend(order=2))+
  labs(shape="CRT Score", linetype="")+
  scale_x_continuous(breaks = c(0,.2,.8,1), 
                     labels=c("8\n(0)","10\n(.2)","16\n(.8)","18\n(1)"))+
  theme(text=element_text(family="Times New Roman", size=12)) 

ggsave("Figure 2.svg", width = 9, height=6.5)




############################################################################################################################################
##########################Percent who completed the the extra task.########################
########################################################################################################################


extratasks<-filtereddata %>% mutate(
  complete1=case_when(cutoff1==1 & (!is.na(es1_1) |!is.na(es1_2)| 
                                      !is.na(es1_3) |!is.na(es1_4)|
                                      !is.na(es1_5) |!is.na(es1_6) |
                                      !is.na(es1_7) | !is.na(es1_8)) ~1),
  complete1=case_when( cutoff1==1 & (is.na(es1_1) & is.na(es1_2)& 
                                       is.na(es1_3) & is.na(es1_4)&
                                       is.na(es1_5) & is.na(es1_6) & 
                                       is.na(es1_7) & is.na(es1_8)) ~ 0),
  complete2=case_when(cutoff2==1 & mid==10 & (!is.na(es2_1_1) |!is.na(es2_1_2)| 
                                                !is.na(es2_1_3) |!is.na(es2_1_4)|
                                                !is.na(es2_1_5) |!is.na(es2_1_6) |
                                                !is.na(es2_1_7) | !is.na(es2_1_8)|
                                                !is.na(es2_1_9) | !is.na(es2_1_10)) ~1),
  complete2=case_when(cutoff2==1 & mid==16 & (!is.na(es2_2_1) |!is.na(es2_2_2)| 
                                                !is.na(es2_2_3) |!is.na(es2_2_4)|
                                                !is.na(es2_2_5) |!is.na(es2_2_6) |
                                                !is.na(es2_2_7) | !is.na(es2_2_8)|
                                                !is.na(es2_2_9) | !is.na(es2_2_10)|
                                                !is.na(es2_2_11) | !is.na(es2_2_12)|
                                                !is.na(es2_2_13) | !is.na(es2_2_14)|
                                                !is.na(es2_2_15) | !is.na(es2_2_16)) ~ 1),
  complete2=case_when(cutoff2==1 & mid==10 & (is.na(es2_1_1) &is.na(es2_1_2)& 
                                                is.na(es2_1_3) &is.na(es2_1_4)&
                                                is.na(es2_1_5) &is.na(es2_1_6) &
                                                is.na(es2_1_7) & is.na(es2_1_8)&
                                                is.na(es2_1_9) & is.na(es2_1_10)) ~0),
  complete2=case_when(cutoff2==1 & mid==16 & (is.na(es2_2_1) &is.na(es2_2_2)& 
                                                is.na(es2_2_3) &is.na(es2_2_4)&
                                                is.na(es2_2_5) &is.na(es2_2_6) &
                                                is.na(es2_2_7) & is.na(es2_2_8)&
                                                is.na(es2_2_9) & is.na(es2_2_10)&
                                                is.na(es2_2_11) & is.na(es2_2_12)&
                                                is.na(es2_2_13) & is.na(es2_2_14)&
                                                is.na(es2_2_15) & is.na(es2_2_16)) ~ 0),
  complete3=case_when(cutoff3==1 & (!is.na(es3_1) |!is.na(es3_2)| 
                                      !is.na(es3_3) |!is.na(es3_4)|
                                      !is.na(es3_5) |!is.na(es3_6) |
                                      !is.na(es3_7) | !is.na(es3_8)|
                                      !is.na(es3_9) | !is.na(es3_10)|
                                      !is.na(es3_11) | !is.na(es3_12)|
                                      !is.na(es3_13) | !is.na(es3_14)|
                                      !is.na(es3_15) | !is.na(es3_16)|
                                      !is.na(es3_17) | !is.na(es3_18)) ~1),
  complete3=case_when(cutoff3==1 & (is.na(es3_1) &is.na(es3_2)& 
                                      is.na(es3_3) &is.na(es3_4)&
                                      is.na(es3_5) &is.na(es3_6) &
                                      is.na(es3_7) & is.na(es3_8)&
                                      is.na(es3_9) & is.na(es3_10)&
                                      is.na(es3_11) & is.na(es3_12)&
                                      is.na(es3_13) & is.na(es3_14)&
                                      is.na(es3_15) & is.na(es3_16)&
                                      is.na(es3_17) & is.na(es3_18)) ~0))



table(extratasks$complete1)
table(extratasks$complete2)
table(extratasks$complete3)




############################################################################################################################################
####################Analysis of the Raw Judgments in the Online Appendix###################
########################################################################################################################



#COMPARING ABSOLUTE JUDGMENTS FOR THE LOWEST AND HIGHEST OPTIONS
aggregate(wta1~cond, data=subset(filtereddata, crt_split=="low"), mean)
aggregate(wta1~cond, data=subset(filtereddata, crt_split=="high"), mean)
aggregate(wta3~cond, data=subset(filtereddata, crt_split=="low"), mean)
aggregate(wta3~cond, data=subset(filtereddata, crt_split=="high"), mean)

aggregate(wta1~cond, data=subset(filtereddata, crt_split=="low"), median)
aggregate(wta1~cond, data=subset(filtereddata, crt_split=="high"), median)
aggregate(wta3~cond, data=subset(filtereddata, crt_split=="low"), median)
aggregate(wta3~cond, data=subset(filtereddata, crt_split=="high"), median)


aggregate(wta2~cond, data=subset(filtereddata, crt_split=="low"), mean)
aggregate(wta2~cond, data=subset(filtereddata, crt_split=="high"), mean)
aggregate(wta2~cond, data=subset(filtereddata, crt_split=="low"), median)
aggregate(wta2~cond, data=subset(filtereddata, crt_split=="high"), median)


summary(aov(wta1~cond, data=subset(filtereddata, crt_split=="low")))
kruskal.test(subset(filtereddata, crt_split=="low")$wta1~as.factor(subset(filtereddata, crt_split=="low")$cond))
summary(aov(wta1~cond, data=subset(filtereddata, crt_split=="high")))
kruskal.test(subset(filtereddata, crt_split=="high")$wta1~as.factor(subset(filtereddata, crt_split=="high")$cond))
summary(aov(wta3~cond, data=subset(filtereddata, crt_split=="low")))
kruskal.test(subset(filtereddata, crt_split=="low")$wta3~as.factor(subset(filtereddata, crt_split=="low")$cond))
summary(aov(wta3~cond, data=subset(filtereddata, crt_split=="high")))
kruskal.test(subset(filtereddata, crt_split=="high")$wta3~as.factor(subset(filtereddata, crt_split=="high")$cond))


summary(aov(wta2~cond, data=subset(filtereddata, crt_split=="low")))
kruskal.test(subset(filtereddata, crt_split=="low")$wta2~as.factor(subset(filtereddata, crt_split=="low")$cond))
summary(aov(wta2~cond, data=subset(filtereddata, crt_split=="high")))
kruskal.test(subset(filtereddata, crt_split=="high")$wta2~as.factor(subset(filtereddata, crt_split=="high")$cond))

gm_mean <-function(x){exp(mean(log(x)))}

aggregate(wta1 ~ mid, data=filtereddata, gm_mean)
aggregate(wta2 ~ mid , data=filtereddata, gm_mean)
aggregate(wta3 ~ mid, data=filtereddata, gm_mean)

summary(aov(avgjudg~mid, data=filtereddata))


############################################################################################################################################
####################Analyzing data using the systematically incorrect CRT coding####################
########################################################################################################################

#USING OTHER CRT CODED BASED ON INTUITIVE RESPONSES
summary(lm(r.wta2~1+ condcenter +ord_center +condcenter*ord_center , data=filtereddata))
confint(lm(r.wta2~1+ condcenter +ord_center +condcenter*ord_center , data=filtereddata))
eta_sq(lm(r.wta2~1+ condcenter +ord_center +condcenter*ord_center , data=filtereddata))

cor(filtereddata$crt,filtereddata$ordcrt)

########################################################################################################
#######################Figure S5 #######################################################################
########################################################################################################

#absolutes
ldata_wta<- filtereddata  %>% 
  gather(index, wta, wta1:wta3) %>%
  mutate(lnwta=log(wta))
ldata2<- filtereddata %>% 
  gather(index, level_var, level1:level3)
ldata<- ldata_wta %>% bind_cols(ldata2 %>% dplyr::select(level_var)) %>% 
  mutate(declowdummy=ifelse(cond=="dec" & crt_split =="low",1,0),
         dechighdummy=ifelse(cond=="dec" & crt_split =="high",1,0),
         acchighdummy=ifelse(cond=="acc" & crt_split =="high",1,0),
         decdummy=ifelse(cond=="dec",1,0),
         highdummy=ifelse(crt_split=="high",1,0))

gm_mean <-function(x){exp(mean(log(x)))}
blacklist<-c("black", "black", "black")

acc_abs_df<-subset(ldata, cond=="acc")
accplot_abs<-
  ggplot(data=acc_abs_df, aes(x=level_var, y =wta)) +
  coord_cartesian(ylim = c(.25,.90))+
  ylab("Willingness to Accept")+
  xlim(0,1)+ xlab("Multiple-Choice Questions")+
  stat_summary(fun.y=gm_mean, geom="point", aes(shape=as.factor(crt_split)), size=3) +
  stat_summary(fun.y=gm_mean, geom="line", aes(shape=as.factor(crt_split)), linetype="dashed")+
  scale_shape_manual(values=c(16,17), labels=c("High CRT", "Low CRT"))+
  guides(colour=FALSE, linetype=FALSE)+
  labs(shape="", linetype="")+
  scale_x_continuous(breaks = c(0,.2,1), 
                     labels=c("8\n(0)","10\n(.2)","18\n(1)"))+
  theme(text=element_text(family="Times New Roman", size=12),legend.position=c(.8,.4)) 

dec_abs_df<-subset(ldata, cond=="dec")
decplot_abs<-
  ggplot(data=dec_abs_df, aes(x=level_var, y =wta)) +
  coord_cartesian(ylim = c(.25,.90))+
  ylab("Willingness to Accept")+
  xlim(0,1)+ xlab("Multiple-Choice Questions")+
  stat_summary(fun.y=gm_mean, geom="point", aes(shape=as.factor(crt_split)), size=3) +
  stat_summary(fun.y=gm_mean, geom="line", aes(shape=as.factor(crt_split)), linetype="dotted")+
  scale_shape_manual(values=c(16,17), labels=c("High CRT", "Low CRT"))+
  guides(colour=FALSE, linetype=FALSE)+
  labs(shape="", linetype="")+
  scale_x_continuous(breaks = c(0,.8,1), 
                     labels=c("8\n(0)","16\n(.8)","18\n(1)"))+
  theme(text=element_text(family="Times New Roman", size=12),legend.position=c(.8,.4)) 


figures5<-grid.arrange(accplot_abs,decplot_abs, nrow=1)

ggsave("Figure S5.svg", width = 12, height=6.5, figures5)






############################################################################################################################################
####################For those interested in the pattern of nonmonotonic responses####################
########################################################################################################################

#violations of nonmonotonicity by condition
d_mon<-mydata2 %>% mutate(nonmon= ifelse(wta3<=wta1 | wta2<wta1 |wta2>wta3, 1,0),
                          nonmon_low=ifelse(wta2<wta1 & wta3>wta1,1,0),
                          nonmon_high=ifelse(wta2>wta3 & wta3>wta1,1,0),
                          nonmon_other=ifelse(wta3<=wta1,1,0),
                          same=ifelse(wta1==wta2 & wta2==wta3,1,0))
table(d_mon$nonmon, d_mon$same)

table(d_mon$crt_split, d_mon$cond, d_mon$nonmon)
table(d_mon$crt_split, d_mon$cond, d_mon$nonmon_low)
table(d_mon$crt_split, d_mon$cond, d_mon$nonmon_high)
table(d_mon$crt_split, d_mon$cond, d_mon$nonmon_other)
summary(glm(nonmon ~ crt_split + cond + crt_split*cond, data = d_mon, family = "binomial"))

chisq.test(table(d_mon$crt_split,d_mon$nonmon))

lowcrt<-subset(d_mon, crt_split=="low")

fisher.test(table(lowcrt$cond, lowcrt$nonmon_low))
fisher.test(table(lowcrt$cond, lowcrt$nonmon_high))
fisher.test(table(lowcrt$cond, lowcrt$nonmon_other))
fisher.test(table(lowcrt$cond, lowcrt$nonmon))

highcrt<-subset(d_mon, crt_split=="high")

fisher.test(table(highcrt$cond, highcrt$nonmon_high))
fisher.test(table(highcrt$cond, highcrt$nonmon_high))
fisher.test(table(highcrt$cond, highcrt$nonmon_other))
fisher.test(table(highcrt$cond, highcrt$nonmon))




#looking at patterns in those flagged as nonmonotonic
aggregate(cutoff1~crt_split+mid, data=mydata2, mean)
aggregate(cutoff2~crt_split+mid, data=mydata2, mean)
aggregate(cutoff3~crt_split+mid, data=mydata2, mean)
summary(aov(cutoff2~crt_split+cond +crt_split*cond, data=mydata2 ))




