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
mydata <-read_csv("StudyS1.csv")

scaling_func <- function(x, x1, x3)(x-x1)/(x3-x1)  
mydata2.1<- mydata %>% mutate(r.cam1 = 0,
                            r.cam2 = scaling_func(cam_b, cam_a, cam_c),
                            r.cam3 = 1,
                            r.tv1 = 0,
                            r.tv2 = scaling_func(tv_b, tv_a, tv_c),
                            r.tv3 = 1,
                            r.print1 = 0,
                            r.print2 = scaling_func(print_b, print_a, print_c),
                            r.print3 = 1,
                            cam_level1 =0,
                            cam_level2 =ifelse(space_cam=="acc",.3, ifelse(space_cam=="uni",.5, ifelse(space_cam=="dec",.7,NA))),
                            cam_level3 =1,
                            tv_level1 =0,
                            tv_level2 =ifelse(space_tv=="acc",.25, ifelse(space_tv=="uni",.5, ifelse(space_tv=="dec",.75,NA))),
                            tv_level3 =1,
                            print_level1 =0,
                            print_level2 =ifelse(space_print=="acc",.25, ifelse(space_print=="uni",.5, ifelse(space_print=="dec",.75,NA))),
                            print_level3 =1) 

######################################################################################
######################For those interested in the pattern of nonmonotic responses########
#####################################################################################
d_nonmon<-mydata2.1 %>% mutate(nonmon_cam= ifelse(cam_b<cam_a | cam_b>cam_c | cam_c<=cam_a,1,0),
                               nonmon_cam_low=ifelse(cam_b<cam_a & cam_c>cam_a,1,0),
                               nonmon_cam_high=ifelse(cam_b>cam_c & cam_c>cam_a,1,0),
                               nonmon_cam_other=ifelse(cam_c<=cam_a,1,0))%>%
  mutate(nonmon_tv= ifelse(tv_b<tv_a | tv_b>tv_c | tv_c<=tv_a,1,0),
         nonmon_tv_low=ifelse(tv_b<tv_a & tv_c>tv_a,1,0),
         nonmon_tv_high=ifelse(tv_b>tv_c & tv_c>tv_a,1,0),
         nonmon_tv_other=ifelse(tv_c<=tv_a,1,0)) %>%
  mutate(nonmon_print= ifelse(print_b<print_a | print_b>print_c | print_c<=print_a,1,0),
         nonmon_print_low=ifelse(print_b<print_a & print_c>print_a,1,0),
         nonmon_print_high=ifelse(print_b>print_c & print_c>print_a,1,0),
         nonmon_print_other=ifelse(print_c<=print_a,1,0))
  


table(d_nonmon$nonmon_cam)
table(d_nonmon$nonmon_tv)
table(d_nonmon$nonmon_print)





mydata2<-mydata2.1 %>%
  filter(r.cam2 >= 0 & r.cam2 <= 1 & cam_c>cam_a)  %>%
  filter(r.tv2 >= 0 & r.tv2 <= 1  & tv_c>tv_a)  %>%
  filter(r.print2 >= 0 & r.print2 <= 1 & print_c>print_a)

mydata3<-mydata2 %>% gather(index, r.wtp, c(r.cam2,r.tv2,r.print2))
mydata4<-mydata2 %>% gather(index, level, c(cam_level2, tv_level2, print_level2))
mydata4.1<-mydata2 %>% gather(index, condition, c(space_cam, space_tv, space_print))
mydata5<-mydata3 %>% bind_cols(mydata4 %>% select(level), mydata4.1 %>% select(condition)) 
mydata6<-arrange(mydata5, ppnr)
mydata6<-mydata6 %>% mutate(dif=level-r.wtp,
                            dummyuni=ifelse(condition=="uni",1,0),
                            dummyacc=ifelse(condition=="acc",1,0),
                            dummydec=ifelse(condition=="dec",1,0))


aggregate(r.tv2~tv_level2, data=mydata2, mean)
aggregate(r.print2~print_level2, data=mydata2, mean)
aggregate(r.cam2~cam_level2, data=mydata2, mean)
tv<-lm(r.tv2-tv_level2~ 1 + space_tv, data=mydata2)
print<-lm(r.print2-print_level2~ 1 + space_print, data=mydata2)
cam<-lm(r.cam2-cam_level2~ 1 + space_cam, data=mydata2)
summary(tv)
summary(print)
summary(cam)

aggregate(r.wtp~condition, data=mydata6, mean)
aggregate(level~condition, data=mydata6, mean)
summary(lm(dif~1, data=subset(mydata6, condition=="uni")))
summary(lm(dif~1, data=subset(mydata6, condition=="acc")))  
summary(lm(dif~1, data=subset(mydata6, condition=="dec")))


################################################################################################
#FIGURE S1
################################################################################################
r.ldata1<-mydata2 %>% gather(index,r.wtp, c(r.cam1,r.cam2,r.cam3,r.tv1,r.tv2,r.tv3,r.print1,r.print2,r.print3))
absdatalevel<- absdatalevel1 %>% gather(index, level, c(cam_level1, cam_level2, cam_level3, tv_level1, tv_level2, tv_level3, print_level1, print_level2, print_level3))
absdatacond<- absdatalevel1 %>% gather(index, cond_var, c(space_cam1,space_cam2,space_cam3,space_tv1,space_tv2,space_tv3,space_print1,space_print2,space_print3))
absdataproduct<-absdatalevel1 %>% gather(index, product, p1:p9)
r.ldata<- absdatalevel %>% bind_cols(r.ldata1 %>% dplyr::select(r.wtp), absdatacond %>% dplyr::select(cond_var), absdataproduct %>% dplyr::select(product))
r.ldata$spacing_figure<-ifelse(ldata$cond_var=="acc", "Accelerated",
                               ifelse(ldata$cond_var=="uni", "Uniform",
                                      ifelse(ldata$cond_var=="dec", "Decelerated",NA)))
r.ldata$spacing_figure <- factor(ldata$spacing_figure, levels = c("Accelerated", "Uniform", "Decelerated"))



cam_plot<-ggplot(data=subset(r.ldata,product=="cam"), aes(x=level, y =r.wtp, linetype=spacing_figure)) +
  ylab("Willingness to Pay")+
  ylim(0,1)+ xlim(0,1)+ xlab("Number of Megapixels")+
  geom_jitter(data=subset(r.ldata,product=="cam" & level>0 & level<1), width=0.03, hieght=0, color="grey")+
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")+
  #guides(linetype=FALSE)+
  ggtitle("Cameras")+
  scale_linetype_manual(name= "spacing_figure" , 
                        values=c("dashed", "solid", "dotted"),  
                        labels=c("Accelerated","Uniform", "Decelerated"))+
  theme(text=element_text(family="Times New Roman", size=12),
        panel.background = element_blank(),
        legend.position = c(.7,.2),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.3, 0.5, 0.7,1))

print_plot<-ggplot(data=subset(r.ldata,product=="print"), aes(x=level, y =r.wtp, linetype=spacing_figure)) +
  ylab("Willingness to Pay")+
  ylim(0,1)+xlim(0,1)+ xlab("Pages-Per-Minute")+
  geom_jitter(data=subset(r.ldata,product=="cam" & level>0 & level<1), width=0.03, hieght=0, color="grey")+
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")+
  scale_linetype_manual(name= "spacing_figure" , 
                        values=c("dashed", "solid", "dotted"),  
                        labels=c("Accelerated","Uniform", "Decelerated"))+
  ggtitle("Printers")+
  #guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        panel.background = element_blank(),
        legend.position = c(.7,.2),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.25, 0.50, 0.75,1))

tv_plot<-ggplot(data=subset(r.ldata,product=="tv"), aes(x=level, y =r.wtp, linetype=spacing_figure)) +
  ylab("Willingness to Pay")+
  ylim(0,1)+xlim(0,1)+ xlab("Screen Size")+ 
  geom_jitter(data=subset(r.ldata,product=="cam" & level>0 & level<1), width=0.03, hieght=0, color="grey")+
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")+
  scale_linetype_manual(name= "spacing_figure" , 
                        values=c("dashed", "solid", "dotted"),  
                        labels=c("Accelerated","Uniform", "Decelerated"))+
  ggtitle("Televisions")+
  # guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        panel.background = element_blank(),
        legend.position = c(.7,.2),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.25, 0.50, 0.75,1.0))

figures1<-grid.arrange(cam_plot,print_plot,tv_plot, nrow=1)
ggsave("Figure S1.svg", width = 12, height=4, figures1)



################################################################################################
#ANALYSIS OF RAW JUDGMENTS
################################################################################################
absdatalevel1<- mydata %>% mutate(cam_level1 =0,
                            cam_level2 =ifelse(space_cam=="acc",.3, ifelse(space_cam=="uni",.5, ifelse(space_cam=="dec",.7,NA))),
                            cam_level3 =1,
                            tv_level1 =0,
                            tv_level2 =ifelse(space_tv=="acc",.25, ifelse(space_tv=="uni",.5, ifelse(space_tv=="dec",.75,NA))),
                            tv_level3 =1,
                            print_level1 =0,
                            print_level2 =ifelse(space_print=="acc",.25, ifelse(space_print=="uni",.5, ifelse(space_print=="dec",.75,NA))),
                            print_level3 =1,
                            space_cam1=space_cam,
                            space_cam2=space_cam,
                            space_cam3=space_cam,
                            space_tv1 =space_tv,
                            space_tv2 =space_tv,
                            space_tv3 =space_tv,
                            space_print1=space_print,
                            space_print2=space_print,
                            space_print3=space_print,
                            p1="cam", p2="cam", p3="cam", p4="tv", p5="tv",p6="tv",p7="print",p8="print",p9="print")



absdatawtp<- mydata %>% gather(index, wtp, c(cam_a, cam_b, cam_c, tv_a, tv_b, tv_c,print_a, print_b, print_c)) %>% 
  mutate(ln_wtp = log(wtp))
absdatalevel<- absdatalevel1 %>% gather(index, level, c(cam_level1, cam_level2, cam_level3, tv_level1, tv_level2, tv_level3, print_level1, print_level2, print_level3))
absdatacond<- absdatalevel1 %>% gather(index, cond_var, c(space_cam1,space_cam2,space_cam3,space_tv1,space_tv2,space_tv3,space_print1,space_print2,space_print3))
absdataproduct<-absdatalevel1 %>% gather(index, product, p1:p9)
ldata<- absdatalevel %>% bind_cols(absdatawtp %>% select(wtp), absdatacond %>% select(cond_var), absdataproduct %>% select(product))
ldata$spacing_figure<-ifelse(ldata$cond_var=="acc", "Accelerated",
                             ifelse(ldata$cond_var=="uni", "Uniform",
                                    ifelse(ldata$cond_var=="dec", "Decelerated",NA)))
ldata$spacing_figure <- factor(ldata$spacing_figure, levels = c("Accelerated", "Uniform", "Decelerated"))

hist(log(ldata$wtp[ldata$product=='cam']))
hist(log(ldata$wtp[ldata$product=='tv']))
hist(log(ldata$wtp[ldata$product=='print']))

aggregate(log(wtp)~cond_var+level, data=subset(ldata, product=='cam'), mean)


summary(aov(log(wtp)~cond_var, data=subset(ldata, product=='cam' & level==0)))


################################################################################################
#FIGURE S9
################################################################################################
#geom_mean function  
gm_mean <-function(x){exp(mean(log(x)))}


cam_abs<-ggplot(data=subset(ldata,product=="cam"), aes(x=level, y =wtp, linetype=spacing_figure)) +
  coord_cartesian(ylim = c(50,250))+
  ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Number of Megapixels")+
  stat_summary(fun.y=gm_mean, geom="point")+
  stat_summary(fun.y=gm_mean, geom="line")+
  #guides(linetype=FALSE)+
  ggtitle("Cameras")+
  scale_linetype_manual(name= "spacing_figure" , 
                        values=c("dashed", "solid", "dotted"),  
                        labels=c("Accelerated","Uniform", "Decelerated"))+
  theme(text=element_text(family="Times New Roman", size=12),
        panel.background = element_blank(),
        legend.position = c(.7,.2),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.3, 0.5, 0.7,1))

print_abs<-ggplot(data=subset(ldata,product=="print"), aes(x=level, y =wtp, linetype=spacing_figure)) +
  coord_cartesian(ylim = c(75,225))+
  ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Pages-Per-Minute")+
  stat_summary(fun.y=gm_mean, geom="point")+
  stat_summary(fun.y=gm_mean, geom="line")+
  scale_linetype_manual(name= "spacing_figure" , 
                        values=c("dashed", "solid", "dotted"),  
                        labels=c("Accelerated","Uniform", "Decelerated"))+
  ggtitle("Printers")+
  #guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        panel.background = element_blank(),
        legend.position = c(.7,.2),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.25, 0.50, 0.75,1))

tv_abs<-ggplot(data=subset(ldata,product=="tv"), aes(x=level, y =wtp, linetype=spacing_figure)) +
  coord_cartesian(ylim = c(100,800))+
  ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Screen Size")+
  stat_summary(fun.y=gm_mean, geom="point")+
  stat_summary(fun.y=gm_mean, geom="line")+
  scale_linetype_manual(name= "spacing_figure" , 
                     values=c("dashed", "solid", "dotted"),  
                     labels=c("Accelerated","Uniform", "Decelerated"))+
  ggtitle("Televisions")+
   # guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        panel.background = element_blank(),
        legend.position = c(.7,.2),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.25, 0.50, 0.75,1.0))

figures9<-grid.arrange(cam_abs,print_abs,tv_abs, nrow=1)
ggsave("Figure S9.svg", width = 16, height=5, figures9)




