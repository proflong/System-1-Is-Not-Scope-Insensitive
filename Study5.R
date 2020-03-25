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
mydata <- read_csv("Study5.csv")

#define stimuli distribution
scaling_func <- function(x, x1, x3)(x-x1)/(x3-x1)  

d1 <- mydata %>% mutate(level1 = 0, 
                       level2 = 0.5,
                       level3 = 1,
                       plotlevel1=level1,
                       plotlevel2=ifelse(metric=="ppm", 0.25,level2),
                       plotlevel3=level3,
                       wtp1 = ifelse(metric=="spp", spp_1, ppm_1),
                       wtp2 = ifelse(metric=="spp", spp_2, ppm_2),
                       wtp3 = ifelse(metric=="spp", spp_3, ppm_3),
                       metricdummy = ifelse(metric=="spp",0,1),
                       timedummy=ifelse(timepressure=="no",0,1),
                       d_int=metricdummy*timedummy,
                       r.wtp1 = 0,
                       r.wtp2 = scaling_func(wtp2, wtp1, wtp3),
                       r.wtp3 = 1,
                       dif = wtp3-wtp1,
                       logdif=log(dif),
                       deleted= case_when(r.wtp2<0 | r.wtp2>1 | dif<=0 ~1, 
                                          r.wtp2>=0 | r.wtp2<=1 | dif>0 ~0)) 

table(d1$gender)
mean(d1$age, na.rm=T)
sd(d1$age, na.rm=T)

#removing those who failed the attention check (preregistered)
d2<-d1%>%
  filter(attnCheck==1)

#10 duplicated IP addresses (preregistered)
d3<-d2%>%filter(duplicated(IPAddress, incomparables=FALSE)== FALSE)


#removing non-monotonic responses (preregistered)
d <-d3 %>%
  filter(r.wtp2>=0 & r.wtp2<=1 & wtp1<wtp3) %>%
  mutate(mturkcode= 1:length(unique(ResponseId)))
  
                       
st.err<-function(x){
  sd(x)/sqrt(length(x))
}

#Means and sd by time pressure
aggregate(r.wtp2~ timepressure, data=d, mean)
aggregate(r.wtp2~ timepressure, data=d, sd)
#Does the time pressure simply produce greater variance in responses? No
bartlett.test(r.wtp2~timepressure, d)

#means and sd by condition
aggregate(r.wtp2~metric + timepressure, data=d, mean)


####model presented in paper
papermodel<-lm(r.wtp2~1+metricdummy +timedummy +metricdummy*timedummy, data=d)
summary(papermodel)
confint(papermodel)
eta_sq(papermodel)

#testing simple effect
spp_m<-lm(r.wtp2~1+timedummy, data=subset(d, metric == "spp"))
ppm_m<-lm(r.wtp2~1+timedummy, data=subset(d, metric == "ppm"))

summary(spp_m)
confint(spp_m)
eta_sq(spp_m)

summary(ppm_m)
confint(ppm_m)
eta_sq(ppm_m)


################################################################################################
#FIGURE 3
################################################################################################
r.ldata1<- d %>% 
  gather(index, r.wtp, r.wtp1:r.wtp3)
r.ldata2<- d %>% 
  gather(index, plotlevel, plotlevel1:plotlevel3)
r.ldata<- r.ldata1 %>% bind_cols(r.ldata2 %>% dplyr::select(plotlevel)) 

ppmdata<-subset(r.ldata, metric=="ppm")
ppmplot<-
  ggplot(data=ppmdata , aes(x=plotlevel, y =r.wtp)) +
  ylim(0,1)+ ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Pages-Per-Minute")+
  geom_jitter(data=subset(ppmdata, plotlevel>0 & plotlevel<1), 
              aes(shape=timepressure), width=.03, height=0.00, colour="grey")+
  stat_summary(fun.y=mean, geom="point", size=3, aes(shape=timepressure)) +
  stat_summary(fun.y=mean, geom="line", aes(linetype=timepressure))+
  scale_linetype_manual(name= "timepressure" , 
                        values=c("solid", "solid"))+
  scale_shape_manual(name= "timepressure" , 
                     values=c(16,17),  labels=c("Control","Time Pressure"))+
  guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = c(.7,.45),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,.25,1), 
                     labels=c("10\n(0.00)","15\n(0.25)","30\n(1.00)"))
ppmplot

sppdata<-subset(r.ldata, metric=="spp")
sppplot<-
  ggplot(data=sppdata , aes(x=plotlevel, y =r.wtp)) +
  ylim(0,1)+ ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Seconds-Per-Page")+
  geom_jitter(data=subset(sppdata, plotlevel>0 & plotlevel<1), 
              aes(shape=timepressure), width=.03, height=0.00,  colour="grey")+
  stat_summary(fun.y=mean, geom="point", size=3, aes(shape=timepressure)) +
  stat_summary(fun.y=mean, geom="line", aes(linetype=timepressure))+
  scale_linetype_manual(name= "timepressure" , 
                        values=c("solid", "solid"))+
  scale_shape_manual(name= "timepressure" , 
                     values=c(16,17),  labels=c("Control","Time Pressure"))+
  guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = c(.7,.45),
        legend.title = element_blank())+
  scale_x_continuous(breaks = c(0,.5,1), 
                     labels=c("6\n(0.00)","4\n(0.50)","2\n(1.00)"))
sppplot
#plot with ppm on x-axis
figure3<-grid.arrange(sppplot,ppmplot, nrow=1)

ggsave("Figure 3.svg", width = 12, height=6, figure3)


################################################################################################
#Online Appendix: COMPARING ABSOLUTE JUDGMENTS FOR THE LOWEST AND HIGHEST OPTIONS
################################################################################################

aggregate(wtp1~metric, data=subset(d, timepressure=="no"), mean)
aggregate(wtp1~metric, data=subset(d, timepressure=="yes"), mean)
aggregate(wtp3~metric, data=subset(d, timepressure=="no"), mean)
aggregate(wtp3~metric, data=subset(d, timepressure=="yes"), mean)

aggregate(wtp1~metric, data=subset(d, timepressure=="no"), median)
aggregate(wtp1~metric, data=subset(d, timepressure=="yes"), median)
aggregate(wtp3~metric, data=subset(d, timepressure=="no"), median)
aggregate(wtp3~metric, data=subset(d, timepressure=="yes"), median)

aggregate(wtp2~metric, data=subset(d, timepressure=="no"), mean)
aggregate(wtp2~metric, data=subset(d, timepressure=="yes"), mean)
aggregate(wtp2~metric, data=subset(d, timepressure=="no"), median)
aggregate(wtp2~metric, data=subset(d, timepressure=="yes"), median)


summary(aov(wtp1~metric, data=subset(d, timepressure=="no")))
kruskal.test(subset(d, timepressure=="no")$wtp1~as.factor(subset(d, timepressure=="no")$metric))
summary(aov(wtp1~metric, data=subset(d, timepressure=="yes")))
kruskal.test(subset(d, timepressure=="yes")$wtp1~as.factor(subset(d, timepressure=="yes")$metric))
summary(aov(wtp3~metric, data=subset(d, timepressure=="no")))
kruskal.test(subset(d, timepressure=="no")$wtp3~as.factor(subset(d, timepressure=="no")$metric))
summary(aov(wtp3~metric, data=subset(d, timepressure=="yes")))
kruskal.test(subset(d, timepressure=="yes")$wtp3~as.factor(subset(d, timepressure=="yes")$metric))


summary(aov(wtp2~metric, data=subset(d, timepressure=="no")))
kruskal.test(subset(d, timepressure=="no")$wtp2~as.factor(subset(d, timepressure=="no")$metric))
summary(aov(wtp2~metric, data=subset(d, timepressure=="yes")))
kruskal.test(subset(d, timepressure=="yes")$wtp2~as.factor(subset(d, timepressure=="yes")$metric))


################################################################################################
#FIGURE S6
################################################################################################
ldata1<- d %>% 
  gather(index, wtp, wtp1:wtp3)
ldata2<- d %>% 
    gather(index, plotlevel, plotlevel1:plotlevel3)
ldata<- ldata1 %>% bind_cols(ldata2 %>% select(plotlevel))

#geom_mean function  
gm_mean <-function(x){exp(mean(log(x+1)))}

ppmdata_abs<-subset(ldata, metric=="ppm")
ppmplot_abs<-ggplot(data=ppmdata_abs, aes(x=plotlevel, y =wtp, linetype=timepressure, shape=timepressure)) +
  ylab("Willingness to Pay")+
  coord_cartesian(ylim = c(40,120))+
 # ylim(25,75)+
  xlab("Pages Per Minute")+
 # geom_jitter(width=.03, colour="grey")+
  stat_summary(fun.y=gm_mean, geom="point") +
  stat_summary(fun.y=gm_mean, geom="line", size=.25)+
  #  facet_wrap(~cond, nrow=1,ncol=3)+
  # scale_color_manual(values=blacklist)+
  #theme(legend.position="none")+
  scale_linetype_manual(name= "timepressure" , 
                        values=c("solid", "solid"))+
  scale_shape_manual(name= "timepressure" , 
                     values=c(16,17),  labels=c("Control","Time Pressure"))+
  guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = c(.7,.45),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,.25,1), 
                     labels=c("10\n(0.00)","15\n(0.25)","30\n(1.00)"))
sppdata_abs<-subset(ldata, metric=="spp")
sppplot_abs<-ggplot(data=sppdata_abs, aes(x=plotlevel, y =wtp, linetype=timepressure, shape=timepressure)) +
  ylab("Willingness to Pay")+
  coord_cartesian(ylim = c(40,120))+
  #  ylim(25,75)+
  xlab("Seconds Per Page")+
 # geom_jitter(width=.03, colour="grey")+
  stat_summary(fun.y=gm_mean, geom="point") +
  stat_summary(fun.y=gm_mean, geom="line", size=.25)+
  #  facet_wrap(~cond, nrow=1,ncol=3)+
  # scale_color_manual(values=blacklist)+
  #theme(legend.position="none")+
  scale_linetype_manual(name= "timepressure" , 
                        values=c("solid", "solid"))+
  scale_shape_manual(name= "timepressure" , 
                     values=c(16,17),  labels=c("Control","Time Pressure"))+
  guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = c(.7,.45),
        legend.title = element_blank())+
  scale_x_continuous(breaks = c(0,.5,1), 
                     labels=c("6\n(0.00)","4\n(0.50)","2\n(1.00)"))


figures6<-grid.arrange(sppplot_abs,ppmplot_abs, nrow=1)
ggsave("Figure S6.svg", width = 12, height=6, figures6)





######################################################################################
######################For those interested in the pattern of nonmonotic responses########
#####################################################################################

#violations of nonmonotonicity by condition
d_mon<-d3 %>% mutate(nonmon= ifelse(wtp3<=wtp1 | wtp2<wtp1 |wtp2>wtp3, 1,0),
                     nonmon_low=ifelse(wtp2<wtp1 & wtp3>wtp1,1,0),
                     nonmon_high=ifelse(wtp2>wtp3 & wtp3>wtp1,1,0),
                     nonmon_other=ifelse(wtp3<=wtp1,1,0))
table(d_mon$timepressure, d_mon$metric, d_mon$nonmon)
table(d_mon$timepressure, d_mon$metric, d_mon$nonmon_low)
table(d_mon$timepressure, d_mon$metric, d_mon$nonmon_high)
table(d_mon$timepressure, d_mon$metric, d_mon$nonmon_other)
summary(glm(nonmon ~ timepressure + metric + timepressure*metric, data = d_mon, family = "binomial"))

control<-subset(d_mon, timepressure=="no")

fisher.test(table(control$metric, control$nonmon_low))
fisher.test(table(control$metric, control$nonmon_high))
fisher.test(table(control$metric, control$nonmon_other))
fisher.test(table(control$metric, control$nonmon))

timepressure<-subset(d_mon, timepressure=="yes")

fisher.test(table(timepressure$metric, timepressure$nonmon_low))
fisher.test(table(timepressure$metric, timepressure$nonmon_high))
fisher.test(table(timepressure$metric, timepressure$nonmon_other))
fisher.test(table(timepressure$metric, timepressure$nonmon))

#Poisson model
Poissonmod<- glm(nonmon_other ~metric+timepressure, family = "poisson", data = d_mon)
summary(Poissonmod)
Poissonmodint<- glm(nonmon_other ~metric*timepressure, family = "poisson", data = d_mon)
summary(Poissonmodint)




