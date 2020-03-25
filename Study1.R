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
mydata <- read_csv("Study1.csv")


#age gender for total sample
table(mydata$gender)
mean(mydata$age)
sd(mydata$age)

#Scaling function for calculating relative willingness to pay and relative attribute leves
scaling_func <- function(x, x1, x3)(x-x1)/(x3-x1)
d <- mydata %>% mutate(level1 = 0, 
                  level2 = plyr::revalue(spacing, c('acc' = .25, 'uni' = .5, 'dec' = .75)) %>% as.numeric,
                  level3 = 1,
                  freq1 = 0,
                  freq2 = .5,
                  freq3 = 1,
        #note, verbal labels are called "ordinal-labels" and numeric labels are called "control" in this datafile
                  labels_collapsed = ifelse(labels=="no", "control", "ordinal-labels" )) %>% 
  mutate(r.wtp1 = 0,
         r.wtp2 = scaling_func(pizza2, pizza1, pizza3),
         r.wtp3 = 1) %>%
  #weak monotonicity between intermediate option and small and large options.
  filter(r.wtp2 >= 0 & r.wtp2 <= 1) %>%
  #strong monotonicity between smallest and largest option.
  filter(pizza3 >= pizza1) %>%
  mutate(
    #these are all additional variables used in supplementary analyses
         rangewtp = pizza3-pizza1,
         avgwtp = (pizza3+pizza2+pizza1)/3,
         log_avgwtp = log(avgwtp),
         avgwtp_center = log_avgwtp-mean(log_avgwtp),
         log_range= log(rangewtp+1),
         log_wtp3=log(pizza3+1),
         range_center=log_range-mean(log_range,na.rm=T),
         label_number=ifelse(labels_collapsed=="control",0,ifelse(labels_collapsed=="ordinal-labels",1,NA)),
         label_center=(label_number-mean(label_number, na.rm=T)),
         level_center=(level2-.5))



#moving data to long form for plotting later on
d1 <- d %>% gather(index, r.wtp, r.wtp1:r.wtp3)
d2 <- d %>% gather(index, level, level1:level3)
d3 <- d %>% gather(index, freq, freq1:freq3)
d3.1<- d%>% gather (index, wtp, pizza1:pizza3)
d4 <- d1 %>% bind_cols(d2 %>% dplyr::select(level), d3 %>% dplyr::select(freq), d3.1 %>% dplyr::select(wtp)) %>%
  mutate(level_abs=(level*8)+12)
###remove for absolute graphs %>% filter(wtp<500)
d5<-d4 %>% filter(range_center<4)

#DESCRIPTIVES FOR THE RESCALED INTERMEDIATE ALTERNATIVE BY CONDITION
aggregate(r.wtp2~labels_collapsed+spacing, data=d , mean)
aggregate(r.wtp2~labels_collapsed+spacing, data=d , sd)


#T-TEST OF RELATIVE WTP MINUS RELATIVE ATTRIBUTE LEVEL
summary(lm(r.wtp2-level2~ 1 , data=subset(d, spacing=="acc" & labels_collapsed=="control")))
summary(lm(r.wtp2-level2~ 1 , data=subset(d, spacing=="acc" & labels_collapsed=="ordinal-labels")))
summary(lm(r.wtp2-level2~ 1 , data=subset(d, spacing=="dec" & labels_collapsed=="control")))
summary(lm(r.wtp2-level2~ 1 , data=subset(d, spacing=="dec" & labels_collapsed=="ordinal-labels")))
summary(lm(r.wtp2-level2~ 1 , data=subset(d, spacing=="uni" & labels_collapsed=="control")))
summary(lm(r.wtp2-level2~ 1 , data=subset(d, spacing=="uni" & labels_collapsed=="ordinal-labels")))

#Calculating confidence intervals on the mean values
confint(lm(r.wtp2~ 1 , data=subset(d, spacing=="acc" & labels_collapsed=="control")))
confint(lm(r.wtp2~ 1 , data=subset(d, spacing=="acc" & labels_collapsed=="ordinal-labels")))
confint(lm(r.wtp2~ 1 , data=subset(d, spacing=="dec" & labels_collapsed=="control")))
confint(lm(r.wtp2~ 1 , data=subset(d, spacing=="dec" & labels_collapsed=="ordinal-labels")))
confint(lm(r.wtp2~ 1 , data=subset(d, spacing=="uni" & labels_collapsed=="control")))
confint(lm(r.wtp2~ 1 , data=subset(d, spacing=="uni" & labels_collapsed=="ordinal-labels")))



acc<-lm(r.wtp2-level2~ 1+labels_collapsed , data=subset(d, spacing=="acc"))
uni<-lm(r.wtp2-level2~ 1+labels_collapsed ,  data=subset(d, spacing=="uni"))
dec<-lm(r.wtp2-level2~ 1+labels_collapsed , data=subset(d, spacing=="dec"))
summary(acc)
summary(uni)
summary(dec)

#aggregate model
total<-lm(r.wtp2~ label_number*level_center ,  data=d)
summary(total)
confint(total)
eta_sq(total)



###############################################################################
############Re-analysis of Study 1 using controlling for heterogeneity in range of responses ####
###############################################################################

#For analysis with absolute willingness to pay, there is a giant outlier that must be addressed
d_abs<-d%>% filter(range_center<4) %>%
  #recalculating range_center since the grand mean will shift after removing outlier
  mutate(range_center=log_range-mean(log_range,na.rm=T))


acc_cont<-lm(r.wtp2-level2~ 1 +labels_collapsed+range_center + labels_collapsed:range_center , data=subset(d, spacing=="acc"))
uni_cont<-lm(r.wtp2-level2~ 1 +labels_collapsed+range_center + labels_collapsed:range_center , data=subset(d, spacing=="uni"))
dec_cont<-lm(r.wtp2-level2~ 1 +labels_collapsed+range_center + labels_collapsed:range_center, data=subset(d, spacing=="dec"))
totalmodel_cont<-lm(r.wtp2-level2~1+ relevel(as.factor(spacing), ref= "uni")*labels_collapsed, data=d_abs)
totalmodel_cont<-lm(r.wtp2~1+ level2*labels_collapsed, data=d_abs)

summary(acc_cont)
summary(uni_cont)
summary(dec_cont)
summary(totalmodel_cont)

############################################################
############Figure 1########################################
############################################################
d4$spacing_figure<-ifelse(d4$spacing=="acc", "Accelerated",
                          ifelse(d4$spacing=="uni", "Uniform",
                                 ifelse(d4$spacing=="dec", "Decelerated",NA)))
d4$spacing_figure <- factor(d4$spacing_figure, levels = c("Accelerated", "Uniform", "Decelerated"))


allplot<-ggplot(d4, aes(y=r.wtp, x=level,  linetype=spacing_figure, shape=labels_collapsed)) +
  ylab("Willingness to Pay")+ ylim(0,1)+
  xlab("Pizza Size")+
  #ggtitle(names(name))+
  geom_jitter(data=subset(d4, level>0 & level<1), width=.03, height=0.0, colour="grey")+
  stat_summary(fun.y=mean, geom="line") +
  stat_summary(fun.y=mean, geom="point", size=3) +
  theme(legend.title = element_blank(),
        strip.background = element_blank())+
  scale_linetype_manual(name= "spacing_figure" , 
                        values=c("dashed","solid", "dotted"))+
  scale_shape_manual(name= "labels_collapsed" , 
                     values=c(16,17),  labels=c("No Labels","Ordinal Labels"))+
  theme(text=element_text(size=12), axis.text = element_text(size=12))+
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(size=12)) +
  guides(linetype=guide_legend(order=1), shape=guide_legend(order=2))+
  theme(plot.title=element_text(family="Times New Roman", size=12, face="plain"))+ 
  #guides(linetype=FALSE)+
  scale_x_continuous(breaks = c(0,.25, 0.5,.75,1), 
                     labels=c("10\n(0)","12\n(.25)","14\n(.5)","16\n(.75)","18\n(1)"))



ggsave("Figure 1.svg", width = 9, height=6.5)

############################################################
############Analysis of Study 1 using the Raw Judgments ####
############################################################



#Means and medians
aggregate(pizza1~spacing +labels_collapsed, data=d, mean)
aggregate(pizza2~spacing +labels_collapsed, data=d, mean)
aggregate(pizza3~spacing +labels_collapsed, data=d, mean)

aggregate(pizza1~spacing +labels_collapsed, data=d, median)
aggregate(pizza2~spacing +labels_collapsed, data=d, median)
aggregate(pizza3~spacing +labels_collapsed, data=d, median)

aggregate(log(wtp3)~cond, data=mydata2, mean)

#ANOVAS and KRUSKAL-WALLIS tests analyzing raw judgments
summary(aov(pizza1~spacing, data=subset(d, labels_collapsed=="control")))
kruskal.test(subset(d, labels_collapsed=="control")$pizza1~as.factor(subset(d, labels_collapsed=="control")$spacing))
summary(aov(pizza1~spacing, data=subset(d, labels_collapsed=="ordinal-labels")))
kruskal.test(subset(d, labels_collapsed=="ordinal-labels")$pizza1~as.factor(subset(d, labels_collapsed=="ordinal-labels")$spacing))
summary(aov(pizza3~spacing, data=subset(d, labels_collapsed=="control")))
kruskal.test(subset(d, labels_collapsed=="control")$pizza3~as.factor(subset(d, labels_collapsed=="control")$spacing))
summary(aov(pizza3~spacing, data=subset(d, labels_collapsed=="ordinal-labels")))
kruskal.test(subset(d, labels_collapsed=="ordinal-labels")$pizza3~as.factor(subset(d, labels_collapsed=="ordinal-labels")$spacing))


summary(aov(pizza2~spacing, data=subset(d, labels_collapsed=="control")))
kruskal.test(subset(d, labels_collapsed=="control")$pizza2~as.factor(subset(d, labels_collapsed=="control")$spacing))
summary(aov(pizza2~spacing, data=subset(d, labels_collapsed=="ordinal-labels")))
kruskal.test(subset(d, labels_collapsed=="ordinal-labels")$pizza2~as.factor(subset(d, labels_collapsed=="ordinal-labels")$spacing))

################################################################################
############Figure S4 from Web Appendix###########
################################################################################

ldata1<- d %>% 
  gather(index, pizza, pizza1:pizza3)
ldata2<- d %>% 
  gather(index, level, level1:level3)
ldata<- ldata1 %>% bind_cols(ldata2 %>% dplyr::select(level))

ldata$spacing_figure<-ifelse(ldata$spacing=="acc", "Accelerated",
                             ifelse(ldata$spacing=="uni", "Uniform",
                                    ifelse(ldata$spacing=="dec", "Decelerated",NA)))
ldata$spacing_figure <- factor(ldata$spacing_figure, levels = c("Accelerated", "Uniform", "Decelerated"))


#geom_mean function  
gm_mean <-function(x){exp(mean(log(x+1)))}




acc_abs_df<-subset(ldata, spacing=="acc")

acc_abs<- ggplot(data=acc_abs_df, aes(x=level, y =pizza,  shape=labels_collapsed)) +
  ylab("Willingness to Pay")+
  coord_cartesian(ylim = c(5,20))+
  # ylim(25,75)+
  xlab("Pizza Size")+
  # geom_jitter(width=.03, colour="grey")+
  stat_summary(fun.y=gm_mean, geom="point") +
  stat_summary(fun.y=gm_mean, geom="line", size=.25, linetype="dashed")+
  #  facet_wrap(~cond, nrow=1,ncol=3)+
  # scale_color_manual(values=blacklist)+
  #theme(legend.position="none")+
  scale_shape_manual(name= "labels_collapsed" , 
                     values=c(16,17),  labels=c("No Labels","Ordinal Labels"))+
  guides(linetype=guide_legend(order=1), shape=guide_legend(order=2))+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = c(.7,.35),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,.25, 1), 
                     labels=c("10\n(0)","12\n(.25)","18\n(1)"))

uni_abs_df<-subset(ldata, spacing=="uni")

uni_abs<- ggplot(data=uni_abs_df, aes(x=level, y =pizza,  shape=labels_collapsed)) +
  ylab("Willingness to Pay")+
  coord_cartesian(ylim = c(5,20))+
  # ylim(25,75)+
  xlab("Pizza Size")+
  # geom_jitter(width=.03, colour="grey")+
  stat_summary(fun.y=gm_mean, geom="point") +
  stat_summary(fun.y=gm_mean, geom="line", size=.25, linetype="solid")+
  #  facet_wrap(~cond, nrow=1,ncol=3)+
  # scale_color_manual(values=blacklist)+
  #theme(legend.position="none")+
  scale_shape_manual(name= "labels_collapsed" , 
                     values=c(16,17),  labels=c("No Labels","Ordinal Labels"))+
  guides(linetype=guide_legend(order=1), shape=guide_legend(order=2))+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = c(.7,.35),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.5,1), 
                     labels=c("10\n(0)","14\n(.5)","18\n(1)"))

dec_abs_df<-subset(ldata, spacing=="dec")

dec_abs<- ggplot(data=dec_abs_df, aes(x=level, y =pizza,  shape=labels_collapsed)) +
  ylab("Willingness to Pay")+
  coord_cartesian(ylim = c(5,20))+
  # ylim(25,75)+
  xlab("Pizza Size")+
  # geom_jitter(width=.03, colour="grey")+
  stat_summary(fun.y=gm_mean, geom="point") +
  stat_summary(fun.y=gm_mean, geom="line", size=.25, linetype="dotted")+
  #  facet_wrap(~cond, nrow=1,ncol=3)+
  # scale_color_manual(values=blacklist)+
  #theme(legend.position="none")+
  scale_shape_manual(name= "labels_collapsed" , 
                     values=c(16,17),  labels=c("No Labels","Ordinal Labels"))+
  guides(linetype=guide_legend(order=1), shape=guide_legend(order=2))+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = c(.7,.35),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,.75,1), 
                     labels=c("10\n(0)","16\n(.75)","18\n(1)"))


figures4<-grid.arrange(acc_abs,uni_abs,dec_abs, nrow=1)
ggsave("Figure S5.svg", width = 12, height=6.5, figures4)



################################################################################
#####For those interested in our exclusion of monotonicity violations#####3
################################################################################

#violations of nonmonotonicity by condition
d_mon<-mydata %>% mutate(nonmon= ifelse(pizza3<=pizza1 | pizza2<pizza1 |pizza2>pizza3, 1,0),
                         nonmon_low=ifelse(pizza2<pizza1 & pizza3>pizza1,1,0),
                         nonmon_high=ifelse(pizza2>pizza3 & pizza3>pizza1,1,0),
                         nonmon_other=ifelse(pizza3<=pizza1,1,0),
                         labels_collapsed = ifelse(labels=="no", "control", "ordinal-labels" )) 
table(d_mon$labels_collapsed, d_mon$spacing, d_mon$nonmon)
table(d_mon$labels_collapsed, d_mon$spacing, d_mon$nonmon_low)
table(d_mon$labels_collapsed, d_mon$spacing, d_mon$nonmon_high)


table(d_mon$labels_collapsed, d_mon$spacing, d_mon$nonmon_other)
#probability of exhibiting a non-monotonic violation is similar across conditions.
summary(glm(nonmon ~ labels_collapsed + spacing + labels_collapsed*spacing, data = d_mon, family = "binomial"))

#only for control condition
d_mon_control<-subset(d_mon, labels_collapsed=="control")
chisq.test(table(d_mon_control$spacing, d_mon_control$nonmon_low))
fisher.test(table(d_mon_control$spacing, d_mon_control$nonmon_high))
fisher.test(table(d_mon_control$spacing, d_mon_control$nonmon_other))
fisher.test(table(d_mon_control$spacing, d_mon_control$nonmon))

nonmontable_cont<-table(d_mon_control$spacing,  d_mon_control$nonmon)   
nonmontable_cont
chisq.test(nonmontable_cont)
fisher.test(nonmontable_cont)
#only for ordinal prime condition
d_mon_ord<-subset(d_mon, labels_collapsed=="ordinal-labels")
chisq.test(table(d_mon_ord$spacing, d_mon_ord$nonmon_low))
fisher.test(table(d_mon_ord$spacing, d_mon_ord$nonmon_high))
fisher.test(table(d_mon_ord$spacing, d_mon_ord$nonmon_other))
fisher.test(table(d_mon_ord$spacing, d_mon_ord$nonmon))

nonmontable_ord<-table(d_mon_ord$spacing,  d_mon_ord$nonmon)   
nonmontable_ord
chisq.test(nonmontable_ord)
nonmontable<-table(d_mon$spacing,d_mon$labels_collapsed,  d_mon$nonmon)








  







