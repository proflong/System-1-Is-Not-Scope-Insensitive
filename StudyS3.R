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
library(lme4)
library(Cairo)
library(mlogit)
library(sjstats)
library(svglite)


#Importing dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mydata <- read_csv("StudyS3.csv")

#Aligning the data, setting the relative attribute levels for each stimuli
mydata2<- mydata %>% 
  mutate(
    att1 = case_when(
    x1 == 1 ~ 0,
    x1 == 2 ~ 0.5,
    x1 == 3 ~ 1),
    att2 = case_when(
    x2 == 1 ~ 0,
    (category == 'Apartment' & spacex2 == 'acc' & x2 == 2) | (category == 'Insurance' & spacex2 == 'acc' & x2 == 2) ~ 0.25,
    (category == 'TV' & spacex2 == 'acc' & x2 == 2) ~ ((42-32)/(60-32)),
    (spacex2 == 'uni' & x2 == 2) ~ 0.5,
    (category == 'Apartment' & spacex2 == 'dec' & x2 == 2) | (category == 'Insurance' & spacex2 == 'dec' & x2 == 2) ~ 0.75,
    (category == 'TV' & spacex2 == 'dec' & x2 == 2) ~ ((50-32)/(60-32)),
    x2 == 3 ~1),
    att3 = case_when(
      x3 == 1 ~ 0,
      (category == 'Apartment' & spacex3 == 'acc' & x3 == 2) | (category == 'Insurance' & spacex3 == 'acc' & x3 == 2) ~ 0.25,
      (category == 'TV' & spacex3 == 'acc' & x3 == 2) ~ ((4-1)/(9-1)),
      (spacex3 == 'uni' & x3 == 2) ~ 0.5,
      (category == 'Apartment' & spacex3 == 'dec' & x3 == 2) | (category == 'Insurance' & spacex3 == 'dec' & x3 == 2) ~ 0.75,
      (category == 'TV' & spacex3 == 'dec' & x3 == 2) ~ ((6-1)/(9-1)),
    x3 == 3 ~1),
    att4 = case_when(
      x4 == 1 ~ 0,
      (category == 'Apartment' & spacex4 == 'acc' & x4 == 2) ~ ((30-20)/(50-20)),
      (category == 'Insurance' & spacex4 == 'acc' & x4 == 2) ~ 0.25,
      (category == 'TV' & spacex4 == 'acc' & x4 == 2) ~ ((30-10)/(90-10)),
      (spacex4 == 'uni' & x4 == 2) ~ 0.5,
      (category == 'Apartment' & spacex4 == 'dec' & x4 == 2) ~ ((40-20)/(50-20)),
      (category == 'Insurance' & spacex4 == 'dec' & x4 == 2) ~ 0.75,
      (category == 'TV' & spacex4 == 'dec' & x4 == 2) ~ ((70-10)/(90-10)),
      x4 == 3 ~1)
  ) 
#At this point there are still 7344 observations (136 participants X 3 Product categories x 18 alternatives per product category)


#long form of data for a total of 29376 observations (7344 X 4 attributes per product judgment, one of the 4 attributes was price)
long <- mydata2 %>% 
  gather(attributenumber, lmh, x1:x4 )%>%
  mutate(spacex1="uni")
long2_1<-mydata2 %>%
  gather(att_index,att_level, att1:att4)
long2_2<-mydata2 %>%
  mutate(spacex1="uni")%>%
  gather(cond_index,cond_level, c(spacex1, spacex2, spacex3,spacex4))
long2_3<-long2_1 %>%  bind_cols(long2_2 %>% select(cond_level)) %>%
  mutate(lg_rating=log(rating+1),
         lg_att=log((att_level*100)+1))




#long exploration disregard this chunk
long2<-long %>% filter(set %in% c(7,8)) %>% filter (category %in% c("Apartment")) %>%
  group_by(category, spacex2,spacex3,spacex4, set) %>% 
  summarize(mean_rate = mean(rating, na.rm=TRUE))

#Calculating the mean for each product attribute across the 6 presentations for each product attribute
#For efficiency I aggregate means seperately in this step
aggregated <- long %>%
  group_by(ppnr, category, attributenumber, lmh) %>%
             dplyr::summarize(mean_rating = mean(rating, na.rm=TRUE)) %>%
  ungroup() %>%
  spread(lmh, mean_rating)
#And I aggregate attribute seperately in this step
aggregated2 <- long %>%
  mutate(attribute_index= case_when(
    attributenumber == 'x1' ~ att1,
    attributenumber == 'x2' ~ att2,
    attributenumber == 'x3' ~ att3,
    attributenumber == 'x4' ~ att4))%>%
  group_by(ppnr, category, attributenumber, lmh) %>%
  dplyr::summarize(nattribute = mean(attribute_index, na.rm=TRUE)) %>%
  ungroup() %>%
  spread(lmh, nattribute)

#When restructuring the data with the above method I had to rename the focal columns being aggregated
names(aggregated)[4:6]<-c("low","mid","high")
names(aggregated2)[4:6]<-c("level_low","level_mid","level_high")

#I then bind mean ratings and attribute levels into the same dataframe
aggregate<- aggregated %>% bind_cols(aggregated2 %>% dplyr::select(level_low, level_mid, level_high))
#The actual deletions are from 1224 to 717 not 1632 (this dataframe includes price which is not varied by condition so 3/4 of 1632 is 1224)
final_nonexcl<-aggregate %>%
  #the ifelse statements are simply reverse coding the two attributes where higher numbers were worse (e.g., deductable)
  mutate(rc_low=ifelse((category=='Insurance' & attributenumber =='x4')|(category=='TV' & attributenumber =='x4'), high, low),
         rc_mid=mid,
         rc_high=ifelse((category=='Insurance' & attributenumber =='x4')|(category=='TV' & attributenumber =='x4'), low, high),
         rc_level_low= level_low,
         nattribute=ifelse((category=='Insurance' & attributenumber =='x4')|(category=='TV' & attributenumber =='x4'), level_high-level_mid, level_mid),
         rc_level_high=level_high,
         r_low=(rc_low-rc_low)/(rc_high-rc_low),
         nrating=(rc_mid-rc_low)/(rc_high-rc_low),
         r_high=(rc_high-rc_low)/(rc_high-rc_low),
         condition= as.factor(ifelse(nattribute<0.5, "acc", ifelse(nattribute==0.5, "uni", ifelse(nattribute>0.5, "dec", NA)))),
         #THIS IS THE FOCAL DEPENDENT VARIABLE JUDGMENT MINUS RELATIVE ATTRIBUTE LEVEL
         dif= nrating-nattribute,
         rank_min=0, rank_mid=0.5, rank_max=1,
         index=rep(1:12, length(unique(ppnr))),
         rangedif=rc_high-rc_low,
         logrange=log(rangedif+1),
         range_center=rangedif-mean(rangedif, na.rm=T)) %>%
  #removing price since it is not manipulated
        filter(attributenumber %in% c('x2', 'x3', 'x4')) %>%
   mutate(i=1:length(ppnr),
          nonmon=ifelse(rc_mid<rc_low | rc_mid>rc_high| rc_low>=rc_high,1,0),
          nonmon_low=ifelse(rc_mid<rc_low & rc_low<rc_high,1,0),
          nonmon_high=ifelse(rc_mid>rc_high & rc_low<rc_high,1,0),
          nonmon_other=ifelse(rc_low>=rc_high,1,0))
final<-final_nonexcl%>% #take out this next bit when exploring nonmontonicity a few steps lower
  #removing responses that are not strictly monotonic between high and low and options and weakly monotonic for intermediate options
     filter(nrating>=0 & nrating<=1) %>%
  filter(rc_high>rc_low)


#Exploring non-monotonicities
fisher.test(table(final_nonexcl$condition, final_nonexcl$nonmon))
fisher.test(table(final_nonexcl$condition, final_nonexcl$nonmon_low))
fisher.test(table(final_nonexcl$condition, final_nonexcl$nonmon_high))
fisher.test(table(final_nonexcl$condition, final_nonexcl$nonmon_other))
kruskal.test(final_nonexcl$nonmon_low~as.factor(final_nonexcl$condition))
kruskal.test(final_nonexcl$nonmon_high~as.factor(final_nonexcl$condition))

aggregate(rc_low~condition, data=final_nonexcl, mean)
aggregate(rc_mid~condition, data=final_nonexcl, mean)
aggregate(rc_high~condition, data=final_nonexcl, mean)

final_non_mon<-final_nonexcl %>% filter(nonmon_other ==0) %>% 
  mutate(accdummy= ifelse(condition=='acc',1,0),
         decdummy= ifelse(condition == 'dec',1,0))
plotdata1<-final_non_mon %>% gather(index, rawrating, c(rc_low, rc_mid, rc_high))
plotdata2<-final_non_mon %>% gather(index, level, c(rc_level_low, nattribute, rc_level_high)) %>%
  mutate(level_avg= ifelse(level >0 & level < .5, 0.28, ifelse(level >0.5 & level < 1, 0.72, level)))
plotdata<-plotdata1 %>% bind_cols(plotdata2 %>% dplyr::select(level_avg))  %>%
  bind_cols(plotdata2 %>% dplyr::select(level)) 
ggplot(data=plotdata, aes(x=level_avg, y=rawrating, linetype= condition)) +
  ylab("Preference Rating")+
  xlab("Attribute Level")+
  stat_summary(fun.y="mean", geom="point", aes(shape=condition)) +
  stat_summary(fun.y="mean", geom="line", aes(linetype=condition)) 





#I put the data back in long form for later graphical purposes
#similar to above, this procedure is more efficient when split into the steps below
final_long1<- final %>%
  gather(lmh, abs_rating, c(rc_low, rc_mid, rc_high))
final_long2<- final %>%
  gather(lmh, level, c(rc_level_low, nattribute, rc_level_high))
final_long3<- final %>%
  gather(lmh, rel_rating, c(r_low, nrating, r_high))
final_long<-final_long1 %>%
  bind_cols(final_long2 %>% dplyr::select(level)) %>%  bind_cols(final_long3 %>% dplyr::select(rel_rating)) %>% 
  #mutate(rating=rating/100) %>%
  arrange(ppnr, category, attributenumber, level) %>%
  mutate(level=round(level, digits = 2)) %>%
  mutate(i1=i, i2=1:length(ppnr)) %>%
  #This is simply adding a vector of features so that I can efficiently call these values when plotting below.
  mutate(abs_att = case_when(
    index == 2 & lmh =='rc_low' ~ 850,
    index == 2 & lmh =='rc_mid' & condition=='acc'~ 900,
    index == 2 & lmh =='rc_mid' & condition=='uni'~ 950,
    index == 2 & lmh =='rc_mid' & condition=='dec'~ 1000,
    index == 2 & lmh =='rc_high' ~ 1050,
    index == 3 & lmh =='rc_low' ~ 3,
    index == 3 & lmh =='rc_mid' & condition=='acc'~ 4,
    index == 3 & lmh =='rc_mid' & condition=='uni'~ 5,
    index == 3 & lmh =='rc_mid' & condition=='dec'~ 6,
    index == 3 & lmh =='rc_high' ~ 7,
    index == 4 & lmh =='rc_low' ~ 20,
    index == 4 & lmh =='rc_mid' & condition=='acc'~ 30,
    index == 4 & lmh =='rc_mid' & condition=='uni'~ 35,
    index == 4 & lmh =='rc_mid' & condition=='dec'~ 40,
    index == 4 & lmh =='rc_high' ~ 50,
    index == 6 & lmh =='rc_low' ~ 10,
    index == 6 & lmh =='rc_mid' & condition=='acc'~ 20,
    index == 6 & lmh =='rc_mid' & condition=='uni'~ 30,
    index == 6 & lmh =='rc_mid' & condition=='dec'~ 40,
    index == 6 & lmh =='rc_high' ~ 50,
    index == 7 & lmh =='rc_low' ~ 5,
    index == 7 & lmh =='rc_mid' & condition=='acc'~ 7.5,
    index == 7 & lmh =='rc_mid' & condition=='uni'~ 10,
    index == 7 & lmh =='rc_mid' & condition=='dec'~ 12.5,
    index == 7 & lmh =='rc_high' ~ 15,
    index == 8 & lmh =='rc_low' ~ 0,
    index == 8 & lmh =='rc_mid' & condition=='acc'~ 250,
    index == 8 & lmh =='rc_mid' & condition=='uni'~ 500,
    index == 8 & lmh =='rc_mid' & condition=='dec'~ 750,
    index == 8 & lmh =='rc_high' ~ 1000,
    index == 10 & lmh =='rc_low' ~ 32,
    index == 10 & lmh =='rc_mid' & condition=='acc'~ 42,
    index == 10 & lmh =='rc_mid' & condition=='uni'~ 46,
    index == 10 & lmh =='rc_mid' & condition=='dec'~ 50,
    index == 10 & lmh =='rc_high' ~ 60,
    index == 11 & lmh =='rc_low' ~ 1,
    index == 11 & lmh =='rc_mid' & condition=='acc'~ 4,
    index == 11 & lmh =='rc_mid' & condition=='uni'~ 5,
    index == 11 & lmh =='rc_mid' & condition=='dec'~ 6,
    index == 11 & lmh =='rc_high' ~ 9,
    index == 12 & lmh =='rc_low' ~ 10,
    index == 12 & lmh =='rc_mid' & condition=='acc'~ 30,
    index == 12 & lmh =='rc_mid' & condition=='uni'~ 50,
    index == 12 & lmh =='rc_mid' & condition=='dec'~ 70,
    index == 12 & lmh =='rc_high' ~ 90
        )) %>%
  mutate(abs_att_label = case_when(
    index == 2 ~ 'Sqaure Feet',
    index == 3 ~ 'Number of Windows',
    index == 4 ~ 'Decibels',
    index == 6 ~ 'Thousand of Dollars',
    index == 7 ~ 'Thousand of Dollars',
    index == 8 ~ 'Dollars',
    index == 10 ~ 'Inches',
    index == 11 ~ 'Number of Devices',
    index == 12 ~ 'Percent'
  ))


  
################################################Tests
aggregate(nrating~condition, data=final, sd)
final$condition<-relevel(final$condition, ref = 'uni')
summary(lm(dif~condition, data=final))
summary(lm(dif~1, data=subset(final, condition=='acc')))
summary(lm(dif~1, data=subset(final, condition=='uni')))
summary(lm(dif~1, data=subset(final, condition=='dec')))


###############################################Hierarchical Model
hier_model<-lmer(dif~ condition + as.factor(index) +(1 | ppnr), data=final)

summary(hier_model)

#CHECKING CORRELATIONS
library(PerformanceAnalytics)
chart.Correlation(final[c("dif","range_center", "rc_high")])

library(lmerTest)
hier_model<-lmer(dif~ condition + range_center + range_center:condition+ as.factor(index) +(1 | ppnr), data=final)
summary(hier_model)
confint(hier_model, method="boot", nsim=1000, parm=1:9)




################################################Analysis of raw judgments
gm_mean <-function(x){exp(mean(log(x+1)))}

aggregate(rc_low~condition, data=final, gm_mean)
aggregate(rc_low~condition, data=final, median)
aggregate(mid~condition, data=final, gm_mean)
aggregate(mid~condition, data=final, median)

aggregate(rc_high~condition, data=final, gm_mean)
aggregate(rc_high~condition, data=final, median)
summary(aov(rc_low~condition, data=final))
summary(aov(nrating~condition, data=final))
summary(aov(rc_high~condition, data=final))
kruskal.test(final$rc_low~final$condition)
kruskal.test(final$nrating~final$condition)
kruskal.test(final$rc_high~final$condition)



aggregate(level_mid~condition, data=final, gm_mean)

summary(aov(rc_low~factor(condition)+Error(factor(ppnr)), data = final))
summary(aov(rc_high~factor(condition)+Error(factor(ppnr)), data = final))


##############################################################################################
####FIGURE S3 and S11
###############################################################################################

#generating open list to deposit return on function
plot_list_rescaled=list()
#function to create ggplot for each product category and attribute
plot_function_rescaled <-function(df, name, na.rm=TRUE, ...){
  jitterdf= df %>% filter (level> min(level) & level <max(level))
    plot.vector<- ggplot(df, aes(y=rel_rating, x=level, linetype=condition)) +
      geom_jitter(data=jitterdf, width=.03, height=0.0, color="grey")+
       stat_summary(fun.y="mean", geom="point") +
      stat_summary(fun.y="mean", geom="line") +
      ylab("Preference Rating")+
      xlab("Attribute Level")+
      ggtitle(names(name))+
      theme(legend.position="none")+
      scale_linetype_manual(name= "condition" , values=c("dashed","dotted","solid"))+
      theme(text=element_text(family="Times New Roman", size=12), axis.text.x = element_text(size=8)) +
      theme(plot.title=element_text(family="Times New Roman", size=12, face="plain")) +
      scale_x_continuous(breaks=unique(df$level))
    
  return(plot.vector)
}

#same procedure for the raw (i.e., untransformed) values
plot_list_raw=list()
plot_function_raw <-function(df, name, na.rm=TRUE, ...){
  plot.vector<- ggplot(df, aes(y=abs_rating, x=abs_att, linetype=condition)) +
  #  geom_jitter(width=((max(df$abs_att)-min(df$abs_att))/33.3), height=0.0, color="grey")+
    stat_summary(fun.y="mean", geom="point") +
    stat_summary(fun.y="mean", geom="line") +
    ylab("Preference Rating")+ coord_cartesian(ylim=c(25, 75))+
    xlab("")+ 
    ggtitle(names(name))+
    theme(legend.position="none")+
    scale_linetype_manual(name= "condition" , values=c("dashed","dotted","solid"))+
    theme(text=element_text(family="Times New Roman", size=12), axis.text.x = element_text(size=8)) +
    theme(plot.title=element_text(family="Times New Roman", size=12, face="plain"))+
    scale_x_continuous(name =df$abs_att_label,  breaks=unique(df$abs_att),limits=c((min(df$abs_att)-((max(df$abs_att)-min(df$abs_att))/3)),(max(df$abs_att)+((max(df$abs_att)-min(df$abs_att))/3))))
  
 
  return(plot.vector)
}

#reference list for the product categories and attributes
  stimulus_list <-sort(unique(final_long$index))
  #names for titles
  names(stimulus_list)<-c("(A) Apartment Size", "(B) Apartment Windows", "(C) Apartment Insulation", "(D) Insurance Liability Coverage", "(E) Insurance Collision Coverage", "(F) Insurance Deductable", "(G) TV Screen Size", "(H) TV Number of Devices", "(I) TV Repair Rate")

  #executing the functions with the stimulus_list and dataframe
  #for each product category/attribute in the stimulus list 
   for (j in seq_along(stimulus_list)){
     #use the dataframe "final_long" selecting only the portion of the data frame for a given product attribute.
    df=final_long %>% filter(index==stimulus_list[j])
    #then plot 
    plot_rescaled=plot_function_rescaled(df, stimulus_list[j])
        plot_list_rescaled[[j]]= plot_rescaled
        #same as above
    plot_raw=plot_function_raw(df, stimulus_list[j])
        plot_list_raw[[j]]= plot_raw
   }
  #reordering the list of elements so that they show up where I want them in the figure.
  plot_list_rescaled_ordered<-c(plot_list_rescaled[1],plot_list_rescaled[4], plot_list_rescaled[7], plot_list_rescaled[2], plot_list_rescaled[5], plot_list_rescaled[8], plot_list_rescaled[3], plot_list_rescaled[6], plot_list_rescaled[9])
  plot_list_raw_ordered<-c(plot_list_raw[1],plot_list_raw[4], plot_list_raw[7], plot_list_raw[2], plot_list_raw[5], plot_list_raw[8], plot_list_raw[3], plot_list_raw[6], plot_list_raw[9])
  
  #function that can pull a list of figures and plot them in any matrix configuration

 figures3<-  marrangeGrob(plot_list_rescaled_ordered, nrow=3, ncol=3 )
 figures11<- marrangeGrob(plot_list_raw_ordered, nrow=3, ncol=3 )

 ggsave("Figure S3.svg", width = 12, height=12, figures3)
 ggsave("Figure S11.svg", width = 12, height=12, figures11)
 
 
 
 
 Cairo(1600,1600, file="C:/Users/Schley/Dropbox (RSM)/DanStuff/ByProject/BartDeLanghe/50 Shades of Medium/Study 3/StudyS3_plot.png", bg="white", res=128)
marrangeGrob(plot_list_rescaled_ordered, nrow=3, ncol=3 )
dev.off() 

Cairo(1600,1600, file="C:/Users/Schley/Dropbox (RSM)/DanStuff/ByProject/BartDeLanghe/50 Shades of Medium/Study 3/StudyS3_abs.png", bg="white", res=128)
marrangeGrob(plot_list_raw_ordered, nrow=3, ncol=3 )
dev.off() 
  
  #practice plot since the functions above are big and scary. When you don't want to break all of the above code.
  ggplot(df, aes(y=abs_rating, x=abs_att, linetype=condition)) +
    stat_summary(fun.y="mean", geom="point") +
    stat_summary(fun.y="mean", geom="line") +
    geom_jitter(width= 0.05, alpha=.2)+
    ylab("Preference Rating")+ ylim(25,75)+
    xlab("")+   ggtitle("cat")+
    theme(legend.position="none")+
    scale_linetype_manual(name= "condition" , values=c(3,5,1))+
    theme(text=element_text(family="Times New Roman", size=12), axis.text.x = element_text(size=8)) +
    theme(plot.title=element_text(family="Times New Roman", size=12, face="plain"))+
    scale_x_continuous(name =df$abs_att_label,  breaks=unique(df$abs_att),limits=c((min(df$abs_att)-((max(df$abs_att)-min(df$abs_att))/4)),(max(df$abs_att)+((max(df$abs_att)-min(df$abs_att))/4))))
                
  
  #histograms
  blacklist<-c("black", "black", "black")
  plot_hist<-ggplot(transform(final_long, condition=factor(condition, levels=c("acc", "uni", "dec"), labels=c("Accelerated", "Uniform", "Decelerating"))),
                   aes(x=abs_rating, fill=condition))+
    xlab("Preference Rating")+
    ylab("Frequency")+
    labs(fill="Condition")+
    geom_histogram(binwidth=10, alpha=.8, aes(fill=condition), position="dodge")+
    scale_color_manual(values=blacklist)+
    scale_fill_manual(values=c("gray87", "gray40", "black"))+
    theme(text=element_text(family="Times New Roman", size=12))          
plot_hist
