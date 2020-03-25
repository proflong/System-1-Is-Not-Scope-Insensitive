rm(list=ls())


library(ggplot2)

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


#Importing dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mydata <- read_csv("Study6.csv")
set.seed(13)

#scaling function for relative WTP
scaling_func <- function(x, x1, x3)(x-x1)/(x3-x1)

#realigning columns
mydata2<-mydata %>%
  mutate(level1=0,
         level2= case_when(
    !is.na(a_wtp3)  ~ .2, !is.na(b_wtp3)~.4,
    !is.na(c_wtp3)  ~ .5, !is.na(d_wtp3)~.6,
    !is.na(e_wtp3)  ~ .8),
        level3=1,
    wtp3= case_when(
    level2==.2 ~ a_wtp3, level2==.4 ~ b_wtp3, 
    level2==.5 ~ c_wtp3, level2==.6 ~ d_wtp3, 
    level2==.8 ~ e_wtp3),
    wtp2= case_when(
      level2==.2 ~ a_wtp2, level2==.4 ~ b_wtp2, 
      level2==.5 ~ c_wtp2, level2==.6 ~ d_wtp2, 
      level2==.8 ~ e_wtp2),
    wtp1= case_when(
      level2==.2 ~ a_wtp1, level2==.4 ~ b_wtp1, 
      level2==.5 ~ c_wtp1, level2==.6 ~ d_wtp1, 
      level2==.8 ~ e_wtp1),
    heuristic= case_when(
      level2==.4 | level2==.6 ~ "High",
      level2==.2 | level2==.8 ~ "Low"),
    spacing= case_when(
      level2==.2 | level2==.4 ~ "Accelerating",
      level2==.6 | level2==.8 ~ "Decelerating"),
    r.wtp1 = scaling_func(wtp1, wtp1, wtp3),
    r.wtp2 = scaling_func(wtp2, wtp1, wtp3),
    r.wtp3 = scaling_func(wtp3, wtp1, wtp3),
    freq1=0, freq2=.5, freq3=1,
    relativeshift= ((r.wtp2-level2)*(abs(level2-.5)*100))
    )

#Uniform condition was only there as a check N=322 without this condition. This condition was collected as a control to assess whether there were any main effects on judgments (relative wtp greater or less than 0.50) in this context. 
mydata3<-mydata2 %>% filter(level2 != .5)
mean(mydata3$age, na.rm=T)
sd(mydata3$age, na.rm=T)

table(mydata3$sex)

#filtering based on montonicity, 11 violations
d<-mydata3 %>% filter(wtp3>wtp1) %>% filter(wtp3>=wtp2 & wtp2>=wtp1) %>%
  mutate(range=wtp3-wtp1,
         logrange=log(range+1),
         range_center=logrange-mean(logrange, na.rm=T))


  
#####Descriptives
aggregate(r.wtp2~spacing + heuristic, data=d, mean)



################################################################################################
#FIGURE 4
################################################################################################
r.ldata1<- d %>% 
  gather(index, r.wtp, r.wtp1:r.wtp3)
r.ldata2<- d %>% 
  gather(index, level, level1:level3)
r.ldata_wtp<- r.ldata1 %>%
  bind_cols(r.ldata2 %>% dplyr::select(level))
  

lowdata<-subset(r.ldata_wtp, heuristic=="Low")
lowplot<-ggplot(lowdata, aes(y=r.wtp, x=level)) +
  ylab("Willingness to Pay")+ ylim(0,1)+
  xlab("Coffee Size (oz.)")+
  ggtitle("Low Heuristic Accuracy")+
  geom_jitter(data=subset(lowdata, level>0 & level<1),aes(y=r.wtp, x=level, shape=spacing), 
              width=.03, height=0, colour="grey")+
  stat_summary(fun.y=mean, geom="line", aes(linetype=spacing)) +
  stat_summary(fun.y=mean, geom="point", size=3, aes(y=r.wtp, x=level, shape=spacing))+
  # stat_summary(fun.y=mean, geom="point", size=3, aes(y=yhat, x=level),shape=4)+
  scale_linetype_manual(name= "spacing" , 
                        values=c("dashed", "dotted"))+
  scale_shape_manual(name= "spacing" , values=c(16,17), 
                     labels=c("Accelerated","Decelerated"))+
  geom_segment(aes(x = 0.15, xend = 0.25, y = 0.50, yend = 0.50)) +
  geom_segment(aes(x = 0.15, xend = 0.25, y = 0.20, yend = 0.20)) +
  geom_segment(aes(x = 0.75, xend = 0.85, y = 0.50, yend = 0.50)) +
  geom_segment(aes(x = 0.75, xend = 0.85, y = 0.80, yend = 0.80)) +
  annotate("text", x=.08, y=.50, label = "Ordinal", family="Times New Roman")+
  annotate("text", x=.08, y=.20, label = "Cardinal", family="Times New Roman")+
  annotate("text", x=.92, y=.50, label = "Ordinal", family="Times New Roman")+
  annotate("text", x=.92, y=.80, label = "Cardinal", family="Times New Roman")+
  theme(text=element_text(size=12), axis.text = element_text(size=12),
        legend.position="None")+
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(size=12))+
  #  guides(linetype=FALSE)+
  scale_x_continuous(breaks = c(0,.2,.8,1), 
                     labels=c("10\n(0.00)","12\n(0.20)","18\n(0.80)","20\n(1.00)"))


highdata<-subset(r.ldata_wtp, heuristic=="High")
highplot<-ggplot(highdata, aes(y=r.wtp, x=level)) +
  ylab("Willingness to Pay")+ ylim(0,1)+
  xlab("Coffee Size (oz.)")+
  ggtitle("High Heuristic Accuracy")+
  geom_jitter(data=subset(highdata, level>0 & level<1),aes(y=r.wtp, x=level, shape=spacing), 
              width=.03, height=0, colour="grey")+
  stat_summary(fun.y=mean, geom="line", aes(linetype=spacing)) +
  stat_summary(fun.y=mean, geom="point", size=3, aes(y=r.wtp, x=level, shape=spacing))+
  scale_linetype_manual(name= "spacing" , 
                        values=c("dashed", "dotted"), labels=c("Accelerated","Decelerated"))+
  scale_shape_manual(name= "spacing" , values=c(16,17), 
                     labels=c("Accelerated","Decelerated"))+
  geom_segment(aes(x = 0.35, xend = 0.45, y = 0.50, yend = 0.50)) +
  geom_segment(aes(x = 0.35, xend = 0.45, y = 0.40, yend = 0.40)) +
  geom_segment(aes(x = 0.55, xend = 0.65, y = 0.50, yend = 0.50)) +
  geom_segment(aes(x = 0.55, xend = 0.65, y = 0.60, yend = 0.60)) +
  annotate("text", x=.28, y=.50, label = "Ordinal", family="Times New Roman")+
  annotate("text", x=.28, y=.40, label = "Cardinal", family="Times New Roman")+
  annotate("text", x=.72, y=.50, label = "Ordinal", family="Times New Roman")+
  annotate("text", x=.72, y=.60, label = "Cardinal", family="Times New Roman")+
  theme(text=element_text(size=12), axis.text = element_text(size=12),
        legend.title = element_blank(), legend.position=c(.75,.50))+
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(size=12))+
  guides(linetype=FALSE)+
  scale_x_continuous(breaks = c(0,.40,.60,1), 
                     labels=c("10\n(0.00)","14\n(0.40)","16\n(0.60)","20\n(1.00)"))



figure4<-grid.arrange(lowplot,highplot, nrow=1)

ggsave("Figure 4.svg", width = 12, height=6, figure4)










################################################################################################
#FIGURE S7
################################################################################################
r.ldata1<- d %>% 
  gather(index, wtp, wtp1:wtp3)
r.ldata2<- d %>% 
  gather(index, level, level1:level3)
r.ldata_wtp<- r.ldata1 %>% 
  bind_cols(r.ldata2 %>% dplyr::select(level)) 

#geom_mean function  
gm_mean <-function(x){exp(mean(log(x+1)))}

lowdata_abs<-subset(r.ldata_wtp, heuristic=="Low")
lowplot_abs<-
  ggplot(lowdata_abs, aes(y=wtp, x=level)) +
  coord_cartesian(ylim = c(2,5))+
  ylab("Willingness to Pay")+ 
  xlab("Coffee Size (oz.)")+
  ggtitle("Low Heuristic Accuracy")+
 # geom_jitter(data=lowdata_abs,aes(y=wtp, x=level), 
              #width=.03, height=0, colour="grey")+
  stat_summary(fun.y=gm_mean, geom="line", aes(linetype=spacing)) +
  stat_summary(fun.y=gm_mean, geom="point", size=3, aes(shape=spacing))+
  scale_linetype_manual(name= "spacing" , 
                        values=c("dashed", "dotted"))+
  scale_shape_manual(name= "spacing" , 
                     values=c(16,16),  labels=c("Accelerated","Decelerated"))+
    theme(text=element_text(size=12), axis.text = element_text(size=12),
        legend.position="None")+
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(size=12))+
  guides(linetype=FALSE)+
  scale_x_continuous(breaks = c(0,.2,.8,1), 
                     labels=c("10\n(0.00)","12\n(0.20)","18\n(0.80)","20\n(1.00)"))


highdata<-subset(r.ldata_wtp, heuristic=="High")
highplot_abs<-
  ggplot(highdata, aes(y=wtp, x=level)) +
    coord_cartesian(ylim = c(2,5))+
    ylab("Willingness to Pay")+
  xlab("Coffee Size (oz.)")+
  ggtitle("High Heuristic Accuracy")+
  #geom_jitter(data=highdata,aes(y=wtp, x=level), 
   #           width=.03, height=0, colour="grey")+
  stat_summary(fun.y=gm_mean, geom="line", aes(linetype=spacing)) +
  stat_summary(fun.y=gm_mean, geom="point", size=3, aes(shape=spacing))+
  scale_linetype_manual(name= "spacing" , 
                        values=c("dashed", "dotted"), labels=c("Accelerated","Decelerated"))+
  scale_shape_manual(name= "spacing" , 
                     values=c(16,16),labels=c("Accelerated","Decelerated")  )+
 theme(text=element_text(size=12), axis.text = element_text(size=12),
        legend.title = element_blank(), legend.position=c(.8,.5))+
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(size=12))+
  guides(shape=FALSE)+
  scale_x_continuous(breaks = c(0,.40,.60,1), 
                     labels=c("10\n(0.00)","14\n(0.40)","16\n(0.60)","20\n(1.00)"))


figures7<-grid.arrange(lowplot_abs,highplot_abs, nrow=1)

ggsave("Figure S7.svg", width = 12, height=6, figures7)




  
#putting data into long for for later use  
d0 <- d %>% mutate(d_space=ifelse(spacing=="Accelerating",1,0),
         d_space_c=d_space-mean(d_space),
         d_heur=ifelse(heuristic=="Low",1,0),
         d_heur_c=d_heur-mean(d_heur),
         d_int=d_space_c*d_heur_c)
summary(lm(abs(r.wtp2-level2)~1+d_space_c+d_heur_c+d_int,data=d0))


 

################################################################################################
#BAYESIAN MODEL
################################################################################################


d0 <- d %>% mutate(d_space=ifelse(spacing=="Accelerating",0,1),
                   d_space_c=d_space-mean(d_space),
                   d_heur=ifelse(heuristic=="Low",0,1),
                   d_heur_c=d_heur-mean(d_heur),
                   d_int=d_space*d_heur)


d1 <- d0 %>% gather(index, r.wtp, r.wtp1:r.wtp3)
d2 <- d0 %>% gather(index, level, level1:level3)
d3 <- d0 %>% gather(index, freq, freq1:freq3)
d4 <- d1 %>% bind_cols(d2 %>% dplyr::select(level), d3 %>% dplyr::select(freq)) 


#Creating data set of variables
reg_data <- d4 %>% transmute(x = level, r = freq, y = r.wtp, mturkcode, z_a=d_space, z_b=d_heur,z_c=d_int, i = mturkcode %>% factor %>% forcats::fct_anon() %>% as.integer) %>% filter(x>0&x<1) %>% as.list
#Creating data set of constants
const_data <- list(n_obs = length(reg_data$x), n_respondents = max(reg_data$i))
#combining sets as environement
stan_data <- c(reg_data, const_data) %>% as.environment


#STAN CODE
stan_code_inv <- "
data {
int n_obs;
int i[n_obs];
real x[n_obs]; //this is the range component from Range-Frequency Theory
real r[n_obs]; //this is the frequency component from Range-Frequency Theory
real y[n_obs]; //this is the judgment
vector[n_obs] z_a;
vector[n_obs] z_b;
vector[n_obs] z_c;
}

parameters {
real<lower = 0> s2; // Some variance greater than 0
real<lower = 0, upper = 1> w_bar; // this constrains estimates of the average w between 0 and 1
real<lower = -1, upper = 1> w_a;
real<lower = -1, upper = 1> w_b;
real<lower = -1, upper = 1> w_c;
}

transformed parameters {
vector[n_obs] w; //this constrains estimates of w per participant between 0 and 1
w=inv_logit(w_bar+ w_a*z_a+ w_b*z_b +w_c*z_c);

}
model {
s2 ~ exponential(5); //specifying a common variance structure

for (o in 1:n_obs) {
y[o] ~ normal((w[o])* x[o] + (1-(w[o])) * r[o], s2); //RFT model at the individual participant level

}

}
"


#This compiles the STAN code into C++ code (which is where STAN runs natively)
m_inv <- stan_model(model_name = 'my_model', model_code = stan_code_inv)
#This starts the sampling procedure
fit_inv <- sampling(m_inv, data = stan_data, iter=5000, chains=4)
#This extracts the fitted values, particular each participants' w parameter
w_bar <- get_posterior_mean(fit_inv, 'w_bar') %>% .[,5]
w_a <- get_posterior_mean(fit_inv, 'w_a') %>% .[,5]
w_b <- get_posterior_mean(fit_inv, 'w_b') %>% .[,5]
w_c <- get_posterior_mean(fit_inv, 'w_c') %>% .[,5]

####This just looks at convergence of parameters
traceplot(fit_inv, pars=c("lp__","w_bar", "w_a","w_b", "w_c"))

#Gives the mean parameter values
phis=summary(fit_inv, pars = c("lp__","w_bar", "w_a","w_b", "w_c"), probs=c(0.025, 0.975))$summary
phis

#extract conflicts between rstan and tidyr, this just extracts parameters
list_of_draws <- rstan::extract(fit_inv)
#this is to make sure I got the right parameters
print(names(list_of_draws))

#put into matrix and add names
matrix_of_draws <- as.matrix(fit_inv)
print(colnames(matrix_of_draws[,1:5]))

#convert to dataframe, note because of the logit transformation
logitestimates_df<-as.data.frame(matrix_of_draws[,1:5]) %>%
  #converts from logit back to raw w
  mutate(raw_w_acclow=1/(1+exp(-(w_bar))),
         raw_w_declow=1/(1+exp(-(w_bar+w_a))),
         raw_w_acchigh=1/(1+exp(-(w_bar+w_b))),
         raw_w_dechigh=1/(1+exp(-(w_bar+w_a+w_b+w_c))),
  #calculating differences between posterior draws
         main_w_acc=(raw_w_acchigh+raw_w_acclow)/2,
         main_w_dec=(raw_w_dechigh+raw_w_declow)/2,
         main_w_low=(raw_w_acclow+raw_w_declow)/2,
         main_w_high=(raw_w_acchigh+raw_w_dechigh)/2,
  #this is essentially a Bayesian main effect drawn from the posterior
         effect_space=((raw_w_acchigh+raw_w_acclow)/2)-((raw_w_dechigh+raw_w_declow)/2),
         effect_heur=((raw_w_acclow+raw_w_declow)/2)-((raw_w_acchigh+raw_w_dechigh)/2),
  #this is essentially a Bayesian interaction term drawn from the posterior
         int_w=(raw_w_acchigh-raw_w_acclow)-(raw_w_dechigh-raw_w_declow)
          )
mean(logitestimates_df$raw_w_acclow)
mean(logitestimates_df$raw_w_declow)
mean(logitestimates_df$raw_w_acchigh)
mean(logitestimates_df$raw_w_dechigh)
mean(logitestimates_df$main_w_acc)
mean(logitestimates_df$main_w_dec)
mean(logitestimates_df$main_w_low)
mean(logitestimates_df$main_w_high)
#main effect of attribute spacing
mean(logitestimates_df$effect_space)
#main effect of heuristic
mean(logitestimates_df$effect_heur)
#interaction term
mean(logitestimates_df$int_w)

quantile(logitestimates_df$raw_w_acclow, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$raw_w_declow, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$raw_w_acchigh, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$raw_w_dechigh, probs=c(0.025, 0.5, 0.975))

quantile(logitestimates_df$main_w_acc, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$main_w_dec, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$main_w_low, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$main_w_high, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$effect_space, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$effect_heur, probs=c(0.025, 0.5, 0.975))

quantile(logitestimates_df$int_w, probs=c(0.025, 0.5, 0.975))





#NEW PRETTIER PLOTS
r.ldata1<- fitteddata %>% 
  gather(index, yhat, yhat1:yhat3)
r.ldata1.1<- fitteddata %>% 
  gather(index, r.wtp, r.wtp1:r.wtp3)
r.ldata2<- fitteddata %>% 
  gather(index, level, level1:level3)
r.ldata_wtp<- r.ldata1 %>% bind_cols(r.ldata1.1 %>% dplyr::select(r.wtp)) %>%
  bind_cols(r.ldata2 %>% dplyr::select(level)) %>% 
  gather(yindex, y, c(r.wtp,yhat)) %>% 
  mutate(modeled=ifelse(yindex=="yhat","Mean (Data)", "Model Prediction"))

lowdata<-subset(r.ldata_wtp, heuristic=="Low")
lowplot<-ggplot(lowdata, aes(y=y, x=level)) +
  ylab("Willingness to Pay")+ ylim(0,1)+
  xlab("Coffee Size (oz.)")+
  ggtitle("Low Heuristic Accuracy")+
  geom_jitter(data=subset(lowdata, level>0 & level<1),aes(y=y, x=level), 
              width=.03, height=0, colour="grey", shape=1)+
  stat_summary(fun.y=mean, geom="line", aes(linetype=spacing)) +
  stat_summary(fun.y=mean, geom="point", size=5, aes(y=, x=level, shape=modeled))+
 # stat_summary(fun.y=mean, geom="point", size=3, aes(y=yhat, x=level),shape=4)+
      scale_linetype_manual(name= "spacing" , 
                        values=c("dashed", "dotted"))+
  scale_shape_manual(name= "modeled" , values=c(0,4), 
                     labels=c("Mean (Data)","Model Prediction"))+
  geom_segment(aes(x = 0.15, xend = 0.25, y = 0.50, yend = 0.50)) +
  geom_segment(aes(x = 0.15, xend = 0.25, y = 0.20, yend = 0.20)) +
  geom_segment(aes(x = 0.75, xend = 0.85, y = 0.50, yend = 0.50)) +
  geom_segment(aes(x = 0.75, xend = 0.85, y = 0.80, yend = 0.80)) +
  annotate("text", x=.08, y=.50, label = "Ordinal", family="Times New Roman")+
  annotate("text", x=.08, y=.20, label = "Cardinal", family="Times New Roman")+
  annotate("text", x=.92, y=.50, label = "Ordinal", family="Times New Roman")+
  annotate("text", x=.92, y=.80, label = "Cardinal", family="Times New Roman")+
  theme(text=element_text(size=12), axis.text = element_text(size=12),
        legend.position="None")+
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(size=12))+
#  guides(linetype=FALSE)+
  scale_x_continuous(breaks = c(0,.2,.8,1), 
                     labels=c("10\n(0.00)","12\n(0.20)","18\n(0.80)","20\n(1.00)"))


highdata<-subset(r.ldata_wtp, heuristic=="High")
highplot<-ggplot(highdata, aes(y=y, x=level)) +
  ylab("Willingness to Pay")+ ylim(0,1)+
  xlab("Coffee Size (oz.)")+
  ggtitle("High Heuristic Accuracy")+
  geom_jitter(data=subset(highdata, level>0 & level<1),aes(y=y, x=level), 
              width=.03, height=0, colour="grey",shape=1)+
  stat_summary(fun.y=mean, geom="line", aes(linetype=spacing)) +
  stat_summary(fun.y=mean, geom="point", size=5, aes(y=, x=level, shape=modeled))+
  scale_linetype_manual(name= "spacing" , 
                        values=c("dashed", "dotted"), labels=c("Accelerated","Decelerated"))+
  scale_shape_manual(name= "modeled" , values=c(4,0), 
                   labels=c("Model Prediction","Mean (Data)"))+
  geom_segment(aes(x = 0.35, xend = 0.45, y = 0.50, yend = 0.50)) +
  geom_segment(aes(x = 0.35, xend = 0.45, y = 0.40, yend = 0.40)) +
  geom_segment(aes(x = 0.55, xend = 0.65, y = 0.50, yend = 0.50)) +
  geom_segment(aes(x = 0.55, xend = 0.65, y = 0.60, yend = 0.60)) +
  annotate("text", x=.28, y=.50, label = "Ordinal", family="Times New Roman")+
  annotate("text", x=.28, y=.40, label = "Cardinal", family="Times New Roman")+
  annotate("text", x=.72, y=.50, label = "Ordinal", family="Times New Roman")+
  annotate("text", x=.72, y=.60, label = "Cardinal", family="Times New Roman")+
  theme(text=element_text(size=12), axis.text = element_text(size=12),
        legend.title = element_blank(), legend.position=c(.75,.3))+
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(size=12))+
  guides(linetype= guide_legend(order=1), shape=guide_legend(order=2))+
    scale_x_continuous(breaks = c(0,.40,.60,1), 
                     labels=c("10\n(0.00)","14\n(0.40)","16\n(0.60)","20\n(1.00)"))



grid.arrange(lowplot,highplot, nrow=1)

Cairo(1600,800, file="C:/Users/Schley/Dropbox (RSM)/DanStuff/ByProject/BartDeLanghe/50 Shades of Medium/studies3a3b/heuristic_fit.png", bg="white", res=128)
grid.arrange(lowplot,highplot, nrow=1)
dev.off() 



#Cairo(1200,1000, file="ordinalprimewtp.png", bg="white", res=128)
#allplot
#dev.off()




###################################################################################
################Online Appendix: Analysis of Raw Judgments######################
###################################################################################



#####COMPARING ABSOLUTE JUDGMENTS FOR THE LOWEST AND HIGHEST OPTIONS
aggregate(wtp1~spacing, data=subset(d, heuristic=="Low"), mean)
aggregate(wtp1~spacing, data=subset(d, heuristic=="High"), mean)
aggregate(wtp3~spacing, data=subset(d, heuristic=="Low"), mean)
aggregate(wtp3~spacing, data=subset(d, heuristic=="High"), mean)

aggregate(wtp1~spacing, data=subset(d, heuristic=="Low"), median)
aggregate(wtp1~spacing, data=subset(d, heuristic=="High"), median)
aggregate(wtp3~spacing, data=subset(d, heuristic=="Low"), median)
aggregate(wtp3~spacing, data=subset(d, heuristic=="High"), median)

aggregate(wtp2~spacing, data=subset(d, heuristic=="Low"), mean)
aggregate(wtp2~spacing, data=subset(d, heuristic=="High"), mean)
aggregate(wtp2~spacing, data=subset(d, heuristic=="Low"), median)
aggregate(wtp2~spacing, data=subset(d, heuristic=="High"), median)

summary(aov(wtp1~spacing, data=subset(d, heuristic=="Low")))
kruskal.test(subset(d, heuristic=="Low")$wtp1~as.factor(subset(d, heuristic=="Low")$spacing))
summary(aov(wtp1~spacing, data=subset(d, heuristic=="High")))
kruskal.test(subset(d, heuristic=="High")$wtp1~as.factor(subset(d, heuristic=="High")$spacing))
summary(aov(wtp3~spacing, data=subset(d, heuristic=="Low")))
kruskal.test(subset(d, heuristic=="Low")$wtp3~as.factor(subset(d, heuristic=="Low")$spacing))
summary(aov(wtp3~spacing, data=subset(d, heuristic=="High")))
kruskal.test(subset(d, heuristic=="High")$wtp3~as.factor(subset(d, heuristic=="High")$spacing))

summary(aov(wtp1~as.factor(level2), data=d))
summary(aov(wtp2~as.factor(level2), data=d))
summary(aov(wtp3~as.factor(level2), data=d))
kruskal.test(d$wtp1~as.factor(d$level2))
kruskal.test(d$wtp2~as.factor(d$level2))
kruskal.test(d$wtp3~as.factor(d$level2))


###################################################################################
################For those interested in the pattern of monotonicity violiations
###################################################################################


#violations by condition
d_mon<-mydata3 %>% mutate(nonmon= ifelse(wtp3<=wtp1 | wtp2<wtp1 |wtp2>wtp3, 1,0),
                          nonmon_low= ifelse(wtp2<wtp1 & wtp3>wtp1,1,0),
                          nonmon_high= ifelse(wtp2>wtp3 & wtp3>wtp1,1,0),
                          nonmon_other= ifelse(wtp3<=wtp1,1,0))

nonmontable<-table(d_mon$level2, d_mon$nonmon)     
chisq.test(nonmontable)
table(d_mon$level2, d_mon$nonmon_low)
table(d_mon$level2, d_mon$nonmon_high)
fisher.test(table(d_mon$level2, d_mon$nonmon_other))
