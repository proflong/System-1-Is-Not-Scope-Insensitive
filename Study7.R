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
#Importing dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mydata <- read_csv("Study7.csv")
set.seed(13)


#Scaling function for relative willingness to pay 
scaling_func <- function(x, x1, x5)(x-x1)/(x5-x1)  
#Specifying range and frequency values
d <- mydata %>% mutate( wtp.low=ifelse(attributes=="multi",multi.l,single.l),
                        wtp.mid.1=ifelse(attributes=="multi",multi.m1,single.m1),
                        wtp.mid.2=ifelse(attributes=="multi",multi.m2,single.m2),
                        wtp.mid.3=ifelse(attributes=="multi",multi.m3,single.m3),
                        wtp.high=ifelse(attributes=="multi",multi.h,single.h),
                       level1 = 0, 
                       level2 = plyr::revalue(functionShape, c('inverseS' = .025, 's' = .475)) %>% as.numeric,
                       level3 = 0.5,
                       level4 = plyr::revalue(functionShape, c('inverseS' = .975, 's' = .525)) %>% as.numeric,
                       level5 = 1,
                       freq1 = 0,
                       freq2 = .25,
                       freq3 = .5,
                       freq4 = .75,
                       freq5 = 1) %>% 
  mutate(r.wtp1 = 0,
         r.wtp2 = scaling_func(wtp.mid.1, wtp.low, wtp.high),
         r.wtp3 = scaling_func(wtp.mid.2, wtp.low, wtp.high),
         r.wtp4 = scaling_func(wtp.mid.3, wtp.low, wtp.high),
         r.wtp5 = 1
          ) %>%
  filter(r.wtp2 >= 0 & r.wtp2 <= 1) %>%
  filter(r.wtp3 >= 0 & r.wtp3 <= 1) %>%
  filter(r.wtp4 >= 0 & r.wtp4 <= 1) %>%
  filter(wtp.high>wtp.low)%>%
  mutate(range= wtp.high-wtp.low,
         logrange=log(range+1),
         range_center=logrange-mean(logrange, na.rm = T))
d$id<-1:length(d$ResponseId)

#Age and gender
mean(as.numeric(d$age), na.rm=T)
sd(as.numeric(d$age), na.rm=T)
table(d$gender)



###########################################################################
#######################BAYESIAN MODEL FITTING W###########################
############################################################################
#putting data in long form and creating some dummy codes:

d1 <- d %>% gather(index, r.wtp, r.wtp1:r.wtp5)
d1.1<- d %>% gather(index,wtp, c(wtp.low,wtp.mid.1,wtp.mid.2,wtp.mid.3,wtp.high))
d2 <- d %>% gather(index, level, level1:level5)
d3 <- d %>% gather(index, freq, freq1:freq5)
d4 <- d1 %>% bind_cols(d2 %>% dplyr::select(level), d3 %>% dplyr::select(freq), d1.1 %>% dplyr::select(wtp)) %>%
  arrange(id) %>% 
  mutate(d_att=ifelse(attributes=="multi",1,0),
         d_space=ifelse(functionShape=="s",1,0),
         d_att_c=d_att-mean(d_att),
         d_space_c=d_space-mean(d_space),
         d_int=d_space*d_att,
         id_sorted=sort(rep(1:length(unique(d1$id)), 5)))
d4$attributes<-relevel(as.factor(d4$attributes), ref="single")

#For some R installations this step is needed to realigns the paths for rtools for some computers
#devtools::setup_rtools()
###########################################TEST########################
#Using the STAN package for Bayesian estimation
#specifying the data and giving variables easy names

reg_data <- d4 %>% transmute(x = level, r = freq, y = r.wtp, id = id_sorted,z_a=d_space, z_b=d_att, z_c=d_int %>% factor %>% forcats::fct_anon() %>% as.integer) %>% as.list
#Specifying the constants for the model
const_data <- list(n_obs = length(reg_data$x), n_respondents = max(reg_data$id), nvar=4)
#grouping these two data files as an environment
stan_data <- c(reg_data, const_data) %>% as.environment

#STAN code
stan_code <- "
data {
int n_obs;
int n_respondents;
int id[n_obs];
real x[n_obs]; //this is the range component from Range-Frequency Theory
real r[n_obs]; //this is the frequency component from Range-Frequency Theory
real y[n_obs]; //this is the judgment
vector [n_obs] z_a;
vector [n_obs] z_b;
vector [n_obs] z_c;
int nvar;
}

parameters {
matrix[nvar, n_respondents] alpha;
real<lower=0> s2;
// variance components;
vector<lower=0>[nvar] nu;
// correlation components; 
cholesky_factor_corr[nvar] L_Omega; //prior correlation
real<lower = 0, upper = 1> w_bar; // this constrains estimates of the average w between 0 and 1
real<lower = -1, upper = 1> w_a;
real<lower = -1, upper = 1> w_b;
real<lower = -1, upper = 1> w_c;
}

transformed parameters {
matrix[n_respondents, nvar] w_i;
matrix[n_respondents, nvar] Vbeta_reparametrized;
row_vector[nvar] w_aggregate;
// draw beta for all H and all nvar
Vbeta_reparametrized = (diag_pre_multiply(nu, L_Omega)*alpha)';
w_aggregate=[w_bar, w_a, w_b, w_c];
for(h in 1:n_respondents) 
w_i[h]=w_aggregate+Vbeta_reparametrized[h];
}



model {
vector[n_obs] w; //this constrains estimates of w per participant between 0 and 1
nu~exponential(5);
s2 ~ exponential(5); //specifying a common variance structure
to_vector(alpha)~normal(0,1);
L_Omega~lkj_corr_cholesky(1);
w_bar~normal(0.5, 0.1);
w_a~normal(0, 0.5);
w_b~normal(0, 0.5);
w_c~normal(0, 0.5);

for (o in 1:n_obs) {
w[o]=inv_logit(w_i[id[o],1]+ w_i[id[o],2]*z_a[o]+ w_i[id[o],3]*z_b[o] + w_i[id[o],4]*z_c[o]);
y[o] ~ normal(w[o] * x[o] + (1-w[o]) * r[o], s2); //RFT model at the individual participant level
}

}

generated quantities {
corr_matrix[nvar] Omega;
Omega=L_Omega*L_Omega';
}

"
#This compiles the STAN code into C++ code (which is where STAN runs natively)
m <- stan_model(model_name = 'my_model', model_code = stan_code)
#This starts the sampling procedure
#Don't worry about convergence warning that relates to the diagonals of the omega matrix

fit <- sampling(m, data = stan_data, iter=5000, chains=4)


####This just looks at convergence of parameters
traceplot(fit, pars=c("lp__","w_bar", "w_a","w_b", "w_c","Omega"))


#This extracts the fitted values, particular each participants' w parameter
w_bar <- get_posterior_mean(fit, 'w_bar') %>% .[,5]
w_a <- get_posterior_mean(fit, 'w_a') %>% .[,5]
w_b <- get_posterior_mean(fit, 'w_b') %>% .[,5]
w_c <- get_posterior_mean(fit, 'w_c') %>% .[,5]

phis=summary(fit, pars = c("lp__","w_bar","w_a", "w_b","w_c", "Omega"), probs=c(0.025, 0.975))$summary
phis

list_of_draws <- rstan::extract(fit)
print(names(list_of_draws))

matrix_of_draws <- as.matrix(fit)
print(colnames(matrix_of_draws[,718:721]))

#convert to df
logitestimates_df<-as.data.frame(matrix_of_draws[,718:721]) %>%
  mutate(raw_w_singleinv=1/(1+exp(-(w_bar))),
         raw_w_singles=1/(1+exp(-(w_bar+w_a))),
         raw_w_multiinv=1/(1+exp(-(w_bar+w_b))),
         raw_w_multis=1/(1+exp(-(w_bar+w_a+w_b))),
         raw_w_s=(raw_w_multis+raw_w_singles)/2,
         raw_w_inv=(raw_w_multiinv+raw_w_singleinv)/2,
         raw_w_multi=(raw_w_multiinv+raw_w_multis)/2,
         raw_w_single=(raw_w_singleinv+raw_w_singles)/2,
         effect_inv=((raw_w_multiinv+raw_w_singleinv)/2)-((raw_w_multis+raw_w_singles)/2),
         effect_multi=((raw_w_multiinv+raw_w_multis)/2)-((raw_w_singleinv+raw_w_singles)/2),
         w_int= (raw_w_multiinv-raw_w_singleinv)-(raw_w_multis-raw_w_singles)
  )

mean(logitestimates_df$raw_w_singleinv)
mean(logitestimates_df$raw_w_singles)
mean(logitestimates_df$raw_w_multiinv)
mean(logitestimates_df$raw_w_multis)

mean(logitestimates_df$raw_w_s)
mean(logitestimates_df$raw_w_inv)
mean(logitestimates_df$raw_w_multi)
mean(logitestimates_df$raw_w_single)
mean(logitestimates_df$effect_inv)
mean(logitestimates_df$effect_multi)

mean(logitestimates_df$w_int)


quantile(logitestimates_df$raw_w_singleinv, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$raw_w_singles, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$raw_w_multiinv, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$raw_w_multis, probs=c(0.025, 0.5, 0.975))

quantile(logitestimates_df$raw_w_s, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$raw_w_inv, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$raw_w_multi, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$raw_w_single, probs=c(0.025, 0.5, 0.975))

quantile(logitestimates_df$effect_inv, probs=c(0.025, 0.5, 0.975))
quantile(logitestimates_df$effect_multi, probs=c(0.025, 0.5, 0.975))

quantile(logitestimates_df$w_int, probs=c(0.025, 0.5, 0.975))


#This is to make sure the w parameters have some face validity. 
#Higher w's should also have a steeper (i.e., more linear) pattern in the data.
data_frame(i = 1:const_data$n_respondents, w) %>% 
  inner_join(reg_data %>% as_data_frame) %>% 
  mutate(cut_w = cut_number(w,4)) %>% 
  filter(y>0&y<1) %>%
  ggplot(aes(x = x, y = y)) + geom_point() + facet_wrap(~cut_w) + stat_smooth(method = 'lm')

#arranging the original and fitted data files then joining them by ID
fittedw<- data_frame(id = 1:const_data$n_respondents, w_bar, w_a, w_b) %>% 
  inner_join(reg_data %>% as_data_frame)
fittedw<-subset(fittedw, r==.5)
fittedw %>% arrange(id)
d %>% arrange(id)
fitteddata <- d %>% mutate(d_att=ifelse(attributes=="multi",1,0),
                           d_space=ifelse(functionShape=="s",1,0))%>%
  inner_join(fittedw)%>%
  mutate(w=1/(1+exp(-(w_bar+(d_space*w_a)+(d_att*w_b)))),
         yhat1=(w*level1)+((1-w)*0),
         yhat2=(w*level2)+((1-w)*.25),
         yhat3=(w*level3)+((1-w)*.5),
         yhat4=(w*level4)+((1-w)*.75),
         yhat5=(w*level5)+((1-w)*1))


#Descriptives by condition
aggregate(w~attributes + functionShape, data=fitteddata, mean)
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}
aggregate(w~attributes , data=fitteddata, sd)




########################################################################################
##########FIGURE 6######################################### 
########################################################################################

d1 <- d %>% gather(index, r.wtp, r.wtp1:r.wtp5)
d1.1<- d %>% gather(index,wtp, c(wtp.low,wtp.mid.1,wtp.mid.2,wtp.mid.3,wtp.high))
d2 <- d %>% gather(index, level, level1:level5)
d3 <- d %>% gather(index, freq, freq1:freq5)
d4 <- d1 %>% bind_cols(d2 %>% dplyr::select(level), d3 %>% dplyr::select(freq), d1 %>% dplyr::select(r.wtp)) %>%
  mutate(d_att=ifelse(attributes=="multi",1,0),
         d_space=ifelse(functionShape=="s",1,0),
         d_int=d_space*d_att)
d4$attributes<-relevel(as.factor(d4$attributes), ref="single")


####PLOTS##########
inverses_data<-subset(d4, functionShape=="inverseS")
inversesplot<-ggplot(data=inverses_data , aes(x=level, y =r.wtp)) +
  ylim(0,1)+ ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Apartment Square Footage")+
  geom_jitter(data=subset(inverses_data, level>0 & level<1), 
              aes(shape=attributes), width=.01, colour="grey")+
  stat_summary(fun.y=mean, geom="point", size=3, aes(shape=attributes)) +
  stat_summary(fun.y=mean, geom="line", aes(linetype=attributes))+
  scale_linetype_manual(name= "attributes" , 
                        values=c("solid", "solid"))+
  scale_shape_manual(name= "attributes" , 
                     values=c(16,17),  labels=c("Single-Attribute","Multi-Attribute"))+
  guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = "none",
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.5,1), 
                     labels=c("1000\n(0.00)","1500\n(0.50)","2000\n(1.00)"))

s_data<-subset(d4, functionShape=="s")
splot<-ggplot(data=s_data , aes(x=level, y =r.wtp)) +
  ylim(0,1)+ ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Apartment Square Footage")+
  geom_jitter(data=subset(s_data, level>0 & level<1), 
              aes(shape=attributes), width=.01, colour="grey")+
  stat_summary(fun.y=mean, geom="point", size=3, aes(shape=attributes)) +
  stat_summary(fun.y=mean, geom="line", aes(linetype=attributes))+
  scale_linetype_manual(name= "attributes" , 
                        values=c("solid", "solid"))+
  scale_shape_manual(name= "attributes" , 
                     values=c(16,17),  labels=c("Single-Attribute","Multi-Attribute"))+
  guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = c(.7,.45),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.5,1), 
                     labels=c("1000\n(0.00)","1500\n(0.50)","2000\n(1.00)"))


figure6<-grid.arrange(inversesplot,splot, nrow=1)

ggsave("Figure 6.svg", width = 12, height=6, figure6)



########################################################################################
##########Figure S8######################################### 
########################################################################################



#putting data into long form
d1 <- d %>% gather(index, r.wtp, r.wtp1:r.wtp5)
d1.1<- d %>% gather(index,wtp, c(wtp.low,wtp.mid.1,wtp.mid.2,wtp.mid.3,wtp.high))
d2 <- d %>% gather(index, level, level1:level5)
d3 <- d %>% gather(index, freq, freq1:freq5)
d4 <- d1 %>% bind_cols(d2 %>% dplyr::select(level), d3 %>% dplyr::select(freq), d1.1 %>% dplyr::select(wtp)) %>%
  mutate(d_att=ifelse(attributes=="multi",1,0),
         d_space=ifelse(functionShape=="s",1,0),
         d_int=d_space*d_att)
d4$attributes<-relevel(as.factor(d4$attributes), ref="single")


gm_mean <-function(x){exp(mean(log(x)))}
inverses_data<-subset(d4, functionShape=="inverseS")
inversesplot_abs<-
  ggplot(data=inverses_data , aes(x=level, y =wtp)) +
  coord_cartesian(ylim = c(500,1500))+
  ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Apartment Square Footage")+
  stat_summary(fun.y=gm_mean, geom="point", size=3, aes(shape=attributes)) +
  stat_summary(fun.y=gm_mean, geom="line", aes(linetype=attributes))+
  scale_linetype_manual(name= "attributes" , 
                        values=c("solid", "solid"))+
  scale_shape_manual(name= "attributes" , 
                     values=c(16,17),  labels=c("Single-Attribute","Multi-Attribute"))+
  guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = "none",
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.5,1), 
                     labels=c("1000\n(0.00)","1500\n(0.50)","2000\n(1.00)"))

s_data<-subset(d4, functionShape=="s")
splot_abs<-
  ggplot(data=s_data , aes(x=level, y =wtp)) +
  coord_cartesian(ylim = c(500,1500))+
  ylab("Willingness to Pay")+
  xlim(0,1)+ xlab("Apartment Square Footage")+
  stat_summary(fun.y=gm_mean, geom="point", size=3, aes(shape=attributes)) +
  stat_summary(fun.y=gm_mean, geom="line", aes(linetype=attributes))+
  scale_linetype_manual(name= "attributes" , 
                        values=c("solid", "solid"))+
  scale_shape_manual(name= "attributes" , 
                     values=c(16,17),  labels=c("Single-Attribute","Multi-Attribute"))+
  guides(linetype=FALSE)+
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position = c(.7,.45),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0,0.5,1), 
                     labels=c("1000\n(0.00)","1500\n(0.50)","2000\n(1.00)"))


figures8<-grid.arrange(inversesplot_abs,splot_abs, nrow=1)

ggsave("Figure S8.svg", width = 12, height=6, figures8)



#putting data into long form
d1 <- d %>% gather(index, r.wtp, r.wtp1:r.wtp5)
d1.1<- d %>% gather(index,wtp, c(wtp.low,wtp.mid.1,wtp.mid.2,wtp.mid.3,wtp.high))
d2 <- d %>% gather(index, level, level1:level5)
d3 <- d %>% gather(index, freq, freq1:freq5)
d4 <- d1 %>% bind_cols(d2 %>% dplyr::select(level), d3 %>% dplyr::select(freq), d1.1 %>% dplyr::select(wtp)) %>%
  arrange(id) %>% 
  mutate(d_att=ifelse(attributes=="multi",1,0),
         d_space=ifelse(functionShape=="s",1,0),
         d_att_c=d_att-mean(d_att),
         d_space_c=d_space-mean(d_space),
         d_int=d_space*d_att,
         id_sorted=sort(rep(1:length(unique(d1$id)), 5)))
d4$attributes<-relevel(as.factor(d4$attributes), ref="single")


########################################################################################
##########Analysis of Raw Judgments Online Appendix######################################### 
########################################################################################

#COMPARING ABSOLUTE JUDGMENTS FOR THE LOWEST AND HIGHEST OPTIONS
aggregate(wtp.low~functionShape, data=subset(d, attributes=="single"), mean)
aggregate(wtp.low~functionShape, data=subset(d, attributes=="multi"), mean)
aggregate(wtp.high~functionShape, data=subset(d, attributes=="single"), mean)
aggregate(wtp.high~functionShape, data=subset(d, attributes=="multi"), mean)

aggregate(wtp.low~functionShape, data=subset(d, attributes=="single"), median)
aggregate(wtp.low~functionShape, data=subset(d, attributes=="multi"), median)
aggregate(wtp.high~functionShape, data=subset(d, attributes=="single"), median)
aggregate(wtp.high~functionShape, data=subset(d, attributes=="multi"), median)


summary(aov(wtp.low~functionShape, data=subset(d, attributes=="single")))
kruskal.test(subset(d, attributes=="single")$wtp.low~as.factor(subset(d, attributes=="single")$functionShape))
summary(aov(wtp.low~functionShape, data=subset(d, attributes=="multi")))
kruskal.test(subset(d, attributes=="multi")$wtp.low~as.factor(subset(d, attributes=="multi")$functionShape))
summary(aov(wtp.high~functionShape, data=subset(d, attributes=="single")))
kruskal.test(subset(d, attributes=="single")$wtp.high~as.factor(subset(d, attributes=="single")$functionShape))
summary(aov(wtp.high~functionShape, data=subset(d, attributes=="multi")))
kruskal.test(subset(d, attributes=="multi")$wtp.high~as.factor(subset(d, attributes=="multi")$functionShape))

aggregate(wtp.mid.1~functionShape, data=subset(d, attributes=="single"), mean)
aggregate(wtp.mid.1~functionShape, data=subset(d, attributes=="multi"), mean)
aggregate(wtp.mid.2~functionShape, data=subset(d, attributes=="single"), mean)
aggregate(wtp.mid.2~functionShape, data=subset(d, attributes=="multi"), mean)
aggregate(wtp.mid.3~functionShape, data=subset(d, attributes=="single"), mean)
aggregate(wtp.mid.3~functionShape, data=subset(d, attributes=="multi"), mean)

aggregate(wtp.mid.1~functionShape, data=subset(d, attributes=="single"), median)
aggregate(wtp.mid.1~functionShape, data=subset(d, attributes=="multi"), median)
aggregate(wtp.mid.2~functionShape, data=subset(d, attributes=="single"), median)
aggregate(wtp.mid.2~functionShape, data=subset(d, attributes=="multi"), median)
aggregate(wtp.mid.3~functionShape, data=subset(d, attributes=="single"), median)
aggregate(wtp.mid.3~functionShape, data=subset(d, attributes=="multi"), median)





summary(aov(wtp.mid.1~functionShape, data=subset(d, attributes=="single")))
kruskal.test(subset(d, attributes=="single")$wtp.mid.1~as.factor(subset(d, attributes=="single")$functionShape))
summary(aov(wtp.mid.1~functionShape, data=subset(d, attributes=="multi")))
kruskal.test(subset(d, attributes=="multi")$wtp.mid.1~as.factor(subset(d, attributes=="multi")$functionShape))

summary(aov(wtp.mid.2~functionShape, data=subset(d, attributes=="single")))
kruskal.test(subset(d, attributes=="single")$wtp.mid.2~as.factor(subset(d, attributes=="single")$functionShape))
summary(aov(wtp.mid.2~functionShape, data=subset(d, attributes=="multi")))
kruskal.test(subset(d, attributes=="multi")$wtp.mid.2~as.factor(subset(d, attributes=="multi")$functionShape))

summary(aov(wtp.mid.3~functionShape, data=subset(d, attributes=="single")))
kruskal.test(subset(d, attributes=="single")$wtp.mid.3~as.factor(subset(d, attributes=="single")$functionShape))
summary(aov(wtp.mid.3~functionShape, data=subset(d, attributes=="multi")))
kruskal.test(subset(d, attributes=="multi")$wtp.mid.3~as.factor(subset(d, attributes=="multi")$functionShape))









################################################END TEST

#Using the STAN package for Bayesian estimation
#specifying the data and giving variables easy names
reg_data <- d4 %>% transmute(x = level, r = freq, y = r.wtp, id, i = id %>% factor %>% forcats::fct_anon() %>% as.integer) %>% as.list
#Specifying the constants for the model
const_data <- list(n_obs = length(reg_data$x), n_respondents = max(reg_data$i))
#grouping these two data files as an environment
stan_data <- c(reg_data, const_data) %>% as.environment

#STAN code
stan_code <- "
data {
int n_obs;
int n_respondents;
int i[n_obs];
real x[n_obs]; //this is the range component from Range-Frequency Theory
real r[n_obs]; //this is the frequency component from Range-Frequency Theory
real y[n_obs]; //this is the judgment
}
parameters {
real<lower = 0, upper = 1> w[n_respondents]; //this constrains estimates of w per participant between 0 and 1
real<lower = 0> s2; // Some variance greater than 0
real<lower = 0, upper = 1> w_bar; // this constrains estimates of the average w between 0 and 1
}
model {
s2 ~ inv_chi_square(3); //specifying a common variance structure


for (o in 1:n_obs) {
y[o] ~ normal(w[i[o]] * x[o] + (1-w[i[o]]) * r[o], sqrt(s2)); //RFT model at the individual participant level
}

for (j in 1:n_respondents) {
w[j] ~ normal(w_bar, 1)T[0,1]; //This tells the model to estimate the average w_bar (i.e., hierarchical)
}
}
"
#This compiles the STAN code into C++ code (which is where STAN runs natively)
m <- stan_model(model_name = 'my_model', model_code = stan_code)
#This starts the sampling procedure
fit <- sampling(m, data = stan_data, iter=2000, chains=4)
#This extracts the fitted values, particular each participants' w parameter
w <- get_posterior_mean(fit, 'w') %>% .[,5]

#This is to make sure the w parameters have some face validity. 
#Higher w's should also have a steeper (i.e., more linear) pattern in the data.
data_frame(i = 1:const_data$n_respondents, w) %>% 
  inner_join(reg_data %>% as_data_frame) %>% 
  mutate(cut_w = cut_number(w,4)) %>% 
  filter(y>0&y<1) %>%
  ggplot(aes(x = x, y = y)) + geom_point() + facet_wrap(~cut_w) + stat_smooth(method = 'lm')

#arranging the original and fitted data files then joining them by ID
fittedw<- data_frame(i = 1:const_data$n_respondents, w) %>% 
  inner_join(reg_data %>% as_data_frame)
fittedw<-subset(fittedw, r==.5)
fittedw %>% arrange(id)
d %>% arrange(id)
fitteddata <- d %>% inner_join(fittedw)

#Descriptives by condition
aggregate(w~attributes + functionShape, data=fitteddata, mean)
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}
aggregate(w~attributes , data=fitteddata, sd)



#reg_data %>% as_data_frame %>% gather(var, val, -y, -i) %>% ggplot(aes(x = val, y = y, colour = factor(i))) + geom_point(show.legend = FALSE) + facet_wrap(~var) + geom_smooth(method="lm", se = FALSE, show.legend = FALSE)
anova <- aov(w~functionShape+attributes+functionShape:attributes, data=fitteddata)
summary(anova)
summary(aov(w~attributes,data=subset(fitteddata, functionShape == "inverseS")))
summary(aov(w~attributes,data=subset(fitteddata, functionShape == "s")))


means<-aggregate(r.wtp~level+cond,d4, mean)

ggplot(data=means, aes(x=level, y=r.wtp, colour=cond))+
 geom_jitter(data = d4, aes(x=level, y=r.wtp), color = "gray", alpha=0.5, width=0.05, height=0.05)+
  ylab("Rescaled Value")+ xlab("Rescaled Attribute Level")+
 xlim(0,1)+ ylim(0,1)+
theme(text=element_text(family="Times New Roman"), legend.title=element_blank())+
  geom_line(aes(x=level, y=r.wtp, colour=cond),size=1)+geom_point(aes(x=level, y=r.wtp, colour=cond),size=3)+
  scale_colour_discrete(breaks=c("sinv", "minv", "ss", "ms"), labels=c("Inv S/Single Attribute","Inv S/Multi Attribute","S/Single Attribute", "S/Multi Attribute"))






























###################################################################################
################Online Appendix: Analysis of Raw Judgments######################
###################################################################################




#One huge outlier when analyzing absolute data. Removing them before this step
fitted_minus_outlier<-fitteddata %>% filter(range_center<5) %>%
  #recalculate range-center since the mean will have shifted
  mutate(range_center=logrange-mean(logrange, na.rm = T),
#changing reference levels for easier intpertation
        attributes=as.factor(attributes),
        functionShape=as.factor(functionShape),
        spacing=relevel(functionShape, ref="s"),
        condition=relevel(attributes, ref="single"),
        spacing_num_raw=ifelse(spacing=="s",-.5,.5),
        spacing_centered=spacing_num_raw-mean(spacing_num_raw),
        cond_num_raw=ifelse(condition=="single",-.5,.5),
        cond_centered=cond_num_raw-mean(cond_num_raw)
  )
         

#Controlling for range of WTP judgments
anova <- aov(w~functionShape + attributes+range_center+ functionShape:attributes+ functionShape:range_center, data=fitted_minus_outlier)
summary(anova)
reg <- lm(w~1+ spacing_centered + cond_centered+range_center+ spacing_centered:cond_centered+ spacing_centered:range_center + cond_centered:range_center, data=fitted_minus_outlier)
summary(reg)

#Adding the log WTP and absolute levels back into the dataset for plots
d5<-d4 %>% filter(range_center<5) %>% 
  mutate(log_wtp=log(wtp+1),
         level_abs=((level*1000)+1000))



#remember to detach plyr otherwise summarize function does not use group_by 
detach(package:plyr)
abs_mean<-d5  %>%
  #Do the next step for each of these two variables
  group_by(level_abs, cond) %>%
  #Calculates the mean within the block specified in the previous step
  summarise(meanwtp= mean(log_wtp, na.rm=TRUE))%>%
  #Creating two columns with the conditions respecified
  mutate(Condition= ifelse((cond=="minv" | cond=="ms"), "Multi-Attribute","Single-Attribute"),
         Spacing = ifelse((cond=="minv" | cond=="sinv"), "Spread Out", "Centered"))

ggplot(data=abs_mean, aes(x=level_abs, y=meanwtp))+
  geom_jitter(data = d5, aes(x=level_abs, y=wtp), alpha=0.1, width=50 , show.legend=FALSE)+
  ylab("Willingness to Pay")+ xlab("Square Feet")+
  xlim(1000,2000)+ ylim(650,1300)+
  theme(text=element_text(family="Times New Roman"), legend.title=element_blank())+
  geom_line(aes(x=level_abs, y=exp(meanwtp), colour=Condition, linetype=Spacing),size=1)+
  geom_point(aes(x=level_abs, y=exp(meanwtp),colour=Condition, shape=Spacing),size=3)+
  scale_fill_manual(values=c("black","darkgrey"))+
  scale_color_manual(values=c("black","darkgrey"))+
  guides(colour = guide_legend(override.aes = list(shape = NA)))
  
  
  
  
  scale_colour_discrete(breaks=c("sinv", "minv", "ss", "ms"), labels=c("Inv S/Single Attribute","Inv S/Multi Attribute","S/Single Attribute", "S/Multi Attribute"))


  ###################################################################################
  ################For those interested in the pattern of monotonicity violiations
  ###################################################################################
  
  #violations of nonmonotonicity by condition
  d_mon<-mydata %>% mutate(nonmon= ifelse(wtp.high<=wtp.low | wtp.mid.1<wtp.low |wtp.mid.1>wtp.high  | wtp.mid.2<wtp.low |wtp.mid.2>wtp.high | wtp.mid.3<wtp.low |wtp.mid.3>wtp.high, 1,0),
                           nonmon_outside=ifelse((wtp.mid.1<wtp.low | wtp.mid.3>wtp.high) & wtp.high>wtp.low, 1,0),
                           nonmon_other=ifelse(nonmon==1 & nonmon_outside==0, 1,0),
                           nonmon_low=ifelse((wtp.mid.1<wtp.low & wtp.low<wtp.high),1,0),
                           nonmon_high=ifelse((wtp.mid.3>wtp.high & wtp.low<wtp.high), 1,0),
                           nonmon_slow=ifelse((wtp.mid.1>wtp.high & wtp.low<wtp.high),1,0),
                           nonmon_shigh=ifelse((wtp.mid.3<wtp.low & wtp.low<wtp.high),1,0),
                           mid1_low=ifelse(wtp.mid.1<wtp.low & wtp.low<wtp.high, 1,0),
                           mid1_high=ifelse(wtp.mid.1>wtp.high & wtp.low<wtp.high, 1,0),
                           mid2_low=ifelse(wtp.mid.2<wtp.low & wtp.low<wtp.high, 1,0),
                           mid2_high=ifelse(wtp.mid.2>wtp.high & wtp.low<wtp.high, 1,0),
                           mid3_low=ifelse(wtp.mid.3<wtp.low & wtp.low<wtp.high, 1,0),
                           mid3_high=ifelse(wtp.mid.3>wtp.high & wtp.low<wtp.high, 1,0)
  )
  
  
  table(d_mon$functionShape,  d_mon$attributes, d_mon$ mid1_low)
  table(d_mon$functionShape,  d_mon$attributes, d_mon$ mid1_high)
  table(d_mon$functionShape,  d_mon$attributes, d_mon$ mid2_low)
  table(d_mon$functionShape,  d_mon$attributes, d_mon$ mid2_high)
  table(d_mon$functionShape,  d_mon$attributes, d_mon$ mid3_low)
  table(d_mon$functionShape,  d_mon$attributes, d_mon$ mid3_high)
  
  
  table(d_mon$functionShape,  d_mon$attributes, d_mon$nonmon_low)
  table(d_mon$functionShape,  d_mon$attributes, d_mon$nonmon_high)
  
  table(d_mon$functionShape,  d_mon$attributes, d_mon$nonmon_slow)
  table(d_mon$functionShape,  d_mon$attributes, d_mon$nonmon_shigh)
  
  
  table(d_mon$functionShape,  d_mon$attributes, d_mon$nonmon)
  table(d_mon$functionShape,  d_mon$attributes, d_mon$nonmon_outside) 
  table(d_mon$functionShape,  d_mon$attributes, d_mon$nonmon_other) 
  
  mon_multi<-subset(d_mon, attributes=="multi")
  fisher.test(table(mon_multi$functionShape, mon_multi$nonmon_outside))
  fisher.test(table(mon_multi$functionShape, mon_multi$nonmon_other) )
  fisher.test(table(mon_multi$functionShape, mon_multi$nonmon) )
  
  Poissonmod<- glm(nonmon ~functionShape+attributes, family = "poisson", data = d_mon)
  summary(Poissonmod)
  Poissonmodint<- glm(nonmon ~functionShape*attributes, family = "poisson", data = d_mon)
  summary(Poissonmodint)
