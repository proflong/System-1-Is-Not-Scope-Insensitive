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
library(svglite)


#Importing dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mydata <- read_csv("Study2.csv")

#Lining up columns by condition
mydata2 <- mydata %>% mutate(cond=ifelse(!is.na(A_ACC), "control", ifelse(!is.na(A_UNI),"control", 
                                   ifelse(!is.na(A_DEC), "control", ifelse(!is.na(B_ACC),"ordinal labels", 
                                    ifelse(!is.na(B_UNI),"ordinal labels", ifelse(!is.na(B_DEC),"ordinal labels",NA)))))),
                             spacing=ifelse(!is.na(A_ACC), "ACC", ifelse(!is.na(A_UNI),"UNI", 
                                    ifelse(!is.na(A_DEC), "DEC", ifelse(!is.na(B_ACC),"ACC", 
                                    ifelse(!is.na(B_UNI),"UNI", ifelse(!is.na(B_DEC),"DEC",NA)))))),
                             choice=ifelse(!is.na(A_ACC), A_ACC, ifelse(!is.na(A_UNI),A_UNI, 
                                     ifelse(!is.na(A_DEC), A_DEC, ifelse(!is.na(B_ACC),B_ACC, 
                                                   ifelse(!is.na(B_UNI),B_UNI, ifelse(!is.na(B_DEC),B_DEC,NA)))))),
                             spacingquant=ifelse(!is.na(A_ACC), -2, ifelse(!is.na(A_UNI),0, 
                                    ifelse(!is.na(A_DEC), 2, ifelse(!is.na(B_ACC),-2, 
                                    ifelse(!is.na(B_UNI),0, ifelse(!is.na(B_DEC),2,NA)))))))
mydata2$choicenomid<-ifelse(mydata2$choice== 1, 0, ifelse(mydata2$choice==3,1,NA))
mydata2$choicemid<-ifelse(mydata2$choice== 2, 1, 0)

####AGE GENDER#####
table(mydata$SEX)
mean(mydata$AGE)
sd(mydata$AGE)

#######Table 1###################
proportions<- mydata2 %>%
  group_by(cond, spacing, choice) %>%
  summarize(n=n()) %>%
  mutate(freq=n/sum(n))


####################################################################################
#logistic regresion predicting preference for the middle option as a function of condition
logisticprefmid<-glm(choicemid~ cond + spacingquant+ cond*spacingquant, data=mydata2, family = binomial(link=logit))

summary(logisticprefmid)
require(MASS)
confint(logisticprefmid)
exp(cbind(coef(logisticprefmid)))

#logistic regresion predicting preference for the exterior options as a function of condition
logisticprefnomid<-glm(choicenomid~ cond + spacingquant+ cond*spacingquant, data=mydata2, family = binomial(link=logit))

summary(logisticprefnomid)
confint(logisticprefnomid)



#chisquare uniform condition
#Did participants discriminate within the uniform condition between the three options?
tableuniformall <- table(subset(mydata2, spacing == "UNI")$choice)
chisq.test(tableuniformall)

#Was there a difference in preference in the uniform condition between the contorl and ordinal labels condition
tableuniform <- table(subset(mydata2, spacing == "UNI")$choice, subset(mydata2, spacing == "UNI")$cond)
chisq.test(tableuniform)

#chisquare low and high options
lowtb<-table(subset(mydata2, choice==1)$cond)
chisq.test(lowtb)
hightb<-table(subset(mydata2, choice==3)$cond)
chisq.test(hightb)

#Do we find evidence of the similarity effect? Does it change as a function of the ordinal labels?
similarity<-glm(choicenomid~cond + spacingquant+ cond*spacingquant, data=mydata2, family = binomial(link=logit))
summary(similarity) 
confint(similarity)
exp(cbind(coef(similarity)))
