library(effsize)
library(lavaan)
library(ggplot2)
library(sjPlot)
mtea<-read.csv("Study2_data.csv")

#Violence Exposure and Emotional Awareness
cohen.d(mtea$EM_AWARE~as.factor(mtea$ANY_ABUSE_CHILD_DV))
t.test(mtea$EM_AWARE~mtea$ANY_ABUSE_CHILD_DV)
summary(lm(EM_AWARE~S1AGE+SEX+nonwhite+INC_NEEDS+ANY_ABUSE_CHILD_DV,data=mtea))

#Emotional awareness and P
cor.test(mtea$EM_AWARE,mtea$P_bi_comb)
scatter_theme<- theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28),axis.text = element_text(size=24))
ggplot(mtea, aes(x=EM_AWARE-10, y=P_bi_comb)) + geom_point(shape=1,size=3) +  geom_smooth(method=lm) + scatter_theme + ylab("P-factor") + xlab("Low Emotional Awareness")+xlim(0,40)+ylim(-2,2)
summary(lm(P_bi_comb~EM_AWARE+S1AGE+SEX+nonwhite+INC_NEEDS+ANY_ABUSE_CHILD_DV,data=mtea))
summary(lm(P_bi_comb_FU~P_bi_comb+EM_AWARE+S1AGE+SEX+nonwhite+INC_NEEDS+ANY_ABUSE_CHILD_DV+Years,data=mtea))

#Mediation model
mod<-'
EM_AWARE~a*ANY_ABUSE_CHILD_DV+S1AGE
P_bi_comb_FU~b*EM_AWARE+c*ANY_ABUSE_CHILD_DV+P_bi_comb+S1AGE
ab1:=a*b
tot := a*b+c
EM_AWARE~~EM_AWARE
ANY_ABUSE_CHILD_DV~~ANY_ABUSE_CHILD_DV
P_bi_comb~~P_bi_comb
P_bi_comb_FU~~P_bi_comb_FU
EM_AWARE~1
ANY_ABUSE_CHILD_DV~1
P_bi_comb~1
P_bi_comb_FU~1
S1AGE~~S1AGE
S1AGE~1'
summary(lavaan(mod,data=mtea,se="bootstrap",bootstrap=1000),standardized=T,ci=T)
