library(effsize)
library(lavaan)
library(boot)
library(ggplot2)
library(sjPlot)
mtea<-read.csv("Study2_data.csv")
mtea$Sex<-"F"
mtea$Sex[which(mtea$SEX==0)]<-"M"

#Interaction of Age and Sex in relation to Emotional Awareness
summary(lm(EM_AWARE~S1AGE*SEX,data=mtea))
scatter_theme<- theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),axis.text = element_text(size=18), legend.title = element_text(size = 18), legend.text = element_text(size = 18))
qplot(x = S1AGE, y = (EM_AWARE-10), data = mtea, color = Sex) +
  geom_smooth(method = "lm") + xlab("Age") + ylab("Low Emotional Awareness") + geom_point(size=.1) + scatter_theme + xlim(c(7.5,18)) + ylim(c(0,40)) + scale_x_continuous(breaks=c(9,12,15,18))


#Violence Exposure and Emotional Awareness
cohen.d(mtea$EM_AWARE~as.factor(mtea$ANY_ABUSE_CHILD_DV))
t.test(mtea$EM_AWARE~mtea$ANY_ABUSE_CHILD_DV)
summary(lm(EM_AWARE~nonwhite+INC_NEEDS+ANY_ABUSE_CHILD_DV,data=mtea))

#Interaction of age and sex in relation to P Factor
summary(lm(P_bi_comb~Sex*S1AGE,data=mtea))
scatter_theme<- theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),axis.text = element_text(size=18), legend.title = element_text(size = 18), legend.text = element_text(size = 18))
qplot(x = S1AGE, y = P_bi_comb, data = mtea, color = Sex) +
  geom_smooth(method = "lm") + ylab("P-Factor") + xlab("Age") + scatter_theme

#Emotional awareness and P
cor.test(mtea$EM_AWARE,mtea$P_bi_comb)
scatter_theme<- theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28),axis.text = element_text(size=24))
ggplot(mtea, aes(x=EM_AWARE-10, y=P_bi_comb)) + geom_point(shape=1,size=3) +  geom_smooth(method=lm) + scatter_theme + ylab("P-factor") + xlab("Low Emotional Awareness")+xlim(0,40)+ylim(-2,2)
summary(lm(P_bi_comb~EM_AWARE+S1AGE*SEX+nonwhite+INC_NEEDS+ANY_ABUSE_CHILD_DV,data=mtea))
summary(lm(P_bi_comb_FU~P_bi_comb+EM_AWARE+scale(S1AGE,scale=F)*scale(SEX,scale=F)+nonwhite+INC_NEEDS+ANY_ABUSE_CHILD_DV+Years,data=mtea))

#Mediation of the effects of violence exposure on P by emotional awareness

myModMed_IDDesc <- function(x, id) {
  data <- x[id,]
  OutcomeModel <- coef(lm(P_bi_comb_FU ~ P_bi_comb + S1AGE*SEX + EM_AWARE + ANY_ABUSE_CHILD_DV, data = data))
  OutcomeModelAlone <- coef(lm(P_bi_comb_FU ~ S1AGE*SEX, data = data))
  MedModel <- coef(lm(EM_AWARE ~ S1AGE*SEX + ANY_ABUSE_CHILD_DV, data = data))
  type1 <- unname(OutcomeModel["EM_AWARE"] * MedModel["S1AGE:SEX"])
  type2 <- unname(OutcomeModel["EM_AWARE"] * MedModel["ANY_ABUSE_CHILD_DV"])
  return(c(type1 = type1,type2=type2))
}

set.seed(123); boot.ModMed_IDDesc <- boot(mtea, statistic = myModMed_IDDesc, R = 10000)
boot.ModMed_IDDesc 
boot.ci(boot.ModMed_IDDesc, type = "bca", index = 1)
boot.ci(boot.ModMed_IDDesc, type = "bca", index = 2)

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
summary(lavaan(mod,data=mtea,se="bootstrap",bootstrap=10000),standardized=T,ci=T)

mtea$Age<-mtea$S1AGE-mean(mtea$S1AGE)
mtea$AgexSEX<-mtea$Age*mtea$SEX

mod<-'
EM_AWARE~a1*ANY_ABUSE_CHILD_DV+Age+SEX+a2*AgexSEX
P_bi_comb_FU~b*EM_AWARE+c*ANY_ABUSE_CHILD_DV+P_bi_comb+Age+SEX+AgexSEX
ab1:=a1*b
ab2:=a2*b
ab:= a1*b + a2*b
tot:= a1*b +a2*b + c
EM_AWARE~~EM_AWARE
ANY_ABUSE_CHILD_DV~~ANY_ABUSE_CHILD_DV
P_bi_comb~~P_bi_comb
P_bi_comb_FU~~P_bi_comb_FU
EM_AWARE~1
ANY_ABUSE_CHILD_DV~1
P_bi_comb~1
P_bi_comb_FU~1
Age~~Age
Age~1
SEX~~SEX
SEX~1
AgexSEX~~AgexSEX
AgexSEX~1'
summary(lavaan(mod,data=mtea,se="bootstrap",bootstrap=10000),standardized=T,ci=T)
