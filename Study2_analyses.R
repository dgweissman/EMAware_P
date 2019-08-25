library(effsize)
library(lavaan)
library(boot)
library(ggplot2)
library(sjPlot)
library(lm.beta)
mtea<-read.csv("Study2_data.csv")
mtea$Sex<-"F"
mtea$Sex[which(mtea$SEX==0)]<-"M"
mtea$EmA<-mtea$EM_AWARE-min(mtea$EM_AWARE,na.rm=T)

#Interaction of Age and Sex in relation to Emotional Awareness
summary(lm(EM_AWARE~S1AGE*SEX,data=mtea))
cor.test(mtea$EM_AWARE[which(mtea$SEX==1)],mtea$S1AGE[which(mtea$SEX==1)])
cor.test(mtea$EM_AWARE[which(mtea$SEX==0)],mtea$S1AGE[which(mtea$SEX==0)])
scatter_theme<- theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),axis.text = element_text(size=18), legend.title = element_text(size = 18), legend.text = element_text(size = 18))
qplot(x = S1AGE, y = (EmA), data = mtea[-which(is.na(mtea$EmA)),], color = Sex) +
 geom_smooth(method = "lm") + xlab("Age") + ylab("Low Emotional Awareness") + scatter_theme + scale_x_continuous(breaks=c(9,12,15,18),limits = c(7.5, 18)) + ylim(c(0,40))


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
ggplot(mtea[-which(is.na(mtea$EmA)),], aes(x=EmA, y=P_bi_comb)) + geom_point() +  geom_smooth(method=lm) + scatter_theme + ylab("P-factor") + xlab("Low Emotional Awareness")+xlim(0,40)+ylim(-2,2)
summary(lm(P_bi_comb~EM_AWARE+S1AGE*SEX+nonwhite+INC_NEEDS+ANY_ABUSE_CHILD_DV,data=mtea))
summary(lm(P_bi_comb_FU~P_bi_comb+EM_AWARE+scale(S1AGE,scale=F)*scale(SEX,scale=F)+nonwhite+INC_NEEDS+ANY_ABUSE_CHILD_DV+Years,data=mtea))
lm.beta(lm(P_bi_comb_FU~P_bi_comb+EM_AWARE+scale(S1AGE,scale=F)*scale(SEX,scale=F)+nonwhite+INC_NEEDS+ANY_ABUSE_CHILD_DV+Years,data=mtea))

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


mtea$Age<-mtea$S1AGE-mean(mtea$S1AGE)
mtea$AgexSEX<-mtea$Age*mtea$SEX

mod1<-'
EM_AWARE~a1*ANY_ABUSE_CHILD_DV+a2*Age+SEX
P_bi_comb_FU~b*EM_AWARE+c1*ANY_ABUSE_CHILD_DV+P_bi_comb+c2*Age+SEX
ab1:=a1*b
ab2:=a2*b
ab:= a1*b + a2*b
tot1:= a1*b + c1
tot2:= a2*b + c2
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
SEX~1'
fit1<-lavaan(mod1,data=mtea,se="bootstrap",bootstrap=10000)
summary(fit1,standardized=T,ci=T)

mod2<-'
EM_AWARE~a1*ANY_ABUSE_CHILD_DV+a2*Age+SEX+a3*AgexSEX
P_bi_comb_FU~b*EM_AWARE+c1*ANY_ABUSE_CHILD_DV+P_bi_comb+c2*Age+SEX+c3*AgexSEX
ab1:=a1*b
ab2:=a2*b
ab3:=a3*b
ab:= a1*b + a2*b + a3*b
tot1:= a1*b + c1
tot2:= a2*b + c2
tot3:= a3*b + c3
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
fit2<-lavaan(mod2,data=mtea,se="bootstrap",bootstrap=10000)
summary(fit2,standardized=T,ci=T)
anova(fit1,fit2)
