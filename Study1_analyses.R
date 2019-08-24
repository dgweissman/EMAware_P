library(effsize)
library(ggplot2)

#Read in data and extracted p factor scores
ftt<-read.csv("Study1_data.csv")
fscores<-read.table("ftp_nopt.txt")
colnames(fscores)[9]<-"ID"
colnames(fscores)[6]<-"P"
ft<-merge(ftt,fscores[,c(9,6)],by="ID",all.x=T)
colnames(ft)[2]<-"Sex"

#Descriptive statistics
summary(ft)
sapply(ft[c(3:11)],sd,na.rm=T)
cohen.d(ft$Age~ft$Sex)
t.test(ft$Age~ft$Sex)
t.test(ft$Alexithymia~ft$Sex)
cohen.d(ft$Alexithymia~ft$Sex)
cohen.d(ft$SCARED_TOT~ft$Sex)
t.test(ft$SCARED_TOT~ft$Sex)
cohen.d(ft$CDI_TOT~ft$Sex)
cohen.d(ft$PTSD_SEV_COMBINED~ft$Sex)
cohen.d(ft$Aggressive_Total~ft$Sex)
cohen.d(ft$Attention_Total~ft$Sex)
cohen.d(ft$Rule_Total~ft$Sex)

#Correlations
cor(ft[,3:11],use="pairwise.complete.obs")
cor.test(ft$Age,ft$Rule_Total,use="complete.obs")
cor.test(ft$Alexithymia,ft$Age)
cor.test(ft$CDI_TOT,ft$Age)
cor.test(ft$SCARED_TOT,ft$Aggressive_Total)
cor.test(ft$SCARED_TOT,ft$Rule_Total)
cor.test(ft$Attention_Total,ft$PTSD_SEV_COMBINED)

#Interaction of age and sex in relation to emotional awareness
summary(lm(Alexithymia~Sex*Age,data=ft))
scatter_theme<- theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),axis.text = element_text(size=18), legend.title = element_text(size = 18), legend.text = element_text(size = 18))
qplot(x = Age, y = (Alexithymia-10), data = ft[-which(is.na(ft$Alexithymia)),], color = Sex) +
  geom_smooth(method = "lm") + ylab("Low Emotional Awareness") + scatter_theme + ylim(c(0,40)) + xlim(c(7.5,18)) + scale_x_continuous(breaks=c(9,12,15,18))

#Interaction of age and sex in relation to P Factor
cor.test(ft$P,ft$Age)
t.test(ft$P~ft$Sex)
summary(lm(P~Sex*scale(Age,scale=F),data=ft))
summary(lm(P~Sex+Sex:scale(Age,scale=F),data=ft))
scatter_theme<- theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),axis.text = element_text(size=18), legend.title = element_text(size = 18), legend.text = element_text(size = 18))
qplot(x = Age, y = P, data = ft, color = Sex) +
  geom_smooth(method = "lm") + ylab("P-Factor") + scatter_theme


#P Factor and Emotional Awareness
cor.test(ft$Alexithymia,ft$P)
summary(lm(P~Sex*Age+INC_NEEDS+nonwhite+Alexithymia,data=ft))
scatter_theme<- theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28),axis.text = element_text(size=24))
ggplot(ft[-which(is.na(ft$Alexithymia)),c(4,14)], aes(x=Alexithymia-10, y=P)) + geom_point(shape=1,size=3) +  geom_smooth(method=lm) + scatter_theme + ylab("P-factor") + xlab("Low Emotional Awareness")+xlim(0,40)+ylim(-2,2)

