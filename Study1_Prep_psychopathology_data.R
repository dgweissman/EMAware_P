ftt<-read.csv("Study1_data.csv")
#Create datafile for symptom decile scores
ftp<-NULL
ftp$ID<-ftt$ID

ftp$cdi<-rep(NA,times=length(ftt$CDI_TOT))
ftp$scared<-rep(NA,times=length(ftt$SCARED_TOT))
ftp$ptsd<-rep(NA,times=length(ftt$PTSD_SEV_COMBINED))
ftp$att<-rep(NA,times=length(ftt$Attention_Total))
ftp$rule<-rep(NA,times=length(ftt$Rule_Total))
ftp$agg<-rep(NA,times=length(ftt$Aggressive_Total))

#Set decile cutoffs
cq<-quantile(ftt$CDI_TOT,probs=seq(0,1,.1),na.rm=T)
sq<-quantile(ftt$SCARED_TOT,probs=seq(0,1,.1),na.rm=T)
pq<-quantile(ftt$PTSD_SEV_COMBINED,probs=seq(0,1,.1),na.rm=T)
aq<-quantile(ftt$Attention_Total,probs=seq(0,1,.1),na.rm=T)
rq<-quantile(ftt$Rule_Total,probs=seq(0,1,.1),na.rm=T)
gq<-quantile(ftt$Aggressive_Total,probs=seq(0,1,.1),na.rm=T)

#Enter decile scores
ftp$cdi[which(ftt$CDI_TOT==cq[1])]<-1
ftp$scared[which(ftt$SCARED_TOT==sq[1])]<-1
ftp$ptsd[which(ftt$PTSD_SEV_COMBINED==pq[1])]<-1
ftp$att[which(ftt$Attention_Total==aq[1])]<-1
ftp$rule[which(ftt$Rule_Total==rq[1])]<-1
ftp$agg[which(ftt$Aggressive_Total==gq[1])]<-1
for (x in 1:10){
  ftp$cdi[which(ftt$CDI_TOT>cq[x])]<-x
  ftp$scared[which(ftt$SCARED_TOT>sq[x])]<-x
  ftp$ptsd[which(ftt$PTSD_SEV_COMBINED>pq[x])]<-x
  ftp$att[which(ftt$Attention_Total>aq[x])]<-x
  ftp$rule[which(ftt$Rule_Total>rq[x])]<-x
  ftp$agg[which(ftt$Aggressive_Total>gq[x])]<-x
}

ftp<-data.frame(ftp)
write.table(ftp,"ftp10.csv",row.names=F,col.names=F,sep=",",na=".")

