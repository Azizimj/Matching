graphics.off()
rm(list=ls())

setwd("F:/term1/DR/R_DR1")
#hls=read.csv("OHPDP.csv", h=T,stringsAsFactors = T,as.is = T)
#hls=hls[,-c(53:64)] #53:65 is empty and there are 10922 obs
cm=read.csv("columns meaning.csv", h=T,stringsAsFactors = T)
#hls$DATE_O_EXIT_HOMELESSNESS=as.Date(hls$DATE_OF_EXIT_HOMELESSNESS,format = "%m/%d/%Y")
#hls$arrivedate=sample(seq(as.Date("0015-01-01"),
                          #as.Date("0017-01-01"),by="day"),10922,replace = T)
#for (i in 1:dim(hls)[1])
  #if(!is.na(hls[i,50]))
    #hls[i,56]=sample(seq(as.Date("0015-01-01"),as.Date(hls[i,50]),by="day"),1)
#nao=0
#nci=0
#for (i in 1:dim(hls)[1]){
  #if(!is.na(hls[i,50])){
    #nao=nao+1
    #if(hls[i,56] <= hls[i,50])
      #nci=nci+1
  #}
#}
#write.csv(hls,"F:/Acad/YouS30/term1/DR/R_DR1/hls.csv")
hls=read.csv("hls.csv",h=T,stringsAsFactors = T)
hls$DATE_OF_EXIT_HOMELESSNESS=as.Date(hls$DATE_OF_EXIT_HOMELESSNESS,format = "%Y-%m-%d")

#aa=subset(hls,hls$arrivedate <= hls$DATE_OF_EXIT_HOMELESSNESS)

hl=hls #hl is for model building
#hl=hl[,-which(names(hl) %in% c("YOUTH_ID","PET"))]
  #hl=subset(hl,STILL_HOUSED!="")
  #hl$STILL_HOUSED[hl$STILL_HOUSED=="y"]<-1
  #hl$STILL_HOUSED[hl$STILL_HOUSED=="n"]<-0
  #hl$STILL_HOUSED=as.numeric(hl$STILL_HOUSED)
#hl$STILL_HOUSED=droplevels(hl$STILL_HOUSED)
#levels(hl$STILL_HOUSED)=c(levels(hl$STILL_HOUSED),1,0)
#hl=subset(hl,STILL_HOUSED=="1" | STILL_HOUSED=="0")
#hl$STILL_HOUSED=droplevels(hl$STILL_HOUSED)
hl$TYPE_OF_EXIT=as.factor(hl$TYPE_OF_EXIT)
hl$RACE_ETHNICITY=as.factor(hl$RACE_ETHNICITY)
hl$SELF_IDENTIFIED_GENDER=as.factor(hl$SELF_IDENTIFIED_GENDER)
hl$IDENTIFIES_AS_LGBTQQI2=as.factor(hl$IDENTIFIES_AS_LGBTQQI2)
hl$DATE_OF_EXIT_HOMELESSNESS=as.Date(hl$DATE_OF_EXIT_HOMELESSNESS)
hl$X17_OR_YOUNGER=as.factor(hl$X17_OR_YOUNGER)
hl$WHERE_SLEEP=as.factor(hl$WHERE_SLEEP)
hl$GET_ANY_MONEY_LEGAL_OR_OTHER=as.factor(hl$GET_ANY_MONEY_LEGAL_OR_OTHER)
hl$DISABILITY=as.factor(hl$DISABILITY)
hl$TRIMORBIDITY=as.factor(hl$TRIMORBIDITY)
hl$arrivedate=as.Date(hl$arrivedate)
hl$BEEN_IN_JAIL=as.numeric(hl$BEEN_IN_JAIL)
hl$INCARCERATED_BEFORE_18_YRS=as.factor(hl$INCARCERATED_BEFORE_18_YRS)


#hp=hl #for subseting the programs to compaire with "evidence form past.pdf"
#hp=subset(hl,hl$TYPE_OF_EXIT=="PSH"|hl$TYPE_OF_EXIT=="RRH")
hl_new=hl
levels(hl_new$TYPE_OF_EXIT)=c(levels(hl_new$TYPE_OF_EXIT),c("non_housing"))
hl_new$TYPE_OF_EXIT[hl_new$TYPE_OF_EXIT=="Family"|hl_new$TYPE_OF_EXIT=="Self Resolve"]="non_housing"
hp=subset(hl_new,hl_new$TYPE_OF_EXIT=="PSH"|hl_new$TYPE_OF_EXIT=="RRH")
nhp=subset(hl_new,hl_new$TYPE_OF_EXIT=="non_housing")
hp_nhp=subset(hl_new,hl_new$TYPE_OF_EXIT=="PSH"|hl_new$TYPE_OF_EXIT=="RRH"|hl_new$TYPE_OF_EXIT=="non_housing")
#hp_nhp=subset(hl,hl$TYPE_OF_EXIT=="non_housing"|hl$TYPE_OF_EXIT=="PSH"|hl$TYPE_OF_EXIT=="RRH"|hl$TYPE_OF_EXIT=="SSVF")
#|hl$TYPE_OF_EXIT=="Self Resolve"|hl$TYPE_OF_EXIT=="Family")
#hp=subset(hl,hl$TYPE_OF_EXIT=="Family")
#hp=hp[,which(names(hl) %in% c("STILL_HOUSED","AGE","SELF_IDENTIFIED_GENDER","RACE_ETHNICITY"
  #                            ,"IDENTIFIES_AS_LGBTQQI2"))]

#hp$TYPE_OF_EXIT=hl$TYPE_OF_EXIT
# hp$TYPE_OF_EXIT=as.factor(hp$TYPE_OF_EXIT)
# nhp$TYPE_OF_EXIT=as.factor(nhp$TYPE_OF_EXIT)
#levels(hp$TYPE_OF_EXIT)=c(levels(hp$TYPE_OF_EXIT),c(1:11))
#j=1
#for (i in levels(hp$TYPE_OF_EXIT)){
  #j=as.factor(j)
  #hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT==as.character.factor(i)]<-j
 # j=j+1
  #print(i)
#}
#summary(hp$TYPE_OF_EXIT)
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="Boarding Home"]<-1
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="Deceasead"]<-2
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="Deceased"]<-3
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="Family"]<-4
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="Incarcerated"]<-5
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="Pending"]<-6
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="PSH"]<-7
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="RRH"]<-8
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="Self Resolve"]<-9
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="SSVF"]<-10
# hp$TYPE_OF_EXIT[hp$TYPE_OF_EXIT=="Unknown"]<-11
#hp$TYPE_OF_EXIT=droplevels(hp$TYPE_OF_EXIT)
#summary(hp$TYPE_OF_EXIT)

# hp$RACE_ETHNICITY=as.factor(hp$RACE_ETHNICITY)
# nhp$RACE_ETHNICITY=as.factor(nhp$RACE_ETHNICITY)
# levels(hp$RACE_ETHNICITY)=c(levels(hp$RACE_ETHNICITY),c(1:6))
# hp$RACE_ETHNICITY[hp$RACE_ETHNICITY=="Asian"]<-1
# hp$RACE_ETHNICITY[hp$RACE_ETHNICITY=="Black"]<-2
# hp$RACE_ETHNICITY[hp$RACE_ETHNICITY=="Hawaiian/Pacific Islander"]<-3
# hp$RACE_ETHNICITY[hp$RACE_ETHNICITY=="Hispanic"]<-4
# hp$RACE_ETHNICITY[hp$RACE_ETHNICITY=="Native American"]<-5
# hp$RACE_ETHNICITY[hp$RACE_ETHNICITY=="White"]<-6
#hl=subset(hl,STILL_HOUSED=="1" | STILL_HOUSED=="0")
#hp$RACE_ETHNICITY=droplevels(hp$RACE_ETHNICITY)

# hp$SELF_IDENTIFIED_GENDER=as.factor(hp$SELF_IDENTIFIED_GENDER)
# nhp$SELF_IDENTIFIED_GENDER=as.factor(nhp$SELF_IDENTIFIED_GENDER)
# levels(hp$SELF_IDENTIFIED_GENDER)=c(levels(hp$SELF_IDENTIFIED_GENDER),c(NA,1,0))
# hp$SELF_IDENTIFIED_GENDER[hp$SELF_IDENTIFIED_GENDER==""]<-NA
# hp$SELF_IDENTIFIED_GENDER[hp$SELF_IDENTIFIED_GENDER=="f"]<-1
# hp$SELF_IDENTIFIED_GENDER[hp$SELF_IDENTIFIED_GENDER=="m"]<-0
#hp$SELF_IDENTIFIED_GENDER=droplevels(hp$SELF_IDENTIFIED_GENDER)
 
# hp$IDENTIFIES_AS_LGBTQQI2=as.factor(hp$IDENTIFIES_AS_LGBTQQI2)
# nhp$IDENTIFIES_AS_LGBTQQI2=as.factor(nhp$IDENTIFIES_AS_LGBTQQI2)
# levels(hp$IDENTIFIES_AS_LGBTQQI2)=c(levels(hp$IDENTIFIES_AS_LGBTQQI2),c(0:1))
# hp$IDENTIFIES_AS_LGBTQQI2[hp$IDENTIFIES_AS_LGBTQQI2=="n"]<-0
# hp$IDENTIFIES_AS_LGBTQQI2[hp$IDENTIFIES_AS_LGBTQQI2=="y"]<-1
#hp$IDENTIFIES_AS_LGBTQQI2=droplevels(hp$IDENTIFIES_AS_LGBTQQI2)

# hp$DATE_OF_EXIT_HOMELESSNESS=as.Date(hp$DATE_OF_EXIT_HOMELESSNESS)
# nhp$DATE_OF_EXIT_HOMELESSNESS=as.Date(nhp$DATE_OF_EXIT_HOMELESSNESS)
# 
# hp$X17_OR_YOUNGER=as.factor(hp$X17_OR_YOUNGER)
# nhp$X17_OR_YOUNGER=as.factor(nhp$X17_OR_YOUNGER)
# #hp$X17_OR_YOUNGER=droplevels(hp$X17_OR_YOUNGER)
# 
# hp$WHERE_SLEEP=as.factor(hp$WHERE_SLEEP)
# nhp$WHERE_SLEEP=as.factor(nhp$WHERE_SLEEP)
# #hp$WHERE_SLEEP=droplevels(hp$WHERE_SLEEP)
# 
# hp$GET_ANY_MONEY_LEGAL_OR_OTHER=as.factor(hp$GET_ANY_MONEY_LEGAL_OR_OTHER)
# hp$DISABILITY=as.factor(hp$DISABILITY)
# hp$TYPE_OF_EXIT=as.factor(hp$TYPE_OF_EXIT)
# nhp$GET_ANY_MONEY_LEGAL_OR_OTHER=as.factor(nhp$GET_ANY_MONEY_LEGAL_OR_OTHER)
# nhp$DISABILITY=as.factor(nhp$DISABILITY)
# nhp$TYPE_OF_EXIT=as.factor(nhp$TYPE_OF_EXIT)
# hp$TRIMORBIDITY=as.factor(hp$TRIMORBIDITY)
# nhp$TRIMORBIDITY=as.factor(nhp$TRIMORBIDITY)


write.csv(hp,"F:/term1/DR/R_DR1/hp.csv")
write.csv(nhp,"F:/term1/DR/R_DR1/nhp.csv")
write.csv(hl,"F:/term1/DR/R_DR1/hl.csv")
##############################################################
##############################################################
#Sampling and trainning

#install.packages('caTools')
#library(caTools)
#split <- sample.split(hp$STILL_HOUSED, SplitRatio = 0.8)
# hptr <- subset(hp, split == TRUE)
# hptes <- subset(hp, split == FALSE)
smp_size <- floor(0.8 * nrow(hp))
smp_size_n <- floor(0.75 * nrow(nhp)) #non housing training sample size
## set the seed to make your partition reproductible
#set.seed(123)
train_ind <- sample(seq_len(nrow(hp)), size = smp_size)
hptr <- hp[train_ind, ]
hptes <- hp[-train_ind, ]
train_ind_n <- sample(seq_len(nrow(nhp)), size = smp_size_n)
nhptr <- nhp[train_ind_n, ]
nhptes <- nhp[-train_ind_n, ]


##CART
#install.packages('Metrics')
#install.packages('randomForest')
#install.packages('ggplot2')
#install.packages('ggthemes')
#install.packages('dplyr')
#install.packages("rpart.plot")
# library(rpart)
# library(rpart.plot)
# library('Metrics')
# library('randomForest')
# library('ggplot2')
# library('ggthemes')
# library('dplyr')
#fitt <- rpart(STILL_HOUSED ~ AGE+SELF_IDENTIFIED_GENDER+RACE_ETHNICITY+IDENTIFIES_AS_LGBTQQI2-
 #               DATE_OF_EXIT_HOMELESSNESS+TYPE_OF_EXIT+NUM_TIMES_HOMELESS_LAST_3_YEARS+DAYS_SINCE_PSH, hptr,
  #            control=rpart.control(minsplit=10, minbucket=2, cp=0.001))
#fitt <- rpart(STILL_HOUSED ~ AGE+SELF_IDENTIFIED_GENDER+IDENTIFIES_AS_LGBTQQI2, hptr,
   #           control=rpart.control(minsplit=10, minbucket=2, cp=0.001))

#train
fitt=glm(STILL_HOUSED ~ WHERE_SLEEP+X17_OR_YOUNGER+GET_ANY_MONEY_LEGAL_OR_OTHER+DISABILITY+TYPE_OF_EXIT+nst , hptr,family=binomial(link = "logit"))
fitt_n=glm(STILL_HOUSED ~ X17_OR_YOUNGER+TRIMORBIDITY+WHERE_SLEEP+nst+MENTAL_HEALTH_ISSUE , nhptr,family=binomial(link = "logit"))
#install.packages("logistf")
#library("logistf")
#fitt_n=logistf(data=nhptr, STILL_HOUSED ~ X17_OR_YOUNGER+TRIMORBIDITY+WHERE_SLEEP+nst+MENTAL_HEALTH_ISSUE+TYPE_OF_EXIT)
#plotcp(fitt)
#plot(fitt, uniform=TRUE)
#rpart.plot(fitt,type = 3,digits = 3,fallen.leaves = T)
summary(fitt)
summary(fitt_n)
#preds<-predict(fitt,hptes)
#auc(hptes$STILL_HOUSED,preds) # auc 7.18

hptes1=subset(hptes,hptes$STILL_HOUSED=="y");hptes0=subset(hptes,hptes$STILL_HOUSED=="n")
#hptr1=subset(hptr,hptr$STILL_HOUSED==1)
#hptr0=subset(hptr,hptr$STILL_HOUSED==0)
preds1<-predict(fitt,hptes1,type = "response");preds0<-predict(fitt,hptes0,type = "response")
preds1=data.frame(preds1);preds0=data.frame(preds0)
# tp=subset(preds1,preds1$X1>0.5)
# fp=subset(preds1,preds1$X1<0.5)
# tn=subset(preds0,preds0$X1<0.5)
# fn=subset(preds0,preds0$X1>0.5)
tp=subset(preds1,preds1>0.5);fp=subset(preds1,preds1<0.5);tn=subset(preds0,preds0<0.5);fn=subset(preds0,preds0>0.5)
tp=dim(tp)[1];fp=dim(fp)[1];tn=dim(tn)[1];fn=dim(fn)[1]
acu=(tp+tn)/(tp+tn+fp+fn)
acu #0.81

nhptes1=subset(nhptes,nhptes$STILL_HOUSED=="y");nhptes0=subset(nhptes,nhptes$STILL_HOUSED=="n")
#hptr1=subset(hptr,hptr$STILL_HOUSED==1)
#hptr0=subset(hptr,hptr$STILL_HOUSED==0)
preds1<-predict(fitt_n,nhptes1,type = "response");preds0<-predict(fitt_n,nhptes0,type = "response")
preds1=data.frame(preds1);preds0=data.frame(preds0)
# tp=subset(preds1,preds1$X1>0.5)
# fp=subset(preds1,preds1$X1<0.5)
# tn=subset(preds0,preds0$X1<0.5)
# fn=subset(preds0,preds0$X1>0.5)
tp=subset(preds1,preds1>0.5);fp=subset(preds1,preds1<0.5);tn=subset(preds0,preds0<0.5);fn=subset(preds0,preds0>0.5)
tp=dim(tp)[1];fp=dim(fp)[1];tn=dim(tn)[1];fn=dim(fn)[1]
acu=(tp+tn)/(tp+tn+fp+fn)
acu #0.77

#before continuing learn logostoc on whole data
fitt=glm(STILL_HOUSED ~ WHERE_SLEEP+X17_OR_YOUNGER+GET_ANY_MONEY_LEGAL_OR_OTHER+DISABILITY+TYPE_OF_EXIT+nst, hp,family=binomial(link = "logit"))
fitt_n=glm(STILL_HOUSED ~ X17_OR_YOUNGER+TRIMORBIDITY+WHERE_SLEEP+nst+MENTAL_HEALTH_ISSUE, nhp,family=binomial(link = "logit"))
summary(fitt)
#summary(fitt_n)
preds1<-predict(fitt,hptes1,type = "response");preds0<-predict(fitt,hptes0,type = "response")
preds1=data.frame(preds1);preds0=data.frame(preds0)
# tp=subset(preds1,preds1$X1>0.5)
# fp=subset(preds1,preds1$X1<0.5)
# tn=subset(preds0,preds0$X1<0.5)
# fn=subset(preds0,preds0$X1>0.5)
tp=subset(preds1,preds1>0.5);fp=subset(preds1,preds1<0.5);tn=subset(preds0,preds0<0.5);fn=subset(preds0,preds0>0.5)
tp=dim(tp)[1];fp=dim(fp)[1];tn=dim(tn)[1];fn=dim(fn)[1]
acu=(tp+tn)/(tp+tn+fp+fn)
acu #0.81

preds1<-predict(fitt_n,nhptes1,type = "response");preds0<-predict(fitt_n,nhptes0,type = "response")
preds1=data.frame(preds1);preds0=data.frame(preds0)
# tp=subset(preds1,preds1$X1>0.5)
# fp=subset(preds1,preds1$X1<0.5)
# tn=subset(preds0,preds0$X1<0.5)
# fn=subset(preds0,preds0$X1>0.5)
tp=subset(preds1,preds1>0.5);fp=subset(preds1,preds1<0.5);tn=subset(preds0,preds0<0.5);fn=subset(preds0,preds0>0.5)
tp=dim(tp)[1];fp=dim(fp)[1];tn=dim(tn)[1];fn=dim(fn)[1];acu=(tp+tn)/(tp+tn+fp+fn)
acu #0.77

#probability of success plot
# hp_plot=hp
# hp_plot$s[hp_plot$STILL_HOUSED=="y"]=1
# hp_plot$s[hp_plot$STILL_HOUSED=="n"]=0
# plot(hp_plot$nst,hp_plot$s)
# 
# nst_plot=seq(0,14,14/(dim(hp)[1]-1))
# hp_plot$nst=nst_plot
# 
# lines(nst_plot,predict(fitt,hp_plot,type = "response"))

################ P_G_nh learn
#probability of taking a non_housing type of exit (solving by him/herself)
hl_nh_other=subset(hl,hl$TYPE_OF_EXIT!="RRH" & hl$TYPE_OF_EXIT!="PSH" & hl$TYPE_OF_EXIT!="SSVF" & hl$TYPE_OF_EXIT!="Unknown")
hl_nh_other$g_nh[hl_nh_other$TYPE_OF_EXIT=="Family"|hl_nh_other$TYPE_OF_EXIT=="Self Resolve"]=1
hl_nh_other$g_nh[is.na(hl_nh_other$g_nh)]=0

smp_size_nh_other <- floor(0.8 * nrow(hl_nh_other))
train_ind_nh_other <- sample(seq_len(nrow(hl_nh_other)), size = smp_size_nh_other)
hl_nh_other_tr <- hl_nh_other[train_ind_nh_other, ]
hl_nh_other_tes <- hl_nh_other[-train_ind_nh_other, ]

#fitt_g_n=glm(g_nh ~ X17_OR_YOUNGER+TRIMORBIDITY+WHERE_SLEEP+nst+MENTAL_HEALTH_ISSUE, hl_nh_other_tr, family=binomial(link = "logit"))
fitt_g_n=glm(g_nh ~ nst+INCARCERATED_BEFORE_18_YRS, hl_nh_other_tr, family=binomial(link = "logit"))
#summary(fitt_g_n)

hl_nh_other_tes1=subset(hl_nh_other_tes, hl_nh_other_tes$g_nh=="1")
hl_nh_other_tes0=subset(hl_nh_other_tes, hl_nh_other_tes$g_nh=="0")
preds1<-predict(fitt_g_n,hl_nh_other_tes1,type = "response")
preds0<-predict(fitt_g_n,hl_nh_other_tes0,type = "response")
preds1=data.frame(preds1)
preds0=data.frame(preds0)
tp=subset(preds1,preds1>0.5);fp=subset(preds1,preds1<0.5);tn=subset(preds0,preds0<0.5);fn=subset(preds0,preds0>0.5)
tp=dim(tp)[1];fp=dim(fp)[1];tn=dim(tn)[1];fn=dim(fn)[1];acu=(tp+tn)/(tp+tn+fp+fn)
acu  #o.8

fitt_g_n=glm(g_nh ~ X17_OR_YOUNGER+TRIMORBIDITY+WHERE_SLEEP+nst+MENTAL_HEALTH_ISSUE, hl_nh_other, family=binomial(link = "logit"))

preds1<-predict(fitt_g_n,hl_nh_other_tes1,type = "response")
preds0<-predict(fitt_g_n,hl_nh_other_tes0,type = "response")
preds1=data.frame(preds1)
preds0=data.frame(preds0)
tp=subset(preds1,preds1>0.5);fp=subset(preds1,preds1<0.5);tn=subset(preds0,preds0<0.5);fn=subset(preds0,preds0>0.5)
tp=dim(tp)[1];fp=dim(fp)[1];tn=dim(tn)[1];fn=dim(fn)[1];acu=(tp+tn)/(tp+tn+fp+fn)
acu  #o.8

################################################################################
#data of youth and house for model
# ny=500
# nh=500
# ny_out=500
# nh_out=500
sd=hl[sort.list(hl$arrivedate,decreasing = F),]
         #-which(names(hp) %in% c("PET"))] #sorted data 
# boxplot(sd$arrivedate)
# summary(sd$arrivedate)
# summary(sd$DATE_OF_EXIT_HOMELESSNESS)

ys=subset(sd,sd$arrivedate>=as.Date("0015-09-1")
         & sd$arrivedate<=as.Date("0015-10-30"))
         #|sd$arrivedate<as.Date("0015-09-1") & is.na(sd$DATE_OF_EXIT_HOMELESSNESS)) #80% of youth
#sdsam=sample(dim(sd)[1],ny,replace = F)
#ys=sd[sdsam,]
#ys=ys[sort.list(ys$arrivedate, decreasing=F),]

#encoding
ys_encoded=ys
#ys_encoded=ys[,which(names(ys) %in% c("X17_OR_YOUNGER","WHERE_SLEEP","GET_ANY_MONEY_LEGAL_OR_OTHER","DISABILITY","arrivedate"))]
ys_encoded$X17_OR_YOUNGER1[ys_encoded$X17_OR_YOUNGER=="1"]=1
ys_encoded$X17_OR_YOUNGER1[ys_encoded$X17_OR_YOUNGER!=1]=0
ys_encoded$WHERE_SLEEPcouch[ys_encoded$WHERE_SLEEP=="couch"]=1
ys_encoded$WHERE_SLEEPcouch[ys_encoded$WHERE_SLEEP!="couch"]=0
ys_encoded$WHERE_SLEEPoutdoors[ys_encoded$WHERE_SLEEP=="outdoors"]=1
ys_encoded$WHERE_SLEEPoutdoors[ys_encoded$WHERE_SLEEP!="outdoors"]=0
ys_encoded$WHERE_SLEEPshelter[ys_encoded$WHERE_SLEEP=="shelter"]=1
ys_encoded$WHERE_SLEEPshelter[ys_encoded$WHERE_SLEEP!="shelter"]=0
ys_encoded$WHERE_SLEEPcar[ys_encoded$WHERE_SLEEP=="car"]=1
ys_encoded$WHERE_SLEEPcar[ys_encoded$WHERE_SLEEP!="car"]=0
ys_encoded$WHERE_SLEEPtransitional[ys_encoded$WHERE_SLEEP=="transitional"]=1
ys_encoded$WHERE_SLEEPtransitional[ys_encoded$WHERE_SLEEP!="transitional"]=0
ys_encoded$GET_ANY_MONEY_LEGAL_OR_OTHERy[ys_encoded$GET_ANY_MONEY_LEGAL_OR_OTHER=="y"]=1
ys_encoded$GET_ANY_MONEY_LEGAL_OR_OTHERy[ys_encoded$GET_ANY_MONEY_LEGAL_OR_OTHER=="n"]=0
ys_encoded$DISABILITYy[ys_encoded$DISABILITY=="y"]=1
ys_encoded$DISABILITYy[ys_encoded$DISABILITY=="n"]=0
ys_encoded$TRIMORBIDITY_1[ys_encoded$TRIMORBIDITY=="1"]=1
ys_encoded$TRIMORBIDITY_1[ys_encoded$DISABILITY!="1"]=0
#ys_encoded=ys_encoded[,-which(names(ys_encoded) %in% c("X17_OR_YOUNGER","WHERE_SLEEP","GET_ANY_MONEY_LEGAL_OR_OTHER","DISABILITY"))]
write.csv(ys_encoded,"F:/term1/DR/R_DR1/ys.csv")

sd=hl[sort.list(hl$arrivedate,decreasing = F),]
ysout=subset(sd,sd$arrivedate>=as.Date("0016-08-01"))
#ysout=sd[-sdsam,]
# ys_out_sam=sample(dim(ysout)[1],ny_out,replace = F)
# ysout=ysout[ys_out_sam,]
ysout=ysout[sort.list(ysout$arrivedate, decreasing=F),]
#encoding
yss=ysout
#yss=ysout[,which(names(ysout) %in% c("X17_OR_YOUNGER","WHERE_SLEEP","GET_ANY_MONEY_LEGAL_OR_OTHER","DISABILITY","arrivedate"))]
yss$X17_OR_YOUNGER1[yss$X17_OR_YOUNGER=="1"]=1
yss$X17_OR_YOUNGER1[yss$X17_OR_YOUNGER!=1]=0
yss$WHERE_SLEEPcouch[yss$WHERE_SLEEP=="couch"]=1
yss$WHERE_SLEEPcouch[yss$WHERE_SLEEP!="couch"]=0
yss$WHERE_SLEEPoutdoors[yss$WHERE_SLEEP=="outdoors"]=1
yss$WHERE_SLEEPoutdoors[yss$WHERE_SLEEP!="outdoors"]=0
yss$WHERE_SLEEPshelter[yss$WHERE_SLEEP=="shelter"]=1
yss$WHERE_SLEEPshelter[yss$WHERE_SLEEP!="shelter"]=0
yss$WHERE_SLEEPcar[yss$WHERE_SLEEP=="car"]=1
yss$WHERE_SLEEPcar[yss$WHERE_SLEEP!="car"]=0
yss$WHERE_SLEEPtransitional[yss$WHERE_SLEEP=="transitional"]=1
yss$WHERE_SLEEPtransitional[yss$WHERE_SLEEP!="transitional"]=0
yss$GET_ANY_MONEY_LEGAL_OR_OTHERy[yss$GET_ANY_MONEY_LEGAL_OR_OTHER=="y"]=1
yss$GET_ANY_MONEY_LEGAL_OR_OTHERy[yss$GET_ANY_MONEY_LEGAL_OR_OTHER=="n"]=0
yss$DISABILITYy[yss$DISABILITY=="y"]=1
yss$DISABILITYy[yss$DISABILITY=="n"]=0
yss$TRIMORBIDITY_1[yss$TRIMORBIDITY=="1"]=1
yss$TRIMORBIDITY_1[yss$DISABILITY!="1"]=0
#yss=yss[,-which(names(yss) %in% c("X17_OR_YOUNGER","WHERE_SLEEP","GET_ANY_MONEY_LEGAL_OR_OTHER","DISABILITY"))]
write.csv(yss,"F:/term1/DR/R_DR1/ys_out.csv")

ny=dim(ys)[1] # # of youth in sample
ny_out=dim(ysout)[1] # # of youth in out of sample (test set)
#summary(is.na(ys$DATE_OF_EXIT_HOMELESSNESS))


####houses set
#nh=floor(runif(1, min=1 , max = 1.2*dim(ys)[1])) # # of housings in the exp period
#hs=sample(1:3,nh,replace = T) #house set (3 type of housing= PSH/RRH/SSVF)
#hs[hs==1]<-"PSH"; hs[hs==2]<-"RRH";hs[hs==3]<-"SSVF"
#hs=as.data.frame(hs)
#colnames(hs)=c("ht")
sd=hp[sort.list(hp$DATE_OF_EXIT_HOMELESSNESS, decreasing=F),]
hs=subset(sd,sd$DATE_OF_EXIT_HOMELESSNESS>=as.Date("0015-09-1")
        & sd$DATE_OF_EXIT_HOMELESSNESS<=as.Date("0015-11-30"))  #this sd is just housing programm (hp)
# sdsam2=sample(dim(sd)[1],nh,replace = F)
# hs=sd[sdsam2,]
hs=hs[sort.list(hs$DATE_OF_EXIT_HOMELESSNESS, decreasing=F),]
#encoding
hs_encoded=hs[,which(names(hs) %in% c("TYPE_OF_EXIT","DATE_OF_EXIT_HOMELESSNESS"))]
hs_encoded$TYPE_OF_EXIT_RRH[hs_encoded$TYPE_OF_EXIT=="RRH"]=1
hs_encoded$TYPE_OF_EXIT_RRH[hs_encoded$TYPE_OF_EXIT!="RRH"]=0
hs_encoded$TYPE_OF_EXIT_PSH[hs_encoded$TYPE_OF_EXIT=="PSH"]=1
hs_encoded$TYPE_OF_EXIT_PSH[hs_encoded$TYPE_OF_EXIT!="PSH"]=0
#hs_encoded$TYPE_OF_EXIT_SSVF[hs_encoded$TYPE_OF_EXIT=="SSVF"]=1
#hs_encoded$TYPE_OF_EXIT_SSVF[hs_encoded$TYPE_OF_EXIT!="SSVF"]=0
# hs_encoded$TYPE_OF_EXIT_Family[hs_encoded$TYPE_OF_EXIT=="Family"]=1
# hs_encoded$TYPE_OF_EXIT_Family[hs_encoded$TYPE_OF_EXIT!="Family"]=0
# hs_encoded$TYPE_OF_EXIT_Self_Resolve[hs_encoded$TYPE_OF_EXIT=="Self Resolve"]=1
# hs_encoded$TYPE_OF_EXIT_Self_Resolve[hs_encoded$TYPE_OF_EXIT!="Self_Resolve"]=0
#hs_encoded=hs_encoded[,-which(names(hs_encoded) %in% c("TYPE_OF_EXIT"))]
#hs_encoded=hs_encoded[,which(names(hs_encoded) %in% c("TYPE_OF_EXIT","DATE_OF_EXIT_HOMELESSNESS"))]
write.csv(hs_encoded,"F:/term1/DR/R_DR1/hs.csv")

# hsout=sd[-sdsam2,]
# hs_out_sam=sample(dim(hsout)[1],nh_out,replace = F)
# hsout=hsout[hs_out_sam,]
sd=hp[sort.list(hp$DATE_OF_EXIT_HOMELESSNESS, decreasing=F),]
hsout=subset(sd,sd$DATE_OF_EXIT_HOMELESSNESS>=as.Date("0016-9-1"))
               #&sd$DATE_OF_EXIT_HOMELESSNESS<=as.Date("0016-10-1"))
hsout=hsout[sort.list(hsout$DATE_OF_EXIT_HOMELESSNESS, decreasing=F),]
#encoding
hsc=hsout[,which(names(hsout) %in% c("TYPE_OF_EXIT","DATE_OF_EXIT_HOMELESSNESS"))]
hsc$TYPE_OF_EXIT_RRH[hsc$TYPE_OF_EXIT=="RRH"]=1
hsc$TYPE_OF_EXIT_RRH[hsc$TYPE_OF_EXIT!="RRH"]=0
hsc$TYPE_OF_EXIT_PSH[hsc$TYPE_OF_EXIT=="PSH"]=1
hsc$TYPE_OF_EXIT_PSH[hsc$TYPE_OF_EXIT!="PSH"]=0
#hsc$TYPE_OF_EXIT_SSVF[hsc$TYPE_OF_EXIT=="SSVF"]=1
#hsc$TYPE_OF_EXIT_SSVF[hsc$TYPE_OF_EXIT!="SSVF"]=0
# hsc$TYPE_OF_EXIT_Family[hsc$TYPE_OF_EXIT=="Family"]=1
# hsc$TYPE_OF_EXIT_Family[hsc$TYPE_OF_EXIT!="Family"]=0
# hsc$TYPE_OF_EXIT_Self_Resolve[hsc$TYPE_OF_EXIT=="Self Resolve"]=1
# hsc$TYPE_OF_EXIT_Self_Resolve[hsc$TYPE_OF_EXIT!="Self_Resolve"]=0
#hsoutc=hsoutc[,-which(names(hsoutc) %in% c("TYPE_OF_EXIT"))]
#hsoutc=hsout[,which(names(hsout) %in% c("TYPE_OF_EXIT","DATE_OF_EXIT_HOMELESSNESS"))]
write.csv(hsc,"F:/term1/DR/R_DR1/hs_out.csv")

nh=dim(hs)[1]
nh_out=dim(hsout)[1]

##################################
x=as.data.frame(ys[rep(seq_len(nrow(ys)), each=nh),])
hss=as.data.frame(hs[rep(seq_len(nrow(hs)),time=ny),]) # x is like y1[h1...hnh] ... yny[h1...hnh]
x$TYPE_OF_EXIT=hss$TYPE_OF_EXIT
x$DATE_OF_EXIT_HOMELESSNESS=hss$DATE_OF_EXIT_HOMELESSNESS

#objective fumction coefficients   
objc=as.matrix(predict(fitt,x,type = "response"))
p=matrix(objc[,1],ncol=nh,nrow = ny,byrow = T)
write.csv(p,"F:/term1/DR/R_DR1/p.csv")

x_non_housing=ys
x_non_housing$TYPE_OF_EXIT="non_housing"
objc_non_housing=as.matrix(predict(fitt_n,x_non_housing,type = "response"))
p_non_housing=matrix(objc_non_housing[,1],ncol=1,nrow = ny)
write.csv(p_non_housing,"F:/term1/DR/R_DR1/p_nh.csv")

x_go_non_housing=ys
objc_go_non_housing=as.matrix(predict(fitt_g_n,x_go_non_housing,type = "response"))
p_go_non_housing=matrix(objc_go_non_housing[,1],ncol=1,nrow = ny)
write.csv(p_go_non_housing,"F:/term1/DR/R_DR1/p_go_nh.csv") 


#########################
#     HPC
x_out=as.data.frame(ysout[rep(seq_len(nrow(ysout)), each=nh_out),])
hss_out=as.data.frame(hsout[rep(seq_len(nrow(hsout)),time=ny_out),])
x_out$TYPE_OF_EXIT=hss_out$TYPE_OF_EXIT
x_out$DATE_OF_EXIT_HOMELESSNESS=hss_out$DATE_OF_EXIT_HOMELESSNESS

objc_out=as.matrix(predict(fitt,x_out,type = "response"))
p_out=matrix(objc_out[,1],ncol=nh_out,nrow = ny_out,byrow = T)
write.csv(p_out,"F:/term1/DR/R_DR1/p_out.csv") 

x_non_housing_out=ysout
x_non_housing_out$TYPE_OF_EXIT="non_housing"
objc_non_housing_out=as.matrix(predict(fitt_n,x_non_housing_out,type = "response"))
p_non_housing_out=matrix(objc_non_housing_out[,1],ncol=1,nrow = ny_out)
write.csv(p_non_housing_out,"F:/term1/DR/R_DR1/p_nh_out.csv") #HPC

x_go_non_housing_out=ysout
objc_go_non_housing_out=as.matrix(predict(fitt_g_n,x_go_non_housing_out,type = "response"))
p_go_non_housing_out=matrix(objc_go_non_housing_out[,1],ncol=1,nrow = ny_out)
write.csv(p_go_non_housing_out,"F:/term1/DR/R_DR1/p_go_nh_out.csv") #no need if labtop

## END of this part

