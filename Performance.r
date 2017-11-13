rm(list=ls())
setwd("F:/term1/DR/R_DR1")

######################################################
##if use hpc, use this and uncomment #HPC
ysp=read.csv("ys_out.csv",as.is = T) # for OUT OF SAMPLE
hsp=read.csv("hs_out.csv",as.is = T) # for OUT OF SAMPLE

ysp=read.csv("ys.csv",as.is = T)# for IN SAMPLE
hsp=read.csv("hs.csv",as.is = T)# for IN SAMPLE

ysp$arrivedate=as.Date(ysp$arrivedate,format = "%Y-%m-%d")
ysp$TYPE_OF_EXIT=as.factor(ysp$TYPE_OF_EXIT)
ysp$RACE_ETHNICITY=as.factor(ysp$RACE_ETHNICITY)
ysp$SELF_IDENTIFIED_GENDER=as.factor(ysp$SELF_IDENTIFIED_GENDER)
ysp$IDENTIFIES_AS_LGBTQQI2=as.factor(ysp$IDENTIFIES_AS_LGBTQQI2)
ysp$DATE_OF_EXIT_HOMELESSNESS=as.Date(ysp$DATE_OF_EXIT_HOMELESSNESS)
ysp$X17_OR_YOUNGER=as.factor(ysp$X17_OR_YOUNGER)
ysp$WHERE_SLEEP=as.factor(ysp$WHERE_SLEEP)
ysp$GET_ANY_MONEY_LEGAL_OR_OTHER=as.factor(ysp$GET_ANY_MONEY_LEGAL_OR_OTHER)
ysp$DISABILITY=as.factor(ysp$DISABILITY)
ysp$TRIMORBIDITY=as.factor(ysp$TRIMORBIDITY)
ysp$arrivedate=as.Date(ysp$arrivedate)
hsp=read.csv("hs_out.csv")
hsp$DATE_OF_EXIT_HOMELESSNESS=as.Date(hsp$DATE_OF_EXIT_HOMELESSNESS,format = "%Y-%m-%d")
# for in sample
p=read.csv("p.csv")
p_go_nh=read.csv("p_go_nh.csv")
p_nh=read.csv("p_nh.csv")
# for out of sample
p=read.csv("p_out.csv")
p_go_nh=read.csv("p_go_nh_out.csv")
p_nh=read.csv("p_nh_out.csv")

p=p[-1];p_go_nh=p_go_nh[-1];p_nh=p_nh[-1]
###############################################################

###### if use labtop use this #Labtop
# for OUT of SAMPLE
ysp=yss #yss is coded ysout
hsp=hsc #hsc is coded hs out
nhp=dim(hsp)[1]
nyp=dim(ysp)[1]

##for in sample
ysp=ys_encoded
hsp=hs_encoded
nhp=dim(hsp)[1]
nyp=dim(ysp)[1]

###number of youth in buckets
ysp_b1=subset(ysp,ysp$nst>=0 & ysp$nst<=3);
ysp_b2=subset(ysp,ysp$nst>=4 & ysp$nst<=7);
ysp_b3=subset(ysp,ysp$nst>=8);
ny_b=rep(0,3);ny_b[1]=dim(ysp_b1)[1];ny_b[2]=dim(ysp_b2)[1];ny_b[3]=dim(ysp_b3)[1];
ny_b  #number of youth in buckets

####################################################
#######same setting for comparison of nik and phebe
set.seed(11)
rho=runif(nyp,0,1)#same for both methods

############################################################
############################################################
##performance of phebe model
#phebe model's beta
beta_y=matrix(c(0.02, 3.41061e-13, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0),nrow=1,ncol=8)
beta_h=0

gain_ph=0  #gain is performance or acumulate sum of probabilities of success (h and nh)
yath_ph=rep(0,nhp)
tpriority_ph=-1000
new_priority_ph=0
x_ph=matrix(rep(0,nhp*nyp),ncol = nhp, nrow = nyp)
rho_temp_ph=0
num_of_allocation_ph=0
p_h_b_ph=rep(0,3) #probability of success of buckets (housing porgramm success)
n_a_b_ph=rep(0,3) #number of allocation per bucket
n_rrh_a_b_ph=rep(0,3) #number of rrh allocation per bucket
n_psh_a_b_ph=rep(0,3) #number of psh allocation per bucket
#ph model performance 
for (h in 1:nhp){
  for (y in 1:nyp){
    if ( sum(x_ph[y,])==0 ){
      if (ysp[["arrivedate"]][y] <= hsp[["DATE_OF_EXIT_HOMELESSNESS"]][h]){
          new_priority_ph= (beta_y[1]*ysp[["X17_OR_YOUNGER1"]][y]+beta_y[2]*ysp[["WHERE_SLEEPcouch"]][y]+beta_y[3]*ysp[["WHERE_SLEEPoutdoors"]][y]+beta_y[4]*ysp[["WHERE_SLEEPshelter"]][y]+beta_y[5]*ysp[["WHERE_SLEEPtransitional"]][y]+beta_y[6]*ysp[["GET_ANY_MONEY_LEGAL_OR_OTHERy"]][y]+beta_y[7]*ysp[["DISABILITYy"]][y])
          
          if ( tpriority_ph < new_priority_ph){tpriority_ph = new_priority_ph;yath_ph[h] = y;rho_temp_ph=rho[y]}
          if ( tpriority_ph == new_priority_ph && rho[y]>rho_temp_ph ){tpriority_ph = new_priority_ph;yath_ph[h] = y;rho_temp_ph=rho[y]}
      }else{break}
    }
  }
  if (yath_ph[h]!=0){
    x_ph[yath_ph[h],h]=1
    # Labtop
    data_point=ysp[yath_ph[h],];data_point$TYPE_OF_EXIT=hsp[["TYPE_OF_EXIT"]][h]
    p_temp=predict(fitt,data_point,type = "response")
    # HPC
    #p_temp = p[yath_ph[h],h] #HPC
    
    gain_ph=gain_ph+p_temp
    #####acumulate p of success in buckets
    if(ysp[["nst"]][yath_ph[h]]>=0 && ysp[["nst"]][yath_ph[h]]<=3){
      p_h_b_ph[1]=p_h_b_ph[1]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b_ph[1]=n_rrh_a_b_ph[1]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b_ph[1]=n_psh_a_b_ph[1]+1}
      n_a_b_ph[1]=n_a_b_ph[1]+1}
    if(ysp[["nst"]][yath_ph[h]]>=4 && ysp[["nst"]][yath_ph[h]]<=7){
      p_h_b_ph[2]=p_h_b_ph[2]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b_ph[2]=n_rrh_a_b_ph[2]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b_ph[2]=n_psh_a_b_ph[2]+1}
      n_a_b_ph[2]=n_a_b_ph[2]+1}
    if(ysp[["nst"]][yath_ph[h]]>=8){
      p_h_b_ph[3]=p_h_b_ph[3]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b_ph[3]=n_rrh_a_b_ph[3]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b_ph[3]=n_psh_a_b_ph[3]+1}
      n_a_b_ph[3]=n_a_b_ph[3]+1}
  
    num_of_allocation_ph=num_of_allocation_ph+1
  }
  tpriority_ph=-1000
}
gain_ph_save=gain_ph
gain_ph=gain_ph_save

###############
n_a_b_ph #number house allocation in buckets
n_rrh_a_b_ph #number rrh house allocation in buckets
n_psh_a_b_ph #number psh house allocation in buckets
n_a_b_ph/ny_b  #percent of allocation nik
n_a_b_ph/sum(n_a_b_ph) #overall percent of allocation
n_rrh_a_b_ph/sum(n_rrh_a_b_ph) #overall percent of rrh allocation
n_psh_a_b_ph/sum(n_psh_a_b_ph) #overall percent of psh allocation
###########
#Phebe non housing
p_nh_b_ph=rep(0,3)#probability of success of buckets (non housing success) out of sample
n_na_b_ph=rep(0,3) #number of not allocated per bucket
for (y in 1:nyp){
  if (sum(x_ph[y,])==0){
    #labtop
    data_point=ysp[y,]
    data_point$TYPE_OF_EXIT="non_housing"
    p_temp=predict(fitt_n,data_point,type ="response")
    p_go_nh=predict(fitt_g_n,data_point,type ="response") 
    #HPC
    #p_temp = p_nh[yath_nik[h],h]
    #p_go_nh = p_go_nh[y]
    
    gain_ph=gain_ph+p_go_nh*p_temp
    
    if(ysp[["nst"]][y]>=0 && ysp[["nst"]][y]<=3){
      p_nh_b_ph[1]=p_nh_b_ph[1]+p_go_nh*p_temp;n_na_b_ph[1]=n_na_b_ph[1]+1}
    if(ysp[["nst"]][y]>=4 && ysp[["nst"]][y]<=7){
      p_nh_b_ph[2]=p_nh_b_ph[2]+p_go_nh*p_temp;n_na_b_ph[2]=n_na_b_ph[2]+1}
    if(ysp[["nst"]][y]>=8){
      p_nh_b_ph[3]=p_nh_b_ph[3]+p_go_nh*p_temp;n_na_b_ph[3]=n_na_b_ph[3]+1}
  }
}
gain_ph=gain_ph/dim(ysp)[1];
gain_ph
#p of success over buketes for fairness comparison
#p_h_b/ny_b
#p_nh_b/ny_b
p_b_ph=(p_h_b_ph+p_nh_b_ph)/ny_b
p_b_ph
var(p_b_ph)


##################################################################
##################################################################
#########performance of NIK
## learned beta of nikolaos approach
beta_nikos=read.csv("beta_nikos.csv") #pay attention to dim
beta_nikos

#performance of nik in or out of sample for housing
gain_nik=0
yath_nik=rep(0,nhp)
tpriority_nik=-1000
new_priority_nik=0
x_nik=matrix(rep(0,nhp*nyp),ncol = nhp, nrow = nyp)
rho_temp_nik=0
num_of_allocation_nik=0
p_h_b=rep(0,3) #probability of success of buckets (housing porgramm success)
n_a_b=rep(0,3) #number of allocation per bucket
n_rrh_a_b=rep(0,3) #number of rrh allocation per bucket
n_psh_a_b=rep(0,3) #number of psh allocation per bucket
for (h in 1:nhp){
  for (y in 1:nyp){
    if (sum(x_nik[y,])==0){
      if (ysp[["arrivedate"]][y]<=hsp[["DATE_OF_EXIT_HOMELESSNESS"]][h]){
        
        #+beta_nikos$lm1.coefficients[beta_nikos$X=="WHERE_SLEEPcouch"]*ysp[["WHERE_SLEEPcouch"]][y]
        #+beta_nikos$lm1.coefficients[beta_nikos$X=="DISABILITYy"]*ysp[["DISABILITYy"]][y]
        #+beta_nikos$lm1.coefficients[beta_nikos$X=="WHERE_SLEEPoutdoors"]*ysp[["WHERE_SLEEPoutdoors"]][y]
        #+beta_nikos$lm1.coefficients[beta_nikos$X=="GET_ANY_MONEY_LEGAL_OR_OTHERy"]*ysp[["GET_ANY_MONEY_LEGAL_OR_OTHERy"]][y]
        #+beta_nikos$lm1.coefficients[beta_nikos$X=="TYPE_OF_EXITSSVF"]*hsp[["TYPE_OF_EXIT_SSVF"]][h]
        new_priority_nik= (beta_nikos$lm_nik.coefficients[beta_nikos$X=="X17_OR_YOUNGER1"]*ysp[["X17_OR_YOUNGER1"]][y]
                           +beta_nikos$lm_nik.coefficients[beta_nikos$X=="WHERE_SLEEPcouch"]*ysp[["WHERE_SLEEPcouch"]][y]
                           +beta_nikos$lm_nik.coefficients[beta_nikos$X=="WHERE_SLEEPoutdoors"]*ysp[["WHERE_SLEEPoutdoors"]][y]
                           +beta_nikos$lm_nik.coefficients[beta_nikos$X=="WHERE_SLEEPshelter"]*ysp[["WHERE_SLEEPshelter"]][y]
                           +beta_nikos$lm_nik.coefficients[beta_nikos$X=="WHERE_SLEEPtransitional housing"]*ysp[["WHERE_SLEEPtransitional"]][y]
                           +beta_nikos$lm_nik.coefficients[beta_nikos$X=="GET_ANY_MONEY_LEGAL_OR_OTHERy"]*ysp[["GET_ANY_MONEY_LEGAL_OR_OTHERy"]][y]
                           +beta_nikos$lm_nik.coefficients[beta_nikos$X=="DISABILITYy"]*ysp[["DISABILITYy"]][y]
                           +beta_nikos$lm_nik.coefficients[beta_nikos$X=="nst"]*ysp[["nst"]][y]
                           +beta_nikos$lm_nik.coefficients[beta_nikos$X=="TYPE_OF_EXITRRH"]*hsp[["TYPE_OF_EXIT_RRH"]][h]
                           )
        
        if (tpriority_nik<=new_priority_nik){tpriority_nik = new_priority_nik;yath_nik[h]=y;rho_temp_nik=rho[y]}
        if ( tpriority_nik == new_priority_nik && rho[y]>rho_temp_nik ){tpriority_nik = new_priority_nik;yath_nik[h] = y;rho_temp_nik=rho[y]}
      }else {break}
  }
 }
  
  if (yath_nik[h]!=0){
    x_nik[yath_nik[h],h]=1
    # Labtop
    data_point=ysp[yath_nik[h],];data_point$TYPE_OF_EXIT=hsp[["TYPE_OF_EXIT"]][h]
    p_temp=predict(fitt,data_point,type = "response")
    # HPC
    #p_temp = p[yath_nik[h],h] #HPC
    
    gain_nik=gain_nik+p_temp
    #####acumulate p of success in buckets
    if(ysp[["nst"]][yath_nik[h]]>=0 && ysp[["nst"]][yath_nik[h]]<=3){
      p_h_b[1]=p_h_b[1]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b[1]=n_rrh_a_b[1]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b[1]=n_psh_a_b[1]+1}
      n_a_b[1]=n_a_b[1]+1}
    if(ysp[["nst"]][yath_nik[h]]>=4 && ysp[["nst"]][yath_nik[h]]<=7){
      p_h_b[2]=p_h_b[2]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b[2]=n_rrh_a_b[2]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b[2]=n_psh_a_b[2]+1}
      n_a_b[2]=n_a_b[2]+1}
    if(ysp[["nst"]][yath_nik[h]]>=8){
      p_h_b[3]=p_h_b[3]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b[3]=n_rrh_a_b[3]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b[3]=n_psh_a_b[3]+1}
      n_a_b[3]=n_a_b[3]+1}
    
    num_of_allocation_nik=num_of_allocation_nik+1
  }
  tpriority_nik=-1000
}
gain_nik_save=gain_nik
gain_nik=gain_nik_save

###############
n_a_b #number house allocation in buckets
n_rrh_a_b #number rrh house allocation in buckets
n_psh_a_b #number psh house allocation in buckets
n_a_b/ny_b  #percent of allocation nik
n_a_b/sum(n_a_b) #overall percent of allocation
n_rrh_a_b/sum(n_rrh_a_b) #overall percent of rrh allocation
n_psh_a_b/sum(n_psh_a_b) #overall percent of psh allocation
###########
#Nik non housing
p_nh_b=rep(0,3)#probability of success of buckets (non housing success) out of sample
n_na_b=rep(0,3) #number of not allocated per bucket
for (y in 1:nyp){
  if (sum(x_nik[y,])==0){
    #labtop
    data_point=ysp[y,]
    data_point$TYPE_OF_EXIT="non_housing"
    p_temp=predict(fitt_n,data_point,type ="response")
    p_go_nh=predict(fitt_g_n,data_point,type ="response") 
    #HPC
    #p_temp = p_nh[yath_nik[h],h]
    #p_go_nh = p_go_nh[y]
    
    gain_nik=gain_nik+p_go_nh*p_temp
    
    if(ysp[["nst"]][y]>=0 && ysp[["nst"]][y]<=3){
      p_nh_b[1]=p_nh_b[1]+p_go_nh*p_temp;n_na_b[1]=n_na_b[1]+1}
    if(ysp[["nst"]][y]>=4 && ysp[["nst"]][y]<=7){
      p_nh_b[2]=p_nh_b[2]+p_go_nh*p_temp;n_na_b[2]=n_na_b[2]+1}
    if(ysp[["nst"]][y]>=8){
      p_nh_b[3]=p_nh_b[3]+p_go_nh*p_temp;n_na_b[3]=n_na_b[3]+1}
  }
}
gain_nik=gain_nik/dim(ysp)[1];
gain_nik
#p of success over buketes for fairness comparison
#p_h_b/ny_b
#p_nh_b/ny_b
p_b=(p_h_b+p_nh_b)/ny_b
p_b
var(p_b)

######################################################
######################################################
######################################################
#performance of current approach
gain_cur=0
yath_cur=rep(0,nhp)
tpriority_cur=-10000
new_priority_cur=0
x_cur=matrix(rep(0,nhp*nyp),ncol = nhp, nrow = nyp)
rho_temp_cur=0
num_of_allocation_cur=0
p_h_b_cur=rep(0,3) #probability of success of buckets current
n_a_b_cur=rep(0,3) #number of allocation per bucket current
n_rrh_a_b_cur=rep(0,3) #number of rrh allocation per bucket
n_psh_a_b_cur=rep(0,3) #number of psh allocation per bucket
for (h in 1:nhp){
  if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){
    for (y in 1:nyp){
      if (ysp[["nst"]][y]>=4 & ysp[["nst"]][y]<=7){
        if (sum(x_cur[y,])==0){
          if (ysp[["arrivedate"]][y]<=hsp[["DATE_OF_EXIT_HOMELESSNESS"]][h]){
        
            new_priority_cur=hsp[["DATE_OF_EXIT_HOMELESSNESS"]][h]-ysp[["arrivedate"]][y]
        
            if (tpriority_cur<=new_priority_cur){tpriority_cur = new_priority_cur;yath_cur[h]=y;rho_temp_cur=rho[y]}
            if (tpriority_cur == new_priority_cur && rho[y]>rho_temp_cur ){tpriority_cur = new_priority_cur;yath_cur[h] = y;rho_temp_cur=rho[y]}
          }else {break}
        }
      }
    }
  }
  if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){
    for (y in 1:nyp){
      if (ysp[["nst"]][y]>=8){
        if (sum(x_cur[y,])==0){
          if (ysp[["arrivedate"]][y]<=hsp[["DATE_OF_EXIT_HOMELESSNESS"]][h]){
            
            new_priority_cur=hsp[["DATE_OF_EXIT_HOMELESSNESS"]][h]-ysp[["arrivedate"]][y]
            
            if (tpriority_cur<=new_priority_cur){tpriority_cur = new_priority_cur;yath_cur[h]=y;rho_temp_cur=rho[y]}
            if ( tpriority_cur == new_priority_cur && rho[y]>rho_temp_cur ){tpriority_cur = new_priority_cur;yath_cur[h] = y;rho_temp_cur=rho[y]}
          }else {break}
        }
      }
    }
  }
  if (yath_cur[h]!=0){
    x_cur[yath_cur[h],h]=1
    #labtop
    data_point=ysp[yath_cur[h],]
    data_point$TYPE_OF_EXIT=hsp[["TYPE_OF_EXIT"]][h]
    p_temp=predict(fitt,data_point,type = "response")
    #HPC
    #p_temp = p[yath_cur[h],h]
    
    gain_cur=gain_cur+p_temp 
    #####acumulate p of success in buckets
    if(ysp[["nst"]][yath_cur[h]]>=4 && ysp[["nst"]][yath_cur[h]]<=7){
      p_h_b_cur[2]=p_h_b_cur[2]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b_cur[2]=n_rrh_a_b_cur[2]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b_cur[2]=n_psh_a_b_cur[2]+1}
      n_a_b_cur[2]=n_a_b_cur[2]+1}
    if(ysp[["nst"]][yath_cur[h]]>=8){
      p_h_b_cur[3]=p_h_b_cur[3]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b_cur[3]=n_rrh_a_b_cur[3]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b_cur[3]=n_psh_a_b_cur[3]+1}
      n_a_b_cur[3]=n_a_b_cur[3]+1}
    
    num_of_allocation_cur=num_of_allocation_cur+1
    }
  tpriority_cur=-10000
}
gain_cur_save=gain_cur
gain_cur=gain_cur_save

n_a_b_cur #number house allocation in buckets
n_rrh_a_b_cur #number rrh house allocation in buckets
n_psh_a_b_cur #number psh house allocation in buckets
n_a_b_cur/ny_b  #percent of allocation nik
n_a_b_cur/sum(n_a_b_cur) #overall percent of allocation
n_rrh_a_b_cur/sum(n_rrh_a_b_cur) #overall percent of rrh allocation
n_psh_a_b_cur/sum(n_psh_a_b_cur) #overall percent of psh allocation

#Current non_housing success
p_nh_b_cur=rep(0,3)#probability of success of buckets (non housing success) out of sample
n_na_b_cur=rep(0,3) #number of not allocated per bucket
for (y in 1:nyp){
  if (sum(x_cur[y,])==0){
    #labtop
    data_point=ysp[y,]
    data_point$TYPE_OF_EXIT="non_housing"
    p_temp=predict(fitt_n,data_point,type ="response") #Labtop
    p_go_nh=predict(fitt_g_n,data_point,type ="response") #Labtop
    #HPC
    #p_go_nh = p_go_nh[y]
    #p_temp = p_nh[yath_nik[h],h]  
    
    gain_cur=gain_cur+p_go_nh*p_temp
    
    if(ysp[["nst"]][y]>=0 && ysp[["nst"]][y]<=3){
      p_nh_b_cur[1]=p_nh_b_cur[1]+p_go_nh*p_temp;n_na_b_cur[1]=n_na_b_cur[1]+1}
    if(ysp[["nst"]][y]>=4 && ysp[["nst"]][y]<=7){
      p_nh_b_cur[2]=p_nh_b_cur[2]+p_go_nh*p_temp;n_na_b_cur[2]=n_na_b_cur[2]+1}
    if(ysp[["nst"]][y]>=8){
      p_nh_b_cur[3]=p_nh_b_cur[3]+p_go_nh*p_temp;n_na_b_cur[3]=n_na_b_cur[3]+1}
  }
}
gain_cur=gain_cur/dim(ysp)[1];
gain_cur

#p of success over buketes for fairness comparison
#p_h_b/ny_b
#p_nh_b/ny_b
p_b_cur=(p_h_b_cur+p_nh_b_cur)/ny_b
p_b_cur
var(p_b_cur)

write.csv(x_cur,"x_cur.csv") #for OUT of SAMPLE
write.csv(x_cur,"x_cur_in_sample.csv")# for IN SAMPLE


#########################################################
#######################################################
####  Using two linear model (for PSH and RRH)
## learned beta of nikolaos approach
beta_nikos_rrh=read.csv("beta_nikos_rrh.csv")
beta_nikos_psh=read.csv("beta_nikos_psh.csv")
beta_nikos_rrh
beta_nikos_psh

#performance of nik IN or OUT of sample for housing
gain_nik=0
yath_nik=rep(0,nhp)
tpriority_nik=-1000
new_priority_nik=0
x_nik=matrix(rep(0,nhp*nyp),ncol = nhp, nrow = nyp)
rho_temp_nik=0
num_of_allocation_nik=0
p_h_b=rep(0,3) #probability of success of buckets (housing porgramm success)
n_a_b=rep(0,3) #number of allocation per bucket
n_rrh_a_b=rep(0,3) #number of rrh allocation per bucket
n_psh_a_b=rep(0,3) #number of psh allocation per bucket
for (h in 1:nhp){
  for (y in 1:nyp){
    if (sum(x_nik[y,])==0){
      if (ysp[["arrivedate"]][y]<=hsp[["DATE_OF_EXIT_HOMELESSNESS"]][h]){
        
        new_priority_nik= hsp[["TYPE_OF_EXIT_RRH"]][h]*(beta_nikos_rrh$lm_nik_rrh.coefficients[beta_nikos_rrh$X=="X17_OR_YOUNGER1"]*ysp[["X17_OR_YOUNGER1"]][y]
                           +beta_nikos_rrh$lm_nik_rrh.coefficients[beta_nikos_rrh$X=="WHERE_SLEEPcouch"]*ysp[["WHERE_SLEEPcouch"]][y]
                           +beta_nikos_rrh$lm_nik_rrh.coefficients[beta_nikos_rrh$X=="WHERE_SLEEPoutdoors"]*ysp[["WHERE_SLEEPoutdoors"]][y]
                           +beta_nikos_rrh$lm_nik_rrh.coefficients[beta_nikos_rrh$X=="WHERE_SLEEPshelter"]*ysp[["WHERE_SLEEPshelter"]][y]
                           +beta_nikos_rrh$lm_nik_rrh.coefficients[beta_nikos_rrh$X=="WHERE_SLEEPtransitional housing"]*ysp[["WHERE_SLEEPtransitional"]][y]
                           +beta_nikos_rrh$lm_nik_rrh.coefficients[beta_nikos_rrh$X=="GET_ANY_MONEY_LEGAL_OR_OTHERy"]*ysp[["GET_ANY_MONEY_LEGAL_OR_OTHERy"]][y]
                           +beta_nikos_rrh$lm_nik_rrh.coefficients[beta_nikos_rrh$X=="DISABILITYy"]*ysp[["DISABILITYy"]][y]
                           +beta_nikos_rrh$lm_nik_rrh.coefficients[beta_nikos_rrh$X=="nst"]*ysp[["nst"]][y])
                           +hsp[["TYPE_OF_EXIT_PSH"]][h]*(beta_nikos_psh$lm_nik_psh.coefficients[beta_nikos_psh$X=="X17_OR_YOUNGER1"]*ysp[["X17_OR_YOUNGER1"]][y]
                           +beta_nikos_psh$lm_nik_psh.coefficients[beta_nikos_psh$X=="WHERE_SLEEPcouch"]*ysp[["WHERE_SLEEPcouch"]][y]
                           +beta_nikos_psh$lm_nik_psh.coefficients[beta_nikos_psh$X=="WHERE_SLEEPoutdoors"]*ysp[["WHERE_SLEEPoutdoors"]][y]
                           +beta_nikos_psh$lm_nik_psh.coefficients[beta_nikos_psh$X=="WHERE_SLEEPshelter"]*ysp[["WHERE_SLEEPshelter"]][y]
                           +beta_nikos_psh$lm_nik_psh.coefficients[beta_nikos_psh$X=="WHERE_SLEEPtransitional housing"]*ysp[["WHERE_SLEEPtransitional"]][y]
                           +beta_nikos_psh$lm_nik_psh.coefficients[beta_nikos_psh$X=="GET_ANY_MONEY_LEGAL_OR_OTHERy"]*ysp[["GET_ANY_MONEY_LEGAL_OR_OTHERy"]][y]
                           +beta_nikos_psh$lm_nik_psh.coefficients[beta_nikos_psh$X=="DISABILITYy"]*ysp[["DISABILITYy"]][y]
                           +beta_nikos_psh$lm_nik_psh.coefficients[beta_nikos_psh$X=="nst"]*ysp[["nst"]][y])
        
        if (tpriority_nik<=new_priority_nik){tpriority_nik = new_priority_nik;yath_nik[h]=y;rho_temp_nik=rho[y]}
        if ( tpriority_nik == new_priority_nik && rho[y]>rho_temp_nik ){tpriority_nik = new_priority_nik;yath_nik[h] = y;rho_temp_nik=rho[y]}
      }else {break}
    }
  }
  
  if (yath_nik[h]!=0){
    x_nik[yath_nik[h],h]=1
    # Labtop
    data_point=ysp[yath_nik[h],];data_point$TYPE_OF_EXIT=hsp[["TYPE_OF_EXIT"]][h]
    p_temp=predict(fitt,data_point,type = "response")
    # HPC
    #p_temp = p[yath_nik[h],h] #HPC
    
    gain_nik=gain_nik+p_temp
    #####acumulate p of success in buckets
    if(ysp[["nst"]][yath_nik[h]]>=0 && ysp[["nst"]][yath_nik[h]]<=3){
      p_h_b[1]=p_h_b[1]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b[1]=n_rrh_a_b[1]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b[1]=n_psh_a_b[1]+1}
      n_a_b[1]=n_a_b[1]+1}
    if(ysp[["nst"]][yath_nik[h]]>=4 && ysp[["nst"]][yath_nik[h]]<=7){
      p_h_b[2]=p_h_b[2]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b[2]=n_rrh_a_b[2]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b[2]=n_psh_a_b[2]+1}
      n_a_b[2]=n_a_b[2]+1}
    if(ysp[["nst"]][yath_nik[h]]>=8){
      p_h_b[3]=p_h_b[3]+p_temp
      if (hsp[["TYPE_OF_EXIT"]][h]=="RRH"){n_rrh_a_b[3]=n_rrh_a_b[3]+1}
      if (hsp[["TYPE_OF_EXIT"]][h]=="PSH"){n_psh_a_b[3]=n_psh_a_b[3]+1}
      n_a_b[3]=n_a_b[3]+1}
    
    num_of_allocation_nik=num_of_allocation_nik+1
  }
  tpriority_nik=-1000
}
gain_nik_save=gain_nik
gain_nik=gain_nik_save

###############
n_a_b #number house allocation in buckets
n_rrh_a_b #number rrh house allocation in buckets
n_psh_a_b #number psh house allocation in buckets
n_a_b/ny_b  #percent of allocation nik
n_a_b/sum(n_a_b) #overall percent of allocation
n_rrh_a_b/sum(n_rrh_a_b) #overall percent of rrh allocation
n_psh_a_b/sum(n_psh_a_b) #overall percent of psh allocation
###########
#Nik non housing
p_nh_b=rep(0,3)#probability of success of buckets (non housing success) out of sample
n_na_b=rep(0,3) #number of not allocated per bucket
for (y in 1:nyp){
  if (sum(x_nik[y,])==0){
    #labtop
    data_point=ysp[y,]
    data_point$TYPE_OF_EXIT="non_housing"
    p_temp=predict(fitt_n,data_point,type ="response")
    p_go_nh=predict(fitt_g_n,data_point,type ="response") 
    #HPC
    #p_temp = p_nh[yath_nik[h],h]
    #p_go_nh = p_go_nh[y]
    
    gain_nik=gain_nik+p_go_nh*p_temp
    
    if(ysp[["nst"]][y]>=0 && ysp[["nst"]][y]<=3){
      p_nh_b[1]=p_nh_b[1]+p_go_nh*p_temp;n_na_b[1]=n_na_b[1]+1}
    if(ysp[["nst"]][y]>=4 && ysp[["nst"]][y]<=7){
      p_nh_b[2]=p_nh_b[2]+p_go_nh*p_temp;n_na_b[2]=n_na_b[2]+1}
    if(ysp[["nst"]][y]>=8){
      p_nh_b[3]=p_nh_b[3]+p_go_nh*p_temp;n_na_b[3]=n_na_b[3]+1}
  }
}
gain_nik=gain_nik/dim(ysp)[1];
gain_nik
#p of success over buketes for fairness comparison
#p_h_b/ny_b
#p_nh_b/ny_b
p_b=(p_h_b+p_nh_b)/ny_b
p_b
var(p_b)

