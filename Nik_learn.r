  #nik
#Learn C with LM

######################################
## for hpc to read x
ys=read.csv("ys.csv",as.is = T)
hs=read.csv("hs.csv",as.is = T)
ny=nrow(ys)
nh=nrow(hs)
x=as.data.frame(ys[rep(seq_len(nrow(ys)), each=nh),])
hss=as.data.frame(hs[rep(seq_len(nrow(hs)),time=ny),])
x$TYPE_OF_EXIT=hss$TYPE_OF_EXIT
x$DATE_OF_EXIT_HOMELESSNESS=hss$DATE_OF_EXIT_HOMELESSNESS

x$X17_OR_YOUNGER=as.factor(x$X17_OR_YOUNGER)
x$WHERE_SLEEP=as.factor(x$WHERE_SLEEP)
x$GET_ANY_MONEY_LEGAL_OR_OTHER=as.factor(x$GET_ANY_MONEY_LEGAL_OR_OTHER)
x$TYPE_OF_EXIT=as.factor(x$TYPE_OF_EXIT)
x$DISABILITY=as.factor(x$DISABILITY)

######################################
C=read.csv("C.csv",as.is = T) #if use fairness cons use this to read C
C_minus=read.csv("C_minus.csv",as.is = T)
C_no_fair=read.csv("C_no_fair.csv",as.is = T)
Cp=read.csv("Cp.csv",as.is = T)
# C$x1=C$x1*100
# C_minus$x1=C_minus$x1*100
# C_no_fair$x1=C_no_fair$x1*100
#
summary(C$x1)
summary(C_minus$x1)
summary(C_no_fair$x1)
summary(Cp$x1)

x2=x

# use one of these every time
x2$c=as.matrix(C) # P-qTa Nik fair with no Pnh
x2$c=as.matrix(C_no_fair) #nik no fair
x2$c=as.matrix(C_minus) #nik fair
x2$c=as.matrix(Cp) #nik just P

lm_nik=lm(c~ X17_OR_YOUNGER+WHERE_SLEEP+GET_ANY_MONEY_LEGAL_OR_OTHER+DISABILITY+TYPE_OF_EXIT+nst ,x2)
summary(lm_nik)
beta_nikos=as.data.frame(lm_nik$coefficients)
write.csv(beta_nikos,"beta_nikos.csv")

#################
#two linear regressions
x2_rrh=x2[x2$TYPE_OF_EXIT=="RRH",]
x2_psh=x2[x2$TYPE_OF_EXIT=="PSH",]
lm_nik_rrh=lm(c~ X17_OR_YOUNGER+WHERE_SLEEP+GET_ANY_MONEY_LEGAL_OR_OTHER+DISABILITY+nst ,x2_rrh)
lm_nik_psh=lm(c~ X17_OR_YOUNGER+WHERE_SLEEP+GET_ANY_MONEY_LEGAL_OR_OTHER+DISABILITY+nst ,x2_psh)
summary(lm_nik_rrh)
summary(lm_nik_psh)
beta_nikos_rrh=as.data.frame(lm_nik_rrh$coefficients)
beta_nikos_psh=as.data.frame(lm_nik_psh$coefficients)
write.csv(beta_nikos_rrh,"beta_nikos_rrh.csv")
write.csv(beta_nikos_psh,"beta_nikos_psh.csv")

