nst=matrix(rep(0,dim(hls)[1]),ncol=1,nrow = dim(hls)[1])
for (i in 1:dim(nst)[1]){
  if (hls[i,"X17_OR_YOUNGER"]==1){nst[i]=nst[i]+1}
  if (hls[i,"WHERE_SLEEP"]!="shelter"|hls[i,"WHERE_SLEEP"]!="transitional housing"){nst[i]=nst[i]+1}
  if (hls[i,"NUM_TIMES_HOMELESS_LAST_3_YEARS"]>=4){nst[i]=nst[i]+1}
  if (hls[i,"TOTAL_EMERGENCY_SERVICES"]>=4){nst[i]=nst[i]+1}
  if (hls[i,"BEEN_ATTACKED_SINCE_HOMELESS"]=="y"|hls[i,"TRIED_TO_HARM_SELF_OR_OTHERS"]=="y"){nst[i]=nst[i]+1}
  if (hls[i,"LEGAL_ISSUES"]=="y"|hls[i,"INCARCERATED_BEFORE_18_YRS"]=="y"){nst[i]=nst[i]+1}
  if (hls[i,"FORCED_TO_DO_THINGS"]=="y"|hls[i,"DO_RISKY_THINGS"]=="y"){nst[i]=nst[i]+1}
  if (hls[i,"SOMEONE_THINKS_U_OWE_MONEY"]=="y"|hls[i,"GET_ANY_MONEY_LEGAL_OR_OTHER"]=="n"){nst[i]=nst[i]+1}
  if (hls[i,"NICE_ACTIVITIES_MAKE_YOU_HAPPY"]=="n"){nst[i]=nst[i]+1}
  if (hls[i,"ABLE_SATISFY_BASIC_NEEDS"]=="n"){nst[i]=nst[i]+1}
  if (hls[i,"BECAUSE_RAN_AWAY"]=="y"|hls[i,"BECAUSE_DIFFERENCE_CULTURAL_BELIEFS"]=="y"|hls[i,"BECAUSE_KICKED_OUT"]=="y"|hls[i,"BECAUSE_GENDER_ID"]=="y"){nst[i]=nst[i]+1}
  if (hls[i,"BECAUSE_VIOLENCE_FAMILY"]=="y"|hls[i,"BECAUSE_ABUSE"]=="y"){nst[i]=nst[i]+1}
  if (hls[i,"HAD_TO_LEAVE_BECAUSE_PHYSICAL_HEALTH"]=="y"|hls[i,"CHRONIC_HEALTH_LKSLH"]=="y"|hls[i,"INTERESTED_PROGRAM_HIV_AIDS"]=="y"|hls[i,"PHYSICAL_DISABILITIES"]=="y"|hls[i,"AVOID_MEDICAL_HELP"]=="y"|hls[i,"PREGNANT"]=="y"){nst[i]=nst[i]+1}
  if (hls[i,"KICKED_OUT_DRINKING_DRUGS"]=="y"|hls[i,"DIFFICULT_DRINKING_DRUG"]=="y"|hls[i,"MARIJUANA_12_OR_YOUNGER"]=="y"){nst[i]=nst[i]+1}
  if (hls[i,"MENTAL_HEALTH_ISSUE"]=="y"|hls[i,"PAST_HEAD_INJURY"]=="y"|hls[i,"DISABILITY"]=="y"|hls[i,"MENTAL_HEALTH_NEED_HELP"]=="y"){nst[i]=nst[i]+1}
  if (hls[i,"TRIMORBIDITY"]==1){nst[i]=nst[i]+1}
  if (hls[i,"MEDICATIONS_NOT_TAKING"]=="y"|hls[i,"MEDICATIONS_NOT_TAKING_OR_SELLING"]=="y"){nst[i]=nst[i]+1}
}
hls$nst=nst
write.csv(hls,"F:/term1/DR/R_DR1/hls.csv")
