###################
##
## Figure 3

actellic_details = readRDS("data/actellic_details_v2.Rdata")
sumishield_details = readRDS("data/sumishield_details_v2.Rdata")
par(mfrow = c(2,3))


w_Acte = yy_Acte = z_Acte = w_Sumi = yy_Sumi = z_Sumi = array(dim=c(180,4)) 
## column 1 will be the effect if there is no intervention
## column 2 is with ITNs only
## column 3 is with IRS only no loss in coverage
## column 4 is with IRS only loss in coverage
## column 5 is ITN + IRS no loss
## column 6 is ITN + IRS loss

#############################
## 

##Species are different in each location 
PHI_B_mut = 0.85 ## probability of bites in bed
PHI_I_mut = 0.90 ## probability of bites indoors

PHI_B_boa = 0.85 ## probability of bites in bed
PHI_I_boa = 0.90 ## probability of bites indoors

# return(list(mean_prediction,
#             feed2,
#             death2,
#             rep2,
#             deter2))
# actellic_details
# sumishield_details

k0 = 0.699
ksA = actellic_details[[2]]
lsA = actellic_details[[3]]
jsA = 1 - actellic_details[[2]] - actellic_details[[3]]

s_IRS_Acte = ksA/k0 ##feed2
r_IRS_Acte = (1 - ksA/k0)*(jsA/(lsA+jsA)) ##rep2

ksS = sumishield_details[[2]]
lsS = sumishield_details[[3]]
jsS = 1 - sumishield_details[[2]] - sumishield_details[[3]]

s_IRS_Sumi = ksS/k0 ##feed2
r_IRS_Sumi = (1 - ksS/k0)*(jsS/(lsS+jsS)) ##rep2

##210?
w_Acte[,1] = w_Sumi[,1] = rep(1,180) ## Probability that a mosquito bites and survives in the presence of indoor vector control
for(i in 1:180){
  PHI_B = PHI_B_mut
  PHI_I = PHI_I_mut
  w_Acte[i,2] = 1 - PHI_B + PHI_B*s_ITN[i+547]				 ## probability of surviving biting given that there is ITN
  w_Acte[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte[i])*s_IRS_Acte[i]	##			probability of surviving biting given that there is IRS
  w_Acte[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte[i])*s_ITN[i+547]*s_IRS_Acte[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte[i])*s_IRS_Acte[i] ## probability of surviving biting given that there is ITN & IRS
}
for(i in 1:180){
  PHI_B = PHI_B_boa
  PHI_I = PHI_I_boa
  w_Sumi[i,2] = 1 - PHI_B + PHI_B*s_ITN[i+547]				 ## probability of surviving biting given that there is ITN
  w_Sumi[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi[i])*s_IRS_Sumi[i]	##			probability of surviving biting given that there is IRS
  w_Sumi[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi[i])*s_ITN[i+547]*s_IRS_Sumi[i] + (PHI_I - PHI_B)*(1-r_IRS_Sumi[i])*s_IRS_Sumi[i] ## probability of surviving biting given that there is ITN & IRS
  
}

# par(mfrow = c(2,3))


## Probability of any bite (if there is IRS, a mosquito may bite and then die immediately afterwards)
yy_Acte[,1] = w_Acte[,1] 
yy_Acte[,2] = w_Acte[,2]

yy_Sumi[,1] = w_Sumi[,1] 
yy_Sumi[,2] = w_Sumi[,2]

for(i in 1:180){
  PHI_B = PHI_B_mut
  PHI_I = PHI_I_mut
  yy_Acte[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte[i])
  yy_Acte[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte[i])*s_ITN[i+547] + (PHI_I - PHI_B)*(1-r_IRS_Acte[i])
}
for(i in 1:180){
  PHI_B = PHI_B_boa
  PHI_I = PHI_I_boa
  yy_Sumi[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi[i])
  yy_Sumi[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi[i])*s_ITN[i+547] + (PHI_I - PHI_B)*(1-r_IRS_Sumi[i])
  
}
## supplementary figure 1a
plot(yy_Acte[,1] ~ time[1:180],ylim=c(0,1),pch="",
     ylab = "Probability mosquito bites",
     col="black",
     main = "",cex.main=1.2,xlim=c(-60,180),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(-60,150,30)+15,labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)

colsd=c("darkred","red","orange","blue")
for(i in 3){
  lines(yy_Acte[,i] ~ time[1:180],col="darkblue",lwd=2)
  lines(yy_Sumi[,i] ~ time[1:180],col="aquamarine3",lwd=2)
}

legend("topleft",legend = c("Actellic 300CS","SumiShield"),
       col = c("darkblue","aquamarine3"),lwd = 2, lty=c(1,1),cex=1.2,bty="n")

## supplementary figure 1b
plot(w_Acte[,1] ~ time[1:180],ylim=c(0,1),pch="",
     ylab = "Probability mosquito bites and survives",
     col="black",
     main = "",cex.main=1.2,xlim=c(-60,180),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(-60,150,30)+15,labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)

colsd=c("darkred","red","orange","blue")
for(i in 3){
  lines(w_Acte[,i] ~ time[1:180],col="darkblue",lwd = 2)
  lines(w_Sumi[,i] ~ time[1:180],col="aquamarine3",lwd = 2)
}
## Probability repelled
z_Acte[,1] = 0
z_Sumi[,1] = 0

for(i in 1:180){
  z_Acte[i,2] = PHI_B*r_ITN[i+547]
  z_Acte[i,3] = PHI_I*r_IRS_Acte[i]
  z_Acte[i,4] = PHI_B*(r_IRS_Acte[i] + (1-r_IRS_Acte[i])*r_ITN[i+547]) + (PHI_I - PHI_B)*r_IRS_Acte[i]
  
  z_Sumi[i,2] = PHI_B*r_ITN[i+547]
  z_Sumi[i,3] = PHI_I*r_IRS_Sumi[i]
  z_Sumi[i,4] = PHI_B*(r_IRS_Sumi[i] + (1-r_IRS_Sumi[i])*r_ITN[i+547]) + (PHI_I - PHI_B)*r_IRS_Sumi[i]
  
}
## supplementary figure 1c
plot(z_Acte[,1] ~ time[1:180],ylim=c(0,1),pch="",
     ylab = "Probability mosquito is repelled",
     col="black",
     main = "",cex.main=1.2,xlim=c(-60,180),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(-60,150,30)+15,labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)

colsd=c("darkred","red","orange","blue")
for(i in 3){
  lines(z_Acte[,i] ~ time[1:180],col="darkblue",lwd=2)
  lines(z_Sumi[,i] ~ time[1:180],col="aquamarine3",lwd=2)
}

## Campaigns tend to take up to a few months to complete
## We assume the ratio of houses monitored per start month
## reflects the proportion of houses covered by the spray campaign
## in that month

## Houses were tracked from November, December and January (1 in Feb)
## 129/(129+88+27+1) is 52.65% of houses in the community have max protection in Nov
## 88/(129+88+27+1) is 35.92% of houses max protection Dec (52% - any modifications have 1 mont old protection)
## 27/(129+88+27+1) is 11.02% max protection Jan (35.92% - modif 1 month old, 52% - modif 2 month old)


## derived ITN/IRS quantities
## prob bites and survives

w_Acte1 = yy_Acte1 = z_Acte1 = w_Sumi1 = yy_Sumi1 = z_Sumi1 = array(dim=c(365,18,4))
# w_Acte2 = yy_Acte2 = z_Acte2 = w_Sumi2 = yy_Sumi2 = z_Sumi2 = array(dim=c(365,18,4)) 
# w_Acte3 = yy_Acte3 = z_Acte3 = w_Sumi3 = yy_Sumi3 = z_Sumi3 = array(dim=c(365,18,4)) 

## column 1 will be the effect if there is no intervention
## column 2 is with ITNs only
## column 3 is with IRS only no loss in coverage
## column 4 is with IRS only loss in coverage
## column 5 is ITN + IRS no loss
## column 6 is ITN + IRS loss

#############################
## 


# return(list(mean_prediction,
#             feed2,
#             death2,
#             rep2,
#             deter2))
# actellic_details
# sumishield_details

# k0 = 0.699
# ksA = actellic_details[[2]]
# lsA = actellic_details[[3]]
# jsA = 1 - actellic_details[[2]] - actellic_details[[3]]
# 
# ksA1 = actellic_details[[2]]
# lsA1 = actellic_details[[3]]
# jsA1 = 1 - actellic_details[[2]] - actellic_details[[3]]
# 
# ksA2 = c(rep(k0,30),actellic_details[[2]][1:335])
# lsA2 = c(rep(0,30),actellic_details[[3]][1:335])
# jsA2 = 1 - ksA2 - lsA2
# 
# ksA3 = c(rep(k0,61),actellic_details[[2]][1:304])
# lsA3 = c(rep(0,61),actellic_details[[3]][1:304])
# jsA3 = 1 - ksA3 - lsA3


prop_houses_sprayed_WeeklyB = 0.97*c(0,	0.027219794,	## August
                                0.077014558,	0.136261919,	0.196901742,	0.250817066, ## Sept
                                0.301047746,	0.347687015,	0.395348206,	0.464541108, ## Oct
                                0.521818166,	0.581372602,	0.643327061,	0.710948828, ## Nov
                                0.777620537,	0.847130239,	0.911498024,	0.930365931, ## Dec
                                0.947042313,	0.96389906,	0.983400068,	0.991357264,	 ## Jan
                                0.99238783,	1) ## Feb
prop_houses_sprayed_WeeklyM = 0.96*c(0.065358837,	0.193716785,## August
                                0.334444596,	0.432141444,	0.533708885,	0.614060416,##sep
                                0.667531537,	0.711462198,	0.769981823,	0.880751007,##oct
                                0.919045204,	0.944027976,	0.97443187,	0.990922736,  ##nov
                                0.991873307,	0.99744169,	 1,             1,          ##dec
                                1,        1) ## jan

ksA = lsA = jsA = array(dim=c(365,18))
for(w in 1:17){
  ksA[,1] =  actellic_details[[2]][1:365]
  ksA[,w+1] = c(rep(k0,w*7),actellic_details[[2]][1:(365-7*w)])
  
  lsA[,1] = actellic_details[[3]]
  lsA[,w+1] = c(rep(0,w*7),actellic_details[[3]][1:(365-7*w)])

  jsA[,w] = 1 - ksA[,w] - lsA[,w]
  
}
jsA[,18] = 1 - ksA[,18] - lsA[,18]

  
s_IRS_Acte1 = r_IRS_Acte1 = array(dim=c(365,18))
for(w in 1:18){
  s_IRS_Acte1[,w] = ksA[,w]/k0 ##feed2 = when IRS is implemented in month 1 (Nov)
  r_IRS_Acte1[,w] = (1 - ksA[,w]/k0)*(jsA[,w]/(lsA[,w]+jsA[,w])) ##rep2 
  
}


ksS = lsS = jsS = array(dim=c(365,18))
for(w in 1:17){
  ksS[,1] =  sumishield_details[[2]][1:365]
  ksS[,w+1] = c(rep(k0,w*7),sumishield_details[[2]][1:(365-7*w)])
  
  lsS[,1] = sumishield_details[[3]]
  lsS[,w+1] = c(rep(0,w*7),sumishield_details[[3]][1:(365-7*w)])
  
  jsS[,w] = 1 - ksS[,w] - lsS[,w]
  
}
jsS[,18] = 1 - ksS[,18] - lsS[,18]


s_IRS_Sumi1 = r_IRS_Sumi1 = array(dim=c(365,18))
for(w in 1:18){
  s_IRS_Sumi1[,w] = ksS[,w]/k0 ##feed2 = when IRS is implemented in month 1 (Nov)
  r_IRS_Sumi1[,w] = (1 - ksS[,w]/k0)*(jsS[,w]/(lsS[,w]+jsS[,w])) ##rep2 
  
}

# ksS = sumishield_details[[2]]
# lsS = sumishield_details[[3]]
# jsS = 1 - sumishield_details[[2]] - sumishield_details[[3]]
# 
# ksS1 = sumishield_details[[2]]
# lsS1 = sumishield_details[[3]]
# jsS1 = 1 - sumishield_details[[2]] - sumishield_details[[3]]
# 
# ksS2 = c(rep(k0,30),sumishield_details[[2]][1:335])
# lsS2 = c(rep(k0,30),sumishield_details[[3]][1:335])
# jsS2 = 1 - ksS2 - lsS2
# 
# ksS3 = c(rep(k0,61),sumishield_details[[2]][1:304])
# lsS3 = c(rep(k0,61),sumishield_details[[3]][1:304])
# jsS3 = 1 - ksS3 - lsS3

# s_IRS_Sumi1 = ksS1/k0 ##feed2
# r_IRS_Sumi1 = (1 - ksS1/k0)*(jsS1/(lsS1+jsS1)) ##rep2
# 
# s_IRS_Sumi2 = ksS2/k0 ##feed2
# r_IRS_Sumi2 = (1 - ksS2/k0)*(jsS2/(lsS2+jsS2)) ##rep2
# 
# s_IRS_Sumi3 = ksS3/k0 ##feed2
# r_IRS_Sumi3 = (1 - ksS3/k0)*(jsS3/(lsS3+jsS3)) ##rep2

w_Acte1[,,1] = w_Sumi1[,,1] = rep(1,365) 
## Probability that a mosquito bites and survives in the presence of indoor vector control
for(j in 1:18){
  for(i in 1:365){
    PHI_B = 0.85
    PHI_I = 0.9
    w_Acte1[i,j,2] = 1 - PHI_B + PHI_B*s_ITN[i+547]				 ## probability of surviving biting given that there is ITN
    w_Acte1[i,j,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte1[i,j])*s_IRS_Acte1[i,j]	##			probability of surviving biting given that there is IRS
    w_Acte1[i,j,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte1[i,j])*s_ITN[i+547]*s_IRS_Acte1[i,j] + (PHI_I - PHI_B)*(1-r_IRS_Acte1[i,j])*s_IRS_Acte1[i,j] ## probability of surviving biting given that there is ITN & IRS

    
    w_Sumi1[i,j,2] = 1 - PHI_B + PHI_B*s_ITN[i+547]				 ## probability of surviving biting given that there is ITN
    w_Sumi1[i,j,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi1[i,j])*s_IRS_Sumi1[i,j]	##			probability of surviving biting given that there is IRS
    w_Sumi1[i,j,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi1[i,j])*s_ITN[i+547]*s_IRS_Sumi1[i,j] + (PHI_I - PHI_B)*(1-r_IRS_Sumi1[i,j])*s_IRS_Sumi1[i,j] ## probability of surviving biting given that there is ITN & IRS
    
  }
  
}

#   w_Acte2[i,2] = 1 - PHI_B + PHI_B*s_ITN[i+547]				 ## probability of surviving biting given that there is ITN
#   w_Acte2[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte2[i])*s_IRS_Acte2[i]	##			probability of surviving biting given that there is IRS
#   w_Acte2[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte2[i])*s_ITN[i+547]*s_IRS_Acte2[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte2[i])*s_IRS_Acte2[i] ## probability of surviving biting given that there is ITN & IRS
#   
#   w_Acte3[i,2] = 1 - PHI_B + PHI_B*s_ITN[i+547]				 ## probability of surviving biting given that there is ITN
#   w_Acte3[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte3[i])*s_IRS_Acte3[i]	##			probability of surviving biting given that there is IRS
#   w_Acte3[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte3[i])*s_ITN[i+547]*s_IRS_Acte3[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte3[i])*s_IRS_Acte3[i] ## probability of surviving biting given that there is ITN & IRS
# }

prop_this_weekM = c(prop_houses_sprayed_WeeklyM[1],diff(prop_houses_sprayed_WeeklyM)[1:17])
prop_this_weekB = c(prop_houses_sprayed_WeeklyB[1],diff(prop_houses_sprayed_WeeklyB)[1:17])

w_Acte = yy_Acte = z_Acte = w_Sumi = yy_Sumi = z_Sumi = array(dim=c(365,4) )

for(i in 1:365){
  w_Acte[i,1] = sum(w_Acte1[i,,1] * prop_this_weekM)
  w_Acte[i,2] = sum(w_Acte1[i,,2] * prop_this_weekM)
  w_Acte[i,3] = sum(w_Acte1[i,,3] * prop_this_weekM)
  w_Acte[i,4] = sum(w_Acte1[i,,4] * prop_this_weekM)

  w_Sumi[i,1] = sum(w_Sumi1[i,,1] * prop_this_weekB)
  w_Sumi[i,2] = sum(w_Sumi1[i,,2] * prop_this_weekB)
  w_Sumi[i,3] = sum(w_Sumi1[i,,3] * prop_this_weekB)
  w_Sumi[i,4] = sum(w_Sumi1[i,,4] * prop_this_weekB)
}





## Probability of any bite (if there is IRS, a mosquito may bite and then die immediately afterwards)
yy_Acte[,1] = w_Acte[,1] 
yy_Acte[,2] = w_Acte[,2]

# yy_Sumi[,1] = w_Sumi[,1] 
# yy_Sumi[,2] = w_Sumi[,2]

for(j in 1:18){
  for(i in 1:365){
    PHI_B = 0.85
    PHI_I = 0.9
    yy_Acte1[i,j,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte1[i,j])
    yy_Acte1[i,j,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte1[i,j])*s_ITN[i+547] + (PHI_I - PHI_B)*(1-r_IRS_Acte1[i,j])

    yy_Sumi1[i,j,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi1[i,j])
    yy_Sumi1[i,j,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi1[i,j])*s_ITN[i+547] + (PHI_I - PHI_B)*(1-r_IRS_Sumi1[i,j])
  }  
}


for(i in 1:365){
  yy_Acte[i,3] = sum(yy_Acte1[i,,3] * prop_this_weekM)
  yy_Acte[i,4] = sum(yy_Acte1[i,,4] * prop_this_weekM)
  
  yy_Sumi[i,3] = sum(yy_Sumi1[i,,3] * prop_this_weekB)
  yy_Sumi[i,4] = sum(yy_Sumi1[i,,4] * prop_this_weekB)
}

## supplementary figure 1a
plot(yy_Acte[,1] ~ time[1:365],ylim=c(0,1),pch="",
     ylab = "Probability mosquito bites",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,240),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(15,240,30),labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)

colsd=c("darkred","red","orange","blue")
for(i in 3){
  lines(yy_Acte[1:240,i] ~ time[1:240],col="darkblue",lwd=2)
  lines(yy_Sumi[1:240,i] ~ time[1:240],col="aquamarine3",lwd=2)
}

legend("bottomleft",legend = c("Actellic 300CS","SumiShield"),
       col = c("darkblue","aquamarine3"),lwd = 2, lty=c(1,1),cex=1.2,bty="n")

## supplementary figure 1b
plot(w_Acte[,1] ~ time[1:365],ylim=c(0,1),pch="",
     ylab = "Probability mosquito bites and survives",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,240),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(15,240,30),labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)

colsd=c("darkred","red","orange","blue")
for(i in 3){
  lines(w_Acte[1:240,i] ~ time[1:240],col="darkblue",lwd = 2)
  lines(w_Sumi[1:240,i] ~ time[1:240],col="aquamarine3",lwd = 2)
}
## Probability repelled
z_Acte[,1] = 0
z_Sumi[,1] = 0

for(j in 1:18){
  for(i in 1:365){
    z_Acte1[i,j,2] = PHI_B*r_ITN[i+547]
    z_Acte1[i,j,3] = PHI_I*r_IRS_Acte1[i,j]
    z_Acte1[i,j,4] = PHI_B*(r_IRS_Acte1[i,j] + (1-r_IRS_Acte1[i,j])*r_ITN[i+547]) + (PHI_I - PHI_B)*r_IRS_Acte1[i,j]
    
    z_Sumi1[i,j,2] = PHI_B*r_ITN[i+547]
    z_Sumi1[i,j,3] = PHI_I*r_IRS_Sumi1[i,j]
    z_Sumi1[i,j,4] = PHI_B*(r_IRS_Sumi1[i,j] + (1-r_IRS_Sumi1[i,j])*r_ITN[i+547]) + (PHI_I - PHI_B)*r_IRS_Sumi1[i,j]
    
  }  
}

for(i in 1:365){
  z_Acte[i,2] = sum(z_Acte1[i,,3] * prop_this_weekM)
  z_Acte[i,3] = sum(z_Acte1[i,,3] * prop_this_weekM)
  z_Acte[i,4] = sum(z_Acte1[i,,4] * prop_this_weekM)
  
  z_Sumi[i,2] = sum(z_Sumi1[i,,3] * prop_this_weekB)
  z_Sumi[i,3] = sum(z_Sumi1[i,,3] * prop_this_weekB)
  z_Sumi[i,4] = sum(z_Sumi1[i,,4] * prop_this_weekB)
}


## supplementary figure 1c
plot(z_Acte[,1] ~ time[1:365],ylim=c(0,1),pch="",
     ylab = "Probability mosquito is repelled",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,240),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(15,240,30),labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)

colsd=c("darkred","red","orange","blue")
for(i in 3){
  lines(z_Acte[1:240,i] ~ time[1:240],col="darkblue",lwd=2)
  lines(z_Sumi[1:240,i] ~ time[1:240],col="aquamarine3",lwd=2)
}

par(xpd=NA,cex = 1.11)

text(x = -650, y = 2.65,"(A)")
text(x = -340, y = 2.65,"(B)")
text(x = -38, y = 2.65,"(C)")

text(x = -650, y = 1.2,"(D)")
text(x = -340, y = 1.2,"(E)")
text(x = -38, y = 1.2,"(F)")

