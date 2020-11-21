####################################
##
## Figure 4
##
####################################################

par(mfrow=c(2,3))

## derived ITN/IRS quantities
## prob bites and survives

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
PHI_I_boa = 0.9 ## probability of bites indoors

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

w_Acte[,1] = w_Sumi[,1] = rep(1,180) ## Probability that a mosquito bites and survives in the presence of indoor vector control
for(i in 1:180){
  PHI_B = PHI_B_mut
  PHI_I = PHI_I_mut
  w_Acte[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
  w_Acte[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte[i])*s_IRS_Acte[i]	##			probability of surviving biting given that there is IRS
  w_Acte[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte[i])*s_ITN[i]*s_IRS_Acte[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte[i])*s_IRS_Acte[i] ## probability of surviving biting given that there is ITN & IRS
}
for(i in 1:180){
  PHI_B = PHI_B_boa
  PHI_I = PHI_I_boa
  w_Sumi[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
  w_Sumi[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi[i])*s_IRS_Sumi[i]	##			probability of surviving biting given that there is IRS
  w_Sumi[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi[i])*s_ITN[i]*s_IRS_Sumi[i] + (PHI_I - PHI_B)*(1-r_IRS_Sumi[i])*s_IRS_Sumi[i] ## probability of surviving biting given that there is ITN & IRS
  
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
  yy_Acte[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte[i])*s_ITN[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte[i])
}
for(i in 1:180){
  PHI_B = PHI_B_boa
  PHI_I = PHI_I_boa
  yy_Sumi[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi[i])
  yy_Sumi[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi[i])*s_ITN[i] + (PHI_I - PHI_B)*(1-r_IRS_Sumi[i])
  
}

## Probability repelled
z_Acte[,1] = 0
z_Sumi[,1] = 0

for(i in 1:180){
  z_Acte[i,2] = PHI_B*r_ITN[i]
  z_Acte[i,3] = PHI_I*r_IRS_Acte[i]
  z_Acte[i,4] = PHI_B*(r_IRS_Acte[i] + (1-r_IRS_Acte[i])*r_ITN[i]) + (PHI_I - PHI_B)*r_IRS_Acte[i]
  
  z_Sumi[i,2] = PHI_B*r_ITN[i]
  z_Sumi[i,3] = PHI_I*r_IRS_Sumi[i]
  z_Sumi[i,4] = PHI_B*(r_IRS_Sumi[i] + (1-r_IRS_Sumi[i])*r_ITN[i]) + (PHI_I - PHI_B)*r_IRS_Sumi[i]
  
}
## waning usage of IRS with time

modif_months = 1:6
prop_mod_Acte = 1 - c(13.1,8.8,13.7,12.6,8.6,8.3)/100
true_cover_irs_Acte = c(0.95*prop_mod_Acte[1],
                        0.95*prop_mod_Acte[1]*prop_mod_Acte[2],
                        0.95*prop_mod_Acte[1]*prop_mod_Acte[2]*prop_mod_Acte[3],
                        0.95*prop_mod_Acte[1]*prop_mod_Acte[2]*prop_mod_Acte[3]*prop_mod_Acte[4],
                        0.95*prop_mod_Acte[1]*prop_mod_Acte[2]*prop_mod_Acte[3]*prop_mod_Acte[4]*prop_mod_Acte[5],
                        0.95*prop_mod_Acte[1]*prop_mod_Acte[2]*prop_mod_Acte[3]*prop_mod_Acte[4]*prop_mod_Acte[5]*prop_mod_Acte[6])

#House coverage: Matutuine district 96 %
irs_cov_no_loss_Acte = rep(0.96,30*6)
irs_cov_Acte = rep(true_cover_irs_Acte,each=30)

prop_mod_Sumi = 1 - c(3.5,5,5.3,5.4,5.8,5.8)/100
true_cover_irs_Sumi = c(0.95*prop_mod_Sumi[1],
                        0.95*prop_mod_Sumi[1]*prop_mod_Sumi[2],
                        0.95*prop_mod_Sumi[1]*prop_mod_Sumi[2]*prop_mod_Sumi[3],
                        0.95*prop_mod_Sumi[1]*prop_mod_Sumi[2]*prop_mod_Sumi[3]*prop_mod_Sumi[4],
                        0.95*prop_mod_Sumi[1]*prop_mod_Sumi[2]*prop_mod_Sumi[3]*prop_mod_Sumi[4]*prop_mod_Sumi[5],
                        0.95*prop_mod_Sumi[1]*prop_mod_Sumi[2]*prop_mod_Sumi[3]*prop_mod_Sumi[4]*prop_mod_Sumi[5]*prop_mod_Sumi[6])

#House coverage: Boane (sumi) district 97 %, Manhica district (Palmeira) 98 % 
irs_cov_no_loss_Sumi = rep(0.97,30*6)
irs_cov_Sumi = rep(true_cover_irs_Sumi,each=30)

plot(irs_cov_no_loss_Acte[1:180] ~ time[1:180],ylab = "Community IRS cover (%)",
     ylim=c(0,1),col="black",pch="",
     main = "",cex.main=1.2,xlim=c(1,200),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,180,30),labels = seq(0,6,1),cex.axis = 1.4)


lines(irs_cov_no_loss_Acte ~ time[1:180],lty=2,lwd=2,col = "darkblue")
lines(irs_cov_Acte ~ time[1:180],lty=4,lwd=2,col = "darkblue")

lines(irs_cov_no_loss_Sumi ~ c(time[1:180]+1),lty=2,lwd=2,col = "aquamarine3")
lines(irs_cov_Sumi ~ time[1:180],lty=4,lwd=2,col = "aquamarine3")

legend("bottomleft",legend = c("Mututuine","Boane","IRS cover, no loss", "IRS cover, observed loss"),
       col = c("darkblue","aquamarine3","black","black"),lwd = 2, lty=c(1,1,2,4),cex=1.2,bty="n")

cov1A = cov1S = cov2A = cov2S = array(dim=c(180,4))

itn_cov_Acte = 0.52
itn_cov_Sumi = 0.85
## Here we are creating a matrix
## with the coverage or use of nets waning with time
## and the coverage of IRS either staying fixed, or also waning 
## when walls are washed
cov1A[,1] = 1
cov1A[,2] = itn_cov_Acte ## ITN only
cov1A[,3] = irs_cov_no_loss_Acte ## IRS only
cov1A[,4] = itn_cov_Acte*irs_cov_no_loss_Acte ## both interventions

cov2A[,1] = 1
cov2A[,2] = itn_cov_Acte ## ITN only
cov2A[,3] = irs_cov_Acte ## IRS only
cov2A[,4] = itn_cov_Acte*irs_cov_Acte## both interventions

cov1S[,1] = 1
cov1S[,2] = itn_cov_Sumi ## ITN only
cov1S[,3] = irs_cov_no_loss_Sumi ## IRS only
cov1S[,4] = itn_cov_Sumi*irs_cov_no_loss_Sumi ## both interventions

cov2S[,1] = 1
cov2S[,2] = itn_cov_Sumi ## ITN only
cov2S[,3] = irs_cov_Sumi ## IRS only
cov2S[,4] = itn_cov_Sumi*irs_cov_Sumi ## both interventions

# plot(cov1[,1] ~ time,ylim=c(0,1),pch="",
#      ylab = "Intervention use (%)",
#      col="black",
#      main = "",cex.main=1.2,xlim=c(1,365),xaxt="n",
#      xlab="Time in days",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
# axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
# axis(1,at=seq(0,365,120),cex.lab=1.4,cex.axis=1.4)
# 
# colsd=c("grey","purple","aquamarine3","blue")
# for(i in 1:3){
#   lines(cov1[,i] ~ time,col=colsd[i])
#   lines(cov2[,i] ~ time,col=colsd[i],lty=2)
# }
# 
## Entomological model parameters to estimate 

Q0 = 0.92  ## this is anthropophagy - we can use human blood index
chi = 0.86 ## this is endophily (pi_i)

fv0 = 0.333 ## biting rate 1 bite every 3 days
tau1 = 0.69 ## duration of host seeking, assumed to be constant between species (delta_10, altered to delta_1 when interventions used)
tau2 = 1/fv0-tau1  ## indoor feeding endophagy (delta_2)
av0 = Q0*fv0
mu0 = 0.132 ## background mortality from external sources
p10 = exp(-mu0*tau1) ## 
p2 = exp(-mu0*tau2)  ## probability of surviving resting period in absence of intervntion


## These are the adjusted w, z, when coverage is changing
## so these are the intervention coverages
zhi1A = whi1A = zhi2A = whi2A = array(dim=c(180,4))
zhi1S = whi1S = zhi2S = whi2S = array(dim=c(180,4))
zhi1A=cov1A*z_Acte[1:180,]
whi1A=cov1A*w_Acte[1:180,]

zhi1S=cov1S*z_Sumi[1:180,]
whi1S=cov1S*w_Sumi[1:180,]


zhi2A=cov2A*z_Acte[1:180,]
whi2A=cov2A*w_Acte[1:180,]

zhi2S=cov2S*z_Sumi[1:180,]
whi2S=cov2S*w_Sumi[1:180,]

# zhi1[,4]=cov1[,2]*z[,2] * (1 - cov1[,3]*z[,3]) + cov1[,3]*z[,3]
# zhi2[,4]=cov2[,2]*z[,2] * (1 - cov2[,3]*z[,3]) + cov2[,3]*z[,3]
# 
# whi1[,4]=cov1[,2]*w[,2] * (1 - cov1[,3]*w[,3]) + cov1[,3]*w[,3]
# whi2[,4]=cov2[,2]*w[,2] * (1 - cov2[,3]*w[,3]) + cov2[,3]*w[,3]

zbar1A = wbar1A = zbar2A = wbar2A = array(dim=c(180,4)) 
zbar1S = wbar1S = zbar2S = wbar2S = array(dim=c(180,4)) 
for(i in 1:4){
  zbar1A[,i] = Q0*zhi1A[,i]
  wbar1A[,i] = (1 - Q0) + Q0*whi1A[,i]
  zbar2A[,i] = Q0*zhi2A[,i]
  wbar2A[,i] = (1 - Q0) + Q0*whi2A[,i]
  
  zbar1S[,i] = Q0*zhi1S[,i]
  wbar1S[,i] = (1 - Q0) + Q0*whi1S[,i]
  zbar2S[,i] = Q0*zhi2S[,i]
  wbar2S[,i] = (1 - Q0) + Q0*whi2S[,i]
}

## From Walker et al 2016
## Mosquito feeding rate (tau1 is delta10, tau2 is delta2 in the methods)
fR1A = 1 / ((tau1/(1 - zbar1A)) + tau2)
mu1A = -fR1A*log((wbar1A*p10/(1 - zbar1A*p10))*p2) 
Q1A = 1 - (1-Q0)/wbar1A

fR2A = 1 / ((tau1/(1 - zbar2A)) + tau2)
mu2A = -fR2A*log((wbar2A*p10/(1 - zbar2A*p10))*p2) 
Q2A = 1 - (1-Q0)/wbar2A


fR1S = 1 / ((tau1/(1 - zbar1S)) + tau2)
mu1S = -fR1S*log((wbar1S*p10/(1 - zbar1S*p10))*p2) 
Q1S = 1 - (1-Q0)/wbar1S

fR2S = 1 / ((tau1/(1 - zbar2S)) + tau2)
mu2S = -fR2S*log((wbar2S*p10/(1 - zbar2S*p10))*p2) 
Q2S = 1 - (1-Q0)/wbar2S


## Rate at which a person in the popn is bitten by mosquitoes is
lambda1A = lambda2A = array(dim = c(180,4))
lambda1S = lambda2S = array(dim = c(180,4))
for(i in 1:4){
  lambda1A = (Q1A*fR1A*yy_Acte[,i])/whi1A[,i]
  lambda2A = (Q2A*fR2A*yy_Acte[,i])/whi2A[,i]
  
  lambda1S = (Q1S*fR1S*yy_Sumi[,i])/whi1S[,i]
  lambda2S = (Q2S*fR2S*yy_Sumi[,i])/whi2S[,i]
}



## Actually we want to look at the comparison so:
plot(lambda1A[1:180,1] ~ time[1:180],ylim=c(0,2.5),pch="",
     ylab = "Mosquito bites received per person per day",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,180),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,2.5,0.5),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,180,30),labels=seq(0,6,1),cex.lab=1.4,cex.axis=1.4)

for(i in 4){
  lines(lambda1A[1:180,i] ~ time[1:180],col="darkblue",lty=1,lwd=2)
  lines(lambda2A[1:180,i] ~ time[1:180],col="darkblue",lty=2,lwd=2)
  
  lines(lambda1S[1:180,i] ~ time[1:180],col="aquamarine3",lty=1,lwd=2)
  lines(lambda2S[1:180,i] ~ time[1:180],col="aquamarine3",lty=2,lwd=2)
  
}
for(i in 3){
  lines(lambda1A[1:180,i] ~ time[1:180],col="darkblue",lty=1,lwd=1)
  lines(lambda2A[1:180,i] ~ time[1:180],col="darkblue",lty=2,lwd=1)
  
  lines(lambda1S[1:180,i] ~ time[1:180],col="aquamarine3",lty=1,lwd=1)
  lines(lambda2S[1:180,i] ~ time[1:180],col="aquamarine3",lty=2,lwd=1)
  
}

# for(i in 1:4) lines(yy[1:180,i]~time[1:180],col=colsd[i],lwd=2)
# for(i in 1:4) lines(fR1[,i]~time[1:180],col=colsd[i],lwd=2)
# for(i in 1:4) lines(Q1[,i]~time[1:180],col=colsd[i],lwd=2,lty=2)

legend("topleft",legend = c("Matutuine (assuming no ITN)","Boane (assuming no ITN)",
                            "Matutuine (ITN use 52%)","Boane (ITN use 85%)",
                            "IRS no household modification",
                            "IRS with household modification"),
       col = c("darkblue","aquamarine3","darkblue","aquamarine3","black","black"),lwd = c(1,1,2,2,2,2), lty=c(1,1,1,1,1,2),cex=1.2,bty="n")

## Additional infectious bites per person per year 
Estimated_added_EIR = array(dim=c(180,2))
Estimated_added_EIR[,1] = (lambda2A[,4] - lambda1A[,4])
Estimated_added_EIR[,2] = (lambda2S[,4] - lambda1S[,4])

c(sum(Estimated_added_EIR[1:30,1])/30,sum(Estimated_added_EIR[31:60,1])/30,
  sum(Estimated_added_EIR[61:90,1])/30,sum(Estimated_added_EIR[91:120,1])/30,
  sum(Estimated_added_EIR[121:150,1])/30,sum(Estimated_added_EIR[151:180,1])/30)

c(sum(Estimated_added_EIR[1:30,2])/30,sum(Estimated_added_EIR[31:60,2])/30,
  sum(Estimated_added_EIR[61:90,2])/30,sum(Estimated_added_EIR[91:120,2])/30,
  sum(Estimated_added_EIR[121:150,2])/30,sum(Estimated_added_EIR[151:180,2])/30)


## Additional infectious bites per person per year 
Estimated_propn_increase_EIR = array(dim=c(180,2))
Estimated_propn_increase_EIR[,1] = (lambda2A[,4] - lambda1A[,4])/lambda2A[,4]
Estimated_propn_increase_EIR[,2] = (lambda2S[,4] - lambda1S[,4])/lambda2S[,4]


plot(Estimated_propn_increase_EIR[1:180,1] ~ time[1:180],ylim=c(0,1),pch="",
     ylab = "",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,180),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
mtext(side=2, line =4,
      "Relative increase in daily bites ")
mtext(side=2, line =2.7, "due to modifications (%)")
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)

axis(1,at=seq(0,180,30),labels=seq(0,6,1),cex.lab=1.4,cex.axis=1.4)

colsd = c("darkblue","aquamarine3")
for(i in 1:2){
  lines(Estimated_propn_increase_EIR[,i] ~ time[1:180],col=colsd[i],lty=2,lwd=2)
}


Estimated_added_EIR = array(dim=c(180,2))
Estimated_added_EIR[,1] = (lambda2A[,3] - lambda1A[,3])
Estimated_added_EIR[,2] = (lambda2S[,3] - lambda1S[,3])

## Additional infectious bites per person per year 
Estimated_propn_increase_EIR = array(dim=c(180,2))
Estimated_propn_increase_EIR[,1] = (lambda2A[,3] - lambda1A[,3])/lambda2A[,3]
Estimated_propn_increase_EIR[,2] = (lambda2S[,3] - lambda1S[,3])/lambda2S[,3]

for(i in 1:2){
  lines(Estimated_propn_increase_EIR[,i] ~ time[1:180],col=colsd[i],lty=1,lwd=1)
}

mean(Estimated_propn_increase_EIR[1:30,1])
mean(Estimated_propn_increase_EIR[31:60,1])
mean(Estimated_propn_increase_EIR[61:92,1])
mean(Estimated_propn_increase_EIR[93:122,1])
mean(Estimated_propn_increase_EIR[123:153,1])
mean(Estimated_propn_increase_EIR[154:180,1])


legend("topleft",legend = c("Matutuine (assuming no ITN)","Boane (assuming no ITN)",
                            "Matutuine (ITN use 52%)","Boane (ITN use 85%)"),
       col = c("darkblue","aquamarine3","darkblue","aquamarine3"),lwd = c(2,2,1,1), lty=c(1,1,2,2),cex=1.2,bty="n")



####################################
##
## 4 Work out the probability that a feeding attempt by mosquito from species i ends in blood feeding on a person 
##   whilst considering the prolonged nature of application campaigns
##
####################################################

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

w_Acte = yy_Acte = z_Acte = w_Sumi = yy_Sumi = z_Sumi = array(dim=c(180,4)) 


w_Acte1 = yy_Acte1 = z_Acte1 = w_Sumi1 = yy_Sumi1 = z_Sumi1 = array(dim=c(180,4)) 
w_Acte2 = yy_Acte2 = z_Acte2 = w_Sumi2 = yy_Sumi2 = z_Sumi2 = array(dim=c(180,4)) 
w_Acte3 = yy_Acte3 = z_Acte3 = w_Sumi3 = yy_Sumi3 = z_Sumi3 = array(dim=c(180,4)) 

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

ksA1 = actellic_details[[2]]
lsA1 = actellic_details[[3]]
jsA1 = 1 - actellic_details[[2]] - actellic_details[[3]]

ksA2 = c(rep(k0,30),actellic_details[[2]][1:335])
lsA2 = c(rep(0,30),actellic_details[[3]][1:335])
jsA2 = 1 - ksA2 - lsA2

ksA3 = c(rep(k0,61),actellic_details[[2]][1:304])
lsA3 = c(rep(0,61),actellic_details[[3]][1:304])
jsA3 = 1 - ksA3 - lsA3

s_IRS_Acte1 = ksA1/k0 ##feed2 = when IRS is implemented in month 1 (Nov)
r_IRS_Acte1 = (1 - ksA1/k0)*(jsA1/(lsA1+jsA1)) ##rep2 

s_IRS_Acte2 = ksA2/k0 ##feed2 = when IRS is implemented in month 2 (Dec)
r_IRS_Acte2 = (1 - ksA2/k0)*(jsA2/(lsA2+jsA2)) ##rep2 

s_IRS_Acte3 = ksA3/k0 ##feed2 = when IRS is implemented in month 3 (Jan)
r_IRS_Acte3 = (1 - ksA3/k0)*(jsA3/(lsA3+jsA3)) ##rep2 

ksS = sumishield_details[[2]]
lsS = sumishield_details[[3]]
jsS = 1 - sumishield_details[[2]] - sumishield_details[[3]]

ksS1 = sumishield_details[[2]]
lsS1 = sumishield_details[[3]]
jsS1 = 1 - sumishield_details[[2]] - sumishield_details[[3]]

ksS2 = c(rep(k0,30),sumishield_details[[2]][1:335])
lsS2 = c(rep(k0,30),sumishield_details[[3]][1:335])
jsS2 = 1 - ksS2 - lsS2

ksS3 = c(rep(k0,61),sumishield_details[[2]][1:304])
lsS3 = c(rep(k0,61),sumishield_details[[3]][1:304])
jsS3 = 1 - ksS3 - lsS3

s_IRS_Sumi1 = ksS1/k0 ##feed2
r_IRS_Sumi1 = (1 - ksS1/k0)*(jsS1/(lsS1+jsS1)) ##rep2

s_IRS_Sumi2 = ksS2/k0 ##feed2
r_IRS_Sumi2 = (1 - ksS2/k0)*(jsS2/(lsS2+jsS2)) ##rep2

s_IRS_Sumi3 = ksS3/k0 ##feed2
r_IRS_Sumi3 = (1 - ksS3/k0)*(jsS3/(lsS3+jsS3)) ##rep2

w_Acte1[,1] = w_Acte2[,1] = w_Acte3[,1] = #w_Sumi[,1] = 
  w_Sumi1[,1] = w_Sumi2[,1] = w_Sumi3[,1] = 
  rep(1,180) ## Probability that a mosquito bites and survives in the presence of indoor vector control
for(i in 1:180){
  PHI_B = PHI_B_mut
  PHI_I = PHI_I_mut
  w_Acte1[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
  w_Acte1[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte1[i])*s_IRS_Acte1[i]	##			probability of surviving biting given that there is IRS
  w_Acte1[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte1[i])*s_ITN[i]*s_IRS_Acte1[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte1[i])*s_IRS_Acte1[i] ## probability of surviving biting given that there is ITN & IRS
  
  w_Acte2[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
  w_Acte2[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte2[i])*s_IRS_Acte2[i]	##			probability of surviving biting given that there is IRS
  w_Acte2[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte2[i])*s_ITN[i]*s_IRS_Acte2[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte2[i])*s_IRS_Acte2[i] ## probability of surviving biting given that there is ITN & IRS
  
  w_Acte3[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
  w_Acte3[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte3[i])*s_IRS_Acte3[i]	##			probability of surviving biting given that there is IRS
  w_Acte3[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte3[i])*s_ITN[i]*s_IRS_Acte3[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte3[i])*s_IRS_Acte3[i] ## probability of surviving biting given that there is ITN & IRS
}

w_Acte = 
  w_Acte1 * 129/(129+88+27+1) +
  w_Acte2 *  88/(129+88+27+1) +
  w_Acte3 *  27/(129+88+27+1) 



for(i in 1:180){
  PHI_B = PHI_B_boa
  PHI_I = PHI_I_boa
  w_Sumi1[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
  w_Sumi1[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi1[i])*s_IRS_Sumi1[i]	##			probability of surviving biting given that there is IRS
  w_Sumi1[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi1[i])*s_ITN[i]*s_IRS_Sumi1[i] + (PHI_I - PHI_B)*(1-r_IRS_Sumi1[i])*s_IRS_Sumi1[i] ## probability of surviving biting given that there is ITN & IRS
  
  w_Sumi2[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
  w_Sumi2[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi2[i])*s_IRS_Sumi2[i]	##			probability of surviving biting given that there is IRS
  w_Sumi2[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi2[i])*s_ITN[i]*s_IRS_Sumi2[i] + (PHI_I - PHI_B)*(1-r_IRS_Sumi2[i])*s_IRS_Sumi2[i] ## probability of surviving biting given that there is ITN & IRS
  
  w_Sumi3[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
  w_Sumi3[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi3[i])*s_IRS_Sumi3[i]	##			probability of surviving biting given that there is IRS
  w_Sumi3[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi3[i])*s_ITN[i]*s_IRS_Sumi3[i] + (PHI_I - PHI_B)*(1-r_IRS_Sumi3[i])*s_IRS_Sumi3[i] ## probability of surviving biting given that there is ITN & IRS
  
}

w_Sumi = 
  w_Sumi1 * 113/(113+153+76+2) +
  w_Sumi2 * 153/(113+153+76+2) +
  w_Sumi3 *  76/(113+153+76+2) 


# par(mfrow = c(2,3))


## Probability of any bite (if there is IRS, a mosquito may bite and then die immediately afterwards)
yy_Acte[,1] = w_Acte[,1] 
yy_Acte[,2] = w_Acte[,2]

# yy_Sumi[,1] = w_Sumi[,1] 
# yy_Sumi[,2] = w_Sumi[,2]

for(i in 1:180){
  PHI_B = PHI_B_mut
  PHI_I = PHI_I_mut
  yy_Acte1[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte1[i])
  yy_Acte1[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte1[i])*s_ITN[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte1[i])
  
  yy_Acte2[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte2[i])
  yy_Acte2[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte2[i])*s_ITN[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte2[i])
  
  yy_Acte3[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte3[i])
  yy_Acte3[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte3[i])*s_ITN[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte3[i])
}

yy_Acte[,3:4] = 
  yy_Acte1[,3:4] * 129/(129+88+27+1) +
  yy_Acte2[,3:4] *  88/(129+88+27+1) +
  yy_Acte3[,3:4] *  27/(129+88+27+1) 



for(i in 1:180){
  PHI_B = PHI_B_boa
  PHI_I = PHI_I_boa
  yy_Sumi1[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi1[i])
  yy_Sumi1[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi1[i])*s_ITN[i] + (PHI_I - PHI_B)*(1-r_IRS_Sumi1[i])
  
  yy_Sumi2[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi2[i])
  yy_Sumi2[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi2[i])*s_ITN[i] + (PHI_I - PHI_B)*(1-r_IRS_Sumi2[i])
  
  yy_Sumi3[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi3[i])
  yy_Sumi3[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi3[i])*s_ITN[i] + (PHI_I - PHI_B)*(1-r_IRS_Sumi3[i])
  
}

yy_Sumi[,3:4] = 
  yy_Sumi1[,3:4] * 113/(113+153+76+2) +
  yy_Sumi2[,3:4] * 153/(113+153+76+2) +
  yy_Sumi3[,3:4] *  76/(113+153+76+2) 



## Probability repelled
z_Acte[,1] = 0
z_Sumi[,1] = 0

for(i in 1:180){
  z_Acte1[i,2] = PHI_B*r_ITN[i]
  z_Acte1[i,3] = PHI_I*r_IRS_Acte1[i]
  z_Acte1[i,4] = PHI_B*(r_IRS_Acte1[i] + (1-r_IRS_Acte1[i])*r_ITN[i]) + (PHI_I - PHI_B)*r_IRS_Acte1[i]
  
  z_Acte2[i,2] = PHI_B*r_ITN[i]
  z_Acte2[i,3] = PHI_I*r_IRS_Acte2[i]
  z_Acte2[i,4] = PHI_B*(r_IRS_Acte2[i] + (1-r_IRS_Acte2[i])*r_ITN[i]) + (PHI_I - PHI_B)*r_IRS_Acte2[i]
  
  z_Acte3[i,2] = PHI_B*r_ITN[i]
  z_Acte3[i,3] = PHI_I*r_IRS_Acte3[i]
  z_Acte3[i,4] = PHI_B*(r_IRS_Acte3[i] + (1-r_IRS_Acte3[i])*r_ITN[i]) + (PHI_I - PHI_B)*r_IRS_Acte3[i]
  
  z_Sumi1[i,2] = PHI_B*r_ITN[i]
  z_Sumi1[i,3] = PHI_I*r_IRS_Sumi1[i]
  z_Sumi1[i,4] = PHI_B*(r_IRS_Sumi1[i] + (1-r_IRS_Sumi1[i])*r_ITN[i]) + (PHI_I - PHI_B)*r_IRS_Sumi1[i]
  
  z_Sumi2[i,2] = PHI_B*r_ITN[i]
  z_Sumi2[i,3] = PHI_I*r_IRS_Sumi2[i]
  z_Sumi2[i,4] = PHI_B*(r_IRS_Sumi2[i] + (1-r_IRS_Sumi2[i])*r_ITN[i]) + (PHI_I - PHI_B)*r_IRS_Sumi2[i]
  
  z_Sumi3[i,2] = PHI_B*r_ITN[i]
  z_Sumi3[i,3] = PHI_I*r_IRS_Sumi3[i]
  z_Sumi3[i,4] = PHI_B*(r_IRS_Sumi3[i] + (1-r_IRS_Sumi3[i])*r_ITN[i]) + (PHI_I - PHI_B)*r_IRS_Sumi3[i]
  
}

z_Acte[,2:4] = 
  z_Acte1[,2:4] * 129/(129+88+27+1) +
  z_Acte2[,2:4] *  88/(129+88+27+1) +
  z_Acte3[,2:4] *  27/(129+88+27+1) 

z_Sumi[,2:4] = 
  z_Sumi1[,2:4] * 113/(113+153+76+2) +
  z_Sumi2[,2:4] * 153/(113+153+76+2) +
  z_Sumi3[,2:4] *  76/(113+153+76+2) 



## waning usage of IRS with time
## as well as altered cover over time from 3 month time line of campaign

modif_months = 1:6
prop_mod_Acte = 1 - c(13.1,8.8,13.7,12.6,8.6,8.3)/100
true_cover_irs_Acte = c(prop_mod_Acte[1]*129/(129+88+27+1),
                        prop_mod_Acte[1]*(129+88)/(129+88+27+1)*prop_mod_Acte[2],
                        prop_mod_Acte[1]*(129+88+27)/(129+88+27+1)*prop_mod_Acte[2]*prop_mod_Acte[3],
                        prop_mod_Acte[1]*(129+88+27)/(129+88+27+1)*prop_mod_Acte[2]*prop_mod_Acte[3]*prop_mod_Acte[4],
                        prop_mod_Acte[1]*(129+88+27)/(129+88+27+1)*prop_mod_Acte[2]*prop_mod_Acte[3]*prop_mod_Acte[4]*prop_mod_Acte[5],
                        prop_mod_Acte[1]*(129+88+27)/(129+88+27+1)*prop_mod_Acte[2]*prop_mod_Acte[3]*prop_mod_Acte[4]*prop_mod_Acte[5]*prop_mod_Acte[6])

#House coverage: Matutuine district 96 %
irs_cov_no_loss_Acte = rep(0.96,30*6)
irs_cov_Acte = rep(true_cover_irs_Acte,each=30)

prop_mod_Sumi = 1 - c(3.5,5,5.3,5.4,5.8,5.8)/100
true_cover_irs_Sumi = c(113/(113+153+76+2)*prop_mod_Sumi[1],
                        (113+153)/(113+153+76+2)*prop_mod_Sumi[1]*prop_mod_Sumi[2],
                        (113+153+76)/(113+153+76+2)*prop_mod_Sumi[1]*prop_mod_Sumi[2]*prop_mod_Sumi[3],
                        (113+153+76)/(113+153+76+2)*prop_mod_Sumi[1]*prop_mod_Sumi[2]*prop_mod_Sumi[3]*prop_mod_Sumi[4],
                        (113+153+76)/(113+153+76+2)*prop_mod_Sumi[1]*prop_mod_Sumi[2]*prop_mod_Sumi[3]*prop_mod_Sumi[4]*prop_mod_Sumi[5],
                        (113+153+76)/(113+153+76+2)*prop_mod_Sumi[1]*prop_mod_Sumi[2]*prop_mod_Sumi[3]*prop_mod_Sumi[4]*prop_mod_Sumi[5]*prop_mod_Sumi[6])

#House coverage: Boane (sumi) district 97 %, Manhica district (Palmeira) 98 % 
irs_cov_no_loss_Sumi = rep(0.97,30*6)
irs_cov_Sumi = rep(true_cover_irs_Sumi,each=30)

plot(irs_cov_no_loss_Acte ~ time[1:180],ylab = "Community IRS cover (%)",
     ylim=c(0,1),col="black",pch="",
     main = "",cex.main=1.2,xlim=c(1,200),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,180,30),labels = seq(0,6,1),cex.axis = 1.4)


lines(irs_cov_no_loss_Acte ~ time[1:180],lty=2,lwd=2,col = "darkblue")
lines(irs_cov_Acte ~ time[1:180],lty=4,lwd=2,col = "darkblue")

lines(irs_cov_no_loss_Sumi ~ c(time[1:180]+1),lty=2,lwd=2,col = "aquamarine3")
lines(irs_cov_Sumi ~ time[1:180],lty=4,lwd=2,col = "aquamarine3")

# legend("bottomleft",legend = c("Mututuine","Boane","IRS cover, no loss", "IRS cover, observed loss"),
#        col = c("darkblue","aquamarine3","black","black"),lwd = 2, lty=c(1,1,2,4),cex=1.2,bty="n")

cov1A = cov1S = cov2A = cov2S = array(dim=c(180,4))

itn_cov_Acte = 0.52
itn_cov_Sumi = 0.85
## Here we are creating a matrix
## with the coverage or use of nets waning with time
## and the coverage of IRS either staying fixed, or also waning 
## when walls are washed
cov1A[,1] = 1
cov1A[,2] = itn_cov_Acte ## ITN only
cov1A[,3] = irs_cov_no_loss_Acte ## IRS only
cov1A[,4] = itn_cov_Acte*irs_cov_no_loss_Acte ## both interventions

cov2A[,1] = 1
cov2A[,2] = itn_cov_Acte ## ITN only
cov2A[,3] = irs_cov_Acte ## IRS only
cov2A[,4] = itn_cov_Acte*irs_cov_Acte## both interventions

cov1S[,1] = 1
cov1S[,2] = itn_cov_Sumi ## ITN only
cov1S[,3] = irs_cov_no_loss_Sumi ## IRS only
cov1S[,4] = itn_cov_Sumi*irs_cov_no_loss_Sumi ## both interventions

cov2S[,1] = 1
cov2S[,2] = itn_cov_Sumi ## ITN only
cov2S[,3] = irs_cov_Sumi ## IRS only
cov2S[,4] = itn_cov_Sumi*irs_cov_Sumi ## both interventions

# plot(cov1[,1] ~ time,ylim=c(0,1),pch="",
#      ylab = "Intervention use (%)",
#      col="black",
#      main = "",cex.main=1.2,xlim=c(1,365),xaxt="n",
#      xlab="Time in days",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
# axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
# axis(1,at=seq(0,365,120),cex.lab=1.4,cex.axis=1.4)
# 
# colsd=c("grey","purple","aquamarine3","blue")
# for(i in 1:3){
#   lines(cov1[,i] ~ time,col=colsd[i])
#   lines(cov2[,i] ~ time,col=colsd[i],lty=2)
# }
# 
## Entomological model parameters to estimate 

Q0 = 0.92  ## this is anthropophagy - we can use human blood index
chi = 0.86 ## this is endophily (pi_i)

fv0 = 0.333 ## biting rate 1 bite every 3 days
tau1 = 0.69 ## duration of host seeking, assumed to be constant between species (delta_10, altered to delta_1 when interventions used)
tau2 = 1/fv0-tau1  ## indoor feeding endophagy (delta_2)
av0 = Q0*fv0
mu0 = 0.132 ## background mortality from external sources
p10 = exp(-mu0*tau1) ## 
p2 = exp(-mu0*tau2)  ## probability of surviving resting period in absence of intervntion


## These are the adjusted w, z, when coverage is changing
## so these are the intervention coverages
zhi1A = whi1A = zhi2A = whi2A = array(dim=c(180,4))
zhi1S = whi1S = zhi2S = whi2S = array(dim=c(180,4))
zhi1A=cov1A*z_Acte[1:180,]
whi1A=cov1A*w_Acte[1:180,]

zhi1S=cov1S*z_Sumi[1:180,]
whi1S=cov1S*w_Sumi[1:180,]


zhi2A=cov2A*z_Acte[1:180,]
whi2A=cov2A*w_Acte[1:180,]

zhi2S=cov2S*z_Sumi[1:180,]
whi2S=cov2S*w_Sumi[1:180,]

# zhi1[,4]=cov1[,2]*z[,2] * (1 - cov1[,3]*z[,3]) + cov1[,3]*z[,3]
# zhi2[,4]=cov2[,2]*z[,2] * (1 - cov2[,3]*z[,3]) + cov2[,3]*z[,3]
# 
# whi1[,4]=cov1[,2]*w[,2] * (1 - cov1[,3]*w[,3]) + cov1[,3]*w[,3]
# whi2[,4]=cov2[,2]*w[,2] * (1 - cov2[,3]*w[,3]) + cov2[,3]*w[,3]

zbar1A = wbar1A = zbar2A = wbar2A = array(dim=c(180,4)) 
zbar1S = wbar1S = zbar2S = wbar2S = array(dim=c(180,4)) 
for(i in 1:4){
  zbar1A[,i] = Q0*zhi1A[,i]
  wbar1A[,i] = (1 - Q0) + Q0*whi1A[,i]
  zbar2A[,i] = Q0*zhi2A[,i]
  wbar2A[,i] = (1 - Q0) + Q0*whi2A[,i]
  
  zbar1S[,i] = Q0*zhi1S[,i]
  wbar1S[,i] = (1 - Q0) + Q0*whi1S[,i]
  zbar2S[,i] = Q0*zhi2S[,i]
  wbar2S[,i] = (1 - Q0) + Q0*whi2S[,i]
}

## From Walker et al 2016
## Mosquito feeding rate (tau1 is delta10, tau2 is delta2 in the methods)
fR1A = 1 / ((tau1/(1 - zbar1A)) + tau2)
mu1A = -fR1A*log((wbar1A*p10/(1 - zbar1A*p10))*p2) 
Q1A = 1 - (1-Q0)/wbar1A

fR2A = 1 / ((tau1/(1 - zbar2A)) + tau2)
mu2A = -fR2A*log((wbar2A*p10/(1 - zbar2A*p10))*p2) 
Q2A = 1 - (1-Q0)/wbar2A


fR1S = 1 / ((tau1/(1 - zbar1S)) + tau2)
mu1S = -fR1S*log((wbar1S*p10/(1 - zbar1S*p10))*p2) 
Q1S = 1 - (1-Q0)/wbar1S

fR2S = 1 / ((tau1/(1 - zbar2S)) + tau2)
mu2S = -fR2S*log((wbar2S*p10/(1 - zbar2S*p10))*p2) 
Q2S = 1 - (1-Q0)/wbar2S

# plot(mu1[,1] ~ time[1:180],ylim=c(0,1),pch="",
#      ylab = "Intervention induced mortality",
#      col="black",
#      main = "",cex.main=1.2,xlim=c(1,365),xaxt="n",
#      xlab="Time in days",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
# axis(2,las=2,at=seq(0,2.5,0.5),cex.lab=1.4,cex.axis=1.4)
# axis(1,at=seq(0,365,120),cex.lab=1.4,cex.axis=1.4)
# 
# colsd=c("grey","purple","aquamarine3","blue")
# 
# for(i in 1:4){ 
#   lines(mu1[,i] ~ time[1:180],col=colsd[i],lty=1,lwd=2)
#   lines(mu2[,i] ~ time[1:180],col=colsd[i],lty=2,lwd=2)
#   }

## Rate at which a person in the popn is bitten by mosquitoes is
lambda1A = lambda2A = array(dim = c(180,4))
lambda1S = lambda2S = array(dim = c(180,4))
for(i in 1:4){
  lambda1A = (Q1A*fR1A*yy_Acte[,i])/whi1A[,i]
  lambda2A = (Q2A*fR2A*yy_Acte[,i])/whi2A[,i]
  
  lambda1S = (Q1S*fR1S*yy_Sumi[,i])/whi1S[,i]
  lambda2S = (Q2S*fR2S*yy_Sumi[,i])/whi2S[,i]
}


# plot(lambda1A[,1] ~ time[1:180],ylim=c(0,2.5),pch="",
#      ylab = "Entomological innoculation rate",
#      col="black",
#      main = "",cex.main=1.2,xlim=c(1,180),xaxt="n",
#      xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
# axis(2,las=2,at=seq(0,2.5,0.5),cex.lab=1.4,cex.axis=1.4)
# axis(1,at=seq(0,180,30),labels=seq(0,6,1),cex.lab=1.4,cex.axis=1.4)
# 
# colsd=c("grey","purple","aquamarine3","blue")
# ltyd = c(1,2,1,2)
# # for(i in 3:4){
# #   lines(lambda1A[,i] ~ time[1:180],col="darkblue",lty = ltyd[i],lwd=1)
# #   lines(lambda1S[,i] ~ time[1:180],col="aquamarine3",lty=ltyd[i],lwd=1)
# # }
# for(i in 3:4){
#   lines(lambda2A[,i] ~ time[1:180],col="darkblue",lty=ltyd[i],lwd=2)
#   lines(lambda2S[,i] ~ time[1:180],col="aquamarine3",lty=ltyd[i],lwd=2)
#   
# }

## Actually we want to look at the comparison so:
plot(lambda1A[,1] ~ time[1:180],ylim=c(0,2.5),pch="",
     ylab = "Mosquito bites received per person per day",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,180),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,2.5,0.5),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,180,30),labels=seq(0,6,1),cex.lab=1.4,cex.axis=1.4)

for(i in 4){
  lines(lambda1A[,i] ~ time[1:180],col="darkblue",lty=1,lwd=2)
  lines(lambda2A[,i] ~ time[1:180],col="darkblue",lty=2,lwd=2)
  
  lines(lambda1S[,i] ~ time[1:180],col="aquamarine3",lty=1,lwd=2)
  lines(lambda2S[,i] ~ time[1:180],col="aquamarine3",lty=2,lwd=2)
  
}
for(i in 3){
  lines(lambda1A[,i] ~ time[1:180],col="darkblue",lty=1,lwd=1)
  lines(lambda2A[,i] ~ time[1:180],col="darkblue",lty=2,lwd=1)
  
  lines(lambda1S[,i] ~ time[1:180],col="aquamarine3",lty=1,lwd=1)
  lines(lambda2S[,i] ~ time[1:180],col="aquamarine3",lty=2,lwd=1)
  
}

# for(i in 1:4) lines(yy[1:180,i]~time[1:180],col=colsd[i],lwd=2)
# for(i in 1:4) lines(fR1[,i]~time[1:180],col=colsd[i],lwd=2)
# for(i in 1:4) lines(Q1[,i]~time[1:180],col=colsd[i],lwd=2,lty=2)
# 
# legend("topleft",legend = c("Matutuine (assuming no ITN)","Boane (assuming no ITN)",
#                             "Matutuine (ITN use 52%)","Boane (ITN use 85%)","IRS no household modification", "IRS with household modification"),
#        col = c("darkblue","aquamarine3","darkblue","aquamarine3","black","black"),lwd = c(1,1,2,2,2,2), lty=c(1,1,1,1,1,2),cex=1.2,bty="n")

## Additional infectious bites per person per year 
Estimated_added_EIR = array(dim=c(180,2))
Estimated_added_EIR[,1] = (lambda2A[,4] - lambda1A[,4])
Estimated_added_EIR[,2] = (lambda2S[,4] - lambda1S[,4])

c(sum(Estimated_added_EIR[1:30,1])/30,sum(Estimated_added_EIR[31:60,1])/30,
  sum(Estimated_added_EIR[61:90,1])/30,sum(Estimated_added_EIR[91:120,1])/30,
  sum(Estimated_added_EIR[121:150,1])/30,sum(Estimated_added_EIR[151:180,1])/30)

c(sum(Estimated_added_EIR[1:30,2])/30,sum(Estimated_added_EIR[31:60,2])/30,
  sum(Estimated_added_EIR[61:90,2])/30,sum(Estimated_added_EIR[91:120,2])/30,
  sum(Estimated_added_EIR[121:150,2])/30,sum(Estimated_added_EIR[151:180,2])/30)


## Additional infectious bites per person per year 
Estimated_propn_increase_EIR = array(dim=c(180,2))
Estimated_propn_increase_EIR[,1] = (lambda2A[,4] - lambda1A[,4])/lambda2A[,4]
Estimated_propn_increase_EIR[,2] = (lambda2S[,4] - lambda1S[,4])/lambda2S[,4]

mean(Estimated_propn_increase_EIR[1:30,1])
mean(Estimated_propn_increase_EIR[31:60,1])
mean(Estimated_propn_increase_EIR[61:92,1])
mean(Estimated_propn_increase_EIR[93:122,1])
mean(Estimated_propn_increase_EIR[123:153,1])
mean(Estimated_propn_increase_EIR[154:180,1])

plot(Estimated_propn_increase_EIR[,1] ~ time[1:180],ylim=c(0,1),pch="",
     ylab = "",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,180),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,180,30),labels=seq(0,6,1),cex.lab=1.4,cex.axis=1.4)
mtext(side=2, line =4,
      "Relative increase in daily bites")
mtext(side=2, line =2.7, "due to spray campaign & modifications (%)")

colsd = c("darkblue","aquamarine3")
for(i in 1:2){
  lines(Estimated_propn_increase_EIR[,i] ~ time[1:180],col=colsd[i],lty=2,lwd=2)
}


Estimated_added_EIR = array(dim=c(180,2))
Estimated_added_EIR[,1] = (lambda2A[,3] - lambda1A[,3])
Estimated_added_EIR[,2] = (lambda2S[,3] - lambda1S[,3])

## Additional infectious bites per person per year 
Estimated_propn_increase_EIR = array(dim=c(180,2))
Estimated_propn_increase_EIR[,1] = (lambda2A[,3] - lambda1A[,3])/lambda2A[,3]
Estimated_propn_increase_EIR[,2] = (lambda2S[,3] - lambda1S[,3])/lambda2S[,3]

for(i in 1:2){
  lines(Estimated_propn_increase_EIR[,i] ~ time[1:180],col=colsd[i],lty=1,lwd=1)
}

# 
# legend("topleft",legend = c("Matutuine (assuming no ITN)","Boane (assuming no ITN)",
#                             "Matutuine (ITN use 52%)","Boane (ITN use 85%)"),
#        col = c("darkblue","aquamarine3","darkblue","aquamarine3"),lwd = c(2,2,1,1), lty=c(1,1,2,2),cex=1.2,bty="n")


par(xpd=NA,cex = 1.11)
# 
# text(x = -585, y = 2.75,"(A)")
# text(x = -315, y = 2.75,"(B)")
# text(x = -50, y = 2.75,"(C)")
# 
# text(x = -585, y = 1.1,"(D)")
# text(x = -315, y = 1.1,"(E)")
# text(x = -50, y = 1.1,"(F)")
# 

text(x = -490, y = 2.70,"(A)")
text(x = -270, y = 2.70,"(B)")
text(x = -28, y = 2.70,"(C)")

text(x = -490, y = 1.2,"(D)")
text(x = -270, y = 1.2,"(E)")
text(x = -28, y = 1.2,"(F)")

## 1300 width and 750 height