####################################
##
## Figure 4
##
####################################################

actellic_details = readRDS("data/actellic_details_v2.Rdata")
sumishield_details = readRDS("data/sumishield_details_v2.Rdata")

time = 1:365
par(mfrow=c(2,3))

## derived ITN/IRS quantities
## prob bites and survives

w_Acte = yy_Acte = z_Acte = 
  w_Sumi = yy_Sumi = z_Sumi = array(dim=c(240,4)) 
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

w_Acte[,1] = w_Sumi[,1] = rep(1,240) ## Probability that a mosquito bites and survives in the presence of indoor vector control
for(i in 1:240){
  PHI_B = PHI_B_mut
  PHI_I = PHI_I_mut
  w_Acte[i,2] = 1 - PHI_B + PHI_B*s_ITN[i+547]				 ## probability of surviving biting given that there is ITN
  w_Acte[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte[i])*s_IRS_Acte[i]	##			probability of surviving biting given that there is IRS
  w_Acte[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte[i])*s_ITN[i+547]*s_IRS_Acte[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte[i])*s_IRS_Acte[i] ## probability of surviving biting given that there is ITN & IRS
}
for(i in 1:240){
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

for(i in 1:240){
  PHI_B = PHI_B_mut
  PHI_I = PHI_I_mut
  yy_Acte[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte[i])
  yy_Acte[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte[i])*s_ITN[i+547] + (PHI_I - PHI_B)*(1-r_IRS_Acte[i])
}
for(i in 1:240){
  PHI_B = PHI_B_boa
  PHI_I = PHI_I_boa
  yy_Sumi[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Sumi[i])
  yy_Sumi[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Sumi[i])*s_ITN[i+547] + (PHI_I - PHI_B)*(1-r_IRS_Sumi[i])
  
}

## Probability repelled
z_Acte[,1] = 0
z_Sumi[,1] = 0

for(i in 1:240){
  z_Acte[i,2] = PHI_B*r_ITN[i+547]
  z_Acte[i,3] = PHI_I*r_IRS_Acte[i]
  z_Acte[i,4] = PHI_B*(r_IRS_Acte[i] + (1-r_IRS_Acte[i])*r_ITN[i+547]) + (PHI_I - PHI_B)*r_IRS_Acte[i]
  
  z_Sumi[i,2] = PHI_B*r_ITN[i+547]
  z_Sumi[i,3] = PHI_I*r_IRS_Sumi[i]
  z_Sumi[i,4] = PHI_B*(r_IRS_Sumi[i] + (1-r_IRS_Sumi[i])*r_ITN[i+547]) + (PHI_I - PHI_B)*r_IRS_Sumi[i]
  
}
## waning usage of IRS with time

## from Table 1 main manuscript 
# prop_mod_Acte = 1 -  c(rep(0,10), ## august &  & oct
#                        rep(14/129,4), ## nov
#                        rep(14/129,4)+rep((12+7)/(117+88),4), ## dec
#                        rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4), ## jan
#                        rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4)+rep((20+10+3)/(116+85+25),4), ## feb
#                        rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4)+rep((20+10+3)/(116+85+25),4)+rep((12+7+1)/(115+85+25),4), ## mar
#                        rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4)+rep((20+10+3)/(116+85+25),4)+rep((12+7+1)/(115+85+25),4)+rep((16+6+1)/(84+25),4), ## apr
#                        rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4)+rep((20+10+3)/(116+85+25),4)+rep((12+7+1)/(115+85+25),4)+rep((16+6+1)/(84+25),4)+rep((4+1)/(81+25),4)) ## may
# 
# prop_mod_Acte
# 
# 
# true_cover_irs_Acte =  rep(prop_mod_Acte*prop_houses_sprayed_WeeklyM,each=7)
# 
# #House coverage: Matutuine district 96 %
# irs_cov_no_loss_Acte = rep(0.96,30*8)
# irs_cov_Acte = rep(prop_mod_Acte*0.96,each=7)
# 
# prop_mod_Sumi = 1 - c(rep(0,10),##aug & sep & oct
#                       rep(4/153,4),#nov
#                       rep(4/153,4)+rep((4+4)/(144+113),4),#dec
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4),#jan
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4)+rep((2+1)/(138+88+75),4),#feb
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4)+rep((2+1)/(138+88+75),4)+rep((2+1)/(137+86+75),4),#mar
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4)+rep((2+1)/(138+88+75),4)+rep((2+1)/(137+86+75),4)+rep(0,8)#apr-may
# )
# true_cover_irs_Sumi = rep(prop_mod_Sumi*prop_houses_sprayed_WeeklyB,each=7)
# irs_cov_Sumi = rep(prop_mod_Sumi*0.97,each=7)

prop_mod_Acte_temp = c(0.891472868,0.81212081,0.639282555,0.481047262,0.386204738,0.304873387,0.166894353)

##And sorted for weeks:
prop_mod_Acte = c(rep(1,10),rep(prop_mod_Acte_temp,each=4))## for time series of spray campaign

true_cover_irs_Acte =  rep(prop_mod_Acte*0.96,each=7)

#House coverage: Matutuine district 96 %
irs_cov_no_loss_Acte = rep(0.96,30*8)
irs_cov_Acte = true_cover_irs_Acte

# prop_mod_Sumi = 1 - c(rep(0,10),##aug & sep & oct
#                       rep(4/153,4),#nov
#                       rep(4/153,4)+rep((4+4)/(144+113),4),#dec
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4),#jan
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4)+rep((2+1)/(138+88+75),4),#feb
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4)+rep((2+1)/(138+88+75),4)+rep((2+1)/(137+86+75),4),#mar
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4)+rep((2+1)/(138+88+75),4)+rep((2+1)/(137+86+75),4)+rep(0,8)#apr-may
# )

prop_mod_Sumi_temp = c(0.96460177,0.966330299,0.945153088,0.923357485,0.917585479,0.904508888,0.895619142)

##And sorted for weeks:
prop_mod_Sumi = c(rep(1,10),rep(prop_mod_Sumi_temp,each=4))## for time series of spray campaign

true_cover_irs_Sumi = rep(prop_mod_Sumi*prop_houses_sprayed_WeeklyB,each=7)

#House coverage: Boane (sumi) district 97 %, Manhica district (Palmeira) 98 % 
irs_cov_no_loss_Sumi = rep(0.97,30*8)
irs_cov_Sumi = rep(prop_mod_Sumi*0.97,each=7)

time = 1:365
plot(irs_cov_no_loss_Acte[61:240] ~ time[61:240],ylab = "Community IRS cover (%)",
     ylim=c(0,1),col="black",pch="",
     main = "",cex.main=1.2,xlim=c(1,240),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,230,30)+15,labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)


lines(irs_cov_no_loss_Acte[61:240] ~ time[61:240],lty=1,lwd=2,col = "darkblue") ## IRS only no loss 
lines(irs_cov_Acte[61:240] ~ time[61:240],lty=3,lwd=2,col = "darkblue") ## IRS with loss

lines(irs_cov_no_loss_Sumi[61:240] ~ c(time[61:240]+1),lty=1,lwd=2,col = "aquamarine3")
lines(irs_cov_Sumi[61:240] ~ time[61:240],lty=3,lwd=2,col = "aquamarine3")

legend("bottomleft",legend = c("Matutuine","Boane","IRS cover, no loss", "IRS cover, observed loss"),
       col = c("darkblue","aquamarine3","black","black"),lwd = 2, lty=c(NA,NA,1,3),
       pch=c(15,15,NA,NA),cex=1.2,bty="n")

cov1A = cov1S = cov2A = cov2S = array(dim=c(240,4))

# itn_cov_Acte = 0.52
# itn_cov_Sumi = 0.73

## Table 1 data
matu_net_cov = c(27.9,  ## nov
                 mean(c(32.6,42.1)), ##dec
                 mean(c(43.4,43.2,33.3)),##jan
                 mean(c(55.0,44.3,48.2)),##feb
                 mean(c(61.2,56.8,40.7)),##mar
                 mean(c(51.9,61.4,66.7)),##apr
                 mean(c(48.9,55.6)),##may
                 29.6)##june

itn_cov_Acte_temp = c(rep(mean(c(matu_net_cov)),10),
                      rep(mean(c(32.6,42.1)),4), ##dec
                      rep(mean(c(43.4,43.2,33.3)),4),##jan
                      rep(mean(c(55.0,44.3,48.2)),4),##feb
                      rep(mean(c(61.2,56.8,40.7)),4),##mar
                      rep(mean(c(51.9,61.4,66.7)),4),##apr
                      rep(mean(c(48.9,55.6)),4),##may
                      rep(29.6,4))##june



boan_net_cov = c(57.5,  ## nov
                 mean(c(64.6,74.5)), ##dec
                 mean(c(68.1,71.9,67.1)),##jan
                 mean(c(62.8,71.2,79)),##feb
                 mean(c(70.8,82.4,86.8)),##mar
                 mean(c(65.5,81.8,81.6)),##apr
                 mean(c(78.4,84.2)),##may
                 10.5)##june

itn_cov_Boan_temp = c(rep(mean(c(boan_net_cov)),10),
                      rep(mean(c(64.6,74.5)),4), ##dec
                      rep(mean(c(68.1,71.9,67.1)),4),##jan
                      rep(mean(c(62.8,71.2,79)),4),##feb
                      rep(mean(c(70.8,82.4,86.8)),4),##mar
                      rep(mean(c(65.5,81.8,81.6)),4),##apr
                      rep(mean(c(78.4,84.2)),4),##may
                      rep(10.5,4))

itn_cov_Acte = rep(itn_cov_Acte_temp,each=7)/100
itn_cov_Sumi = rep(itn_cov_Boan_temp,each=7)/100

## Here we are creating a matrix
## with the coverage or use of nets waning with time
## and the coverage of IRS either staying fixed, or also waning 
## when walls are washed
cov1A[,1] = 1
cov1A[,2] = itn_cov_Acte[1:240] ## ITN only
cov1A[,3] = irs_cov_no_loss_Acte ## IRS only
cov1A[,4] = itn_cov_Acte[1:240]*irs_cov_no_loss_Acte ## both interventions

cov2A[,1] = 1
cov2A[,2] = itn_cov_Acte[1:240] ## ITN only
cov2A[,3] = irs_cov_Acte[1:240] ## IRS only
cov2A[,4] = itn_cov_Acte[1:240]*irs_cov_Acte[1:240]## both interventions

cov1S[,1] = 1
cov1S[,2] = itn_cov_Sumi[1:240] ## ITN only
cov1S[,3] = irs_cov_no_loss_Sumi ## IRS only
cov1S[,4] = itn_cov_Sumi[1:240]*irs_cov_no_loss_Sumi ## both interventions

cov2S[,1] = 1
cov2S[,2] = itn_cov_Sumi[1:240] ## ITN only
cov2S[,3] = irs_cov_Sumi[1:240] ## IRS only
cov2S[,4] = itn_cov_Sumi[1:240]*irs_cov_Sumi[1:240] ## both interventions

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
zhi1A = whi1A = zhi2A = whi2A = array(dim=c(240,4))
zhi1S = whi1S = zhi2S = whi2S = array(dim=c(240,4))
zhi1A=cov1A*z_Acte[1:240,]
whi1A=cov1A*w_Acte[1:240,]

zhi1S=cov1S*z_Sumi[1:240,]
whi1S=cov1S*w_Sumi[1:240,]


zhi2A=cov2A*z_Acte[1:240,]
whi2A=cov2A*w_Acte[1:240,]

zhi2S=cov2S*z_Sumi[1:240,]
whi2S=cov2S*w_Sumi[1:240,]

# zhi1[,4]=cov1[,2]*z[,2] * (1 - cov1[,3]*z[,3]) + cov1[,3]*z[,3]
# zhi2[,4]=cov2[,2]*z[,2] * (1 - cov2[,3]*z[,3]) + cov2[,3]*z[,3]
# 
# whi1[,4]=cov1[,2]*w[,2] * (1 - cov1[,3]*w[,3]) + cov1[,3]*w[,3]
# whi2[,4]=cov2[,2]*w[,2] * (1 - cov2[,3]*w[,3]) + cov2[,3]*w[,3]

zbar1A = wbar1A = zbar2A = wbar2A = array(dim=c(240,4)) 
zbar1S = wbar1S = zbar2S = wbar2S = array(dim=c(240,4)) 
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
lambda1A = lambda2A = array(dim = c(240,4))
lambda1S = lambda2S = array(dim = c(240,4))
for(i in 1:4){
  lambda1A = (Q1A*fR1A*yy_Acte[,i])/whi1A[,i]
  lambda2A = (Q2A*fR2A*yy_Acte[,i])/whi2A[,i]
  
  lambda1S = (Q1S*fR1S*yy_Sumi[,i])/whi1S[,i]
  lambda2S = (Q2S*fR2S*yy_Sumi[,i])/whi2S[,i]
}



## Actually we want to look at the comparison so:
plot(lambda1A[61:240,1] ~ time[61:240],ylim=c(0,2.5),pch="",
     ylab = "Mosquito bites received per person per day",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,240),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,2.5,0.5),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,230,30)+15,labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)

for(i in 4){
  lines(lambda1A[61:235,i] ~ time[61:235],col="darkblue",lty=2,lwd=2) ## Nets and IRS no loss
  lines(lambda2A[61:235,i] ~ time[61:235],col="darkblue",lty=4,lwd=2) ## Nets and IRS with loss
  
  lines(lambda1S[61:235,i] ~ time[61:235],col="aquamarine3",lty=2,lwd=2)
  lines(lambda2S[61:235,i] ~ time[61:235],col="aquamarine3",lty=4,lwd=2)
  
}
for(i in 3){
  lines(lambda1A[61:235,i] ~ time[61:235],col="darkblue",lty=1,lwd=1) ## IRS only no loss
  lines(lambda2A[61:235,i] ~ time[61:235],col="darkblue",lty=3,lwd=1) ## IRS only with loss
  
  lines(lambda1S[61:235,i] ~ time[61:235],col="aquamarine3",lty=1,lwd=1)
  lines(lambda2S[61:235,i] ~ time[61:235],col="aquamarine3",lty=3,lwd=1)
  
}

# for(i in 1:4) lines(yy[1:180,i]~time[1:180],col=colsd[i],lwd=2)
# for(i in 1:4) lines(fR1[,i]~time[1:180],col=colsd[i],lwd=2)
# for(i in 1:4) lines(Q1[,i]~time[1:180],col=colsd[i],lwd=2,lty=2)

legend("topleft",legend = c("Matutuine (LLIN use)",
                            "Boane (LLIN use)",
                            "IRS no modification, no LLIN",
                            "IRS with household modification, no LLIN",
                            "IRS no modification, with LLIN use",
                            "IRS with household modification, with LLIN use"),
       col = c("darkblue","aquamarine3","black","black","black","black"),
       lwd = 1,pch=c(15,15,NA,NA,NA,NA), 
       lty=c(NA,NA,1,3,2,4),cex=1.2,bty="n")

## Additional infectious bites per person per year 
Estimated_added_EIR = array(dim=c(240,2))
Estimated_added_EIR[,1] = (lambda2A[,4] - lambda1A[,4])
Estimated_added_EIR[,2] = (lambda2S[,4] - lambda1S[,4])

c(sum(Estimated_added_EIR[1:30,1])/30,sum(Estimated_added_EIR[31:60,1])/30,
  sum(Estimated_added_EIR[61:90,1])/30,sum(Estimated_added_EIR[91:120,1])/30,
  sum(Estimated_added_EIR[121:150,1])/30,sum(Estimated_added_EIR[151:180,1])/30)

c(sum(Estimated_added_EIR[1:30,2])/30,sum(Estimated_added_EIR[31:60,2])/30,
  sum(Estimated_added_EIR[61:90,2])/30,sum(Estimated_added_EIR[91:120,2])/30,
  sum(Estimated_added_EIR[121:150,2])/30,sum(Estimated_added_EIR[151:180,2])/30)


## Additional infectious bites per person per year 
Estimated_propn_increase_EIR = array(dim=c(240,2))
Estimated_propn_increase_EIR[,1] = (lambda2A[,4] - lambda1A[,4])/lambda2A[,4]
Estimated_propn_increase_EIR[,2] = (lambda2S[,4] - lambda1S[,4])/lambda2S[,4]

c(sum(Estimated_propn_increase_EIR[1:30,1])/30,sum(Estimated_propn_increase_EIR[31:60,1])/30,
  sum(Estimated_propn_increase_EIR[61:90,1])/30,sum(Estimated_propn_increase_EIR[91:120,1])/30,
  sum(Estimated_propn_increase_EIR[121:150,1])/30,sum(Estimated_propn_increase_EIR[151:180,1])/30)

c(sum(Estimated_propn_increase_EIR[1:30,2])/30,sum(Estimated_propn_increase_EIR[31:60,2])/30,
  sum(Estimated_propn_increase_EIR[61:90,2])/30,sum(Estimated_propn_increase_EIR[91:120,2])/30,
  sum(Estimated_propn_increase_EIR[121:150,2])/30,sum(Estimated_propn_increase_EIR[151:180,2])/30)

mean(Estimated_propn_increase_EIR[15:45,1])##sep
mean(Estimated_propn_increase_EIR[46:75,1])##oct
mean(Estimated_propn_increase_EIR[76:105,1])##nov
mean(Estimated_propn_increase_EIR[106:135,1])##dec
mean(Estimated_propn_increase_EIR[136:165,1])##jan
mean(Estimated_propn_increase_EIR[166:195,1])##feb
mean(Estimated_propn_increase_EIR[196:225,1])##mar
mean(Estimated_propn_increase_EIR[226:240,1])##part april

mean(Estimated_propn_increase_EIR[15:45,2])##sep
mean(Estimated_propn_increase_EIR[46:75,2])##oct
mean(Estimated_propn_increase_EIR[76:105,2])##nov
mean(Estimated_propn_increase_EIR[106:135,2])##dec
mean(Estimated_propn_increase_EIR[136:165,2])##jan
mean(Estimated_propn_increase_EIR[166:195,2])##feb
mean(Estimated_propn_increase_EIR[196:225,2])##mar
mean(Estimated_propn_increase_EIR[226:240,2])##part april


plot(Estimated_propn_increase_EIR[61:240,1] ~ time[61:240],ylim=c(0,1),pch="",
     ylab = "",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,240),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
mtext(side=2, line =4.3,
      "Relative increase in daily bites ")
mtext(side=2, line =3, "due to modifications (%)")
axis(2,las=2,at=seq(0,1,0.2),label=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)

axis(1,at=seq(0,230,30)+15,labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)

colsd = c("darkblue","aquamarine3")
for(i in 1:2){
  lines(Estimated_propn_increase_EIR[61:240,i] ~ time[61:240],col=colsd[i],lty=2,lwd=2)
}


Estimated_added_EIR = array(dim=c(240,2))
Estimated_added_EIR[,1] = (lambda2A[,3] - lambda1A[,3])
Estimated_added_EIR[,2] = (lambda2S[,3] - lambda1S[,3])

## Additional infectious bites per person per year 
Estimated_propn_increase_EIR = array(dim=c(240,2))
Estimated_propn_increase_EIR[,1] = (lambda2A[,3] - lambda1A[,3])/lambda2A[,3]
Estimated_propn_increase_EIR[,2] = (lambda2S[,3] - lambda1S[,3])/lambda2S[,3]

for(i in 1:2){
  lines(Estimated_propn_increase_EIR[61:240,i] ~ time[61:240],col=colsd[i],lty=1,lwd=1)
}

mean(Estimated_propn_increase_EIR[1:30,1])
mean(Estimated_propn_increase_EIR[31:60,1])
mean(Estimated_propn_increase_EIR[61:92,1])
mean(Estimated_propn_increase_EIR[93:122,1])
mean(Estimated_propn_increase_EIR[123:153,1])
mean(Estimated_propn_increase_EIR[154:180,1])


legend("topleft",legend = c("Matutuine (assuming no LLIN)","Boane (assuming no LLIN)",
                            "Matutuine (LLIN use)","Boane (LLIN use)"),
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


## derived LLIN/IRS quantities
## prob bites and survives

w_Acte = yy_Acte = z_Acte = w_Sumi = yy_Sumi = z_Sumi = array(dim=c(365,4)) 
w_Acte1 = yy_Acte1 = z_Acte1 = w_Sumi1 = yy_Sumi1 = z_Sumi1 = array(dim=c(365,18,4))
# 
# 
# w_Acte1 = yy_Acte1 = z_Acte1 = w_Sumi1 = yy_Sumi1 = z_Sumi1 = array(dim=c(180,4)) 
# w_Acte2 = yy_Acte2 = z_Acte2 = w_Sumi2 = yy_Sumi2 = z_Sumi2 = array(dim=c(180,4)) 
# w_Acte3 = yy_Acte3 = z_Acte3 = w_Sumi3 = yy_Sumi3 = z_Sumi3 = array(dim=c(180,4)) 
# 
## column 1 will be the effect if there is no intervention
## column 2 is with LLINs only
## column 3 is with IRS only no loss in coverage
## column 4 is with IRS only loss in coverage
## column 5 is LLIN + IRS no loss
## column 6 is LLIN + IRS loss

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

#   w_Acte2[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
#   w_Acte2[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte2[i])*s_IRS_Acte2[i]	##			probability of surviving biting given that there is IRS
#   w_Acte2[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte2[i])*s_ITN[i]*s_IRS_Acte2[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte2[i])*s_IRS_Acte2[i] ## probability of surviving biting given that there is ITN & IRS
#   
#   w_Acte3[i,2] = 1 - PHI_B + PHI_B*s_ITN[i]				 ## probability of surviving biting given that there is ITN
#   w_Acte3[i,3] = 1 - PHI_I + PHI_I*(1-r_IRS_Acte3[i])*s_IRS_Acte3[i]	##			probability of surviving biting given that there is IRS
#   w_Acte3[i,4] = 1 - PHI_I + PHI_B*(1-r_IRS_Acte3[i])*s_ITN[i]*s_IRS_Acte3[i] + (PHI_I - PHI_B)*(1-r_IRS_Acte3[i])*s_IRS_Acte3[i] ## probability of surviving biting given that there is ITN & IRS
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



## waning usage of IRS with time
## as well as altered cover over time from 3 month time line of campaign
prop_houses_sprayed_WeeklyB = 0.97*c(0,	0.027219794,	## August
                                0.077014558,	0.136261919,	0.196901742,	0.250817066, ## Sept
                                0.301047746,	0.347687015,	0.395348206,	0.464541108, ## Oct
                                0.521818166,	0.581372602,	0.643327061,	0.710948828, ## Nov
                                0.777620537,	0.847130239,	0.911498024,	0.930365931, ## Dec
                                0.947042313,	0.96389906,	0.983400068,	0.991357264,	 ## Jan
                                0.99238783,	1,1,1,## Feb
                                rep(1,12)## mar-may
                                ) 
prop_houses_sprayed_WeeklyM = 0.96*c(0.065358837,	0.193716785,## August
                                0.334444596,	0.432141444,	0.533708885,	0.614060416,##sep
                                0.667531537,	0.711462198,	0.769981823,	0.880751007,##oct
                                0.919045204,	0.944027976,	0.97443187,	0.990922736,  ##nov
                                0.991873307,	0.99744169,	 1,             1,          ##dec
                                1,        1,1,1,## jan
                                rep(1,16)) ## feb-may



## from Table 1 main manuscript 

## Accounting for modifications and added rooms in OCT cohort we have
cov_a_oct = c(0.891472868,0.776978417,0.556962025, 0.393063584,0.302702703, 0.218274112)
## Accounting for modifications and added rooms in NOV cohort we have
cov_a_nov = c(0.863636364,0.694736842,0.538461538,0.426086957,0.341269841,0.288888889)
## Accounting for modifications and added rooms in DEC cohort we have
cov_a_dec = c(0.851851852,0.714285714,0.655172414,0.6,0.566666667,0.548387097)

## Accounting for modifications and added rooms in OCT cohort we have
cov_s_oct = c(0.96460177,0.956140351,0.930434783,0.922413793,0.922413793,0.914529915)
## Accounting for modifications and added rooms in NOV cohort we have
cov_s_nov = c(0.973856209,0.948387097,0.918238994,0.918238994,0.900621118,0.895061728)
## Accounting for modifications and added rooms in DEC cohort we have
cov_s_dec = c(0.960526316,0.935064935,0.909090909,0.897435897,0.897435897,0.897435897)

## And weighting for the proportion of people represented in each survey 129:88:27 for Mut and 113:153:76 for Boane

# prop_mod_Acte = 1 - c(rep(0,10), ## august &  sep & oct
#                   rep(14/129,4), ## nov
#                   rep(14/129,4)+rep((12+7)/(117+88),4), ## dec
#                   rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4), ## jan
#                   rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4)+rep((20+10+3)/(116+85+25),4), ## feb
#                   rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4)+rep((20+10+3)/(116+85+25),4)+rep((12+7+1)/(115+85+25),4), ## mar
#                   rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4)+rep((20+10+3)/(116+85+25),4)+rep((12+7+1)/(115+85+25),4)+rep((16+6+1)/(84+25),4), ## apr
#                   rep(14/129,4)+rep((12+7)/(117+88),4)+rep((20+10+4)/(117+86+27),4)+rep((20+10+3)/(116+85+25),4)+rep((12+7+1)/(115+85+25),4)+rep((16+6+1)/(84+25),4)+rep((4+1)/(81+25),4)) ## may
prop_mod_Acte_temp = c(0.891472868,0.81212081,0.639282555,0.481047262,0.386204738,0.304873387,0.166894353)

##And sorted for weeks:
prop_mod_Acte = c(rep(1,10),rep(prop_mod_Acte_temp,each=4))## for time series of spray campaign

true_cover_irs_Acte =  rep(prop_mod_Acte*prop_houses_sprayed_WeeklyM,each=7)

#House coverage: Matutuine district 96 %
irs_cov_no_loss_Acte = rep(0.96,30*8)
irs_cov_Acte = true_cover_irs_Acte

# prop_mod_Sumi = 1 - c(rep(0,10),##aug & sep & oct
#                       rep(4/153,4),#nov
#                       rep(4/153,4)+rep((4+4)/(144+113),4),#dec
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4),#jan
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4)+rep((2+1)/(138+88+75),4),#feb
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4)+rep((2+1)/(138+88+75),4)+rep((2+1)/(137+86+75),4),#mar
#                       rep(4/153,4)+rep((4+4)/(144+113),4)+rep((2+0+3)/(141+89+76),4)+rep((2+1)/(138+88+75),4)+rep((2+1)/(137+86+75),4)+rep(0,8)#apr-may
# )

prop_mod_Sumi_temp = c(0.96460177,0.966330299,0.945153088,0.923357485,0.917585479,0.904508888,0.895619142)

##And sorted for weeks:
prop_mod_Sumi = c(rep(1,10),rep(prop_mod_Sumi_temp,each=4))## for time series of spray campaign

true_cover_irs_Sumi = rep(prop_mod_Sumi*prop_houses_sprayed_WeeklyB,each=7)

#House coverage: Boane (sumi) district 97 %, Manhica district (Palmeira) 98 % 
irs_cov_no_loss_Sumi = rep(0.97,30*8)
irs_cov_Sumi = true_cover_irs_Sumi

plot(irs_cov_no_loss_Acte[1:240] ~ time[1:240],ylab = "Community IRS cover (%)",
     ylim=c(0,1),col="black",pch="",
     main = "",cex.main=1.2,xlim=c(1,240),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,230,30)+15,labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)


lines(irs_cov_no_loss_Acte[61:240] ~ time[61:240],lty=1,lwd=2,col = "darkblue")
lines(irs_cov_Acte[61:240] ~ time[61:240],lty=3,lwd=2,col = "darkblue")

lines(irs_cov_no_loss_Sumi[61:240] ~ c(time[61:240]+1),lty=1,lwd=2,col = "aquamarine3")
lines(irs_cov_Sumi[61:240] ~ time[61:240],lty=3,lwd=2,col = "aquamarine3")

cov1A = cov1S = cov2A = cov2S = array(dim=c(240,4))

# itn_cov_Acte = 0.52
# itn_cov_Sumi = 0.73

## Table 1 data
matu_net_cov = c(27.9,  ## nov
                 mean(c(32.6,42.1)), ##dec
                 mean(c(43.4,43.2,33.3)),##jan
                 mean(c(55.0,44.3,48.2)),##feb
                 mean(c(61.2,56.8,40.7)),##mar
                 mean(c(51.9,61.4,66.7)),##apr
                 mean(c(48.9,55.6)),##may
                 29.6)##june

itn_cov_Acte_temp = c(rep(mean(c(matu_net_cov)),10),
                   rep(mean(c(32.6,42.1)),4), ##dec
                   rep(mean(c(43.4,43.2,33.3)),4),##jan
                   rep(mean(c(55.0,44.3,48.2)),4),##feb
                   rep(mean(c(61.2,56.8,40.7)),4),##mar
                   rep(mean(c(51.9,61.4,66.7)),4),##apr
                   rep(mean(c(48.9,55.6)),4),##may
                   rep(29.6,4))##june



boan_net_cov = c(57.5,  ## nov
                 mean(c(64.6,74.5)), ##dec
                 mean(c(68.1,71.9,67.1)),##jan
                 mean(c(62.8,71.2,79)),##feb
                 mean(c(70.8,82.4,86.8)),##mar
                 mean(c(65.5,81.8,81.6)),##apr
                 mean(c(78.4,84.2)),##may
                 10.5)##june

itn_cov_Boan_temp = c(rep(mean(c(boan_net_cov)),10),
                      rep(mean(c(64.6,74.5)),4), ##dec
                      rep(mean(c(68.1,71.9,67.1)),4),##jan
                      rep(mean(c(62.8,71.2,79)),4),##feb
                      rep(mean(c(70.8,82.4,86.8)),4),##mar
                      rep(mean(c(65.5,81.8,81.6)),4),##apr
                      rep(mean(c(78.4,84.2)),4),##may
                      rep(10.5,4))

itn_cov_Acte = rep(itn_cov_Acte_temp,each=7)/100
itn_cov_Sumi = rep(itn_cov_Boan_temp,each=7)/100

## Here we are creating a matrix
## with the coverage or use of nets waning with time
## and the coverage of IRS either staying fixed, or also waning 
## when walls are washed
cov1A[,1] = 1
cov1A[,2] = itn_cov_Acte[1:240] ## ITN only
cov1A[,3] = irs_cov_no_loss_Acte ## IRS only
cov1A[,4] = itn_cov_Acte[1:240]*irs_cov_no_loss_Acte ## both interventions

cov2A[,1] = 1
cov2A[,2] = itn_cov_Acte[1:240] ## ITN only
cov2A[,3] = irs_cov_Acte[1:240] ## IRS only
cov2A[,4] = itn_cov_Acte[1:240]*irs_cov_Acte[1:240]## both interventions

cov1S[,1] = 1
cov1S[,2] = itn_cov_Sumi[1:240] ## ITN only
cov1S[,3] = irs_cov_no_loss_Sumi ## IRS only
cov1S[,4] = itn_cov_Sumi[1:240]*irs_cov_no_loss_Sumi ## both interventions

cov2S[,1] = 1
cov2S[,2] = itn_cov_Sumi[1:240] ## ITN only
cov2S[,3] = irs_cov_Sumi[1:240] ## IRS only
cov2S[,4] = itn_cov_Sumi[1:240]*irs_cov_Sumi[1:240] ## both interventions

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
zhi1A = whi1A = zhi2A = whi2A = array(dim=c(240,4))
zhi1S = whi1S = zhi2S = whi2S = array(dim=c(240,4))
zhi1A=cov1A*z_Acte[1:240,]
whi1A=cov1A*w_Acte[1:240,]

zhi1S=cov1S*z_Sumi[1:240,]
whi1S=cov1S*w_Sumi[1:240,]


zhi2A=cov2A*z_Acte[1:240,]
whi2A=cov2A*w_Acte[1:240,]

zhi2S=cov2S*z_Sumi[1:240,]
whi2S=cov2S*w_Sumi[1:240,]

# zhi1[,4]=cov1[,2]*z[,2] * (1 - cov1[,3]*z[,3]) + cov1[,3]*z[,3]
# zhi2[,4]=cov2[,2]*z[,2] * (1 - cov2[,3]*z[,3]) + cov2[,3]*z[,3]
# 
# whi1[,4]=cov1[,2]*w[,2] * (1 - cov1[,3]*w[,3]) + cov1[,3]*w[,3]
# whi2[,4]=cov2[,2]*w[,2] * (1 - cov2[,3]*w[,3]) + cov2[,3]*w[,3]

zbar1A = wbar1A = zbar2A = wbar2A = array(dim=c(240,4)) 
zbar1S = wbar1S = zbar2S = wbar2S = array(dim=c(240,4)) 
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
lambda1A = lambda2A = array(dim = c(240,4))
lambda1S = lambda2S = array(dim = c(240,4))
for(i in 1:4){
  lambda1A = (Q1A*fR1A*yy_Acte[1:240,i])/whi1A[1:240,i]
  lambda2A = (Q2A*fR2A*yy_Acte[1:240,i])/whi2A[1:240,i]
  
  lambda1S = (Q1S*fR1S*yy_Sumi[1:240,i])/whi1S[1:240,i]
  lambda2S = (Q2S*fR2S*yy_Sumi[1:240,i])/whi2S[1:240,i]
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
plot(lambda1A[,1] ~ time[1:240],ylim=c(0,2.5),pch="",
     ylab = "Mosquito bites received per person per day",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,240),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,2.5,0.5),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,230,30)+15,labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)

for(i in 4){
  lines(lambda1A[61:235,i] ~ time[61:235],col="darkblue",lty=2,lwd=2)
  lines(lambda2A[61:235,i] ~ time[61:235],col="darkblue",lty=4,lwd=2)
  
  lines(lambda1S[61:235,i] ~ time[61:235],col="aquamarine3",lty=2,lwd=2)
  lines(lambda2S[61:235,i] ~ time[61:235],col="aquamarine3",lty=4,lwd=2)
  
}
for(i in 3){
  lines(lambda1A[61:235,i] ~ time[61:235],col="darkblue",lty=1,lwd=1)
  lines(lambda2A[61:235,i] ~ time[61:235],col="darkblue",lty=3,lwd=1)
  
  lines(lambda1S[61:235,i] ~ time[61:235],col="aquamarine3",lty=1,lwd=1)
  lines(lambda2S[61:235,i] ~ time[61:235],col="aquamarine3",lty=3,lwd=1)
  
}

# for(i in 1:4) lines(yy[1:180,i]~time[1:180],col=colsd[i],lwd=2)
# for(i in 1:4) lines(fR1[,i]~time[1:180],col=colsd[i],lwd=2)
# for(i in 1:4) lines(Q1[,i]~time[1:180],col=colsd[i],lwd=2,lty=2)
# 
# legend("topleft",legend = c("Matutuine (assuming no ITN)","Boane (assuming no ITN)",
#                             "Matutuine (ITN use 52%)","Boane (ITN use 73%)","IRS no household modification", "IRS with household modification"),
#        col = c("darkblue","aquamarine3","darkblue","aquamarine3","black","black"),lwd = c(1,1,2,2,2,2), lty=c(1,1,1,1,1,2),cex=1.2,bty="n")

## Additional infectious bites per person per year 
Estimated_added_EIR = array(dim=c(240,2))
Estimated_added_EIR[,1] = (lambda2A[,4] - lambda1A[,4])
Estimated_added_EIR[,2] = (lambda2S[,4] - lambda1S[,4])

c(sum(Estimated_added_EIR[1:30,1])/30,sum(Estimated_added_EIR[31:60,1])/30,
  sum(Estimated_added_EIR[61:90,1])/30,sum(Estimated_added_EIR[91:120,1])/30,
  sum(Estimated_added_EIR[121:150,1])/30,sum(Estimated_added_EIR[151:180,1])/30)

c(sum(Estimated_added_EIR[1:30,2])/30,sum(Estimated_added_EIR[31:60,2])/30,
  sum(Estimated_added_EIR[61:90,2])/30,sum(Estimated_added_EIR[91:120,2])/30,
  sum(Estimated_added_EIR[121:150,2])/30,sum(Estimated_added_EIR[151:180,2])/30)


## Additional infectious bites per person per year 
Estimated_propn_increase_EIR = array(dim=c(240,2))
Estimated_propn_increase_EIR[,1] = (lambda2A[,4] - lambda1A[,4])/lambda2A[,4]
Estimated_propn_increase_EIR[,2] = (lambda2S[,4] - lambda1S[,4])/lambda2S[,4]

mean(Estimated_propn_increase_EIR[15:45,1])##sep
mean(Estimated_propn_increase_EIR[46:75,1])##oct
mean(Estimated_propn_increase_EIR[76:105,1])##nov
mean(Estimated_propn_increase_EIR[106:135,1])##dec
mean(Estimated_propn_increase_EIR[136:165,1])##jan
mean(Estimated_propn_increase_EIR[166:195,1])##feb
mean(Estimated_propn_increase_EIR[196:225,1])##mar
mean(Estimated_propn_increase_EIR[226:240,1])##part april


mean(Estimated_propn_increase_EIR[15:45,2])##sep
mean(Estimated_propn_increase_EIR[46:75,2])##oct
mean(Estimated_propn_increase_EIR[76:105,2])##nov
mean(Estimated_propn_increase_EIR[106:135,2])##dec
mean(Estimated_propn_increase_EIR[136:165,2])##jan
mean(Estimated_propn_increase_EIR[166:195,2])##feb
mean(Estimated_propn_increase_EIR[196:225,2])##mar
mean(Estimated_propn_increase_EIR[226:240,2])##part april


plot(Estimated_propn_increase_EIR[,1] ~ time[1:240],ylim=c(0,1),pch="",
     ylab = "",
     col="black",
     main = "",cex.main=1.2,xlim=c(1,240),xaxt="n",
     xlab="Time in months",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,230,30)+15,labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),cex.axis = 1.4)
mtext(side=2, line =4.3,
      "Relative increase in daily bites")
mtext(side=2, line =3, "due to spray campaign & modifications (%)")

colsd = c("darkblue","aquamarine3")
for(i in 1:2){
  lines(Estimated_propn_increase_EIR[61:240,i] ~ time[61:240],col=colsd[i],lty=2,lwd=2)
}


Estimated_added_EIR = array(dim=c(240,2))
Estimated_added_EIR[,1] = (lambda2A[,3] - lambda1A[,3])
Estimated_added_EIR[,2] = (lambda2S[,3] - lambda1S[,3])

## Additional infectious bites per person per year 
Estimated_propn_increase_EIR = array(dim=c(240,2))
Estimated_propn_increase_EIR[,1] = (lambda2A[,3] - lambda1A[,3])/lambda2A[,3]
Estimated_propn_increase_EIR[,2] = (lambda2S[,3] - lambda1S[,3])/lambda2S[,3]

for(i in 1:2){
  lines(Estimated_propn_increase_EIR[61:240,i] ~ time[61:240],col=colsd[i],lty=1,lwd=1)
}


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

text(x = -650, y = 2.65,"(A)")
text(x = -340, y = 2.65,"(B)")
text(x = -38, y = 2.65,"(C)")

text(x = -650, y = 1.2,"(D)")
text(x = -340, y = 1.2,"(E)")
text(x = -38, y = 1.2,"(F)")

## 1300 width and 750 height

