##############################################################
##
## Estimating the probable outcomes of a feeding attempt
## with cone bioassay data for residual efficacy
## using Sumishield data from Mozambique; with Mercy & Krijn
##
##############################################################

## E Sherrard-Smith
## September 2019

## 1 Adjust the probability of feeding, exiting and deterrence by the 
##   same proportions as estimated by experimental hut data

## Experimental hut data Sumishield

library(adegenet)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

IRS_cleaner_f_base = function(data1,k0){
  
  n_t=data1$Ntotalfemalemosq_IRS
  d_t=data1$Ntotaldied_IRS
  fed_t=round(data1$Nbloodfed_IRS * (1 - data1$Ntotaldied_IRS/data1$Ntotalfemalemosq_IRS),0)
  deterrence_IRS = ifelse(c(data1$Ntotalfemalemosq_C-data1$Ntotalfemalemosq_IRS)<0,0,
                          c(data1$Ntotalfemalemosq_C-data1$Ntotalfemalemosq_IRS))
  deterrence_total = c(data1$Ntotalfemalemosq_IRS+data1$Ntotalfemalemosq_C)
  time=data1$Months_since_IRS*30
  
  return(list(N=nrow(data1),
              n_t=n_t,
              d_t=d_t,
              fed_t=fed_t,
              deterrence_IRS = deterrence_IRS,
              deterrence_total = deterrence_total,
              time=time) ) 
}


## Load the raw data for each chemistry
######################
##
## ACTELLIC 300 CS
##
######################
#k0 = 0.699 ##the prob of feeding in absence of interventions

act_dat = read.csv("data/data_summaryActellic.csv",header=TRUE)
act_dat$rep_of_studies = ifelse(act_dat$Study == "Agossa2014", 1,
                                ifelse(act_dat$Study == "Tchicaya2014", 2,
                                       ifelse(act_dat$Study == "Rowland2013", 3,
                                              ifelse(act_dat$Study == "Unpublished_Muller_Sumi_trial",4,
                                                     ifelse(act_dat$Study == "Rowland2013mud",5,
                                                            ifelse(act_dat$Study == "Oxborough2014MalJ",6,7))))))
act_dat$rep_of_studlab = ifelse(act_dat$Study == "Agossa2014", 1,
                                ifelse(act_dat$Study == "Tchicaya2014", 5,
                                       ifelse(act_dat$Study == "Rowland2013", 4,
                                              ifelse(act_dat$Study == "Unpublished_Muller_Sumi_trial",12,
                                                     ifelse(act_dat$Study == "Rowland2013mud",13,
                                                            ifelse(act_dat$Study == "Oxborough2014MalJ",9,20))))))

act_dat2 = subset(act_dat,act_dat$Study != "UNPUBLISHED_Sarah_Moore")
act_dat3 = subset(act_dat2,act_dat2$Ntotalfemalemosq_IRS > 5)
act_dattest_base=IRS_cleaner_f_base(act_dat3,k0)
act_dat3$timedays = act_dat3$time*30


######################
##
## SUMISHIELD - data from Sh-Sm et al 2018 review of experimental huts
##
######################


sum_dat = read.csv("data/data_summarySumishield.csv",header=TRUE)
dim(sum_dat)
unique(sum_dat$Study)
sum_dat$rep_of_studies = ifelse(sum_dat$Study == "UNPUBLISHED_Data_Corbel", 1,
                                ifelse(sum_dat$Study == "UNPUBLISHED_PieMullerCDIvoire", 2,
                                       ifelse(sum_dat$Study == "Ngufor_2017_PLoSOne",3,
                                              ifelse(sum_dat$Study == "Agossa_2018ParaVec",4,20))))

sum_dat$rep_of_studlab = ifelse(sum_dat$Study == "UNPUBLISHED_Data_Corbel", 11,
                                ifelse(sum_dat$Study == "UNPUBLISHED_PieMullerCDIvoire", 12,
                                       ifelse(sum_dat$Study == "Ngufor_2017_PLoSOne",7,
                                              ifelse(sum_dat$Study == "Agossa_2018ParaVec",10,20))))

summary(sum_dat)
sum_dat2 = subset(sum_dat,sum_dat$rep_of_studies == 3)
sum_dat2 = subset(sum_dat,sum_dat$rep_of_studies < 5 & sum_dat$concn > 0.1)
dim(sum_dat2)
sum_dattest_base = IRS_cleaner_f_base(sum_dat2,k0)
sum_dat2$timedays = sum_dat2$Months_since_IRS*30

############################
##
## Estimating the probable outcome of a feeding attempt
##
############################

## Modelling the impact 
stan_Acte <- stan(file="probability_estimates_lq and kq_random_effect_mean.stan", 
                  data=act_dattest_base, 
                  warmup=1000,
                  control = list(adapt_delta = 0.8,
                                 max_treedepth = 20),
                  iter=2000, chains=4)


## Modelling the impact 
stan_Sumi <- stan(file="probability_estimates_lq and kq_random_effect_mean.stan", 
                  data=sum_dattest_base, 
                  warmup=1000,
                  control = list(adapt_delta = 0.8,
                                 max_treedepth = 20),
                  iter=2000, chains=4)


#library(shinystan)  ## can use this to check the model diagnostics
#launch_shinystan(stan_base)

base_Acte <- extract(stan_Acte)
base_Sumi <- extract(stan_Sumi)

time=seq(1,365,length=365)
mean_valssp_Acte  = 1 / (1 + exp(-mean(base_Acte$alpha1) - mean(base_Acte$alpha2)*time))
mean_valssp_ActeU = 1 / (1 + exp(-quantile(base_Acte$alpha1,0.99) - quantile(base_Acte$alpha2,0.99)*time))
mean_valssp_ActeL = 1 / (1 + exp(-quantile(base_Acte$alpha1,0.01) - quantile(base_Acte$alpha2,0.01)*time))

mean_valsfp_Acte  = (1 / (1 + exp(-mean(base_Acte$beta1) - mean(base_Acte$beta2)*time)))
mean_valsfp_ActeU = (1 / (1 + exp(-quantile(base_Acte$beta1,0.99) - quantile(base_Acte$beta2,0.99)*time)))
mean_valsfp_ActeL = (1 / (1 + exp(-quantile(base_Acte$beta1,0.01) - quantile(base_Acte$beta2,0.01)*time)))

##setting same depreciation as mortality for deterrence
mean_valsdet_Acte  = 1 / (1 + exp(-mean(base_Acte$omega1) - mean(base_Acte$alpha2)*time))
mean_valsdet_ActeU = 1 / (1 + exp(-quantile(base_Acte$omega1,0.99) - quantile(base_Acte$alpha2,0.99)*time))
mean_valsdet_ActeL = 1 / (1 + exp(-quantile(base_Acte$omega1,0.01) - quantile(base_Acte$alpha2,0.01)*time))

time=seq(1,365,length=365)
mean_valssp_Sumi  = 1 / (1 + exp(-mean(base_Sumi$alpha1) - mean(base_Sumi$alpha2)*time))
mean_valssp_SumiU = 1 / (1 + exp(-quantile(base_Sumi$alpha1,0.99) - quantile(base_Sumi$alpha2,0.99)*time))
mean_valssp_SumiL = 1 / (1 + exp(-quantile(base_Sumi$alpha1,0.01) - quantile(base_Sumi$alpha2,0.01)*time))

mean_valsfp_Sumi  = (1 / (1 + exp(-mean(base_Sumi$beta1) - mean(base_Sumi$beta2)*time)))
mean_valsfp_SumiU = (1 / (1 + exp(-quantile(base_Sumi$beta1,0.99) - quantile(base_Sumi$beta2,0.99)*time)))
mean_valsfp_SumiL = (1 / (1 + exp(-quantile(base_Sumi$beta1,0.01) - quantile(base_Sumi$beta2,0.01)*time)))

##setting same depreciation as mortality for deterrence
mean_valsdet_Sumi  = 1 / (1 + exp(-mean(base_Sumi$omega1) - mean(base_Sumi$alpha2)*time))
mean_valsdet_SumiU = 1 / (1 + exp(-quantile(base_Sumi$omega1,0.99) - quantile(base_Sumi$alpha2,0.99)*time))
mean_valsdet_SumiL = 1 / (1 + exp(-quantile(base_Sumi$omega1,0.01) - quantile(base_Sumi$alpha2,0.01)*time))

store_data = data.frame(time,
                        mean_valssp_Acte,
                        mean_valssp_ActeL,
                        mean_valssp_ActeU,
                        mean_valsfp_Acte,
                        mean_valsfp_ActeL,
                        mean_valsfp_ActeU,
                        mean_valsdet_Acte,
                        mean_valsdet_ActeL,
                        mean_valsdet_ActeU,
                        mean_valssp_Sumi,
                        mean_valssp_SumiL,
                        mean_valssp_SumiU,
                        mean_valsfp_Sumi,
                        mean_valsfp_SumiL,
                        mean_valsfp_SumiU,
                        mean_valsdet_Sumi,
                        mean_valsdet_SumiL,
                        mean_valsdet_SumiU)

write.csv(store_data,"Analysis_1_spray_model_data.csv")

####################################
##
## Panel 1 shows the mortality estimates from experimental huts and the
## overlaid cone bioassay residual efficacy measured within Mozambique
par(mfrow = c(2,3)) ## creates a 4 panel figure
#####################################
DAT1 = read.csv("data/Analysis_1_spray_model_data.csv",header=TRUE)


time = DAT1$time
mean_valssp_Acte = DAT1$mean_valssp_Acte
mean_valssp_ActeL = DAT1$mean_valssp_ActeL
mean_valssp_ActeU = DAT1$mean_valssp_ActeU
mean_valsfp_Acte = DAT1$mean_valsfp_Acte
mean_valsfp_ActeL = DAT1$mean_valsfp_ActeL
mean_valsfp_ActeU = DAT1$mean_valsfp_ActeU
mean_valsdet_Acte = DAT1$mean_valsdet_Acte
mean_valsdet_ActeL = DAT1$mean_valsdet_ActeL
mean_valsdet_ActeU = DAT1$mean_valsdet_ActeU
mean_valssp_Sumi = DAT1$mean_valssp_Sumi
mean_valssp_SumiL = DAT1$mean_valssp_SumiL
mean_valssp_SumiU = DAT1$mean_valssp_SumiU
mean_valsfp_Sumi = DAT1$mean_valsfp_Sumi
mean_valsfp_SumiL = DAT1$mean_valsfp_SumiL
mean_valsfp_SumiU = DAT1$mean_valsfp_SumiU
mean_valsdet_Sumi = DAT1$mean_valsdet_Sumi
mean_valsdet_SumiL = DAT1$mean_valsdet_SumiL
mean_valsdet_SumiU = DAT1$mean_valsdet_SumiU

DEAD_Acte =act_dattest_base$d_t /act_dattest_base$n_t
FED_Acte = (act_dattest_base$fed_t/act_dattest_base$n_t)
DET_Acte = act_dattest_base$deterrence_IRS/act_dattest_base$deterrence_total
times_Acte = act_dattest_base$time 
act_dat3$fed_adjusted = (act_dat3$Nbloodfed_IRS*(1 - act_dat3$Ntotaldied_IRS/act_dat3$Ntotalfemalemosq_IRS))/act_dat3$Ntotalfemalemosq_IRS
act_dat3$deterrence_IRS = ifelse(c(act_dat3$Ntotalfemalemosq_C-act_dat3$Ntotalfemalemosq_IRS)<0,0,
                                 c(act_dat3$Ntotalfemalemosq_C-act_dat3$Ntotalfemalemosq_IRS))
act_dat3$deterrence_total = c(act_dat3$Ntotalfemalemosq_IRS+act_dat3$Ntotalfemalemosq_C)


DEAD_Sumi =sum_dattest_base$d_t /sum_dattest_base$n_t
FED_Sumi = (sum_dattest_base$fed_t/sum_dattest_base$n_t)
DET_Sumi = sum_dattest_base$deterrence_IRS/sum_dattest_base$deterrence_total
times_Sumi = sum_dattest_base$time 
sum_dat2$fed_adjusted = (sum_dat2$Nbloodfed_IRS*(1 - sum_dat2$Ntotaldied_IRS/sum_dat2$Ntotalfemalemosq_IRS))/sum_dat2$Ntotalfemalemosq_IRS
sum_dat2$deterrence_IRS = ifelse(c(sum_dat2$Ntotalfemalemosq_C-sum_dat2$Ntotalfemalemosq_IRS)<0,0,
                            c(sum_dat2$Ntotalfemalemosq_C-sum_dat2$Ntotalfemalemosq_IRS))
sum_dat2$deterrence_total = c(sum_dat2$Ntotalfemalemosq_IRS+sum_dat2$Ntotalfemalemosq_C)

###############
##
## Function to plot data
## 
plots_fn = function(TESTER,
                    MAIN_HEADING,
                    DEAD,times,
                    mean_valssp_checker4u,
                    mean_valssp_checker4l,
                    mean_valssp_checker4,
                    
                    mean_valsfp_checker4u,
                    mean_valsfp_checker4l,
                    mean_valsfp_checker4,
                    
                    mean_valsdet_checker4u,
                    mean_valsdet_checker4l,
                    mean_valsdet_checker4,
                    
                    Con_bio_d_t_mud,
                    Con_bio_n_t_mud,
                    
                    Con_bio_d_t_cem,
                    Con_bio_n_t_cem,
                    
                    time_sequence,
                    N_data){
  
  ##Figure 1A (and 1D): The first figure for each Actellic or Sumi 
  plot(DEAD ~ times,ylab="Mosquito mortality (%)",ylim=c(0,1),col="aquamarine3",pch="",
       main = MAIN_HEADING,cex.main=1.2,xlim=c(1,365),xaxt="n",
       xlab="",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
  axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
  axis(1,at=seq(0,365,120),labels=seq(0,365,120),cex.lab=1.4,cex.axis=1.4)
  
  time=seq(1,365,length=365)
  polygon(c(time,rev(time)),c(mean_valssp_checker4u,rev(mean_valssp_checker4l)),col=transp("aquamarine3",0.5),border=NA)
  lines(mean_valssp_checker4 ~ time,ylim=c(0,1),col="aquamarine4",lwd=2)
  
  ## Add in a line to demonstrate the residual efficacy estimated by Mercy in MOZAMBIQUE
  data_list_mud = list(N = N_data, ## number
                   d_t = Con_bio_d_t_mud,
                   n_t = Con_bio_n_t_mud,
                   time = time_sequence,
                   N_eff = 1, ## eg '2' for 2 wall types
                   eff = rep(1,N_data))##[the number of reps for each group in your data]
  
  data_list_cem = list(N = N_data, ## number
                       d_t = Con_bio_d_t_cem,
                       n_t = Con_bio_n_t_cem,
                       time = time_sequence,
                       N_eff = 1, ## eg '2' for 2 wall types
                       eff = rep(1,N_data))##[the number of reps for each group in your data]
  
  
  stan_model_mud <- stan(file="logistic_model_kp.stan", 
                     data=data_list_mud, 
                     warmup=500,
                     control = list(adapt_delta = 0.9,
                                    max_treedepth = 20),
                     iter=1000, chains=4)
  
  stan_model_cem <- stan(file="logistic_model_kp.stan", 
                         data=data_list_cem, 
                         warmup=500,
                         control = list(adapt_delta = 0.9,
                                        max_treedepth = 20),
                         iter=1000, chains=4)
  
  
  #library(shinystan)  ## can use this to check the model diagnostics
  #launch_shinystan(stan2_b)
  
  base_moz1 <- extract(stan_model_mud) ## can use this to extract the model parameter estimates
  base_moz2 <- extract(stan_model_cem) ## can use this to extract the model parameter estimates
  
  ## plot it against your data!
  d_t1 = Con_bio_d_t_mud
  n_t1 = Con_bio_n_t_mud
  DEAD1 = d_t1/n_t1
  
  d_t2 = Con_bio_d_t_cem
  n_t2 = Con_bio_n_t_cem
  DEAD2 = d_t2/n_t2
  
  time2 = time_sequence
  time = seq(1,365,by=1)
  
  points(DEAD1 ~ time_sequence,col="darkred",pch=15)
  points(DEAD2 ~ time_sequence,col="grey15",pch=15)
  
  mean_prediction_mud = 1 / (1 + exp(-mean(base_moz1$alpha1[,1]) - mean(base_moz1$alpha2[,1])*time))
  max_prediction_mud = 1 / (1 + exp(-quantile(base_moz1$alpha1[,1],0.9) - quantile(base_moz1$alpha2[,1],0.9)*time))
  min_prediction_mud = 1 / (1 + exp(-quantile(base_moz1$alpha1[,1],0.1) - quantile(base_moz1$alpha2[,1],0.1)*time))
  
  mean_prediction_cem = 1 / (1 + exp(-mean(base_moz2$alpha1[,1]) - mean(base_moz2$alpha2[,1])*time))
  max_prediction_cem = 1 / (1 + exp(-quantile(base_moz2$alpha1[,1],0.9) - quantile(base_moz2$alpha2[,1],0.9)*time))
  min_prediction_cem = 1 / (1 + exp(-quantile(base_moz2$alpha1[,1],0.1) - quantile(base_moz2$alpha2[,1],0.1)*time))
  
  end_time = round(N_data*30,0)
  
  polygon(c(time,rev(time)),c(max_prediction_mud,rev(min_prediction_mud)),col=adegenet::transp("darkred",0.4),border=NA)
  lines(mean_prediction_mud[1:end_time] ~ time[1:end_time],col = "darkred",lwd=2)
  lines(mean_prediction_mud ~ time,col = "darkred",lwd=2,lty=2)
  
  polygon(c(time,rev(time)),c(max_prediction_cem,rev(min_prediction_cem)),col=adegenet::transp("grey",0.4),border=NA)
  lines(mean_prediction_cem[1:end_time] ~ time[1:end_time],col = "black",lwd=2)
  lines(mean_prediction_cem ~ time,col = "black",lwd=2,lty=2)
  
  legend("topright",legend = c("Systematic review","Cone bioassay (mud)","Cone bioassay (cement)"),
         lwd=2,pch=15,col=adegenet::transp(c("aquamarine4","darkred","grey")),
         cex=1.4,bty = "n")
  #####################################
  ##
  ## panel 2 is the relationship between mortality and feeding, exiting, deterrence that we need to 
  ## work out the probability of a feeding attempt ending in a blood meal on a person
  ##
  #############################################
  feed1 = (1 - mean_valssp_checker4) * mean_valsfp_checker4 * (1 - mean_valsdet_checker4)
  death1 = mean_valssp_checker4 * (1 - mean_valsdet_checker4)
  rep1 = (1 - (death1 + feed1)) * (1 - mean_valsdet_checker4)
  deter1 = mean_valsdet_checker4  
  
  TOTS = feed1 + rep1 + death1 + deter1
  
  first_line = feed1 / TOTS
  second_line = (feed1 + rep1) / TOTS
  third_line = (feed1 + rep1 + deter1 ) / TOTS
  
  Time = 1:365
  Time2 = rev(Time)
  minimal = rep(0,length(Time))
  maximal = rep(1,length(Time))
  
  plot(rev(first_line) ~ Time2,ylim=c(0,1),yaxt="n",
       ylab="Probable outcome (%)",xlab="Time in days",xaxt="n",
       main="Systematic review",
       cex.axis=1.4,cex.lab=1.4,bty="n",pch="")
  axis(2,las=2,at=seq(0,1,0.2),labels = seq(0,100,20),cex.axis = 1.4)
  axis(1,at=seq(0,365,120),labels = seq(0,365,120),cex.axis = 1.4)
  polygon(c(Time2,rev(Time2)),c(rev(first_line),rev(minimal)),col=transp("red",0.4),border = NA)
  polygon(c(Time2,rev(Time2)),c(rev(second_line),first_line),col=transp("orange",0.6),border = NA)
  polygon(c(Time2,rev(Time2)),c(rev(third_line),second_line),col=transp("darkgreen",0.4),border = NA)
  polygon(c(Time2,rev(Time2)),c(maximal,third_line),col=transp("royalblue",0.6),border = NA)
  
  text(50,0.80,ifelse(TESTER == "ACTELLIC","","Killed"),col="blue",cex=1.2)
  text(50,0.50,ifelse(TESTER == "ACTELLIC","","Deterred"),col="darkgreen",cex=1.2)
  text(120,0.30,ifelse(TESTER == "ACTELLIC","","Exited"),col="darkorange",cex=1.2)
  text(250,0.20,ifelse(TESTER == "ACTELLIC","","Successfully fed"),col="darkred",cex=1.2)
  ##############################
  ##
  ## ******** 2 Figure out how to extrapolate expt hut info from residual efficacy info
  ##  depends on how similar mortality curves are!
  ###################################
  
  percent_mud = 0.5
  percent_cem = 0.5
  
  mean_prediction = (mean_prediction_mud*percent_mud) + (mean_prediction_cem*percent_cem)
  
  feed2 = (1 - mean_prediction) * mean_valsfp_checker4 * (1 - mean_valsdet_checker4)
  death2 = mean_prediction  * (1 - mean_valsdet_checker4)
  rep2 = (1 - (death1 + feed1)) * (1 - mean_valsdet_checker4)
  deter2 = mean_valsdet_checker4  
  
  TOTS2 = feed2 + rep2 + death2 + deter2
  
  first_line_adj = feed2 / TOTS2
  second_line_adj = (feed2 + rep2) / TOTS2
  third_line_adj = (feed2 + rep2 + deter2 ) / TOTS2
  
  plot(rev(first_line_adj) ~ Time2,ylim=c(0,1),yaxt="n",
       ylab="Probable outcome (%)",xlab="Time in days",xaxt="n",
       main="Cone bioassay",
       cex.axis=1.4,cex.lab=1.4,bty="n",pch="")
  axis(2,las=2,at=seq(0,1,0.2),labels = seq(0,100,20),cex.axis = 1.4)
  axis(1,at=seq(0,365,120),labels = seq(0,365,120),cex.axis = 1.4)
  polygon(c(Time2,rev(Time2)),c(rev(first_line_adj),rev(minimal)),col=transp("red",0.4),border = NA)
  polygon(c(Time2,rev(Time2)),c(rev(second_line_adj),first_line_adj),col=transp("orange",0.6),border = NA)
  polygon(c(Time2,rev(Time2)),c(rev(third_line_adj),second_line_adj),col=transp("darkgreen",0.4),border = NA)
  polygon(c(Time2,rev(Time2)),c(maximal,third_line_adj),col=transp("royalblue",0.6),border = NA)
  
  return(list(mean_prediction,
              feed2,
              death2,
              rep2,
              deter2))
}

#####################################################################
##
## Cone bioassay data from the field 
##
#####################################################################
cone_bios = read.csv("data\\Cone_assay_data_v1.csv",header=TRUE)

Con_bio_Acte_d_t_mud = 
  Con_bio_Acte_n_t_mud = 
  Con_bio_Sumi_d_t_mud = 
  Con_bio_Sumi_n_t_mud = 
  Con_bio_Acte_d_t_cem = 
  Con_bio_Acte_n_t_cem = 
  Con_bio_Sumi_d_t_cem = 
  Con_bio_Sumi_n_t_cem = numeric(length(unique(cone_bios$month_official)))

for(m in 1:length(unique(cone_bios$month_official))){
  Con_bio_Acte_d_t_mud[m] =  sum(cone_bios$total_mortality_24[cone_bios$house_type == "mud" & cone_bios$local == "Magude" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Acte_n_t_mud[m] =  sum(cone_bios$total_exposed[cone_bios$house_type == "mud" & cone_bios$local == "Magude" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Sumi_d_t_mud[m] =  sum(cone_bios$total_mortality_24[cone_bios$house_type == "mud" & cone_bios$local == "Palmeira" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Sumi_n_t_mud[m] =  sum(cone_bios$total_exposed[cone_bios$house_type == "mud" & cone_bios$local == "Palmeira" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  
  Con_bio_Acte_d_t_cem[m] =  sum(cone_bios$total_mortality_24[cone_bios$house_type == "concrete" & cone_bios$local == "Magude" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Acte_n_t_cem[m] =  sum(cone_bios$total_exposed[cone_bios$house_type == "concrete" & cone_bios$local == "Magude" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Sumi_d_t_cem[m] =  sum(cone_bios$total_mortality_24[cone_bios$house_type == "concrete" & cone_bios$local == "Palmeira" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Sumi_n_t_cem[m] =  sum(cone_bios$total_exposed[cone_bios$house_type == "concrete" & cone_bios$local == "Palmeira" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
}


MAIN_HEADING = "Actellic 300CS cone bioassay mortality"
time_sequence_Acte = c(1:7)*30
N_data_Acte = 7

time_sequence_Sumi = c(1:9)*30
N_data_Sumi = 9
##actellic has 7 months data
##sumi has 9

par(mfrow=c(2,3))

actellic_details = plots_fn(TESTER = "ACTELLIC",
                            MAIN_HEADING = "Actellic 300CS cone bioassay mortality",
                            DEAD = DEAD_Acte,
                            times = times_Acte,
                            mean_valssp_checker4u = mean_valssp_ActeU,
                            mean_valssp_checker4l = mean_valssp_ActeL,
                            mean_valssp_checker4 = mean_valssp_Acte,
                            
                            mean_valsfp_checker4u = mean_valsfp_ActeU,
                            mean_valsfp_checker4l = mean_valsfp_ActeL,
                            mean_valsfp_checker4 = mean_valsfp_Acte,
                            
                            mean_valsdet_checker4u = mean_valsdet_ActeU,
                            mean_valsdet_checker4l = mean_valsdet_ActeL,
                            mean_valsdet_checker4 = mean_valsdet_Acte,
                            
                            Con_bio_d_t_mud = Con_bio_Acte_d_t_mud[1:7],
                            Con_bio_n_t_mud = Con_bio_Acte_n_t_mud[1:7],
                            
                            Con_bio_d_t_cem = Con_bio_Acte_d_t_cem[1:7],
                            Con_bio_n_t_cem = Con_bio_Acte_n_t_cem[1:7],
                            
                            time_sequence = time_sequence_Acte,
                            N_data = N_data_Acte)

sumishield_details = plots_fn(TESTER = "SUMISHIELD",
                              MAIN_HEADING = "SumiShield cone bioassay mortality",
                              DEAD = DEAD_Sumi,
                              times = times_Sumi,
                              mean_valssp_checker4u = mean_valssp_SumiU,
                              mean_valssp_checker4l = mean_valssp_SumiL,
                              mean_valssp_checker4 = mean_valssp_Sumi,
                              
                              mean_valsfp_checker4u = mean_valsfp_SumiU,
                              mean_valsfp_checker4l = mean_valsfp_SumiL,
                              mean_valsfp_checker4 = mean_valsfp_Sumi,
                              
                              mean_valsdet_checker4u = mean_valsdet_SumiU,
                              mean_valsdet_checker4l = mean_valsdet_SumiL,
                              mean_valsdet_checker4 = mean_valsdet_Sumi,
                              
                              Con_bio_d_t_mud = Con_bio_Sumi_d_t_mud,
                              Con_bio_n_t_mud = Con_bio_Sumi_n_t_mud,
                              
                              Con_bio_d_t_cem = Con_bio_Sumi_d_t_cem,
                              Con_bio_n_t_cem = Con_bio_Sumi_n_t_cem,
                              
                              time_sequence = time_sequence_Sumi,
                              N_data = N_data_Sumi)

par(xpd=NA,cex = 1.1)

text(x = -1010, y = 2.55,"(A)")
text(x = -550, y = 2.55,"(B)")
text(x = -80, y = 2.55,"(C)")

text(x = -1010, y = 1.1,"(D)")
text(x = -550, y = 1.1,"(E)")
text(x = -80, y = 1.1,"(F)")

##############################
##
## 3 Estimated impact of ITNs, from Churcher et al. 2016
##
###############################

require(lme4)
require(RColorBrewer)
require(scales)

printed = "Standard G2 LN"
is.pbo = 0 #says whether pbo net (0 = standard, 1= PBO)
species =  1 ##  species parameters are generic as we do not yet have enough info!
metric = 1 #1 = best guess, 2= lower 95% confidence interval 3 upper

#Assay to hut mortality conversion		
alpha1=	array(c(rep(0.63445,3),rep(0.012,3),rep(1.294,3)),c(3,3))
alpha2=	array(c(rep(3.997,3),rep(3.171,3),rep(5.119,3)),c(3,3))

#Benefit of PBO in assay		
beta1=	array(c(rep(3.407,2),2.527,rep(2.666,2),1.528,rep(4.331,2),3.547),c(3,3))
beta2=	array(c(rep(5.88,2),0.891,rep(4.754,2),(0.128),rep(6.956,2),1.882),c(3,3))
beta3=	array(c(rep(0.783,2),0,rep(1.038,2),0,rep(0.543,2),0),c(3,3))

#Deterency from mortality		
delta1=	array(c(rep(0.071,3),rep(0.17,3),rep(0.255,3)),c(3,3))
delta2=	array(c(rep(1.257,3),rep(0.627,3),rep(2.073,3)),c(3,3))
delta3=	array(c(rep(-1.517,3),rep(4.03,3),rep(0.646,3)),c(3,3))

#Success from mortality		
theta1=	array(c(rep(0.025,3),rep(0.007,3),rep(0.034,3)),c(3,3))
theta2=	array(c(rep(3.317,3),rep(2.919,3),rep(4.899,3)),c(3,3))

#Decay in insecticide non-PBO net		
mup=	array(c(rep(-2.36,3),rep(2.948,3),rep(1.821,3)),c(3,3))
rhop=	array(c(rep(-3.05,3),rep(3.762,3),rep(2.322,3)),c(3,3))


kp0=0.699
net_halflife=2.64
#1-0.796 Bioko bradley study
##1-0.11 Kagera West study
#c(0.922,0.455)#

surv_bioassay=0		#measure of resistance 0=no resistance 1=100% survival in discriminating dose bioassay}

#Benefit of PBO in assay		
PBO_benefit = 1 / (1 + exp(-5.603 * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) -  -1.431))

PBO_benefit_log_upp = 1 / (1 + exp(-5.252 * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) - -1.538))
PBO_benefit_log_low = 1 / (1 + exp(-5.924 * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) - -1.320))

mort_assay=if(is.pbo==0) 1-surv_bioassay else if(is.pbo==1) PBO_benefit 
mort_hut_a = alpha1[species,metric] + alpha2[species,metric]*(mort_assay-0.5)			              	#relationship mortality in bioassay -> hut trial, logit scale}
mort_hut   = exp(mort_hut_a)/(1+exp(mort_hut_a))

det_hut_a = delta1[species,metric]+delta2[species,metric]*(mort_hut-0.5)+delta3[species,metric]*(mort_hut-0.5)^2	#relationship hut trial mortality -> deterrence}
det_hut   = ifelse(det_hut_a<0,0,det_hut_a)			                  #censored to stop becoming negative}
suc_hut   = theta1[species,metric] *exp(theta2[species,metric] *(1-mort_hut))				              #relationship hut trial mortality -> success}
rep_hut   = 1-suc_hut-mort_hut

n1n0 = 1-det_hut
kp1  = n1n0*suc_hut
jp1  = n1n0*rep_hut+(1-n1n0)
lp1  = n1n0*mort_hut

r_ITN0  = (1-kp1/kp0)*(jp1/(lp1+jp1))		          	#probability of dying with an encounter with ITN (max)}
d_ITN0  = (1-kp1/kp0)*(lp1/(lp1+jp1))		          	#probability of repeating behaviour (max)}
s_ITN0  = 1-d_ITN0-r_ITN0   

mort_max_a = alpha1[species,metric] + alpha2[species,metric]*(1-0.5)				          #maximum mortality seen in huts, used to adjust}
mort_max   = exp(mort_max_a)/(1+exp(mort_max_a))

mort_min_a = alpha1[species,metric] + alpha2[species,metric]*(0-0.5)				          #maximum mortality seen in huts, used to adjust}
mort_min   = exp(mort_min_a)/(1+exp(mort_min_a))

det_max_a = delta1[species,metric]+delta2[species,metric]*(mort_max-0.5)+delta3[species,metric]*(mort_max-0.5)^2	#relationship hut trial mortality -> deterrence}
det_max   = ifelse(det_max_a<0,0,det_max_a)			                  #censored to stop becoming negative}
suc_max   = theta1[species,metric] *exp(theta2[species,metric] *(1-mort_max))				              #relationship hut trial mortality -> success}
rep_max   = 1-suc_max-mort_max

n1n0_max = 1-det_max
kp1_max  = n1n0_max*suc_max
jp1_max  = n1n0_max*rep_max+(1-n1n0_max)
lp1_max  = n1n0_max*mort_max

r_ITN0_max  = (1-kp1_max/kp0)*(jp1_max/(lp1_max+jp1_max))		          	#probability of dying with an encounter with ITN (max)}
d_ITN0_max  = (1-kp1_max/kp0)*(lp1_max/(lp1_max+jp1_max))		          	#probability of repeating behaviour (max)}
s_ITN0_max  = 1-d_ITN0_max-r_ITN0_max


det_min_a = delta1[species,metric]+delta2[species,metric]*(mort_min-0.5)+delta3[species,metric]*(mort_min-0.5)^2	#relationship hut trial mortality -> deterrence}
det_min   = ifelse(det_min_a<0,0,det_min_a)			                  #censored to stop becoming negative}
suc_min   = theta1[species,metric] *exp(theta2[species,metric] *(1-mort_min))				              #relationship hut trial mortality -> success}
rep_min   = 1-suc_min-mort_min

n1n0_min= 1-det_min
kp1_min  = n1n0_min*suc_min
jp1_min  = n1n0_min*rep_min+(1-n1n0_min)
lp1_min  = n1n0_min*mort_min

r_ITN0_min  = (1-kp1_min/kp0)*(jp1_min/(lp1_min+jp1_min))		          	#probability of dying with an encounter with ITN (max)}
d_ITN0_min  = (1-kp1_min/kp0)*(lp1_min/(lp1_min+jp1_min))		          	#probability of repeating behaviour (max)}
s_ITN0_min  = 1-d_ITN0_min-r_ITN0_min

#{halflife}
my_max_washes_a = mup[species,metric] +rhop[species,metric]*(mort_max-0.5)		
my_max_washes   = log(2)/(exp(my_max_washes_a)/(1+exp(my_max_washes_a)))

wash_decay_rate_a = mup[species,metric] +rhop[species,metric]*(mort_hut-0.5)
wash_decay_rate   = log(2)/(exp(wash_decay_rate_a)/(1+exp(wash_decay_rate_a)))
itn_half_life     = wash_decay_rate/my_max_washes*net_halflife

## adjusted to match Griffin et al 2015 Natt Comms
Griff_d_ITN0<-0.51
Griff_r_ITN0<-0.31  ###THINK THIS NEEDS TO BE CHECKED
Griff_s_ITN0<-1-Griff_d_ITN0-Griff_r_ITN0

##ERG parameterisations
##mortality parameters modified to match Jamies paper 
##success paramater scaled to start at jamies paper values and go to elife parameters
ERG_d_ITN0 <- d_ITN0/d_ITN0_max*Griff_d_ITN0
ERG_s_ITN0 <- (Griff_s_ITN0)+(s_ITN0-s_ITN0_max)/(s_ITN0_min-s_ITN0_max)*(s_ITN0_min-Griff_s_ITN0)
ERG_r_ITN0 <- 1-ERG_d_ITN0-ERG_s_ITN0

ERG_r_ITN0;ERG_d_ITN0;itn_half_life

itn_loss = log(2)/itn_half_life
ITN_interval=3*365
## decay in efficacy of net over time
## **** this is wrong need to work this out
ITN_decay = exp(-(time/ITN_interval)*itn_loss)

r_ITN_min=0.24 
d_ITN = ERG_d_ITN0 * ITN_decay 	 		## insecticide mortality rate 
r_ITN = r_ITN_min + (ERG_r_ITN0 - r_ITN_min)*ITN_decay 
s_ITN = 1 - d_ITN - r_ITN			## successful protected human biting 

d_ITN;r_ITN;s_ITN
