#####################################################################
##
## Cone bioassay data from the field 
##
#####################################################################
NEW_DAT = read.csv("data\\raw_data_2016_2017.csv",header=TRUE)
NEW_DAT$total_exposed = NEW_DAT$exposed_mosquito_1 + NEW_DAT$exposed_mosquito_2 + NEW_DAT$exposed_mosquito_3 + NEW_DAT$exposed_mosquito_4 
NEW_DAT$total_mortality_24 = NEW_DAT$mortalidade_24h_1 + NEW_DAT$mortalidade_24h_2 + NEW_DAT$mortalidade_24h_3 + NEW_DAT$mortalidade_24h_4
NEW_DAT$total_mortality_72 = NEW_DAT$mortalidade_72h_1 + NEW_DAT$mortalidade_72h_2 + NEW_DAT$mortalidade_72h_3 + NEW_DAT$mortalidade_72h_4
head(NEW_DAT)

NEW_DAT2 = data.frame(local = rep("Actellic_2016-2017-location",nrow(NEW_DAT)),
                      test_date = NEW_DAT$test_date,
                      month_official = NEW_DAT$Month,
                      house_type = NEW_DAT$house_type,
                      participation = NEW_DAT$participation,
                      mosquito_species = NEW_DAT$mosquito_species,
                      total_exposed = NEW_DAT$total_exposed,
                      total_mortality_24 = NEW_DAT$total_mortality_24,
                      total_mortality_72 = NEW_DAT$total_mortality_72)



Sumi_new_dat = read.csv("data\\raw_data_2018_2019_SumiShield (1).csv",header=TRUE)
Sumi_new_dat$total_exposed = Sumi_new_dat$exposed_mosquito_1 + Sumi_new_dat$exposed_mosquito_2 + Sumi_new_dat$exposed_mosquito_3 #+ Sumi_new_dat$exposed_mosquito_4 
Sumi_new_dat$total_mortality_24 = Sumi_new_dat$mortalidade_24h_1 + Sumi_new_dat$mortalidade_24h_2 + Sumi_new_dat$mortalidade_24h_3# + Sumi_new_dat$mortalidade_24h_4
Sumi_new_dat$total_mortality_72 = Sumi_new_dat$mortalidade_72h_1 + Sumi_new_dat$mortalidade_72h_2 + Sumi_new_dat$mortalidade_72h_3# + Sumi_new_dat$mortalidade_72h_4
Sumi_new_dat$date_of_test = Sumi_new_dat$test_date
head(Sumi_new_dat)

Sumi_new_dat = tidyr::separate(Sumi_new_dat, "test_date", c("Year", "Month", "Day"), sep = "-")

Sumi_new_dat2 = data.frame(local = rep("Sumi_2017-2018-location",nrow(Sumi_new_dat)),
                           test_date = Sumi_new_dat$date_of_test,
                           month_official = Sumi_new_dat$month_test,
                           house_type = Sumi_new_dat$house_type,
                           participation = Sumi_new_dat$participation,
                           mosquito_species = Sumi_new_dat$mosquito_species,
                           total_exposed = Sumi_new_dat$total_exposed,
                           total_mortality_24 = Sumi_new_dat$total_mortality_24,
                           total_mortality_72 = Sumi_new_dat$total_mortality_72)


cone_bios = read.csv("data\\Cone_assay_data_v1.csv",header=TRUE) ## Original

## Decision is to use the 2016-2017 data for Actellic and then the 2017-2018 for Sumi 72 hours
cone_bios = rbind(Sumi_new_dat2, NEW_DAT2)

Con_bio_Acte_d_t_mud = 
  Con_bio_Acte_n_t_mud = 
  Con_bio_Sumi_d_t_mud = 
  Con_bio_Sumi_n_t_mud = 
  Con_bio_Acte_d_t_cem = 
  Con_bio_Acte_n_t_cem = 
  Con_bio_Sumi_d_t_cem = 
  Con_bio_Sumi_n_t_cem = numeric(length(unique(cone_bios$month_official)))

for(m in 1:length(unique(cone_bios$month_official))){
  Con_bio_Acte_d_t_mud[m] =  sum(cone_bios$total_mortality_24[cone_bios$house_type == "Mud" & cone_bios$local == "Actellic_2016-2017-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Acte_n_t_mud[m] =  sum(cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Actellic_2016-2017-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Sumi_d_t_mud[m] =  sum(cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Sumi_n_t_mud[m] =  sum(cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  
  Con_bio_Acte_d_t_cem[m] =  sum(cone_bios$total_mortality_24[cone_bios$house_type == "Cement" & cone_bios$local == "Actellic_2016-2017-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Acte_n_t_cem[m] =  sum(cone_bios$total_exposed[cone_bios$house_type == "Cement" & cone_bios$local == "Actellic_2016-2017-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Sumi_d_t_cem[m] =  sum(cone_bios$total_mortality_72[cone_bios$house_type == "Cement" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
  Con_bio_Sumi_n_t_cem[m] =  sum(cone_bios$total_exposed[cone_bios$house_type == "Cement" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]])
}

#   Con_bio_Sumi_d_t_mud[,m] =  cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]]
#   Con_bio_Sumi_n_t_mud[,m] =  cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]]
#   Con_bio_Sumi_d_t_cem[,m] =  cone_bios$total_mortality_72[cone_bios$house_type == "Cement" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]]
#   Con_bio_Sumi_n_t_cem[,m] =  cone_bios$total_exposed[cone_bios$house_type == "Cement" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[m]]
# 
#   
#   month_breakdown1 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[1]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[1]]
#   month_breakdown2 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[2]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[2]]
#   month_breakdown3 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[3]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[3]]
#   month_breakdown4 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[4]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[4]]
#   month_breakdown5 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[5]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[5]]
#   month_breakdown6 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[6]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[6]]
#   month_breakdown7 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[7]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[7]]
#   month_breakdown8 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[8]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[8]]
#   month_breakdown9 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[9]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[9]]
#   month_breakdown10 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[10]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[10]]
#   month_breakdown11 = cone_bios$total_mortality_72[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[11]]/
#     cone_bios$total_exposed[cone_bios$house_type == "Mud" & cone_bios$local == "Sumi_2017-2018-location" & cone_bios$month_official == unique(cone_bios$month_official)[11]]
#   
# points(month_breakdown1~rep(30,length(month_breakdown1)))
# points(month_breakdown2~rep(60,length(month_breakdown2)))
# points(month_breakdown3~rep(90,length(month_breakdown3)))
# points(month_breakdown4~rep(120,length(month_breakdown4)))
# points(month_breakdown5~rep(150,length(month_breakdown5)))
# points(month_breakdown6~rep(180,length(month_breakdown6)))
# points(month_breakdown7~rep(210,length(month_breakdown7)))
# points(month_breakdown8~rep(240,length(month_breakdown8)))
# points(month_breakdown9~rep(270,length(month_breakdown9)))
# points(month_breakdown10~rep(300,length(month_breakdown10)))
# points(month_breakdown11~rep(330,length(month_breakdown11)))
#        

TESTER = "ACTELLIC"
MAIN_HEADING = "Actellic 300CS cone bioassay mortality"
DEAD = DEAD_Acte
times = times_Acte
mean_valssp_checker4u = mean_valssp_ActeU
mean_valssp_checker4l = mean_valssp_ActeL
mean_valssp_checker4 = mean_valssp_Acte

mean_valsfp_checker4u = mean_valsfp_ActeU
mean_valsfp_checker4l = mean_valsfp_ActeL
mean_valsfp_checker4 = mean_valsfp_Acte

mean_valsdet_checker4u = mean_valsdet_ActeU
mean_valsdet_checker4l = mean_valsdet_ActeL
mean_valsdet_checker4 = mean_valsdet_Acte

Con_bio_d_t_mud = Con_bio_Acte_d_t_mud[1:12]
Con_bio_n_t_mud = Con_bio_Acte_n_t_mud[1:12]

Con_bio_d_t_cem = Con_bio_Acte_d_t_cem[1:12]
Con_bio_n_t_cem = Con_bio_Acte_n_t_cem[1:12]

time_sequence = time_sequence_Acte
N_data = N_data_Acte

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


stan_model_mud <- stan(file="models/log_mod.stan", 
                       data=data_list_mud, 
                       warmup=500,
                       control = list(adapt_delta = 0.9,
                                      max_treedepth = 20),
                       iter=1000, chains=4)

stan_model_cem <- stan(file="models/log_mod.stan", 
                       data=data_list_cem, 
                       warmup=500,
                       control = list(adapt_delta = 0.9,
                                      max_treedepth = 20),
                       iter=1000, chains=4)


##Figure 1A (and 1D): The first figure for each Actellic or Sumi 
plot(DEAD ~ times,ylab="Mosquito mortality (%)",ylim=c(0,1),col="aquamarine3",pch="",
     main = MAIN_HEADING,cex.main=1.2,xlim=c(1,365),xaxt="n",
     xlab="",yaxt="n",cex.lab=1.4,cex.axis=1.4,cex=1.4)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.lab=1.4,cex.axis=1.4)
axis(1,at=seq(0,365,120),labels=seq(0,365,120),cex.lab=1.4,cex.axis=1.4)

time=seq(1,365,length=365)
polygon(c(time,rev(time)),c(mean_valssp_checker4u,rev(mean_valssp_checker4l)),col=transp("aquamarine3",0.5),border=NA)
lines(mean_valssp_checker4 ~ time,ylim=c(0,1),col="aquamarine4",lwd=2)

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
points(DEAD2 ~ time_sequence,col="grey",pch=15)

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
       lwd=2,pch=15,col=c("aquamarine4",adegenet::transp(c("darkred","grey"))),
       cex=1.4,bty = "n")