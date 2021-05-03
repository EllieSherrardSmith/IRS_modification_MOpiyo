###########################################################
##
## Questionnaire 1 Matutuine and Boane
## 
## 
library(dplyr)
library(plotly)
library(crosstalk)
library(htmltools)
library(DT)


setwd('C:/Users/esherrar/Documents/Mercy Opiyo/drive-download-20200520T105638Z-001')
#setwd('C:/Users/qnhacolo/Google Drive/CISM/Studies/IRS-Soc')#Arsenio working directory
dec <- 1
df <- NULL
load('main_data_final_df.rdata')
#df$calendarMonth <- months(as.Date(df$c_date))
df$calendarMonth <- format(as.Date(df$c_date), "%Y-%m")
m1df <- df[df$c_visit_month%in%'M1',] #houses in visit M1, assumed to be the total
q1c <- NULL
load('q1_choices_q1c.rdata')
load('common_data_final_wide_cdfw.rdata')#Data with of only common (c_) variables in wide format
q2 <- NULL#questionnaire 2
smrq2 <- NULL  #sleep modified room questionnaire 2
surq2 <- NULL  #sleep unmodified room questionnaire 2
load('q2.rdata')
load('smrq2.rdata')
load('surq2.rdata')

# #Per district and month given start month
# ld <- levels(df$c_district)
# lm <- levels(df$c_visit_month)
# cm <- sort(unique(df$calendarMonth))
# 

count_by_start_month_f = function(start_date, district){
  counter = numeric(6)
  Matut_houses_oct = sort(df$houseid[df$c_district == district & df$c_visit_month == "M1" & df$calendarMonth == start_date])
  counter[1] = length(Matut_houses_oct)
  m2_mat =sort(df$houseid[df$c_district == district & df$c_visit_month == "M2"])
  m2_mat_counta = match(Matut_houses_oct, m2_mat, nomatch = NA, incomparables = NULL)
  counter[2] = length(na.omit(m2_mat_counta))
  
  m3_mat =sort(df$houseid[df$c_district == district & df$c_visit_month == "M3"])
  m3_mat_counta = match(Matut_houses_oct, m3_mat, nomatch = NA, incomparables = NULL)
  counter[3] = length(na.omit(m3_mat_counta))
  
  m4_mat =sort(df$houseid[df$c_district == district & df$c_visit_month == "M4"])
  m4_mat_counta = match(Matut_houses_oct, m4_mat, nomatch = NA, incomparables = NULL)
  counter[4] = length(na.omit(m4_mat_counta))
  
  m5_mat =sort(df$houseid[df$c_district == district & df$c_visit_month == "M5"])
  m5_mat_counta = match(Matut_houses_oct, m5_mat, nomatch = NA, incomparables = NULL)
  counter[5] = length(na.omit(m5_mat_counta))
  
  m6_mat = sort(df$houseid[df$c_district == district & df$c_visit_month == "M6"])
  m6_mat_counta = match(Matut_houses_oct, m6_mat, nomatch = NA, incomparables = NULL)
  counter[6] = length(na.omit(m6_mat_counta))
  
  return(counter)
}
count_by_start_month_f(district = "Matutuine", start_date = sort(unique(df$calendarMonth))[1])
count_by_start_month_f(district = "Matutuine", start_date = sort(unique(df$calendarMonth))[2])
count_by_start_month_f(district = "Matutuine", start_date = sort(unique(df$calendarMonth))[3])

count_by_start_month_f(district = "Boane", start_date = sort(unique(df$calendarMonth))[1])
count_by_start_month_f(district = "Boane", start_date = sort(unique(df$calendarMonth))[2])
count_by_start_month_f(district = "Boane", start_date = sort(unique(df$calendarMonth))[3])
## This recreates the data in Table 1 main maunscript

## MATUTUINE
## This is our data for Houses tracked from October
Matut_houses_oct = subset(df,df$c_district == "Matutuine" & df$c_visit_month == "M1" & df$calendarMonth == sort(unique(df$calendarMonth))[1])

tapply(Matut_houses_oct$houseid,Matut_houses_oct$housesprayed,length)
for(i in 1:nrow(Matut_houses_oct)){
  Matut_houses_oct$total_rooms[i] = sum(Matut_houses_oct$c_combinedanimalroom[i],Matut_houses_oct$c_combinedlivingroom[i],
                                        Matut_houses_oct$c_combinedbedroom[i],Matut_houses_oct$c_combinedkitchen[i],
                                        Matut_houses_oct$c_combinedstorageroom[i],Matut_houses_oct$c_combinedbathroom[i],
                                        Matut_houses_oct$c_combinedgarage[i],Matut_houses_oct$c_combinedother[i],na.rm=TRUE)
  
  Matut_houses_oct$total_rooms_added[i] = sum(Matut_houses_oct$c_addedanimalroom[i],Matut_houses_oct$c_addedlivingroom[i],
                                        Matut_houses_oct$c_addedbedroom[i],Matut_houses_oct$c_addedkitchen[i],
                                        Matut_houses_oct$c_addedstorageroom[i],Matut_houses_oct$c_addedbathroom[i],
                                        Matut_houses_oct$c_addedgarage[i],Matut_houses_oct$c_addedother[i],na.rm=TRUE)
  
  Matut_houses_oct$total_rooms_mod[i] = sum(Matut_houses_oct$c_modifiedanimalroom[i],Matut_houses_oct$c_modifiedlivingroom[i],
                                        Matut_houses_oct$c_modifiedbedroom[i],Matut_houses_oct$c_modifiedkitchen[i],
                                        Matut_houses_oct$c_modifiedstorageroom[i],Matut_houses_oct$c_modifiedbathroom[i],
                                        Matut_houses_oct$c_modifiedgarage[i],Matut_houses_oct$c_modifiedother[i],na.rm=TRUE)
  
  
}
tapply(Matut_houses_oct$c_modifiedbedroom,Matut_houses_oct$c_combinedbedroom,length)
tapply(Matut_houses_oct$c_combinedbedroom,Matut_houses_oct$c_modifiedbedroom,length)
tapply(Matut_houses_oct$c_combinedbedroom,Matut_houses_oct$c_addedbedroom,length)
## 5 of the 43,  1 bedroom compounds
## 5 of the 31,  2 bedroom compounds
## 1 of the 19,  3 bedroom compounds
## 1 pf the 10,  4 bedroom compounds
## are modified
bedrooms = c(43,31,19,10,2,3,1,0,2,0,1) ## maximum 11 bedroons
modified_bedrooms = c(5,5,1,1,0,0,0,0,0,0,0)
added_bedrooms = rep(0,11)

tapply(Matut_houses_oct$c_modifiedlivingroom,Matut_houses_oct$c_combinedlivingroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedlivingroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_addedlivingroom,length)
## 11 0f the 68, 1 livingroom compounds
## 1 of the 20, 2 living room compounds
livingrooms = c(68,20,3,1,0,1,1)
modified_livingrooms = c(11,1,rep(0,5))
added_livingrooms = rep(0,7)

## no modifications in bathrooms
## no modifications in animal rooms
## no modifications in garages

tapply(Matut_houses_oct$c_modifiedkitchen,Matut_houses_oct$c_combinedkitchen,length)
tapply(Matut_houses_oct$c_combinedkitchen,Matut_houses_oct$c_modifiedkitchen,length)
tapply(Matut_houses_oct$c_combinedkitchen,Matut_houses_oct$c_addedkitchen,length)
## 2 0f the 33, 1 kitchen compounds
kitchens = c(33,3)
modified_kitchens = c(2,0)
added_kitchens = rep(0,2)

tapply(Matut_houses_oct$c_modifiedstorageroom,Matut_houses_oct$c_combinedstorageroom,length)
tapply(Matut_houses_oct$c_combinedstorageroom,Matut_houses_oct$c_modifiedstorageroom,length)
tapply(Matut_houses_oct$c_combinedstorageroom,Matut_houses_oct$c_addedstorageroom,length)
## 2 0f the 33, 1 kitchen compounds
storage = c(9,3,2,1,1)
modified_storage = c(0,1,0,0,0)
added_storage = rep(0,5)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
to_plot_B = c(modified_bedrooms,NA,modified_livingrooms,NA,modified_kitchens,NA,modified_storage)
cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(12,8,3,5))

# par(mfrow=c(2,1))
# 
# barplot(to_plot_A,col=cols_A,
#      xlab="Rooms in compounds",xaxt="n",yaxt="n",
#      ylab="Frequency", main = "Matutuine")
# barplot(to_plot_B,add=TRUE,col = "orange",
#      xlab="",xaxt="n",yaxt="n",
#      ylab="", main = "")
# axis(1,at=seq(1,33,length=28),
#      labels=c(1:11,NA,1:7,NA,1:2,NA,1:5))
# axis(2,las=2, at=c(0,10,20,30,40,50,60))
# 
# legend("topright",
#        legend = c("Bedrooms","Living rooms","Kitchens","Storage rooms"),
#        col=unique(cols_A),pch=15,bty="n",cex=1.2)
       
       
## Total compounds with any modified rooms in M1 october start
length(Matut_houses_oct$total_rooms_mod[Matut_houses_oct$total_rooms_mod > 0])



## Now for october start FOLLOW UP MONTHS
PULL_TOTAL_MOD_ADDED_FOR_Ms = function(dataInitHH,follow_up_month){
  ## Now for M2 october start
  m2_match =Matut_houses_oct$houseid
  Matut_houses_octM2temp2 = subset(df,df$c_district == "Matutuine" & df$c_visit_month == follow_up_month)
  Matut_houses_octM2 = Matut_houses_octM2temp2[Matut_houses_octM2temp2$houseid %in% m2_match, ]
  dim(Matut_houses_octM2)
  
  for(i in 1:nrow(Matut_houses_octM2)){
    Matut_houses_octM2$total_rooms[i] = sum(Matut_houses_octM2$c_combinedanimalroom[i],Matut_houses_octM2$c_combinedlivingroom[i],
                                            Matut_houses_octM2$c_combinedbedroom[i],Matut_houses_octM2$c_combinedkitchen[i],
                                            Matut_houses_octM2$c_combinedstorageroom[i],Matut_houses_octM2$c_combinedbathroom[i],
                                            Matut_houses_octM2$c_combinedgarage[i],Matut_houses_octM2$c_combinedother[i],na.rm=TRUE)
    
    Matut_houses_octM2$total_rooms_added[i] = sum(Matut_houses_octM2$c_addedanimalroom[i],Matut_houses_octM2$c_addedlivingroom[i],
                                                    Matut_houses_octM2$c_addedbedroom[i],Matut_houses_octM2$c_addedkitchen[i],
                                                    Matut_houses_octM2$c_addedstorageroom[i],Matut_houses_octM2$c_addedbathroom[i],
                                                    Matut_houses_octM2$c_addedgarage[i],Matut_houses_octM2$c_addedother[i],na.rm=TRUE)
    
    Matut_houses_octM2$total_rooms_mod[i] = sum(Matut_houses_octM2$c_modifiedanimalroom[i],Matut_houses_octM2$c_modifiedlivingroom[i],
                                                  Matut_houses_octM2$c_modifiedbedroom[i],Matut_houses_octM2$c_modifiedkitchen[i],
                                                  Matut_houses_octM2$c_modifiedstorageroom[i],Matut_houses_octM2$c_modifiedbathroom[i],
                                                  Matut_houses_octM2$c_modifiedgarage[i],Matut_houses_octM2$c_modifiedother[i],na.rm=TRUE)
    
    
  }
  return(
    list(
    c(length(Matut_houses_octM2$total_rooms_mod[Matut_houses_octM2$total_rooms_mod > 0]),
           length(Matut_houses_octM2$total_rooms_added[Matut_houses_octM2$total_rooms_added > 0])),
    Matut_houses_octM2
           )
  )
  
}
Matut_M2 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                            follow_up_month = "M2")
Matut_M3 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                            follow_up_month = "M3")
Matut_M4 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                            follow_up_month = "M4")
Matut_M5 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                            follow_up_month = "M5")
Matut_M6 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                            follow_up_month = "M6")

## For Table 1: modified rooms
c(length(Matut_houses_oct$total_rooms_mod[Matut_houses_oct$total_rooms_mod > 0]),
  Matut_M2[[1]][1],Matut_M3[[1]][1],Matut_M4[[1]][1],Matut_M5[[1]][1],Matut_M6[[1]][1])

c(length(Matut_houses_oct$total_rooms_added[Matut_houses_oct$total_rooms_added > 0]),
  Matut_M2[[1]][2],Matut_M3[[1]][2],Matut_M4[[1]][2],Matut_M5[[1]][2],Matut_M6[[1]][2])

sum(Matut_M2[[2]]$total_rooms_added)
sum(Matut_M2[[2]]$total_rooms_mod)


tapply(Matut_M6[[2]]$c_combinedbedroom,Matut_M6[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M6 = c(4,4,1,0,0,0,0,0,0,0,0)##11
tapply(Matut_M6[[2]]$c_combinedlivingroom,Matut_M6[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M6[[2]]$c_combinedkitchen,Matut_M6[[2]]$c_modifiedkitchen,length)
tapply(Matut_M6[[2]]$c_combinedstorageroom,Matut_M6[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M6 = c(12,0,0,0,0,0,0)##7
modified_kitchens_M6 = c(1,0)##2
modified_storage_M6 = c(1,0,0,0,0)##5

tapply(Matut_M5[[2]]$c_combinedbedroom,Matut_M5[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M5 = c(6,3,0,0,0,0,0,0,0,0,0)##11
tapply(Matut_M5[[2]]$c_combinedlivingroom,Matut_M5[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M5[[2]]$c_combinedkitchen,Matut_M5[[2]]$c_modifiedkitchen,length)
tapply(Matut_M5[[2]]$c_combinedstorageroom,Matut_M5[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M5 = c(12,0,0,0,0,0,0)##7
modified_kitchens_M5 = c(0,0)##2
modified_storage_M5 = c(1,0,0,0,0)##5

tapply(Matut_M4[[2]]$c_combinedbedroom,Matut_M4[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M4 = c(14,3,0,0,0,0,0,0,0,0,0)##11
tapply(Matut_M4[[2]]$c_combinedlivingroom,Matut_M4[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M4[[2]]$c_combinedkitchen,Matut_M4[[2]]$c_modifiedkitchen,length)
tapply(Matut_M4[[2]]$c_combinedstorageroom,Matut_M4[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M4 = c(15,0,0,0,0,0,0)##7
modified_kitchens_M4 = c(1,0)##2
modified_storage_M4 = c(1,0,0,0,0)##5


tapply(Matut_M3[[2]]$c_combinedbedroom,Matut_M3[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M3 = c(12,5,0,0,0,0,0,0,0,0,0)##11
tapply(Matut_M3[[2]]$c_combinedlivingroom,Matut_M3[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M3[[2]]$c_combinedkitchen,Matut_M3[[2]]$c_modifiedkitchen,length)
tapply(Matut_M3[[2]]$c_combinedstorageroom,Matut_M3[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M3 = c(11,0,0,0,0,0,0)##7
modified_kitchens_M3 = c(0,0)##2
modified_storage_M3 = c(1,0,0,0,0)##5


tapply(Matut_M2[[2]]$c_combinedbedroom,Matut_M2[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M2 = c(3,1,0,0,0,0,0,0,0,0,0)##11
tapply(Matut_M2[[2]]$c_combinedlivingroom,Matut_M2[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_modifiedkitchen,length)
tapply(Matut_M2[[2]]$c_combinedstorageroom,Matut_M2[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M2 = c(6,0,0,0,0,0,0)##7
modified_kitchens_M2 = c(0,0)##2
modified_storage_M2 = c(0,0,0,0,0)##5

# par(mfrow=c(2,1))
# 96 bedrooms ## sum(c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6)*c(1:11))
# 69 living rooms ## sum(c(modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6)*c(1:7))
# 96 kitchens ## sum(c(modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5+modified_kitchens_M6)*c(1:2))
# 69 storage ## sum(c(modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5+modified_storage_M6)*c(1:5))
96/sum(Matut_houses_oct$c_combinedbedroom,na.rm = T)
69/sum(Matut_houses_oct$c_combinedlivingroom,na.rm = T)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
to_plot_M6 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6,NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6,NA,modified_kitchens+modified_kitchens_M4+modified_kitchens_M6,NA,modified_storage+modified_storage_M3+modified_storage_M4+modified_storage_M5++modified_storage_M6)
to_plot_M5 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5,                     NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5,                        NA,modified_kitchens+modified_kitchens_M4,                     NA,modified_storage+modified_storage_M3+modified_storage_M4+modified_storage_M5)##GREEN
to_plot_M4 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4,                                          NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4,                                                NA,modified_kitchens+modified_kitchens_M4,                     NA,modified_storage+modified_storage_M3+modified_storage_M4)##RED
to_plot_M3 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3,                                                               NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3,                                                                        NA,modified_kitchens,                                          NA,modified_storage+modified_storage_M3)##BLUE
to_plot_M2 = c(modified_bedrooms+modified_bedrooms_M2,                                                                                    NA,modified_livingrooms+modified_livingrooms_M2,                                                                                                NA,modified_kitchens,                                          NA,modified_storage)##PURPLE
to_plot_M1 = c(modified_bedrooms,NA,modified_livingrooms,NA,modified_kitchens,NA,modified_storage)##ORANGE

length(to_plot_A);length(to_plot_M6);length(to_plot_M5);length(to_plot_M4);length(to_plot_M3);length(to_plot_M2);length(to_plot_M1)

sum(to_plot_M6,na.rm=TRUE)
##TOTAL ROOMS MODIFIED
FROM_to_plot_M6 = c(c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6)*c(1:11),
                    c(modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6)*c(1:7),
                    c(modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5+modified_kitchens_M6)*c(1:2),
                    c(modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5+modified_storage_M6)*c(1:5))
sum(FROM_to_plot_M6,na.rm=TRUE)


cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(12,8,3,5))

# barplot(to_plot_A,col=cols_A,
#         xlab="Rooms in compounds",xaxt="n",yaxt="n",
#         ylab="Frequency", main = "Matutuine")
# 
# barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M1,add=TRUE,col = "darkred",
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# axis(1,at=seq(1,33,length=28),
#      labels=c(1:11,NA,1:7,NA,1:2,NA,1:5))
# axis(2,las=2, at=c(0,10,20,30,40,50,60))
# 
# legend("topright",
#        legend = c("Bedrooms","Living rooms","Kitchens","Storage rooms"),
#        col=unique(cols_A),pch=15,bty="n",cex=1.2)


### And added rooms?

tapply(Matut_M6[[2]]$c_combinedbedroom,Matut_M6[[2]]$c_addedbedroom,length)
added_bedrooms_M6 = c(6,1,0,0,0,0,0,0,0,0,0)##11
tapply(Matut_M6[[2]]$c_combinedlivingroom,Matut_M6[[2]]$c_addedlivingroom,length)
tapply(Matut_M6[[2]]$c_combinedkitchen,Matut_M6[[2]]$c_addedkitchen,length)
tapply(Matut_M6[[2]]$c_combinedstorageroom,Matut_M6[[2]]$c_addedstorageroom,length)
added_livingrooms_M6 = c(4,0,0,0,0,0,0)##7
added_kitchens_M6 = c(0,0)##2
added_storage_M6 = c(1,0,0,0,0)##5

tapply(Matut_M5[[2]]$c_combinedbedroom,Matut_M5[[2]]$c_addedbedroom,length)
added_bedrooms_M5 = c(5,3,0,0,0,0,0,0,0,0,0)##11
tapply(Matut_M5[[2]]$c_combinedlivingroom,Matut_M5[[2]]$c_addedlivingroom,length)
tapply(Matut_M5[[2]]$c_combinedkitchen,Matut_M5[[2]]$c_addedkitchen,length)
tapply(Matut_M5[[2]]$c_combinedstorageroom,Matut_M5[[2]]$c_addedstorageroom,length)
added_livingrooms_M5 = c(3,0,0,0,0,0,0)##7
added_kitchens_M5 = c(0,0)##2
added_storage_M5 = c(2,0,0,0,0)##5

tapply(Matut_M4[[2]]$c_combinedbedroom,Matut_M4[[2]]$c_addedbedroom,length)
added_bedrooms_M4 = c(6,3,0,0,0,0,0,0,0,0,0)##11
tapply(Matut_M4[[2]]$c_combinedlivingroom,Matut_M4[[2]]$c_addedlivingroom,length)
tapply(Matut_M4[[2]]$c_combinedkitchen,Matut_M4[[2]]$c_addedkitchen,length)
tapply(Matut_M4[[2]]$c_combinedstorageroom,Matut_M4[[2]]$c_addedstorageroom,length)
added_livingrooms_M4 = c(2,0,0,0,0,0,0)##7
added_kitchens_M4 = c(2,0)##2
added_storage_M4 = c(1,0,0,0,0)##5


tapply(Matut_M3[[2]]$c_combinedbedroom,Matut_M3[[2]]$c_addedbedroom,length)
added_bedrooms_M3 = c(12,5,0,0,0,0,0,0,0,0,0)##11
tapply(Matut_M3[[2]]$c_combinedlivingroom,Matut_M3[[2]]$c_addedlivingroom,length)
tapply(Matut_M3[[2]]$c_combinedkitchen,Matut_M3[[2]]$c_addedkitchen,length)
tapply(Matut_M3[[2]]$c_combinedstorageroom,Matut_M3[[2]]$c_addedstorageroom,length)
added_livingrooms_M3 = c(11,0,0,0,0,0,0)##7
added_kitchens_M3 = c(0,0)##2
added_storage_M3 = c(1,0,0,0,0)##5


tapply(Matut_M2[[2]]$c_combinedbedroom,Matut_M2[[2]]$c_addedbedroom,length)
added_bedrooms_M2 = c(5,1,0,0,0,0,0,0,0,0,0)##11
tapply(Matut_M2[[2]]$c_combinedlivingroom,Matut_M2[[2]]$c_addedlivingroom,length)
tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_addedkitchen,length)
tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_addedstorageroom,length)
added_livingrooms_M2 = c(1,1,0,0,0,0,0)##7
added_kitchens_M2 = c(4,0)##2
added_storage_M2 = c(2,0,0,0,0)##5

## (There were no added rooms in M1)

par(mfrow=c(2,1))

to_plota_M6 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5+added_bedrooms_M6,NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5+added_livingrooms_M6, NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5+added_kitchens_M6,NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5++added_storage_M6)
to_plota_M5 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5,                  NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5,                      NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5,                  NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5)##GREEN
to_plota_M4 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4,                                    NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4,                                           NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4,                                    NA,added_storage_M2+added_storage_M3+added_storage_M4)##RED
to_plota_M3 = c(added_bedrooms_M2+added_bedrooms_M3,                                                      NA,added_livingrooms_M2+added_livingrooms_M3,                                                                NA,added_kitchens_M2+added_kitchens_M3,                                                      NA,added_storage_M2+added_storage_M3)##BLUE
to_plota_M2 = c(added_bedrooms_M2,                                                                        NA,added_livingrooms_M2,                                                                                     NA,added_kitchens_M2,                                                                        NA,added_storage_M2)##PURPLE




cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(12,8,3,5))

barplot(to_plot_A+to_plota_M6,col=cols_A,lty=2,
        xlab="Rooms in compounds",xaxt="n",yaxt="n",
        ylab="Frequency", main = "Matutuine (October cohort)")
barplot(to_plot_A+to_plota_M5,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M4,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M3,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M2,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",
        ylab="", main = "")


barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M1,add=TRUE,col = "darkred",
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
axis(1,at=seq(1,33,length=28),
     labels=c(1:11,NA,1:7,NA,1:2,NA,1:5))
axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80,90))

abline(v=14,lty=2,lwd=2,col="grey20")
text(7,50,"Bedrooms")

abline(v=23.5,lty=2,lwd=2,col="grey20")
text(18.7,50,"Living rooms")

abline(v=27,lty=2,lwd=2,col="grey20")
text(25,50,"Kitchens")
text(30,50,"Storage rooms")

legend(2.6,90,
       legend = c("Added rooms (above solid line)",
                  "Modified rooms","M1 - M6 (Darker to paler shades)"),
       col=c("grey70","orange",NA),pch=15,bty="n",cex=1)



##
## REPEAT FOR BOANE
## This is our data for Houses tracked from October
Boane_houses_oct = subset(df,df$c_district == "Boane" & df$c_visit_month == "M1" & df$calendarMonth == sort(unique(df$calendarMonth))[1])

tapply(Boane_houses_oct$houseid,Boane_houses_oct$housesprayed,length)
for(i in 1:nrow(Boane_houses_oct)){
  Boane_houses_oct$total_rooms[i] = sum(Boane_houses_oct$c_combinedanimalroom[i],Boane_houses_oct$c_combinedlivingroom[i],
                                        Boane_houses_oct$c_combinedbedroom[i],Boane_houses_oct$c_combinedkitchen[i],
                                        Boane_houses_oct$c_combinedstorageroom[i],Boane_houses_oct$c_combinedbathroom[i],
                                        Boane_houses_oct$c_combinedgarage[i],Boane_houses_oct$c_combinedother[i],na.rm=TRUE)
  
  Boane_houses_oct$total_rooms_added[i] = sum(Boane_houses_oct$c_addedanimalroom[i],Boane_houses_oct$c_addedlivingroom[i],
                                              Boane_houses_oct$c_addedbedroom[i],Boane_houses_oct$c_addedkitchen[i],
                                              Boane_houses_oct$c_addedstorageroom[i],Boane_houses_oct$c_addedbathroom[i],
                                              Boane_houses_oct$c_addedgarage[i],Boane_houses_oct$c_addedother[i],na.rm=TRUE)
  
  Boane_houses_oct$total_rooms_mod[i] = sum(Boane_houses_oct$c_modifiedanimalroom[i],Boane_houses_oct$c_modifiedlivingroom[i],
                                            Boane_houses_oct$c_modifiedbedroom[i],Boane_houses_oct$c_modifiedkitchen[i],
                                            Boane_houses_oct$c_modifiedstorageroom[i],Boane_houses_oct$c_modifiedbathroom[i],
                                            Boane_houses_oct$c_modifiedgarage[i],Boane_houses_oct$c_modifiedother[i],na.rm=TRUE)
  
  
}
tapply(Boane_houses_oct$c_modifiedbedroom,Boane_houses_oct$c_combinedbedroom,length)
tapply(Boane_houses_oct$c_combinedbedroom,Boane_houses_oct$c_modifiedbedroom,length)
tapply(Boane_houses_oct$c_combinedbedroom,Boane_houses_oct$c_addedbedroom,length)
## are modified
bedrooms = c(19,35,25,3,4) ## maximum 5 bedroons
modified_bedrooms = c(1,0,1,0,0)

tapply(Boane_houses_oct$c_modifiedlivingroom,Boane_houses_oct$c_combinedlivingroom,length)
tapply(Boane_houses_oct$c_combinedlivingroom,Boane_houses_oct$c_modifiedlivingroom,length)
tapply(Boane_houses_oct$c_combinedlivingroom,Boane_houses_oct$c_addedlivingroom,length)
## living room compounds
livingrooms = c(65,11)
modified_livingrooms = c(3,1)

## no modifications in bathrooms
## no modifications in animal rooms
## no modifications in garages

tapply(Boane_houses_oct$c_modifiedkitchen,Boane_houses_oct$c_combinedkitchen,length)
tapply(Boane_houses_oct$c_combinedkitchen,Boane_houses_oct$c_modifiedkitchen,length)
tapply(Boane_houses_oct$c_combinedkitchen,Boane_houses_oct$c_addedkitchen,length)
## 2 0f the 33, 1 kitchen compounds
kitchens = c(21)
modified_kitchens = c(0)

tapply(Boane_houses_oct$c_modifiedstorageroom,Boane_houses_oct$c_combinedstorageroom,length)
tapply(Boane_houses_oct$c_combinedstorageroom,Boane_houses_oct$c_modifiedstorageroom,length)
tapply(Boane_houses_oct$c_combinedstorageroom,Boane_houses_oct$c_addedstorageroom,length)
## 2 0f the 33, 1 kitchen compounds
storage = c(4)
modified_storage = c(0)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
# to_plot_B = c(modified_bedrooms,NA,modified_livingrooms,NA,0,NA,0)
cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(6,3,2,2))

# 
# barplot(to_plot_A,col=cols_A,
#         xlab="Rooms in compounds",xaxt="n",yaxt="n",
#         ylab="Frequency", main = "Boaneuine")
# barplot(to_plot_B,add=TRUE,col = "orange",
#         xlab="",xaxt="n",yaxt="n",
#         ylab="", main = "")
# axis(1,at=seq(1,14,length=12),
#      labels=c(1:5,NA,1:2,NA,1,NA,1))
# axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80))
# 

## Total compounds with any modified rooms in M1 october start
length(Boane_houses_oct$total_rooms_mod[Boane_houses_oct$total_rooms_mod > 0])



## Now for october start FOLLOW UP MONTHS
PULL_TOTAL_MOD_ADDED_FOR_Ms = function(dataInitHH,follow_up_month){
  ## Now for M2 october start
  m2_match = Boane_houses_oct$houseid
  Boane_houses_octM2temp2 = subset(df,df$c_district == "Boane" & df$c_visit_month == follow_up_month)
  Boane_houses_octM2 = Boane_houses_octM2temp2[Boane_houses_octM2temp2$houseid %in% m2_match, ]
  dim(Boane_houses_octM2)
  
  for(i in 1:nrow(Boane_houses_octM2)){
    Boane_houses_octM2$total_rooms[i] = sum(Boane_houses_octM2$c_combinedanimalroom[i],Boane_houses_octM2$c_combinedlivingroom[i],
                                            Boane_houses_octM2$c_combinedbedroom[i],Boane_houses_octM2$c_combinedkitchen[i],
                                            Boane_houses_octM2$c_combinedstorageroom[i],Boane_houses_octM2$c_combinedbathroom[i],
                                            Boane_houses_octM2$c_combinedgarage[i],Boane_houses_octM2$c_combinedother[i],na.rm=TRUE)
    
    Boane_houses_octM2$total_rooms_added[i] = sum(Boane_houses_octM2$c_addedanimalroom[i],Boane_houses_octM2$c_addedlivingroom[i],
                                                  Boane_houses_octM2$c_addedbedroom[i],Boane_houses_octM2$c_addedkitchen[i],
                                                  Boane_houses_octM2$c_addedstorageroom[i],Boane_houses_octM2$c_addedbathroom[i],
                                                  Boane_houses_octM2$c_addedgarage[i],Boane_houses_octM2$c_addedother[i],na.rm=TRUE)
    
    Boane_houses_octM2$total_rooms_mod[i] = sum(Boane_houses_octM2$c_modifiedanimalroom[i],Boane_houses_octM2$c_modifiedlivingroom[i],
                                                Boane_houses_octM2$c_modifiedbedroom[i],Boane_houses_octM2$c_modifiedkitchen[i],
                                                Boane_houses_octM2$c_modifiedstorageroom[i],Boane_houses_octM2$c_modifiedbathroom[i],
                                                Boane_houses_octM2$c_modifiedgarage[i],Boane_houses_octM2$c_modifiedother[i],na.rm=TRUE)
    
    
  }
  return(
    list(
      c(length(Boane_houses_octM2$total_rooms_mod[Boane_houses_octM2$total_rooms_mod > 0]),
        length(Boane_houses_octM2$total_rooms_added[Boane_houses_octM2$total_rooms_added > 0])),
      Boane_houses_octM2
    )
  )
  
}
Boane_M2 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M2")
Boane_M3 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M3")
Boane_M4 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M4")
Boane_M5 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M5")
Boane_M6 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M6")

## For Table 1: modified rooms
c(length(Boane_houses_oct$total_rooms_mod[Boane_houses_oct$total_rooms_mod > 0]),
  Boane_M2[[1]][1],Boane_M3[[1]][1],Boane_M4[[1]][1],Boane_M5[[1]][1],Boane_M6[[1]][1])

c(length(Boane_houses_oct$total_rooms_added[Boane_houses_oct$total_rooms_added > 0]),
  Boane_M2[[1]][2],Boane_M3[[1]][2],Boane_M4[[1]][2],Boane_M5[[1]][2],Boane_M6[[1]][2])


tapply(Boane_M6[[2]]$c_combinedbedroom,Boane_M6[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M6 = c(0,0,0,0,0)##5
tapply(Boane_M6[[2]]$c_combinedlivingroom,Boane_M6[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M6[[2]]$c_combinedkitchen,Boane_M6[[2]]$c_modifiedkitchen,length)
tapply(Boane_M6[[2]]$c_combinedstorageroom,Boane_M6[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M6 = c(0,0)##2
modified_kitchens_M6 = c(0)##1
modified_storage_M6 = c(0)##1

tapply(Boane_M5[[2]]$c_combinedbedroom,Boane_M5[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M5 = c(0,0,0,0,0)##11
tapply(Boane_M5[[2]]$c_combinedlivingroom,Boane_M5[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M5[[2]]$c_combinedkitchen,Boane_M5[[2]]$c_modifiedkitchen,length)
tapply(Boane_M5[[2]]$c_combinedstorageroom,Boane_M5[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M5 = c(0,0)##2
modified_kitchens_M5 = c(0)##1
modified_storage_M5 = c(0)##1


tapply(Boane_M4[[2]]$c_combinedbedroom,Boane_M4[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M4 = c(0,0,0,0,0)##11
tapply(Boane_M4[[2]]$c_combinedlivingroom,Boane_M4[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M4[[2]]$c_combinedkitchen,Boane_M4[[2]]$c_modifiedkitchen,length)
tapply(Boane_M4[[2]]$c_combinedstorageroom,Boane_M4[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M4 = c(0,0)##7
modified_kitchens_M4 = c(0)##2
modified_storage_M4 = c(0)##5


tapply(Boane_M3[[2]]$c_combinedbedroom,Boane_M3[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M3 = c(0,1,1,0,0)##11
tapply(Boane_M3[[2]]$c_combinedlivingroom,Boane_M3[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M3[[2]]$c_combinedkitchen,Boane_M3[[2]]$c_modifiedkitchen,length)
tapply(Boane_M3[[2]]$c_combinedstorageroom,Boane_M3[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M3 = c(1,0)##7
modified_kitchens_M3 = c(0)##2
modified_storage_M3 = c(0)##5


tapply(Boane_M2[[2]]$c_combinedbedroom,Boane_M2[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M2 = c(0,0,0,0,0)##11
tapply(Boane_M2[[2]]$c_combinedlivingroom,Boane_M2[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M2[[2]]$c_combinedkitchen,Boane_M2[[2]]$c_modifiedkitchen,length)
tapply(Boane_M2[[2]]$c_combinedstorageroom,Boane_M2[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M2 = c(0,0)##7
modified_kitchens_M2 = c(0)##2
modified_storage_M2 = c(0)##5

# par(mfrow=c(2,1))
# sum(c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6)*c(1,2,3,4,5))
# sum(c(modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6)*c(1,2))
9/sum(Boane_houses_oct$c_combinedbedroom,na.rm = T)
6/sum(Boane_houses_oct$c_combinedlivingroom,na.rm = T)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
to_plot_M6 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6,NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6,NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5+modified_kitchens_M6,NA,0)
to_plot_M5 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5,                     NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5,                        NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5,                     NA,0)##GREEN
to_plot_M4 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4,                                          NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4,                                                NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4,                                          NA,0)##RED
to_plot_M3 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3,                                                               NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3,                                                                        NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3,                                                               NA,0)##BLUE
to_plot_M2 = c(modified_bedrooms+modified_bedrooms_M2,                                                                                    NA,modified_livingrooms+modified_livingrooms_M2,                                                                                                NA,modified_kitchens+modified_kitchens_M2,                                                                                    NA,0)##PURPLE
to_plot_M1 = c(modified_bedrooms,NA,modified_livingrooms,NA,modified_kitchens,NA,modified_storage)##ORANGE

length(to_plot_A);length(to_plot_M6);length(to_plot_M5);length(to_plot_M4);length(to_plot_M3);length(to_plot_M2);length(to_plot_M1)

cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(6,3,2,2))
# 
# barplot(to_plot_A,col=cols_A,
#         xlab="Rooms in compounds",xaxt="n",yaxt="n",
#         ylab="Frequency", main = "Boaneuine")
# 
# barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M1,add=TRUE,col = "darkred",
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# axis(1,at=seq(1,14,length=12),
#      labels=c(1:5,NA,1:2,NA,1,NA,1))
# axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80))
# legend("topright",
#        legend = c("Bedrooms","Living rooms","Kitchens","Storage rooms"),
#        col=unique(cols_A),pch=15,bty="n",cex=1.2)


### And added rooms?

tapply(Boane_M6[[2]]$c_combinedbedroom,Boane_M6[[2]]$c_addedbedroom,length)
added_bedrooms_M6 = c(0,0,0,0,0)##5
tapply(Boane_M6[[2]]$c_combinedlivingroom,Boane_M6[[2]]$c_addedlivingroom,length)
tapply(Boane_M6[[2]]$c_combinedkitchen,Boane_M6[[2]]$c_addedkitchen,length)
tapply(Boane_M6[[2]]$c_combinedstorageroom,Boane_M6[[2]]$c_addedstorageroom,length)
added_livingrooms_M6 = c(0,0)##2
added_kitchens_M6 = c(1)##1
added_storage_M6 = c(0)##1

tapply(Boane_M5[[2]]$c_combinedbedroom,Boane_M5[[2]]$c_addedbedroom,length)
added_bedrooms_M5 = c(0,0,0,0,0)##11
tapply(Boane_M5[[2]]$c_combinedlivingroom,Boane_M5[[2]]$c_addedlivingroom,length)
tapply(Boane_M5[[2]]$c_combinedkitchen,Boane_M5[[2]]$c_addedkitchen,length)
tapply(Boane_M5[[2]]$c_combinedstorageroom,Boane_M5[[2]]$c_addedstorageroom,length)
added_livingrooms_M5 = c(0,0)##2
added_kitchens_M5 = c(0)##1
added_storage_M5 = c(0)##1

tapply(Boane_M4[[2]]$c_combinedbedroom,Boane_M4[[2]]$c_addedbedroom,length)
added_bedrooms_M4 = c(1,0,0,0,0)##11
tapply(Boane_M4[[2]]$c_combinedlivingroom,Boane_M4[[2]]$c_addedlivingroom,length)
tapply(Boane_M4[[2]]$c_combinedkitchen,Boane_M4[[2]]$c_addedkitchen,length)
tapply(Boane_M4[[2]]$c_combinedstorageroom,Boane_M4[[2]]$c_addedstorageroom,length)
added_livingrooms_M4 = c(0,0)##7
added_kitchens_M4 = c(0)##2
added_storage_M4 = c(0)##5


tapply(Boane_M3[[2]]$c_combinedbedroom,Boane_M3[[2]]$c_addedbedroom,length)
added_bedrooms_M3 = c(1,0,0,0,0)##11
tapply(Boane_M3[[2]]$c_combinedlivingroom,Boane_M3[[2]]$c_addedlivingroom,length)
tapply(Boane_M3[[2]]$c_combinedkitchen,Boane_M3[[2]]$c_addedkitchen,length)
tapply(Boane_M3[[2]]$c_combinedstorageroom,Boane_M3[[2]]$c_addedstorageroom,length)
added_livingrooms_M3 = c(0,0)##7
added_kitchens_M3 = c(0)##2
added_storage_M3 = c(0)##5


tapply(Boane_M2[[2]]$c_combinedbedroom,Boane_M2[[2]]$c_addedbedroom,length)
added_bedrooms_M2 = c(0,0,0,0,0)##11
tapply(Boane_M2[[2]]$c_combinedlivingroom,Boane_M2[[2]]$c_addedlivingroom,length)
tapply(Boane_M2[[2]]$c_combinedkitchen,Boane_M2[[2]]$c_addedkitchen,length)
tapply(Boane_M2[[2]]$c_combinedkitchen,Boane_M2[[2]]$c_addedstorageroom,length)
added_livingrooms_M2 = c(0,0)##7
added_kitchens_M2 = c(0)##2
added_storage_M2 = c(0)##5

## (There were no added rooms in M1)

# par(mfrow=c(2,1))

to_plota_M6 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5+added_bedrooms_M6,NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5+added_livingrooms_M6, NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5+added_kitchens_M6,NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5++added_storage_M6)
to_plota_M5 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5,                  NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5,                      NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5,                  NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5)##GREEN
to_plota_M4 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4,                                    NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4,                                           NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4,                                    NA,added_storage_M2+added_storage_M3+added_storage_M4)##RED
to_plota_M3 = c(added_bedrooms_M2+added_bedrooms_M3,                                                      NA,added_livingrooms_M2+added_livingrooms_M3,                                                                NA,added_kitchens_M2+added_kitchens_M3,                                                      NA,added_storage_M2+added_storage_M3)##BLUE
to_plota_M2 = c(added_bedrooms_M2,                                                                        NA,added_livingrooms_M2,                                                                                     NA,added_kitchens_M2,                                                                        NA,added_storage_M2)##PURPLE




cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(6,3,2,2))

barplot(to_plot_A+to_plota_M6,col=cols_A,lty=2,
        xlab="Rooms in compounds",xaxt="n",yaxt="n",
        ylab="Frequency", main = "Boane (October cohort)")
barplot(to_plot_A+to_plota_M5,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M4,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M3,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M2,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",
        ylab="", main = "")


barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M1,add=TRUE,col = "darkred",
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
axis(1,at=seq(1,14,length=12),
     labels=c(1:5,NA,1:2,NA,1,NA,1))
axis(2,las=2, at=c(0,10,20,30,40,50,60))

abline(v=6.5,lty=2,lwd=2,col="grey20")
text(3.2,50,"Bedrooms")

abline(v=10.2,lty=2,lwd=2,col="grey20")
text(9.2,50,"Living rooms")

abline(v=13,lty=2,lwd=2,col="grey20")
text(11.2,50,"Kitchens")
text(14,50,"Storage rooms")

legend(2.6,90,
       legend = c("Added rooms (above solid line)",
                  "Modified rooms","M1 - M6 (Darker to paler shades)"),
       col=c("grey70","orange",NA),pch=15,bty="n",cex=1)


##
###
####
#####
####### Now November cohort
#####
####
###
##
#

## MATUTUINE
## This is our data for Houses tracked from October
Matut_houses_oct = subset(df,df$c_district == "Matutuine" & df$c_visit_month == "M1" & df$calendarMonth == sort(unique(df$calendarMonth))[2])

tapply(Matut_houses_oct$houseid,Matut_houses_oct$housesprayed,length)
for(i in 1:nrow(Matut_houses_oct)){
  Matut_houses_oct$total_rooms[i] = sum(Matut_houses_oct$c_combinedanimalroom[i],Matut_houses_oct$c_combinedlivingroom[i],
                                        Matut_houses_oct$c_combinedbedroom[i],Matut_houses_oct$c_combinedkitchen[i],
                                        Matut_houses_oct$c_combinedstorageroom[i],Matut_houses_oct$c_combinedbathroom[i],
                                        Matut_houses_oct$c_combinedgarage[i],Matut_houses_oct$c_combinedother[i],na.rm=TRUE)
  
  Matut_houses_oct$total_rooms_added[i] = sum(Matut_houses_oct$c_addedanimalroom[i],Matut_houses_oct$c_addedlivingroom[i],
                                              Matut_houses_oct$c_addedbedroom[i],Matut_houses_oct$c_addedkitchen[i],
                                              Matut_houses_oct$c_addedstorageroom[i],Matut_houses_oct$c_addedbathroom[i],
                                              Matut_houses_oct$c_addedgarage[i],Matut_houses_oct$c_addedother[i],na.rm=TRUE)
  
  Matut_houses_oct$total_rooms_mod[i] = sum(Matut_houses_oct$c_modifiedanimalroom[i],Matut_houses_oct$c_modifiedlivingroom[i],
                                            Matut_houses_oct$c_modifiedbedroom[i],Matut_houses_oct$c_modifiedkitchen[i],
                                            Matut_houses_oct$c_modifiedstorageroom[i],Matut_houses_oct$c_modifiedbathroom[i],
                                            Matut_houses_oct$c_modifiedgarage[i],Matut_houses_oct$c_modifiedother[i],na.rm=TRUE)
  
  
}
tapply(Matut_houses_oct$c_modifiedbedroom,Matut_houses_oct$c_combinedbedroom,length)
tapply(Matut_houses_oct$c_combinedbedroom,Matut_houses_oct$c_modifiedbedroom,length)
tapply(Matut_houses_oct$c_combinedbedroom,Matut_houses_oct$c_addedbedroom,length)
## are modified
bedrooms = c(27,27,15,6,2,5) ## maximum 6 bedroons
modified_bedrooms = c(5,3,1,0,1,0)

tapply(Matut_houses_oct$c_modifiedlivingroom,Matut_houses_oct$c_combinedlivingroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedlivingroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_addedlivingroom,length)
## 11 0f the 68, 1 livingroom compounds
## 1 of the 20, 2 living room compounds
livingrooms = c(52,13,6,4)##4
modified_livingrooms = c(9,1,0,1)

tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedbathroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedanimalroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedgarage,length)
## no modifications in bathrooms
## no modifications in animal rooms
## no modifications in garages

tapply(Matut_houses_oct$c_modifiedkitchen,Matut_houses_oct$c_combinedkitchen,length)
tapply(Matut_houses_oct$c_combinedkitchen,Matut_houses_oct$c_modifiedkitchen,length)
tapply(Matut_houses_oct$c_combinedkitchen,Matut_houses_oct$c_addedkitchen,length)
## 2 0f the 33, 1 kitchen compounds
kitchens = c(47,3)##2
modified_kitchens = c(0,1)

tapply(Matut_houses_oct$c_modifiedstorageroom,Matut_houses_oct$c_combinedstorageroom,length)
tapply(Matut_houses_oct$c_combinedstorageroom,Matut_houses_oct$c_modifiedstorageroom,length)
tapply(Matut_houses_oct$c_combinedstorageroom,Matut_houses_oct$c_addedstorageroom,length)
## 2 0f the 33, 1 kitchen compounds
storage = c(5,6,1)##3
modified_storage = c(0,0,0)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
to_plot_B = c(modified_bedrooms,NA,modified_livingrooms,NA,modified_kitchens,NA,modified_storage)
cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(7,5,2,3))

# par(mfrow=c(2,1))
# 
# barplot(to_plot_A,col=cols_A,
#      xlab="Rooms in compounds",xaxt="n",yaxt="n",
#      ylab="Frequency", main = "Matutuine")
# barplot(to_plot_B,add=TRUE,col = "orange",
#      xlab="",xaxt="n",yaxt="n",
#      ylab="", main = "")
# axis(1,at=seq(1,33,length=28),
#      labels=c(1:11,NA,1:7,NA,1:2,NA,1:5))
# axis(2,las=2, at=c(0,10,20,30,40,50,60))
# 
# legend("topright",
#        legend = c("Bedrooms","Living rooms","Kitchens","Storage rooms"),
#        col=unique(cols_A),pch=15,bty="n",cex=1.2)


## Total compounds with any modified rooms in M1 october start
length(Matut_houses_oct$total_rooms_mod[Matut_houses_oct$total_rooms_mod > 0])




## Now for october start FOLLOW UP MONTHS
PULL_TOTAL_MOD_ADDED_FOR_Ms = function(dataInitHH,follow_up_month){
  ## Now for M2 october start
  m2_match =Matut_houses_oct$houseid
  Matut_houses_octM2temp2 = subset(df,df$c_district == "Matutuine" & df$c_visit_month == follow_up_month)
  Matut_houses_octM2 = Matut_houses_octM2temp2[Matut_houses_octM2temp2$houseid %in% m2_match, ]
  dim(Matut_houses_octM2)
  
  for(i in 1:nrow(Matut_houses_octM2)){
    Matut_houses_octM2$total_rooms[i] = sum(Matut_houses_octM2$c_combinedanimalroom[i],Matut_houses_octM2$c_combinedlivingroom[i],
                                            Matut_houses_octM2$c_combinedbedroom[i],Matut_houses_octM2$c_combinedkitchen[i],
                                            Matut_houses_octM2$c_combinedstorageroom[i],Matut_houses_octM2$c_combinedbathroom[i],
                                            Matut_houses_octM2$c_combinedgarage[i],Matut_houses_octM2$c_combinedother[i],na.rm=TRUE)
    
    Matut_houses_octM2$total_rooms_added[i] = sum(Matut_houses_octM2$c_addedanimalroom[i],Matut_houses_octM2$c_addedlivingroom[i],
                                                  Matut_houses_octM2$c_addedbedroom[i],Matut_houses_octM2$c_addedkitchen[i],
                                                  Matut_houses_octM2$c_addedstorageroom[i],Matut_houses_octM2$c_addedbathroom[i],
                                                  Matut_houses_octM2$c_addedgarage[i],Matut_houses_octM2$c_addedother[i],na.rm=TRUE)
    
    Matut_houses_octM2$total_rooms_mod[i] = sum(Matut_houses_octM2$c_modifiedanimalroom[i],Matut_houses_octM2$c_modifiedlivingroom[i],
                                                Matut_houses_octM2$c_modifiedbedroom[i],Matut_houses_octM2$c_modifiedkitchen[i],
                                                Matut_houses_octM2$c_modifiedstorageroom[i],Matut_houses_octM2$c_modifiedbathroom[i],
                                                Matut_houses_octM2$c_modifiedgarage[i],Matut_houses_octM2$c_modifiedother[i],na.rm=TRUE)
    
    
  }
  return(
    list(
      c(length(Matut_houses_octM2$total_rooms_mod[Matut_houses_octM2$total_rooms_mod > 0]),
        length(Matut_houses_octM2$total_rooms_added[Matut_houses_octM2$total_rooms_added > 0])),
      Matut_houses_octM2
    )
  )
  
}
Matut_M2 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M2")
Matut_M3 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M3")
Matut_M4 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M4")
Matut_M5 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M5")
Matut_M6 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M6")

## For Table 1: modified rooms
c(length(Matut_houses_oct$total_rooms_mod[Matut_houses_oct$total_rooms_mod > 0]),
  Matut_M2[[1]][1],Matut_M3[[1]][1],Matut_M4[[1]][1],Matut_M5[[1]][1],Matut_M6[[1]][1])

c(length(Matut_houses_oct$total_rooms_added[Matut_houses_oct$total_rooms_added > 0]),
  Matut_M2[[1]][2],Matut_M3[[1]][2],Matut_M4[[1]][2],Matut_M5[[1]][2],Matut_M6[[1]][2])

tapply(Matut_M6[[2]]$c_combinedbedroom,Matut_M6[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M6 = c(1,1,0,1,0,0)##6
tapply(Matut_M6[[2]]$c_combinedlivingroom,Matut_M6[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M6[[2]]$c_combinedkitchen,Matut_M6[[2]]$c_modifiedkitchen,length)
tapply(Matut_M6[[2]]$c_combinedstorageroom,Matut_M6[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M6 = c(2,1,1,0)##4
modified_kitchens_M6 = c(1,0)##2
modified_storage_M6 = c(0,0,0)##3

tapply(Matut_M5[[2]]$c_combinedbedroom,Matut_M5[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M5 = c(4,1,1,0,0,0)##6
tapply(Matut_M5[[2]]$c_combinedlivingroom,Matut_M5[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M5[[2]]$c_combinedkitchen,Matut_M5[[2]]$c_modifiedkitchen,length)
tapply(Matut_M5[[2]]$c_combinedstorageroom,Matut_M5[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M5 = c(4,1,0,0)##4
modified_kitchens_M5 = c(1,0)##2
modified_storage_M5 = c(1,0,0)##3

tapply(Matut_M4[[2]]$c_combinedbedroom,Matut_M4[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M4 = c(4,1,0,1,0,0)##6
tapply(Matut_M4[[2]]$c_combinedlivingroom,Matut_M4[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M4[[2]]$c_combinedkitchen,Matut_M4[[2]]$c_modifiedkitchen,length)
tapply(Matut_M4[[2]]$c_combinedstorageroom,Matut_M4[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M4 = c(5,0,1,0)##4
modified_kitchens_M4 = c(0,0)##2
modified_storage_M4 = c(0,0,0)##3


tapply(Matut_M3[[2]]$c_combinedbedroom,Matut_M3[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M3 = c(8,1,1,0,0,0)##6
tapply(Matut_M3[[2]]$c_combinedlivingroom,Matut_M3[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M3[[2]]$c_combinedkitchen,Matut_M3[[2]]$c_modifiedkitchen,length)
tapply(Matut_M3[[2]]$c_combinedstorageroom,Matut_M3[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M3 = c(6,0,1,0)##
modified_kitchens_M3 = c(0,0)##2
modified_storage_M3 = c(0,0,0)##3


tapply(Matut_M2[[2]]$c_combinedbedroom,Matut_M2[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M2 = c(7,1,1,0,0,0)##6
tapply(Matut_M2[[2]]$c_combinedlivingroom,Matut_M2[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_modifiedkitchen,length)
tapply(Matut_M2[[2]]$c_combinedstorageroom,Matut_M2[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M2 = c(9,0,1,0)##4
modified_kitchens_M2 = c(0,0)##2
modified_storage_M2 = c(0,0,0)##5

# par(mfrow=c(2,1))
# 70 bedrooms ## sum(c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6)*c(1:6))
# 57 living rooms ## sum(c(modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6)*c(1:4))
# 96 kitchens ## sum(c(modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5+modified_kitchens_M6)*c(1:2))
# 69 storage ## sum(c(modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5+modified_storage_M6)*c(1:3))
70/sum(Matut_houses_oct$c_combinedbedroom,na.rm = T)
57/sum(Matut_houses_oct$c_combinedlivingroom,na.rm = T)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
to_plot_M6 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6,NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6,NA,modified_kitchens+modified_kitchens_M4+modified_kitchens_M6,NA,modified_storage+modified_storage_M3+modified_storage_M4+modified_storage_M5++modified_storage_M6)
to_plot_M5 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5,                     NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5,                        NA,modified_kitchens+modified_kitchens_M4,                     NA,modified_storage+modified_storage_M3+modified_storage_M4+modified_storage_M5)##GREEN
to_plot_M4 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4,                                          NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4,                                                NA,modified_kitchens+modified_kitchens_M4,                     NA,modified_storage+modified_storage_M3+modified_storage_M4)##RED
to_plot_M3 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3,                                                               NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3,                                                                        NA,modified_kitchens,                                          NA,modified_storage+modified_storage_M3)##BLUE
to_plot_M2 = c(modified_bedrooms+modified_bedrooms_M2,                                                                                    NA,modified_livingrooms+modified_livingrooms_M2,                                                                                                NA,modified_kitchens,                                          NA,modified_storage)##PURPLE
to_plot_M1 = c(modified_bedrooms,NA,modified_livingrooms,NA,modified_kitchens,NA,modified_storage)##ORANGE

length(to_plot_A);length(to_plot_M6);length(to_plot_M5);length(to_plot_M4);length(to_plot_M3);length(to_plot_M2);length(to_plot_M1)

##TOTAL ROOMS MODIFIED
FROM_to_plot_M6 = c(c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6)*c(1:6),
                    c(modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6)*c(1:4),
                    c(modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5+modified_kitchens_M6)*c(1:2),
                    c(modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5+modified_storage_M6)*c(1:3))
sum(FROM_to_plot_M6,na.rm=TRUE)


length(to_plot_A);length(to_plot_M6);length(to_plot_M5);length(to_plot_M4);length(to_plot_M3);length(to_plot_M2);length(to_plot_M1)

# barplot(to_plot_A,col=cols_A,
#         xlab="Rooms in compounds",xaxt="n",yaxt="n",
#         ylab="Frequency", main = "Matutuine")
# 
# barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M1,add=TRUE,col = "darkred",
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# axis(1,at=seq(1,33,length=28),
#      labels=c(1:11,NA,1:7,NA,1:2,NA,1:5))
# axis(2,las=2, at=c(0,10,20,30,40,50,60))
# 
# legend("topright",
#        legend = c("Bedrooms","Living rooms","Kitchens","Storage rooms"),
#        col=unique(cols_A),pch=15,bty="n",cex=1.2)


### And added rooms?

tapply(Matut_M6[[2]]$c_combinedbedroom,Matut_M6[[2]]$c_addedbedroom,length)
added_bedrooms_M6 = c(3,2,0,0,0,0)##6
tapply(Matut_M6[[2]]$c_combinedlivingroom,Matut_M6[[2]]$c_addedlivingroom,length)
tapply(Matut_M6[[2]]$c_combinedkitchen,Matut_M6[[2]]$c_addedkitchen,length)
tapply(Matut_M6[[2]]$c_combinedstorageroom,Matut_M6[[2]]$c_addedstorageroom,length)
added_livingrooms_M6 = c(5,1,0,0)##4
added_kitchens_M6 = c(1,0)##2
added_storage_M6 = c(0,0,0)##5

tapply(Matut_M5[[2]]$c_combinedbedroom,Matut_M5[[2]]$c_addedbedroom,length)
added_bedrooms_M5 = c(5,2,0,0,0,0)##
tapply(Matut_M5[[2]]$c_combinedlivingroom,Matut_M5[[2]]$c_addedlivingroom,length)
tapply(Matut_M5[[2]]$c_combinedkitchen,Matut_M5[[2]]$c_addedkitchen,length)
tapply(Matut_M5[[2]]$c_combinedstorageroom,Matut_M5[[2]]$c_addedstorageroom,length)
added_livingrooms_M5 = c(7,0,0,0)##4
added_kitchens_M5 = c(3,0)##2
added_storage_M5 = c(2,0,0)##5

tapply(Matut_M4[[2]]$c_combinedbedroom,Matut_M4[[2]]$c_addedbedroom,length)
added_bedrooms_M4 = c(4,3,1,0,0,0)##6
tapply(Matut_M4[[2]]$c_combinedlivingroom,Matut_M4[[2]]$c_addedlivingroom,length)
tapply(Matut_M4[[2]]$c_combinedkitchen,Matut_M4[[2]]$c_addedkitchen,length)
tapply(Matut_M4[[2]]$c_combinedstorageroom,Matut_M4[[2]]$c_addedstorageroom,length)
added_livingrooms_M4 = c(7,0,0,0)##4
added_kitchens_M4 = c(1,0)##2
added_storage_M4 = c(0,0,0)##3


tapply(Matut_M3[[2]]$c_combinedbedroom,Matut_M3[[2]]$c_addedbedroom,length)
added_bedrooms_M3 = c(5,0,0,1,0,0)##11
tapply(Matut_M3[[2]]$c_combinedlivingroom,Matut_M3[[2]]$c_addedlivingroom,length)
tapply(Matut_M3[[2]]$c_combinedkitchen,Matut_M3[[2]]$c_addedkitchen,length)
tapply(Matut_M3[[2]]$c_combinedstorageroom,Matut_M3[[2]]$c_addedstorageroom,length)
added_livingrooms_M3 = c(5,0,0,0)##7
added_kitchens_M3 = c(1,0)##2
added_storage_M3 = c(0,0,0)##5


tapply(Matut_M2[[2]]$c_combinedbedroom,Matut_M2[[2]]$c_addedbedroom,length)
added_bedrooms_M2 = c(4,0,0,0,0,0)##6
tapply(Matut_M2[[2]]$c_combinedlivingroom,Matut_M2[[2]]$c_addedlivingroom,length)
tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_addedkitchen,length)
tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_addedstorageroom,length)
added_livingrooms_M2 = c(3,0,0,0)##7
added_kitchens_M2 = c(1,0)##2
added_storage_M2 = c(1,0,0)##5

## (There were no added rooms in M1)

par(mfrow=c(2,1))

to_plota_M6 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5+added_bedrooms_M6,NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5+added_livingrooms_M6, NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5+added_kitchens_M6,NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5++added_storage_M6)
to_plota_M5 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5,                  NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5,                      NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5,                  NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5)##GREEN
to_plota_M4 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4,                                    NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4,                                           NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4,                                    NA,added_storage_M2+added_storage_M3+added_storage_M4)##RED
to_plota_M3 = c(added_bedrooms_M2+added_bedrooms_M3,                                                      NA,added_livingrooms_M2+added_livingrooms_M3,                                                                NA,added_kitchens_M2+added_kitchens_M3,                                                      NA,added_storage_M2+added_storage_M3)##BLUE
to_plota_M2 = c(added_bedrooms_M2,                                                                        NA,added_livingrooms_M2,                                                                                     NA,added_kitchens_M2,                                                                        NA,added_storage_M2)##PURPLE




cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(12,8,3,5))

barplot(to_plot_A+to_plota_M6,col=cols_A,lty=2,
        xlab="Rooms in compounds",xaxt="n",yaxt="n",
        ylab="Frequency", main = "Matutuine (November cohort)")
barplot(to_plot_A+to_plota_M5,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M4,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M3,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M2,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",
        ylab="", main = "")


barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M1,add=TRUE,col = "darkred",
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
axis(1,at=seq(1,21.4,length=18),
     labels=c(1:6,NA,1:4,NA,1:2,NA,1:3))
axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80))

abline(v=seq(1,21.4,length=18)[7],lty=2,lwd=2,col="grey20")
text(seq(1,21.4,length=18)[3],35,"Bedrooms")

abline(v=seq(1,21.4,length=18)[12],lty=2,lwd=2,col="grey20")
text(seq(1,21.4,length=18)[10],35,"Living rooms")

abline(v=seq(1,21.4,length=18)[15],lty=2,lwd=2,col="grey20")
text(seq(1,21.4,length=18)[13.5],70,"Kitchens")
text(seq(1,21.4,length=18)[17],35,"Storage rooms")

legend(1,75,
       legend = c("Added rooms (above solid line)",
                  "Modified rooms","M1 - M6 (Darker to paler shades)"),
       col=c("grey70","orange",NA),pch=15,bty="n",cex=1)



##
## REPEAT FOR BOANE
## This is our data for Houses tracked from October
Boane_houses_oct = subset(df,df$c_district == "Boane" & df$c_visit_month == "M1" & df$calendarMonth == sort(unique(df$calendarMonth))[2])

tapply(Boane_houses_oct$houseid,Boane_houses_oct$housesprayed,length)
for(i in 1:nrow(Boane_houses_oct)){
  Boane_houses_oct$total_rooms[i] = sum(Boane_houses_oct$c_combinedanimalroom[i],Boane_houses_oct$c_combinedlivingroom[i],
                                        Boane_houses_oct$c_combinedbedroom[i],Boane_houses_oct$c_combinedkitchen[i],
                                        Boane_houses_oct$c_combinedstorageroom[i],Boane_houses_oct$c_combinedbathroom[i],
                                        Boane_houses_oct$c_combinedgarage[i],Boane_houses_oct$c_combinedother[i],na.rm=TRUE)
  
  Boane_houses_oct$total_rooms_added[i] = sum(Boane_houses_oct$c_addedanimalroom[i],Boane_houses_oct$c_addedlivingroom[i],
                                              Boane_houses_oct$c_addedbedroom[i],Boane_houses_oct$c_addedkitchen[i],
                                              Boane_houses_oct$c_addedstorageroom[i],Boane_houses_oct$c_addedbathroom[i],
                                              Boane_houses_oct$c_addedgarage[i],Boane_houses_oct$c_addedother[i],na.rm=TRUE)
  
  Boane_houses_oct$total_rooms_mod[i] = sum(Boane_houses_oct$c_modifiedanimalroom[i],Boane_houses_oct$c_modifiedlivingroom[i],
                                            Boane_houses_oct$c_modifiedbedroom[i],Boane_houses_oct$c_modifiedkitchen[i],
                                            Boane_houses_oct$c_modifiedstorageroom[i],Boane_houses_oct$c_modifiedbathroom[i],
                                            Boane_houses_oct$c_modifiedgarage[i],Boane_houses_oct$c_modifiedother[i],na.rm=TRUE)
  
  
}
tapply(Boane_houses_oct$c_modifiedbedroom,Boane_houses_oct$c_combinedbedroom,length)
tapply(Boane_houses_oct$c_combinedbedroom,Boane_houses_oct$c_modifiedbedroom,length)
tapply(Boane_houses_oct$c_combinedbedroom,Boane_houses_oct$c_addedbedroom,length)
## are modified
bedrooms = c(24,40,47,19,7,4,1,0,1) ## maximum 9 bedroons
modified_bedrooms = c(1,2,1,rep(0,6))

tapply(Boane_houses_oct$c_modifiedlivingroom,Boane_houses_oct$c_combinedlivingroom,length)
tapply(Boane_houses_oct$c_combinedlivingroom,Boane_houses_oct$c_modifiedlivingroom,length)
tapply(Boane_houses_oct$c_combinedlivingroom,Boane_houses_oct$c_addedlivingroom,length)
## living room compounds
livingrooms = c(96,23,1,1) ## max 4 living rooms
modified_livingrooms = c(2,0,0,0)

## no modifications in bathrooms
## no modifications in animal rooms
## no modifications in garages

tapply(Boane_houses_oct$c_modifiedkitchen,Boane_houses_oct$c_combinedkitchen,length)
tapply(Boane_houses_oct$c_combinedkitchen,Boane_houses_oct$c_modifiedkitchen,length)
tapply(Boane_houses_oct$c_combinedkitchen,Boane_houses_oct$c_addedkitchen,length)
## 2 0f the 33, 1 kitchen compounds
kitchens = c(57,3)
modified_kitchens = c(0,0)

tapply(Boane_houses_oct$c_modifiedstorageroom,Boane_houses_oct$c_combinedstorageroom,length)
tapply(Boane_houses_oct$c_combinedstorageroom,Boane_houses_oct$c_modifiedstorageroom,length)
tapply(Boane_houses_oct$c_combinedstorageroom,Boane_houses_oct$c_addedstorageroom,length)
## 2 0f the 33, 1 kitchen compounds
storage = c(4,1)
modified_storage = c(0,0)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
# to_plot_B = c(modified_bedrooms,NA,modified_livingrooms,NA,0,NA,0)
cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(10,5,3,3))

# 
# barplot(to_plot_A,col=cols_A,
#         xlab="Rooms in compounds",xaxt="n",yaxt="n",
#         ylab="Frequency", main = "Boaneuine")
# barplot(to_plot_B,add=TRUE,col = "orange",
#         xlab="",xaxt="n",yaxt="n",
#         ylab="", main = "")
# axis(1,at=seq(1,14,length=12),
#      labels=c(1:5,NA,1:2,NA,1,NA,1))
# axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80))
# 

## Total compounds with any modified rooms in M1 october start
length(Boane_houses_oct$total_rooms_mod[Boane_houses_oct$total_rooms_mod > 0])



## Now for october start FOLLOW UP MONTHS
PULL_TOTAL_MOD_ADDED_FOR_Ms = function(dataInitHH,follow_up_month){
  ## Now for M2 october start
  m2_match = Boane_houses_oct$houseid
  Boane_houses_octM2temp2 = subset(df,df$c_district == "Boane" & df$c_visit_month == follow_up_month)
  Boane_houses_octM2 = Boane_houses_octM2temp2[Boane_houses_octM2temp2$houseid %in% m2_match, ]
  dim(Boane_houses_octM2)
  
  for(i in 1:nrow(Boane_houses_octM2)){
    Boane_houses_octM2$total_rooms[i] = sum(Boane_houses_octM2$c_combinedanimalroom[i],Boane_houses_octM2$c_combinedlivingroom[i],
                                            Boane_houses_octM2$c_combinedbedroom[i],Boane_houses_octM2$c_combinedkitchen[i],
                                            Boane_houses_octM2$c_combinedstorageroom[i],Boane_houses_octM2$c_combinedbathroom[i],
                                            Boane_houses_octM2$c_combinedgarage[i],Boane_houses_octM2$c_combinedother[i],na.rm=TRUE)
    
    Boane_houses_octM2$total_rooms_added[i] = sum(Boane_houses_octM2$c_addedanimalroom[i],Boane_houses_octM2$c_addedlivingroom[i],
                                                  Boane_houses_octM2$c_addedbedroom[i],Boane_houses_octM2$c_addedkitchen[i],
                                                  Boane_houses_octM2$c_addedstorageroom[i],Boane_houses_octM2$c_addedbathroom[i],
                                                  Boane_houses_octM2$c_addedgarage[i],Boane_houses_octM2$c_addedother[i],na.rm=TRUE)
    
    Boane_houses_octM2$total_rooms_mod[i] = sum(Boane_houses_octM2$c_modifiedanimalroom[i],Boane_houses_octM2$c_modifiedlivingroom[i],
                                                Boane_houses_octM2$c_modifiedbedroom[i],Boane_houses_octM2$c_modifiedkitchen[i],
                                                Boane_houses_octM2$c_modifiedstorageroom[i],Boane_houses_octM2$c_modifiedbathroom[i],
                                                Boane_houses_octM2$c_modifiedgarage[i],Boane_houses_octM2$c_modifiedother[i],na.rm=TRUE)
    
    
  }
  return(
    list(
      c(length(Boane_houses_octM2$total_rooms_mod[Boane_houses_octM2$total_rooms_mod > 0]),
        length(Boane_houses_octM2$total_rooms_added[Boane_houses_octM2$total_rooms_added > 0])),
      Boane_houses_octM2
    )
  )
  
}
Boane_M2 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M2")
Boane_M3 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M3")
Boane_M4 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M4")
Boane_M5 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M5")
Boane_M6 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M6")

## For Table 1: modified rooms
c(length(Boane_houses_oct$total_rooms_mod[Boane_houses_oct$total_rooms_mod > 0]),
  Boane_M2[[1]][1],Boane_M3[[1]][1],Boane_M4[[1]][1],Boane_M5[[1]][1],Boane_M6[[1]][1])

c(length(Boane_houses_oct$total_rooms_added[Boane_houses_oct$total_rooms_added > 0]),
  Boane_M2[[1]][2],Boane_M3[[1]][2],Boane_M4[[1]][2],Boane_M5[[1]][2],Boane_M6[[1]][2])

tapply(Boane_M6[[2]]$c_combinedbedroom,Boane_M6[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M6 = c(rep(0,9))##9
tapply(Boane_M6[[2]]$c_combinedlivingroom,Boane_M6[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M6[[2]]$c_combinedkitchen,Boane_M6[[2]]$c_modifiedkitchen,length)
tapply(Boane_M6[[2]]$c_combinedstorageroom,Boane_M6[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M6 = c(0,0,0,0)##4
modified_kitchens_M6 = c(0,0)##2
modified_storage_M6 = c(0,0)##2

tapply(Boane_M5[[2]]$c_combinedbedroom,Boane_M5[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M5 = rep(0,9)##11
tapply(Boane_M5[[2]]$c_combinedlivingroom,Boane_M5[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M5[[2]]$c_combinedkitchen,Boane_M5[[2]]$c_modifiedkitchen,length)
tapply(Boane_M5[[2]]$c_combinedstorageroom,Boane_M5[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M5 = c(1,0,0,0)##2
modified_kitchens_M5 = c(0,0)##1
modified_storage_M5 = c(0,0)##1


tapply(Boane_M4[[2]]$c_combinedbedroom,Boane_M4[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M4 = c(rep(0,9))##9
tapply(Boane_M4[[2]]$c_combinedlivingroom,Boane_M4[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M4[[2]]$c_combinedkitchen,Boane_M4[[2]]$c_modifiedkitchen,length)
tapply(Boane_M4[[2]]$c_combinedstorageroom,Boane_M4[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M4 = c(0,0,0,0)##4
modified_kitchens_M4 = c(0,0)##2
modified_storage_M4 = c(0,0)##2


tapply(Boane_M3[[2]]$c_combinedbedroom,Boane_M3[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M3  = c(rep(0,9))##9
tapply(Boane_M3[[2]]$c_combinedlivingroom,Boane_M3[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M3[[2]]$c_combinedkitchen,Boane_M3[[2]]$c_modifiedkitchen,length)
tapply(Boane_M3[[2]]$c_combinedstorageroom,Boane_M3[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M3 = c(0,0,0,0)##4
modified_kitchens_M3 = c(0,0)##2
modified_storage_M3 = c(0,0)##2


tapply(Boane_M2[[2]]$c_combinedbedroom,Boane_M2[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M2 = c(0,1,0,0,0,0,0,0,0)##9
tapply(Boane_M2[[2]]$c_combinedlivingroom,Boane_M2[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M2[[2]]$c_combinedkitchen,Boane_M2[[2]]$c_modifiedkitchen,length)
tapply(Boane_M2[[2]]$c_combinedstorageroom,Boane_M2[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M2 = c(1,0,0,0)##4
modified_kitchens_M2 = c(0,0)##2
modified_storage_M2 = c(0,0)##5

# par(mfrow=c(2,1))
# sum(c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6)*c(1,2,3,4,5))
# sum(c(modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6)*c(1,2))
9/sum(Boane_houses_oct$c_combinedbedroom,na.rm = T)
6/sum(Boane_houses_oct$c_combinedlivingroom,na.rm = T)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
to_plot_M6 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6,NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6,NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5+modified_kitchens_M6,NA,modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5+modified_storage_M6)
to_plot_M5 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5,                     NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5,                        NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5,                     NA,modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5)##GREEN
to_plot_M4 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4,                                          NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4,                                                NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4,                                          NA,modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4)##RED
to_plot_M3 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3,                                                               NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3,                                                                        NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3,                                                               NA,modified_storage+modified_storage_M2+modified_storage_M3)##BLUE
to_plot_M2 = c(modified_bedrooms+modified_bedrooms_M2,                                                                                    NA,modified_livingrooms+modified_livingrooms_M2,                                                                                                NA,modified_kitchens+modified_kitchens_M2,                                                                                    NA,modified_storage+modified_storage_M2)##PURPLE
to_plot_M1 = c(modified_bedrooms,NA,modified_livingrooms,NA,modified_kitchens,NA,modified_storage)##ORANGE

length(to_plot_A);length(to_plot_M6);length(to_plot_M5);length(to_plot_M4);length(to_plot_M3);length(to_plot_M2);length(to_plot_M1)

# 
# barplot(to_plot_A,col=cols_A,
#         xlab="Rooms in compounds",xaxt="n",yaxt="n",
#         ylab="Frequency", main = "Boaneuine")
# 
# barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M1,add=TRUE,col = "darkred",
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# axis(1,at=seq(1,14,length=12),
#      labels=c(1:5,NA,1:2,NA,1,NA,1))
# axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80))
# legend("topright",
#        legend = c("Bedrooms","Living rooms","Kitchens","Storage rooms"),
#        col=unique(cols_A),pch=15,bty="n",cex=1.2)


### And added rooms?

tapply(Boane_M6[[2]]$c_combinedbedroom,Boane_M6[[2]]$c_addedbedroom,length)
added_bedrooms_M6 = c(1,rep(0,8))##5
tapply(Boane_M6[[2]]$c_combinedlivingroom,Boane_M6[[2]]$c_addedlivingroom,length)
tapply(Boane_M6[[2]]$c_combinedkitchen,Boane_M6[[2]]$c_addedkitchen,length)
tapply(Boane_M6[[2]]$c_combinedstorageroom,Boane_M6[[2]]$c_addedstorageroom,length)
added_livingrooms_M6 = c(1,0,0,0)##2
added_kitchens_M6 = c(0,0)##1
added_storage_M6 = c(0,0)##1

tapply(Boane_M5[[2]]$c_combinedbedroom,Boane_M5[[2]]$c_addedbedroom,length)
added_bedrooms_M5 = c(1,rep(0,8))##5
tapply(Boane_M5[[2]]$c_combinedlivingroom,Boane_M5[[2]]$c_addedlivingroom,length)
tapply(Boane_M5[[2]]$c_combinedkitchen,Boane_M5[[2]]$c_addedkitchen,length)
tapply(Boane_M5[[2]]$c_combinedstorageroom,Boane_M5[[2]]$c_addedstorageroom,length)
added_livingrooms_M5 = c(0,0,0,0)##2
added_kitchens_M5 = c(0,1)##1
added_storage_M5 = c(0,0)##1

tapply(Boane_M4[[2]]$c_combinedbedroom,Boane_M4[[2]]$c_addedbedroom,length)
added_bedrooms_M4 = rep(0,9)
tapply(Boane_M4[[2]]$c_combinedlivingroom,Boane_M4[[2]]$c_addedlivingroom,length)
tapply(Boane_M4[[2]]$c_combinedkitchen,Boane_M4[[2]]$c_addedkitchen,length)
tapply(Boane_M4[[2]]$c_combinedstorageroom,Boane_M4[[2]]$c_addedstorageroom,length)
added_livingrooms_M4 = c(0,0,0,0)##7
added_kitchens_M4 = c(0,0)##2
added_storage_M4 = c(0,0)##5


tapply(Boane_M3[[2]]$c_combinedbedroom,Boane_M3[[2]]$c_addedbedroom,length)
added_bedrooms_M3 = c(2,0,0,0,0,0,0,0,0)##9
tapply(Boane_M3[[2]]$c_combinedlivingroom,Boane_M3[[2]]$c_addedlivingroom,length)
tapply(Boane_M3[[2]]$c_combinedkitchen,Boane_M3[[2]]$c_addedkitchen,length)
tapply(Boane_M3[[2]]$c_combinedstorageroom,Boane_M3[[2]]$c_addedstorageroom,length)
added_livingrooms_M3 = c(0,0,0,0)##7
added_kitchens_M3 = c(0,0)##2
added_storage_M3 = c(0,0)##5


tapply(Boane_M2[[2]]$c_combinedbedroom,Boane_M2[[2]]$c_addedbedroom,length)
added_bedrooms_M2 = c(2,0,0,0,0,0,0,0,0)##9
tapply(Boane_M2[[2]]$c_combinedlivingroom,Boane_M2[[2]]$c_addedlivingroom,length)
tapply(Boane_M2[[2]]$c_combinedkitchen,Boane_M2[[2]]$c_addedkitchen,length)
tapply(Boane_M2[[2]]$c_combinedkitchen,Boane_M2[[2]]$c_addedstorageroom,length)
added_livingrooms_M2 = c(0,0,0,0)##7
added_kitchens_M2 = c(0,0)##2
added_storage_M2 = c(0,0)##5

## (There were no added rooms in M1)

# par(mfrow=c(2,1))

to_plota_M6 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5+added_bedrooms_M6,NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5+added_livingrooms_M6, NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5+added_kitchens_M6,NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5++added_storage_M6)
to_plota_M5 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5,                  NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5,                      NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5,                  NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5)##GREEN
to_plota_M4 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4,                                    NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4,                                           NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4,                                    NA,added_storage_M2+added_storage_M3+added_storage_M4)##RED
to_plota_M3 = c(added_bedrooms_M2+added_bedrooms_M3,                                                      NA,added_livingrooms_M2+added_livingrooms_M3,                                                                NA,added_kitchens_M2+added_kitchens_M3,                                                      NA,added_storage_M2+added_storage_M3)##BLUE
to_plota_M2 = c(added_bedrooms_M2,                                                                        NA,added_livingrooms_M2,                                                                                     NA,added_kitchens_M2,                                                                        NA,added_storage_M2)##PURPLE





barplot(to_plot_A+to_plota_M6,col=cols_A,lty=2,
        xlab="Rooms in compounds",xaxt="n",yaxt="n",
        ylab="Frequency", main = "Boane (November cohort)")
barplot(to_plot_A+to_plota_M5,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M4,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M3,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M2,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",
        ylab="", main = "")


barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M1,add=TRUE,col = "darkred",
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
axis(1,at=seq(1,23.5,length=20),
     labels=c(1:9,NA,1:4,NA,1:2,NA,1:2))
axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80,90))

abline(v=seq(1,23.5,length=20)[10],lty=2,lwd=2,col="grey20")
text(seq(1,23.5,length=20)[5],50,"Bedrooms")

abline(v=seq(1,23.5,length=20)[15],lty=2,lwd=2,col="grey20")
text(seq(1,23.5,length=20)[13],50,"Living rooms")

abline(v=seq(1,23.5,length=20)[18],lty=2,lwd=2,col="grey20")
text(seq(1,23.5,length=20)[16.7],80,"Kitchens")
text(seq(1,23.5,length=20)[19.6],50,"Storage rooms")

legend(1,90,
       legend = c("Added rooms (above solid line)",
                  "Modified rooms","M1 - M6 (Darker to paler shades)"),
       col=c("grey70","orange",NA),pch=15,bty="n",cex=1)


#
###
####
#####
######## Now repeat for Decemober cohort
#####
####
###
##
#
Matut_houses_oct = subset(df,df$c_district == "Matutuine" & df$c_visit_month == "M1" & df$calendarMonth == sort(unique(df$calendarMonth))[3])

tapply(Matut_houses_oct$houseid,Matut_houses_oct$housesprayed,length)
for(i in 1:nrow(Matut_houses_oct)){
  Matut_houses_oct$total_rooms[i] = sum(Matut_houses_oct$c_combinedanimalroom[i],Matut_houses_oct$c_combinedlivingroom[i],
                                        Matut_houses_oct$c_combinedbedroom[i],Matut_houses_oct$c_combinedkitchen[i],
                                        Matut_houses_oct$c_combinedstorageroom[i],Matut_houses_oct$c_combinedbathroom[i],
                                        Matut_houses_oct$c_combinedgarage[i],Matut_houses_oct$c_combinedother[i],na.rm=TRUE)
  
  Matut_houses_oct$total_rooms_added[i] = sum(Matut_houses_oct$c_addedanimalroom[i],Matut_houses_oct$c_addedlivingroom[i],
                                              Matut_houses_oct$c_addedbedroom[i],Matut_houses_oct$c_addedkitchen[i],
                                              Matut_houses_oct$c_addedstorageroom[i],Matut_houses_oct$c_addedbathroom[i],
                                              Matut_houses_oct$c_addedgarage[i],Matut_houses_oct$c_addedother[i],na.rm=TRUE)
  
  Matut_houses_oct$total_rooms_mod[i] = sum(Matut_houses_oct$c_modifiedanimalroom[i],Matut_houses_oct$c_modifiedlivingroom[i],
                                            Matut_houses_oct$c_modifiedbedroom[i],Matut_houses_oct$c_modifiedkitchen[i],
                                            Matut_houses_oct$c_modifiedstorageroom[i],Matut_houses_oct$c_modifiedbathroom[i],
                                            Matut_houses_oct$c_modifiedgarage[i],Matut_houses_oct$c_modifiedother[i],na.rm=TRUE)
  
  
}
tapply(Matut_houses_oct$c_modifiedbedroom,Matut_houses_oct$c_combinedbedroom,length)
tapply(Matut_houses_oct$c_combinedbedroom,Matut_houses_oct$c_modifiedbedroom,length)
tapply(Matut_houses_oct$c_combinedbedroom,Matut_houses_oct$c_addedbedroom,length)
## are modified
bedrooms = c(8,6,3,3,1,1) ## maximum 6 bedroons
modified_bedrooms = c(2,1,0,0,0,0)

tapply(Matut_houses_oct$c_modifiedlivingroom,Matut_houses_oct$c_combinedlivingroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedlivingroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_addedlivingroom,length)
## 11 0f the 68, 1 livingroom compounds
## 1 of the 20, 2 living room compounds
livingrooms = c(10,7,1,1,1)##5
modified_livingrooms = c(0,1,0,0,0)

tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedbathroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedanimalroom,length)
tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedgarage,length)
## no modifications in bathrooms
## no modifications in animal rooms
## no modifications in garages

tapply(Matut_houses_oct$c_modifiedkitchen,Matut_houses_oct$c_combinedkitchen,length)
tapply(Matut_houses_oct$c_combinedkitchen,Matut_houses_oct$c_modifiedkitchen,length)
tapply(Matut_houses_oct$c_combinedkitchen,Matut_houses_oct$c_addedkitchen,length)
## 2 0f the 33, 1 kitchen compounds
kitchens = c(14,2)##2
modified_kitchens = c(0,0)

tapply(Matut_houses_oct$c_modifiedstorageroom,Matut_houses_oct$c_combinedstorageroom,length)
tapply(Matut_houses_oct$c_combinedstorageroom,Matut_houses_oct$c_modifiedstorageroom,length)
tapply(Matut_houses_oct$c_combinedstorageroom,Matut_houses_oct$c_addedstorageroom,length)
## 2 0f the 33, 1 kitchen compounds
storage = c(6,2)##2
modified_storage = c(1,0)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
to_plot_B = c(modified_bedrooms,NA,modified_livingrooms,NA,modified_kitchens,NA,modified_storage)
cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(7,6,3,3))

# par(mfrow=c(2,1))
# 
# barplot(to_plot_A,col=cols_A,
#      xlab="Rooms in compounds",xaxt="n",yaxt="n",
#      ylab="Frequency", main = "Matutuine")
# barplot(to_plot_B,add=TRUE,col = "orange",
#      xlab="",xaxt="n",yaxt="n",
#      ylab="", main = "")
# axis(1,at=seq(1,33,length=28),
#      labels=c(1:11,NA,1:7,NA,1:2,NA,1:5))
# axis(2,las=2, at=c(0,10,20,30,40,50,60))
# 
# legend("topright",
#        legend = c("Bedrooms","Living rooms","Kitchens","Storage rooms"),
#        col=unique(cols_A),pch=15,bty="n",cex=1.2)


## Total compounds with any modified rooms in M1 october start
length(Matut_houses_oct$total_rooms_mod[Matut_houses_oct$total_rooms_mod > 0])




## Now for october start FOLLOW UP MONTHS
PULL_TOTAL_MOD_ADDED_FOR_Ms = function(dataInitHH,follow_up_month){
  ## Now for M2 october start
  m2_match =Matut_houses_oct$houseid
  Matut_houses_octM2temp2 = subset(df,df$c_district == "Matutuine" & df$c_visit_month == follow_up_month)
  Matut_houses_octM2 = Matut_houses_octM2temp2[Matut_houses_octM2temp2$houseid %in% m2_match, ]
  dim(Matut_houses_octM2)
  
  for(i in 1:nrow(Matut_houses_octM2)){
    Matut_houses_octM2$total_rooms[i] = sum(Matut_houses_octM2$c_combinedanimalroom[i],Matut_houses_octM2$c_combinedlivingroom[i],
                                            Matut_houses_octM2$c_combinedbedroom[i],Matut_houses_octM2$c_combinedkitchen[i],
                                            Matut_houses_octM2$c_combinedstorageroom[i],Matut_houses_octM2$c_combinedbathroom[i],
                                            Matut_houses_octM2$c_combinedgarage[i],Matut_houses_octM2$c_combinedother[i],na.rm=TRUE)
    
    Matut_houses_octM2$total_rooms_added[i] = sum(Matut_houses_octM2$c_addedanimalroom[i],Matut_houses_octM2$c_addedlivingroom[i],
                                                  Matut_houses_octM2$c_addedbedroom[i],Matut_houses_octM2$c_addedkitchen[i],
                                                  Matut_houses_octM2$c_addedstorageroom[i],Matut_houses_octM2$c_addedbathroom[i],
                                                  Matut_houses_octM2$c_addedgarage[i],Matut_houses_octM2$c_addedother[i],na.rm=TRUE)
    
    Matut_houses_octM2$total_rooms_mod[i] = sum(Matut_houses_octM2$c_modifiedanimalroom[i],Matut_houses_octM2$c_modifiedlivingroom[i],
                                                Matut_houses_octM2$c_modifiedbedroom[i],Matut_houses_octM2$c_modifiedkitchen[i],
                                                Matut_houses_octM2$c_modifiedstorageroom[i],Matut_houses_octM2$c_modifiedbathroom[i],
                                                Matut_houses_octM2$c_modifiedgarage[i],Matut_houses_octM2$c_modifiedother[i],na.rm=TRUE)
    
    
  }
  return(
    list(
      c(length(Matut_houses_octM2$total_rooms_mod[Matut_houses_octM2$total_rooms_mod > 0]),
        length(Matut_houses_octM2$total_rooms_added[Matut_houses_octM2$total_rooms_added > 0])),
      Matut_houses_octM2
    )
  )
  
}
Matut_M2 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M2")
Matut_M3 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M3")
Matut_M4 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M4")
Matut_M5 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M5")
Matut_M6 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M6")

## For Table 1: modified rooms
c(length(Matut_houses_oct$total_rooms_mod[Matut_houses_oct$total_rooms_mod > 0]),
  Matut_M2[[1]][1],Matut_M3[[1]][1],Matut_M4[[1]][1],Matut_M5[[1]][1],Matut_M6[[1]][1])

c(length(Matut_houses_oct$total_rooms_added[Matut_houses_oct$total_rooms_added > 0]),
  Matut_M2[[1]][2],Matut_M3[[1]][2],Matut_M4[[1]][2],Matut_M5[[1]][2],Matut_M6[[1]][2])


tapply(Matut_M6[[2]]$c_combinedbedroom,Matut_M6[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M6 = c(0,0,0,0,0,0)##6
tapply(Matut_M6[[2]]$c_combinedlivingroom,Matut_M6[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M6[[2]]$c_combinedkitchen,Matut_M6[[2]]$c_modifiedkitchen,length)
tapply(Matut_M6[[2]]$c_combinedstorageroom,Matut_M6[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M6 = c(0,0,0,0,0)##4
modified_kitchens_M6 = c(0,0)##2
modified_storage_M6 = c(0,0)##3

tapply(Matut_M5[[2]]$c_combinedbedroom,Matut_M5[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M5 = c(0,1,0,0,0,0)##6
tapply(Matut_M5[[2]]$c_combinedlivingroom,Matut_M5[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M5[[2]]$c_combinedkitchen,Matut_M5[[2]]$c_modifiedkitchen,length)
tapply(Matut_M5[[2]]$c_combinedstorageroom,Matut_M5[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M5 = c(0,1,0,0,0)##4
modified_kitchens_M5 = c(0,0)##2
modified_storage_M5 = c(0,0)##3

tapply(Matut_M4[[2]]$c_combinedbedroom,Matut_M4[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M4 = c(1,0,0,0,0,0)##6
tapply(Matut_M4[[2]]$c_combinedlivingroom,Matut_M4[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M4[[2]]$c_combinedkitchen,Matut_M4[[2]]$c_modifiedkitchen,length)
tapply(Matut_M4[[2]]$c_combinedstorageroom,Matut_M4[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M4 = c(1,0,0,0,0)##4
modified_kitchens_M4 = c(0,0)##2
modified_storage_M4 = c(0,0)##3


tapply(Matut_M3[[2]]$c_combinedbedroom,Matut_M3[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M3 = c(0,1,0,0,0,0)##6
tapply(Matut_M3[[2]]$c_combinedlivingroom,Matut_M3[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M3[[2]]$c_combinedkitchen,Matut_M3[[2]]$c_modifiedkitchen,length)
tapply(Matut_M3[[2]]$c_combinedstorageroom,Matut_M3[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M3 = c(1,1,0,0,0)##
modified_kitchens_M3 = c(0,0)##2
modified_storage_M3 = c(0,0)##3


tapply(Matut_M2[[2]]$c_combinedbedroom,Matut_M2[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M2 = c(2,1,0,0,0,0)##6
tapply(Matut_M2[[2]]$c_combinedlivingroom,Matut_M2[[2]]$c_modifiedlivingroom,length)
tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_modifiedkitchen,length)
tapply(Matut_M2[[2]]$c_combinedstorageroom,Matut_M2[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M2 = c(0,0,0,0,0)##4
modified_kitchens_M2 = c(0,0)##2
modified_storage_M2 = c(0,0)##5

# par(mfrow=c(2,1))
# par(mfrow=c(2,1))
# 13 bedrooms ## sum(c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6)*c(1:6))
# 8 living rooms ## sum(c(modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6)*c(1:5))
13/sum(Matut_houses_oct$c_combinedbedroom,na.rm = T)
8/sum(Matut_houses_oct$c_combinedlivingroom,na.rm = T)
# 96 kitchens ## sum(c(modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5+modified_kitchens_M6)*c(1:2))
# 69 storage ## sum(c(modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5+modified_storage_M6)*c(1:3))

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
to_plot_M6 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6,NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6,NA,modified_kitchens+modified_kitchens_M4+modified_kitchens_M6,NA,modified_storage+modified_storage_M3+modified_storage_M4+modified_storage_M5++modified_storage_M6)
to_plot_M5 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5,                     NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5,                        NA,modified_kitchens+modified_kitchens_M4,                     NA,modified_storage+modified_storage_M3+modified_storage_M4+modified_storage_M5)##GREEN
to_plot_M4 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4,                                          NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4,                                                NA,modified_kitchens+modified_kitchens_M4,                     NA,modified_storage+modified_storage_M3+modified_storage_M4)##RED
to_plot_M3 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3,                                                               NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3,                                                                        NA,modified_kitchens,                                          NA,modified_storage+modified_storage_M3)##BLUE
to_plot_M2 = c(modified_bedrooms+modified_bedrooms_M2,                                                                                    NA,modified_livingrooms+modified_livingrooms_M2,                                                                                                NA,modified_kitchens,                                          NA,modified_storage)##PURPLE
to_plot_M1 = c(modified_bedrooms,NA,modified_livingrooms,NA,modified_kitchens,NA,modified_storage)##ORANGE

length(to_plot_A);length(to_plot_M6);length(to_plot_M5);length(to_plot_M4);length(to_plot_M3);length(to_plot_M2);length(to_plot_M1)

##TOTAL ROOMS MODIFIED
FROM_to_plot_M6 = c(c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6)*c(1:6),
                    c(modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6)*c(1:5),
                    c(modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5+modified_kitchens_M6)*c(1:2),
                    c(modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5+modified_storage_M6)*c(1:2))
sum(FROM_to_plot_M6,na.rm=TRUE)

# barplot(to_plot_A,col=cols_A,
#         xlab="Rooms in compounds",xaxt="n",yaxt="n",
#         ylab="Frequency", main = "Matutuine")
# 
# barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M1,add=TRUE,col = "darkred",
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# axis(1,at=seq(1,33,length=28),
#      labels=c(1:11,NA,1:7,NA,1:2,NA,1:5))
# axis(2,las=2, at=c(0,10,20,30,40,50,60))
# 
# legend("topright",
#        legend = c("Bedrooms","Living rooms","Kitchens","Storage rooms"),
#        col=unique(cols_A),pch=15,bty="n",cex=1.2)


### And added rooms?

tapply(Matut_M6[[2]]$c_combinedbedroom,Matut_M6[[2]]$c_addedbedroom,length)
added_bedrooms_M6 = c(1,0,0,0,0,0)##6
tapply(Matut_M6[[2]]$c_combinedlivingroom,Matut_M6[[2]]$c_addedlivingroom,length)
tapply(Matut_M6[[2]]$c_combinedkitchen,Matut_M6[[2]]$c_addedkitchen,length)
tapply(Matut_M6[[2]]$c_combinedstorageroom,Matut_M6[[2]]$c_addedstorageroom,length)
added_livingrooms_M6 = c(1,0,0,0,0)##4
added_kitchens_M6 = c(0,0)##2
added_storage_M6 = c(0,0)##5

tapply(Matut_M5[[2]]$c_combinedbedroom,Matut_M5[[2]]$c_addedbedroom,length)
added_bedrooms_M5 = c(0,0,0,0,0,0)##
tapply(Matut_M5[[2]]$c_combinedlivingroom,Matut_M5[[2]]$c_addedlivingroom,length)
tapply(Matut_M5[[2]]$c_combinedkitchen,Matut_M5[[2]]$c_addedkitchen,length)
tapply(Matut_M5[[2]]$c_combinedstorageroom,Matut_M5[[2]]$c_addedstorageroom,length)
added_livingrooms_M5 = c(0,0,0,0,0)##4
added_kitchens_M5 = c(0,0)##2
added_storage_M5 = c(0,0)##5

tapply(Matut_M4[[2]]$c_combinedbedroom,Matut_M4[[2]]$c_addedbedroom,length)
added_bedrooms_M4 = c(0,0,0,0,0,0)##6
tapply(Matut_M4[[2]]$c_combinedlivingroom,Matut_M4[[2]]$c_addedlivingroom,length)
tapply(Matut_M4[[2]]$c_combinedkitchen,Matut_M4[[2]]$c_addedkitchen,length)
tapply(Matut_M4[[2]]$c_combinedstorageroom,Matut_M4[[2]]$c_addedstorageroom,length)
added_livingrooms_M4 = c(0,0,0,0,0)##4
added_kitchens_M4 = c(0,0)##2
added_storage_M4 = c(0,0)##3


tapply(Matut_M3[[2]]$c_combinedbedroom,Matut_M3[[2]]$c_addedbedroom,length)
added_bedrooms_M3 = c(0,0,0,0,0,0)##11
tapply(Matut_M3[[2]]$c_combinedlivingroom,Matut_M3[[2]]$c_addedlivingroom,length)
tapply(Matut_M3[[2]]$c_combinedkitchen,Matut_M3[[2]]$c_addedkitchen,length)
tapply(Matut_M3[[2]]$c_combinedstorageroom,Matut_M3[[2]]$c_addedstorageroom,length)
added_livingrooms_M3 = c(0,0,0,0,0)##7
added_kitchens_M3 = c(0,0)##2
added_storage_M3 = c(0,0)##5


tapply(Matut_M2[[2]]$c_combinedbedroom,Matut_M2[[2]]$c_addedbedroom,length)
added_bedrooms_M2 = c(1,0,0,0,0,0)##6
tapply(Matut_M2[[2]]$c_combinedlivingroom,Matut_M2[[2]]$c_addedlivingroom,length)
tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_addedkitchen,length)
tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_addedstorageroom,length)
added_livingrooms_M2 = c(0,0,0,0,0)##7
added_kitchens_M2 = c(0,0)##2
added_storage_M2 = c(0,0)##5

## (There were no added rooms in M1)

par(mfrow=c(2,1))

to_plota_M6 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5+added_bedrooms_M6,NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5+added_livingrooms_M6, NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5+added_kitchens_M6,NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5++added_storage_M6)
to_plota_M5 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5,                  NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5,                      NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5,                  NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5)##GREEN
to_plota_M4 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4,                                    NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4,                                           NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4,                                    NA,added_storage_M2+added_storage_M3+added_storage_M4)##RED
to_plota_M3 = c(added_bedrooms_M2+added_bedrooms_M3,                                                      NA,added_livingrooms_M2+added_livingrooms_M3,                                                                NA,added_kitchens_M2+added_kitchens_M3,                                                      NA,added_storage_M2+added_storage_M3)##BLUE
to_plota_M2 = c(added_bedrooms_M2,                                                                        NA,added_livingrooms_M2,                                                                                     NA,added_kitchens_M2,                                                                        NA,added_storage_M2)##PURPLE


barplot(to_plot_A+to_plota_M6,col=cols_A,lty=2,
        xlab="Rooms in compounds",xaxt="n",yaxt="n",
        ylab="Frequency", main = "Matutuine (December cohort)")
barplot(to_plot_A+to_plota_M5,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M4,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M3,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M2,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",
        ylab="", main = "")


barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M1,add=TRUE,col = "darkred",
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
axis(1,at=seq(1,21.4,length=18),
     labels=c(1:6,NA,1:5,NA,1:2,NA,1:2))
axis(2,las=2, at=c(0,2,4,6,8,10,12))

abline(v=seq(1,21.4,length=18)[7],lty=2,lwd=2,col="grey20")
text(seq(1,21.4,length=18)[3],6,"Bedrooms")

abline(v=seq(1,21.4,length=18)[13],lty=2,lwd=2,col="grey20")
text(seq(1,21.4,length=18)[11],6,"Living rooms")

abline(v=seq(1,21.4,length=18)[16],lty=2,lwd=2,col="grey20")
text(seq(1,21.4,length=18)[15.8],6,"Kitchens")
text(seq(1,21.4,length=18)[17],8,"Storage rooms")

legend(1,12,
       legend = c("Added rooms (above solid line)",
                  "Modified rooms","M1 - M6 (Darker to paler shades)"),
       col=c("grey70","orange",NA),pch=15,bty="n",cex=1)



##
## REPEAT FOR BOANE
## This is our data for Houses tracked from October
Boane_houses_oct = subset(df,df$c_district == "Boane" & df$c_visit_month == "M1" & df$calendarMonth == sort(unique(df$calendarMonth))[3])

tapply(Boane_houses_oct$houseid,Boane_houses_oct$housesprayed,length)
for(i in 1:nrow(Boane_houses_oct)){
  Boane_houses_oct$total_rooms[i] = sum(Boane_houses_oct$c_combinedanimalroom[i],Boane_houses_oct$c_combinedlivingroom[i],
                                        Boane_houses_oct$c_combinedbedroom[i],Boane_houses_oct$c_combinedkitchen[i],
                                        Boane_houses_oct$c_combinedstorageroom[i],Boane_houses_oct$c_combinedbathroom[i],
                                        Boane_houses_oct$c_combinedgarage[i],Boane_houses_oct$c_combinedother[i],na.rm=TRUE)
  
  Boane_houses_oct$total_rooms_added[i] = sum(Boane_houses_oct$c_addedanimalroom[i],Boane_houses_oct$c_addedlivingroom[i],
                                              Boane_houses_oct$c_addedbedroom[i],Boane_houses_oct$c_addedkitchen[i],
                                              Boane_houses_oct$c_addedstorageroom[i],Boane_houses_oct$c_addedbathroom[i],
                                              Boane_houses_oct$c_addedgarage[i],Boane_houses_oct$c_addedother[i],na.rm=TRUE)
  
  Boane_houses_oct$total_rooms_mod[i] = sum(Boane_houses_oct$c_modifiedanimalroom[i],Boane_houses_oct$c_modifiedlivingroom[i],
                                            Boane_houses_oct$c_modifiedbedroom[i],Boane_houses_oct$c_modifiedkitchen[i],
                                            Boane_houses_oct$c_modifiedstorageroom[i],Boane_houses_oct$c_modifiedbathroom[i],
                                            Boane_houses_oct$c_modifiedgarage[i],Boane_houses_oct$c_modifiedother[i],na.rm=TRUE)
  
  
}
tapply(Boane_houses_oct$c_modifiedbedroom,Boane_houses_oct$c_combinedbedroom,length)
tapply(Boane_houses_oct$c_combinedbedroom,Boane_houses_oct$c_modifiedbedroom,length)
tapply(Boane_houses_oct$c_combinedbedroom,Boane_houses_oct$c_addedbedroom,length)
## are modified
bedrooms = c(4,14,26,19,8,3,0,1) ## maximum 8 bedroons
modified_bedrooms = c(1,rep(0,7))

tapply(Boane_houses_oct$c_modifiedlivingroom,Boane_houses_oct$c_combinedlivingroom,length)
tapply(Boane_houses_oct$c_combinedlivingroom,Boane_houses_oct$c_modifiedlivingroom,length)
tapply(Boane_houses_oct$c_combinedlivingroom,Boane_houses_oct$c_addedlivingroom,length)
## living room compounds
livingrooms = c(47,19,6) ## max 3 living rooms
modified_livingrooms = c(1,0,0)

## no modifications in bathrooms
## no modifications in animal rooms
## no modifications in garages

tapply(Boane_houses_oct$c_modifiedkitchen,Boane_houses_oct$c_combinedkitchen,length)
tapply(Boane_houses_oct$c_combinedkitchen,Boane_houses_oct$c_modifiedkitchen,length)
tapply(Boane_houses_oct$c_combinedkitchen,Boane_houses_oct$c_addedkitchen,length)
## 2 0f the 33, 1 kitchen compounds
kitchens = c(48,3)
modified_kitchens = c(0,0)

tapply(Boane_houses_oct$c_modifiedstorageroom,Boane_houses_oct$c_combinedstorageroom,length)
tapply(Boane_houses_oct$c_combinedstorageroom,Boane_houses_oct$c_modifiedstorageroom,length)
tapply(Boane_houses_oct$c_combinedstorageroom,Boane_houses_oct$c_addedstorageroom,length)
## 2 0f the 33, 1 kitchen compounds
storage = c(8,4)
modified_storage = c(1,0)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
# to_plot_B = c(modified_bedrooms,NA,modified_livingrooms,NA,0,NA,0)
cols_A = rep(adegenet::transp("grey",c(0.1,0.4,0.7,0.9)),c(9,4,3,3))

# 
# barplot(to_plot_A,col=cols_A,
#         xlab="Rooms in compounds",xaxt="n",yaxt="n",
#         ylab="Frequency", main = "Boaneuine")
# barplot(to_plot_B,add=TRUE,col = "orange",
#         xlab="",xaxt="n",yaxt="n",
#         ylab="", main = "")
# axis(1,at=seq(1,14,length=12),
#      labels=c(1:5,NA,1:2,NA,1,NA,1))
# axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80))
# 

## Total compounds with any modified rooms in M1 october start
length(Boane_houses_oct$total_rooms_mod[Boane_houses_oct$total_rooms_mod > 0])



## Now for october start FOLLOW UP MONTHS
PULL_TOTAL_MOD_ADDED_FOR_Ms = function(dataInitHH,follow_up_month){
  ## Now for M2 october start
  m2_match = Boane_houses_oct$houseid
  Boane_houses_octM2temp2 = subset(df,df$c_district == "Boane" & df$c_visit_month == follow_up_month)
  Boane_houses_octM2 = Boane_houses_octM2temp2[Boane_houses_octM2temp2$houseid %in% m2_match, ]
  dim(Boane_houses_octM2)
  
  for(i in 1:nrow(Boane_houses_octM2)){
    Boane_houses_octM2$total_rooms[i] = sum(Boane_houses_octM2$c_combinedanimalroom[i],Boane_houses_octM2$c_combinedlivingroom[i],
                                            Boane_houses_octM2$c_combinedbedroom[i],Boane_houses_octM2$c_combinedkitchen[i],
                                            Boane_houses_octM2$c_combinedstorageroom[i],Boane_houses_octM2$c_combinedbathroom[i],
                                            Boane_houses_octM2$c_combinedgarage[i],Boane_houses_octM2$c_combinedother[i],na.rm=TRUE)
    
    Boane_houses_octM2$total_rooms_added[i] = sum(Boane_houses_octM2$c_addedanimalroom[i],Boane_houses_octM2$c_addedlivingroom[i],
                                                  Boane_houses_octM2$c_addedbedroom[i],Boane_houses_octM2$c_addedkitchen[i],
                                                  Boane_houses_octM2$c_addedstorageroom[i],Boane_houses_octM2$c_addedbathroom[i],
                                                  Boane_houses_octM2$c_addedgarage[i],Boane_houses_octM2$c_addedother[i],na.rm=TRUE)
    
    Boane_houses_octM2$total_rooms_mod[i] = sum(Boane_houses_octM2$c_modifiedanimalroom[i],Boane_houses_octM2$c_modifiedlivingroom[i],
                                                Boane_houses_octM2$c_modifiedbedroom[i],Boane_houses_octM2$c_modifiedkitchen[i],
                                                Boane_houses_octM2$c_modifiedstorageroom[i],Boane_houses_octM2$c_modifiedbathroom[i],
                                                Boane_houses_octM2$c_modifiedgarage[i],Boane_houses_octM2$c_modifiedother[i],na.rm=TRUE)
    
    
  }
  return(
    list(
      c(length(Boane_houses_octM2$total_rooms_mod[Boane_houses_octM2$total_rooms_mod > 0]),
        length(Boane_houses_octM2$total_rooms_added[Boane_houses_octM2$total_rooms_added > 0])),
      Boane_houses_octM2
    )
  )
  
}
Boane_M2 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M2")
Boane_M3 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M3")
Boane_M4 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M4")
Boane_M5 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M5")
Boane_M6 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Boane_houses_oct$houseid,
                                       follow_up_month = "M6")

## For Table 1: modified rooms
c(length(Boane_houses_oct$total_rooms_mod[Boane_houses_oct$total_rooms_mod > 0]),
  Boane_M2[[1]][1],Boane_M3[[1]][1],Boane_M4[[1]][1],Boane_M5[[1]][1],Boane_M6[[1]][1])

c(length(Boane_houses_oct$total_rooms_added[Boane_houses_oct$total_rooms_added > 0]),
  Boane_M2[[1]][2],Boane_M3[[1]][2],Boane_M4[[1]][2],Boane_M5[[1]][2],Boane_M6[[1]][2])


tapply(Boane_M6[[2]]$c_combinedbedroom,Boane_M6[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M6 = c(rep(0,8))##8
tapply(Boane_M6[[2]]$c_combinedlivingroom,Boane_M6[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M6[[2]]$c_combinedkitchen,Boane_M6[[2]]$c_modifiedkitchen,length)
tapply(Boane_M6[[2]]$c_combinedstorageroom,Boane_M6[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M6 = c(0,0,0)##3
modified_kitchens_M6 = c(0,0)##2
modified_storage_M6 = c(0,0)##2

tapply(Boane_M5[[2]]$c_combinedbedroom,Boane_M5[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M5 = rep(0,8)##11
tapply(Boane_M5[[2]]$c_combinedlivingroom,Boane_M5[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M5[[2]]$c_combinedkitchen,Boane_M5[[2]]$c_modifiedkitchen,length)
tapply(Boane_M5[[2]]$c_combinedstorageroom,Boane_M5[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M5 = c(0,0,0)##2
modified_kitchens_M5 = c(0,0)##1
modified_storage_M5 = c(0,0)##1


tapply(Boane_M4[[2]]$c_combinedbedroom,Boane_M4[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M4 = c(rep(0,8))##9
tapply(Boane_M4[[2]]$c_combinedlivingroom,Boane_M4[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M4[[2]]$c_combinedkitchen,Boane_M4[[2]]$c_modifiedkitchen,length)
tapply(Boane_M4[[2]]$c_combinedstorageroom,Boane_M4[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M4 = c(0,0,0)##4
modified_kitchens_M4 = c(0,0)##2
modified_storage_M4 = c(0,0)##2


tapply(Boane_M3[[2]]$c_combinedbedroom,Boane_M3[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M3  = c(0,0,1,rep(0,5))##9
tapply(Boane_M3[[2]]$c_combinedlivingroom,Boane_M3[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M3[[2]]$c_combinedkitchen,Boane_M3[[2]]$c_modifiedkitchen,length)
tapply(Boane_M3[[2]]$c_combinedstorageroom,Boane_M3[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M3 = c(1,0,0)##4
modified_kitchens_M3 = c(0,0)##2
modified_storage_M3 = c(0,0)##2


tapply(Boane_M2[[2]]$c_combinedbedroom,Boane_M2[[2]]$c_modifiedbedroom,length)
modified_bedrooms_M2 = c(rep(0,8))##9
tapply(Boane_M2[[2]]$c_combinedlivingroom,Boane_M2[[2]]$c_modifiedlivingroom,length)
tapply(Boane_M2[[2]]$c_combinedkitchen,Boane_M2[[2]]$c_modifiedkitchen,length)
tapply(Boane_M2[[2]]$c_combinedstorageroom,Boane_M2[[2]]$c_modifiedstorageroom,length)
modified_livingrooms_M2 = c(0,0,0)##4
modified_kitchens_M2 = c(1,0)##2
modified_storage_M2 = c(0,0)##5

# par(mfrow=c(2,1))
# sum(c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6)*c(1,2,3,4,5))
# sum(c(modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6)*c(1,2))
9/sum(Boane_houses_oct$c_combinedbedroom,na.rm = T)
6/sum(Boane_houses_oct$c_combinedlivingroom,na.rm = T)

to_plot_A = c(bedrooms,NA,livingrooms,NA,kitchens,NA,storage)
to_plot_M6 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5+modified_bedrooms_M6,NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5+modified_livingrooms_M6,NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5+modified_kitchens_M6,NA,modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5+modified_storage_M6)
to_plot_M5 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4+modified_bedrooms_M5,                     NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4+modified_livingrooms_M5,                        NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4+modified_kitchens_M5,                     NA,modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4+modified_storage_M5)##GREEN
to_plot_M4 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3+modified_bedrooms_M4,                                          NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3+modified_livingrooms_M4,                                                NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3+modified_kitchens_M4,                                          NA,modified_storage+modified_storage_M2+modified_storage_M3+modified_storage_M4)##RED
to_plot_M3 = c(modified_bedrooms+modified_bedrooms_M2+modified_bedrooms_M3,                                                               NA,modified_livingrooms+modified_livingrooms_M2+modified_livingrooms_M3,                                                                        NA,modified_kitchens+modified_kitchens_M2+modified_kitchens_M3,                                                               NA,modified_storage+modified_storage_M2+modified_storage_M3)##BLUE
to_plot_M2 = c(modified_bedrooms+modified_bedrooms_M2,                                                                                    NA,modified_livingrooms+modified_livingrooms_M2,                                                                                                NA,modified_kitchens+modified_kitchens_M2,                                                                                    NA,modified_storage+modified_storage_M2)##PURPLE
to_plot_M1 = c(modified_bedrooms,NA,modified_livingrooms,NA,modified_kitchens,NA,modified_storage)##ORANGE

length(to_plot_A);length(to_plot_M6);length(to_plot_M5);length(to_plot_M4);length(to_plot_M3);length(to_plot_M2);length(to_plot_M1)

# 
# barplot(to_plot_A,col=cols_A,
#         xlab="Rooms in compounds",xaxt="n",yaxt="n",
#         ylab="Frequency", main = "Boaneuine")
# 
# barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# barplot(to_plot_M1,add=TRUE,col = "darkred",
#         xlab="",xaxt="n",yaxt="n",border=NA,
#         ylab="", main = "")
# axis(1,at=seq(1,14,length=12),
#      labels=c(1:5,NA,1:2,NA,1,NA,1))
# axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80))
# legend("topright",
#        legend = c("Bedrooms","Living rooms","Kitchens","Storage rooms"),
#        col=unique(cols_A),pch=15,bty="n",cex=1.2)


### And added rooms?

tapply(Boane_M6[[2]]$c_combinedbedroom,Boane_M6[[2]]$c_addedbedroom,length)
added_bedrooms_M6 = c(rep(0,8))##5
tapply(Boane_M6[[2]]$c_combinedlivingroom,Boane_M6[[2]]$c_addedlivingroom,length)
tapply(Boane_M6[[2]]$c_combinedkitchen,Boane_M6[[2]]$c_addedkitchen,length)
tapply(Boane_M6[[2]]$c_combinedstorageroom,Boane_M6[[2]]$c_addedstorageroom,length)
added_livingrooms_M6 = c(0,0,0)##2
added_kitchens_M6 = c(0,0)##1
added_storage_M6 = c(0,0)##1

tapply(Boane_M5[[2]]$c_combinedbedroom,Boane_M5[[2]]$c_addedbedroom,length)
added_bedrooms_M5 = c(rep(0,8))##5
tapply(Boane_M5[[2]]$c_combinedlivingroom,Boane_M5[[2]]$c_addedlivingroom,length)
tapply(Boane_M5[[2]]$c_combinedkitchen,Boane_M5[[2]]$c_addedkitchen,length)
tapply(Boane_M5[[2]]$c_combinedstorageroom,Boane_M5[[2]]$c_addedstorageroom,length)
added_livingrooms_M5 = c(0,0,0)##2
added_kitchens_M5 = c(0,0)##1
added_storage_M5 = c(0,0)##1

tapply(Boane_M4[[2]]$c_combinedbedroom,Boane_M4[[2]]$c_addedbedroom,length)
added_bedrooms_M4 = c(1,rep(0,7))
tapply(Boane_M4[[2]]$c_combinedlivingroom,Boane_M4[[2]]$c_addedlivingroom,length)
tapply(Boane_M4[[2]]$c_combinedkitchen,Boane_M4[[2]]$c_addedkitchen,length)
tapply(Boane_M4[[2]]$c_combinedstorageroom,Boane_M4[[2]]$c_addedstorageroom,length)
added_livingrooms_M4 = c(0,0,0)##7
added_kitchens_M4 = c(0,0)##2
added_storage_M4 = c(0,0)##5


tapply(Boane_M3[[2]]$c_combinedbedroom,Boane_M3[[2]]$c_addedbedroom,length)
added_bedrooms_M3 = c(rep(0,8))##9
tapply(Boane_M3[[2]]$c_combinedlivingroom,Boane_M3[[2]]$c_addedlivingroom,length)
tapply(Boane_M3[[2]]$c_combinedkitchen,Boane_M3[[2]]$c_addedkitchen,length)
tapply(Boane_M3[[2]]$c_combinedstorageroom,Boane_M3[[2]]$c_addedstorageroom,length)
added_livingrooms_M3 = c(0,0,0)##7
added_kitchens_M3 = c(0,0)##2
added_storage_M3 = c(0,0)##5


tapply(Boane_M2[[2]]$c_combinedbedroom,Boane_M2[[2]]$c_addedbedroom,length)
added_bedrooms_M2 = c(rep(0,8))##9
tapply(Boane_M2[[2]]$c_combinedlivingroom,Boane_M2[[2]]$c_addedlivingroom,length)
tapply(Boane_M2[[2]]$c_combinedkitchen,Boane_M2[[2]]$c_addedkitchen,length)
tapply(Boane_M2[[2]]$c_combinedkitchen,Boane_M2[[2]]$c_addedstorageroom,length)
added_livingrooms_M2 = c(0,0,0)##7
added_kitchens_M2 = c(0,0)##2
added_storage_M2 = c(0,0)##5

## (There were no added rooms in M1)

# par(mfrow=c(2,1))

to_plota_M6 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5+added_bedrooms_M6,NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5+added_livingrooms_M6, NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5+added_kitchens_M6,NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5++added_storage_M6)
to_plota_M5 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4+added_bedrooms_M5,                  NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4+added_livingrooms_M5,                      NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4+added_kitchens_M5,                  NA,added_storage_M2+added_storage_M3+added_storage_M4+added_storage_M5)##GREEN
to_plota_M4 = c(added_bedrooms_M2+added_bedrooms_M3+added_bedrooms_M4,                                    NA,added_livingrooms_M2+added_livingrooms_M3+added_livingrooms_M4,                                           NA,added_kitchens_M2+added_kitchens_M3+added_kitchens_M4,                                    NA,added_storage_M2+added_storage_M3+added_storage_M4)##RED
to_plota_M3 = c(added_bedrooms_M2+added_bedrooms_M3,                                                      NA,added_livingrooms_M2+added_livingrooms_M3,                                                                NA,added_kitchens_M2+added_kitchens_M3,                                                      NA,added_storage_M2+added_storage_M3)##BLUE
to_plota_M2 = c(added_bedrooms_M2,                                                                        NA,added_livingrooms_M2,                                                                                     NA,added_kitchens_M2,                                                                        NA,added_storage_M2)##PURPLE





barplot(to_plot_A+to_plota_M6,col=cols_A,lty=2,
        xlab="Rooms in compounds",xaxt="n",yaxt="n",
        ylab="Frequency", main = "Boane (December cohort)")
barplot(to_plot_A+to_plota_M5,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M4,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M3,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A+to_plota_M2,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_A,add=TRUE,col = adegenet::transp("grey",0.2),
        xlab="",xaxt="n",yaxt="n",
        ylab="", main = "")


barplot(to_plot_M6,add=TRUE,col = adegenet::transp("yellow",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M5,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M4,add=TRUE,col = adegenet::transp("orange",0.3),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M3,add=TRUE,col = adegenet::transp("orange"),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M2,add=TRUE,col = adegenet::transp("darkred",0.7),
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
barplot(to_plot_M1,add=TRUE,col = "darkred",
        xlab="",xaxt="n",yaxt="n",border=NA,
        ylab="", main = "")
axis(1,at=seq(1,21.3,length=18),
     labels=c(1:8,NA,1:3,NA,1:2,NA,1:2))
axis(2,las=2, at=c(0,10,20,30,40,50,60,70,80,90))

abline(v=seq(1,21.3,length=18)[9],lty=2,lwd=2,col="grey20")
text(seq(1,21.3,length=18)[4.5],30,"Bedrooms")

abline(v=seq(1,21.3,length=18)[13],lty=2,lwd=2,col="grey20")
text(seq(1,23.5,length=18)[11],30,"Living rooms")

abline(v=seq(1,21.3,length=18)[16],lty=2,lwd=2,col="grey20")
text(seq(1,21.3,length=18)[15.1],30,"Kitchens")
text(seq(1,21.3,length=18)[17.1],30,"Storage rooms")

legend(1,50,
       legend = c("Added rooms (above solid line)",
                  "Modified rooms","M1 - M6 (Darker to paler shades)"),
       col=c("grey70","orange",NA),pch=15,bty="n",cex=1)



###########################################################################
##
## Now we want to know about the proportion of the rooms sprayed that were modified
## And specifically whether 25% or more were modified

# howmanywalls

# iswallmodified

# which wall type was modified? modifiedwallrooms
#
# 1 modi􀃙edwallrooms___1 Living room
# 2 modi􀃙edwallrooms___2 Bedroom
# 3 modi􀃙edwallrooms___3 Kitchen
# 4 modi􀃙edwallrooms___4 Storage room
# 5 modi􀃙edwallrooms___5 Toilet and/orbath/shower
# 6 modi􀃙edwallrooms___6 Animal room
# 7 modi􀃙edwallrooms___7 Garage
# 8 modi􀃙edwallrooms___8 Other

# what proportion of walls are modified within rooms Function
prop_mod_f = function(cal_month, district){
  Matut_houses_oct = subset(df,df$c_district == district & df$c_visit_month == "M1" & df$calendarMonth == sort(unique(df$calendarMonth))[cal_month])
  
  for(i in 1:nrow(Matut_houses_oct)){
    Matut_houses_oct$total_rooms[i] = sum(Matut_houses_oct$c_combinedanimalroom[i],Matut_houses_oct$c_combinedlivingroom[i],
                                          Matut_houses_oct$c_combinedbedroom[i],Matut_houses_oct$c_combinedkitchen[i],
                                          Matut_houses_oct$c_combinedstorageroom[i],Matut_houses_oct$c_combinedbathroom[i],
                                          Matut_houses_oct$c_combinedgarage[i],Matut_houses_oct$c_combinedother[i],na.rm=TRUE)
    
    Matut_houses_oct$total_rooms_mod[i] = sum(Matut_houses_oct$c_modifiedanimalroom[i],Matut_houses_oct$c_modifiedlivingroom[i],
                                              Matut_houses_oct$c_modifiedbedroom[i],Matut_houses_oct$c_modifiedkitchen[i],
                                              Matut_houses_oct$c_modifiedstorageroom[i],Matut_houses_oct$c_modifiedbathroom[i],
                                              Matut_houses_oct$c_modifiedgarage[i],Matut_houses_oct$c_modifiedother[i],na.rm=TRUE)
    
    
  }
  
  N_rooms_more_than_25pc = array(dim=c(7,3,6)) ## 7 room tyes, 3 info (room type, 25% mod, any mod), 6 follow up months - M1 - M6
  N_rooms_more_than_25pc[,1,] = c("liv","bed","kit","stor","bath","ani","gar")
  N_rooms_more_than_25pc[1,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___1 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___1 == "Checked"] > 0.25)
  N_rooms_more_than_25pc[2,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___2 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___2 == "Checked"] > 0.25)
  N_rooms_more_than_25pc[3,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___3 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___3 == "Checked"] > 0.25)
  N_rooms_more_than_25pc[4,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___4 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___4 == "Checked"] > 0.25)
  N_rooms_more_than_25pc[5,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___5 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___5 == "Checked"] > 0.25)
  N_rooms_more_than_25pc[6,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___6 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___6 == "Checked"] > 0.25)
  N_rooms_more_than_25pc[7,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___7 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___7 == "Checked"] > 0.25)
  
  N_rooms_more_than_25pc[1,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedlivingroom,length)))
  N_rooms_more_than_25pc[2,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedbedroom,Matut_houses_oct$c_modifiedbedroom,length)))
  N_rooms_more_than_25pc[3,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedkitchen,Matut_houses_oct$c_modifiedkitchen,length)))
  N_rooms_more_than_25pc[4,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedstorageroom,Matut_houses_oct$c_modifiedstorageroom,length)))
  N_rooms_more_than_25pc[5,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedbathroom,Matut_houses_oct$c_modifiedbathroom,length)))
  N_rooms_more_than_25pc[6,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedanimalroom,Matut_houses_oct$c_modifiedanimalroom,length)))
  N_rooms_more_than_25pc[7,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedgarage,Matut_houses_oct$c_modifiedgarage,length)))
  
  
  
  
  PULL_TOTAL_MOD_ADDED_FOR_Ms = function(dataInitHH,follow_up_month){
    ## Now for M2 october start
    m2_match =Matut_houses_oct$houseid
    Matut_houses_octM2temp2 = subset(df,df$c_district == "Matutuine" & df$c_visit_month == follow_up_month)
    Matut_houses_octM2 = Matut_houses_octM2temp2[Matut_houses_octM2temp2$houseid %in% m2_match, ]
    dim(Matut_houses_octM2)
    
    for(i in 1:nrow(Matut_houses_octM2)){
      Matut_houses_octM2$total_rooms[i] = sum(Matut_houses_octM2$c_combinedanimalroom[i],Matut_houses_octM2$c_combinedlivingroom[i],
                                              Matut_houses_octM2$c_combinedbedroom[i],Matut_houses_octM2$c_combinedkitchen[i],
                                              Matut_houses_octM2$c_combinedstorageroom[i],Matut_houses_octM2$c_combinedbathroom[i],
                                              Matut_houses_octM2$c_combinedgarage[i],Matut_houses_octM2$c_combinedother[i],na.rm=TRUE)
      
      Matut_houses_octM2$total_rooms_mod[i] = sum(Matut_houses_octM2$c_modifiedanimalroom[i],Matut_houses_octM2$c_modifiedlivingroom[i],
                                                  Matut_houses_octM2$c_modifiedbedroom[i],Matut_houses_octM2$c_modifiedkitchen[i],
                                                  Matut_houses_octM2$c_modifiedstorageroom[i],Matut_houses_octM2$c_modifiedbathroom[i],
                                                  Matut_houses_octM2$c_modifiedgarage[i],Matut_houses_octM2$c_modifiedother[i],na.rm=TRUE)
      
      
    }
    return(
      list(
        c(length(Matut_houses_octM2$total_rooms_mod[Matut_houses_octM2$total_rooms_mod > 0]),
          length(Matut_houses_octM2$total_rooms_added[Matut_houses_octM2$total_rooms_added > 0])),
        Matut_houses_octM2
      )
    )
    
  }
  Matut_M2 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                         follow_up_month = "M2")
  Matut_M3 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                         follow_up_month = "M3")
  Matut_M4 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                         follow_up_month = "M4")
  Matut_M5 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                         follow_up_month = "M5")
  Matut_M6 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                         follow_up_month = "M6")
  
  ## M2
  N_rooms_more_than_25pc[1,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25,na.rm=TRUE)
  N_rooms_more_than_25pc[2,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25,na.rm=TRUE)
  N_rooms_more_than_25pc[3,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25,na.rm=TRUE)
  N_rooms_more_than_25pc[4,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25,na.rm=TRUE)
  N_rooms_more_than_25pc[5,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25,na.rm=TRUE)
  N_rooms_more_than_25pc[6,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25,na.rm=TRUE)
  N_rooms_more_than_25pc[7,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25,na.rm=TRUE)
  
  N_rooms_more_than_25pc[1,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedlivingroom,Matut_M2[[2]]$c_modifiedlivingroom,length))
  N_rooms_more_than_25pc[2,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedbedroom,Matut_M2[[2]]$c_modifiedbedroom,length))
  N_rooms_more_than_25pc[3,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_modifiedkitchen,length))
  N_rooms_more_than_25pc[4,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedstorageroom,Matut_M2[[2]]$c_modifiedstorageroom,length))
  N_rooms_more_than_25pc[5,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedbathroom,Matut_M2[[2]]$c_modifiedbathroom,length))
  N_rooms_more_than_25pc[6,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedanimalroom,Matut_M2[[2]]$c_modifiedanimalroom,length))
  N_rooms_more_than_25pc[7,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedgarage,Matut_M2[[2]]$c_modifiedgarage,length))
  
  
  ##M3
  N_rooms_more_than_25pc[1,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[2,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[3,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[4,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[5,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[6,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[7,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25, na.rm=TRUE)
  
  N_rooms_more_than_25pc[1,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedlivingroom,Matut_M3[[2]]$c_modifiedlivingroom,length))
  N_rooms_more_than_25pc[2,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedbedroom,Matut_M3[[2]]$c_modifiedbedroom,length))
  N_rooms_more_than_25pc[3,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedkitchen,Matut_M3[[2]]$c_modifiedkitchen,length))
  N_rooms_more_than_25pc[4,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedstorageroom,Matut_M3[[2]]$c_modifiedstorageroom,length))
  N_rooms_more_than_25pc[5,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedbathroom,Matut_M3[[2]]$c_modifiedbathroom,length))
  N_rooms_more_than_25pc[6,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedanimalroom,Matut_M3[[2]]$c_modifiedanimalroom,length))
  N_rooms_more_than_25pc[7,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedgarage,Matut_M3[[2]]$c_modifiedgarage,length))
  
  
  ##M4
  N_rooms_more_than_25pc[1,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[2,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[3,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[4,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[5,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[6,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[7,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25, na.rm=TRUE)
  
  N_rooms_more_than_25pc[1,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedlivingroom,Matut_M4[[2]]$c_modifiedlivingroom,length))
  N_rooms_more_than_25pc[2,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedbedroom,Matut_M4[[2]]$c_modifiedbedroom,length))
  N_rooms_more_than_25pc[3,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedkitchen,Matut_M4[[2]]$c_modifiedkitchen,length))
  N_rooms_more_than_25pc[4,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedstorageroom,Matut_M4[[2]]$c_modifiedstorageroom,length))
  N_rooms_more_than_25pc[5,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedbathroom,Matut_M4[[2]]$c_modifiedbathroom,length))
  N_rooms_more_than_25pc[6,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedanimalroom,Matut_M4[[2]]$c_modifiedanimalroom,length))
  N_rooms_more_than_25pc[7,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedgarage,Matut_M4[[2]]$c_modifiedgarage,length))
  
  
  ##M5
  N_rooms_more_than_25pc[1,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[2,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[3,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[4,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[5,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[6,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[7,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25, na.rm=TRUE)
  
  N_rooms_more_than_25pc[1,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedlivingroom,Matut_M5[[2]]$c_modifiedlivingroom,length))
  N_rooms_more_than_25pc[2,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedbedroom,Matut_M5[[2]]$c_modifiedbedroom,length))
  N_rooms_more_than_25pc[3,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedkitchen,Matut_M5[[2]]$c_modifiedkitchen,length))
  N_rooms_more_than_25pc[4,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedstorageroom,Matut_M5[[2]]$c_modifiedstorageroom,length))
  N_rooms_more_than_25pc[5,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedbathroom,Matut_M5[[2]]$c_modifiedbathroom,length))
  N_rooms_more_than_25pc[6,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedanimalroom,Matut_M5[[2]]$c_modifiedanimalroom,length))
  N_rooms_more_than_25pc[7,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedgarage,Matut_M5[[2]]$c_modifiedgarage,length))
  
  
  
  ##M6
  N_rooms_more_than_25pc[1,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[2,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[3,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[4,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[5,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[6,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25, na.rm=TRUE)
  N_rooms_more_than_25pc[7,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25, na.rm=TRUE)
  
  N_rooms_more_than_25pc[1,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedlivingroom,Matut_M6[[2]]$c_modifiedlivingroom,length))
  N_rooms_more_than_25pc[2,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedbedroom,Matut_M6[[2]]$c_modifiedbedroom,length))
  N_rooms_more_than_25pc[3,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedkitchen,Matut_M6[[2]]$c_modifiedkitchen,length))
  N_rooms_more_than_25pc[4,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedstorageroom,Matut_M6[[2]]$c_modifiedstorageroom,length))
  N_rooms_more_than_25pc[5,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedbathroom,Matut_M6[[2]]$c_modifiedbathroom,length))
  N_rooms_more_than_25pc[6,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedanimalroom,Matut_M6[[2]]$c_modifiedanimalroom,length))
  N_rooms_more_than_25pc[7,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedgarage,Matut_M6[[2]]$c_modifiedgarage,length))
  
  ## caution!!! check as the later months have no data so it is not a true 0
  
  return(N_rooms_more_than_25pc)
  
}
Mat_oct = prop_mod_f(cal_month = 1, district = "Matutuine") ## can only use M1 and M2
Mat_nov = prop_mod_f(cal_month = 2, district = "Matutuine") ## can only use M1 and M2
Mat_dec = prop_mod_f(cal_month = 3, district = "Matutuine") ## can only use M1

Boa_oct = prop_mod_f(cal_month = 1, district = "Boane") ## can only use M1 and M2
Boa_nov = prop_mod_f(cal_month = 2, district = "Boane") ## can only use M1 and M2
Boa_dec = prop_mod_f(cal_month = 3, district = "Boane") ## can only use M1


## Concern for this section is lack of data for lots...
## In first follow up all rooms modified have over 25% modification
Mat_oct[,,1] ## column 2 is the number of rooms where the proportion of walls in the rooms where modifications happened is over 25% 
             ## column 3 is the number of rooms with any modifications
## In second follow up 
## 5/6 livingrooms
## 2/4 bedrooms reported modifications covered over 25% modification
Mat_oct[,,2]

## In first follow up all rooms modified have over 25% modification
Mat_nov[,,1]
## In second follow up 
## 5/10 livingrooms
## 5/9 bedrooms reported modifications covered over 25% modification
Mat_nov[,,2]



## Repeating function below incase you want to look at bespoke examples
Matut_houses_oct = subset(df,df$c_district == "Boane" & df$c_visit_month == "M1" & df$calendarMonth == sort(unique(df$calendarMonth))[3])

for(i in 1:nrow(Matut_houses_oct)){
  Matut_houses_oct$total_rooms[i] = sum(Matut_houses_oct$c_combinedanimalroom[i],Matut_houses_oct$c_combinedlivingroom[i],
                                        Matut_houses_oct$c_combinedbedroom[i],Matut_houses_oct$c_combinedkitchen[i],
                                        Matut_houses_oct$c_combinedstorageroom[i],Matut_houses_oct$c_combinedbathroom[i],
                                        Matut_houses_oct$c_combinedgarage[i],Matut_houses_oct$c_combinedother[i],na.rm=TRUE)
  
  Matut_houses_oct$total_rooms_mod[i] = sum(Matut_houses_oct$c_modifiedanimalroom[i],Matut_houses_oct$c_modifiedlivingroom[i],
                                            Matut_houses_oct$c_modifiedbedroom[i],Matut_houses_oct$c_modifiedkitchen[i],
                                            Matut_houses_oct$c_modifiedstorageroom[i],Matut_houses_oct$c_modifiedbathroom[i],
                                            Matut_houses_oct$c_modifiedgarage[i],Matut_houses_oct$c_modifiedother[i],na.rm=TRUE)
  
  
}

N_rooms_more_than_25pc = array(dim=c(7,3,6)) ## 7 room tyes, 3 info (room type, 25% mod, any mod), 6 follow up months - M1 - M6
N_rooms_more_than_25pc[,1,] = c("liv","bed","kit","stor","bath","ani","gar")
N_rooms_more_than_25pc[1,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___1 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___1 == "Checked"] > 0.25)
N_rooms_more_than_25pc[2,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___2 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___2 == "Checked"] > 0.25)
N_rooms_more_than_25pc[3,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___3 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___3 == "Checked"] > 0.25)
N_rooms_more_than_25pc[4,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___4 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___4 == "Checked"] > 0.25)
N_rooms_more_than_25pc[5,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___5 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___5 == "Checked"] > 0.25)
N_rooms_more_than_25pc[6,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___6 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___6 == "Checked"] > 0.25)
N_rooms_more_than_25pc[7,2,1] = length(Matut_houses_oct$modifiedwalls[Matut_houses_oct$modifiedwallrooms___7 == "Checked"]/Matut_houses_oct$howmanywalls[Matut_houses_oct$modifiedwallrooms___7 == "Checked"] > 0.25)

N_rooms_more_than_25pc[1,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedlivingroom,Matut_houses_oct$c_modifiedlivingroom,length)))
N_rooms_more_than_25pc[2,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedbedroom,Matut_houses_oct$c_modifiedbedroom,length)))
N_rooms_more_than_25pc[3,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedkitchen,Matut_houses_oct$c_modifiedkitchen,length)))
N_rooms_more_than_25pc[4,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedstorageroom,Matut_houses_oct$c_modifiedstorageroom,length)))
N_rooms_more_than_25pc[5,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedbathroom,Matut_houses_oct$c_modifiedbathroom,length)))
N_rooms_more_than_25pc[6,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedanimalroom,Matut_houses_oct$c_modifiedanimalroom,length)))
N_rooms_more_than_25pc[7,3,1] = sum(as.numeric(tapply(Matut_houses_oct$c_combinedgarage,Matut_houses_oct$c_modifiedgarage,length)))




PULL_TOTAL_MOD_ADDED_FOR_Ms = function(dataInitHH,follow_up_month){
  ## Now for M2 october start
  m2_match =Matut_houses_oct$houseid
  Matut_houses_octM2temp2 = subset(df,df$c_district == "Matutuine" & df$c_visit_month == follow_up_month)
  Matut_houses_octM2 = Matut_houses_octM2temp2[Matut_houses_octM2temp2$houseid %in% m2_match, ]
  dim(Matut_houses_octM2)
  
  for(i in 1:nrow(Matut_houses_octM2)){
    Matut_houses_octM2$total_rooms[i] = sum(Matut_houses_octM2$c_combinedanimalroom[i],Matut_houses_octM2$c_combinedlivingroom[i],
                                            Matut_houses_octM2$c_combinedbedroom[i],Matut_houses_octM2$c_combinedkitchen[i],
                                            Matut_houses_octM2$c_combinedstorageroom[i],Matut_houses_octM2$c_combinedbathroom[i],
                                            Matut_houses_octM2$c_combinedgarage[i],Matut_houses_octM2$c_combinedother[i],na.rm=TRUE)
    
    Matut_houses_octM2$total_rooms_mod[i] = sum(Matut_houses_octM2$c_modifiedanimalroom[i],Matut_houses_octM2$c_modifiedlivingroom[i],
                                                Matut_houses_octM2$c_modifiedbedroom[i],Matut_houses_octM2$c_modifiedkitchen[i],
                                                Matut_houses_octM2$c_modifiedstorageroom[i],Matut_houses_octM2$c_modifiedbathroom[i],
                                                Matut_houses_octM2$c_modifiedgarage[i],Matut_houses_octM2$c_modifiedother[i],na.rm=TRUE)
    
    
  }
  return(
    list(
      c(length(Matut_houses_octM2$total_rooms_mod[Matut_houses_octM2$total_rooms_mod > 0]),
        length(Matut_houses_octM2$total_rooms_added[Matut_houses_octM2$total_rooms_added > 0])),
      Matut_houses_octM2
    )
  )
  
}
Matut_M2 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M2")
Matut_M3 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M3")
Matut_M4 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M4")
Matut_M5 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M5")
Matut_M6 = PULL_TOTAL_MOD_ADDED_FOR_Ms(dataInitHH = Matut_houses_oct$houseid,
                                       follow_up_month = "M6")

## M2
N_rooms_more_than_25pc[1,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25,na.rm=TRUE)
N_rooms_more_than_25pc[2,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25,na.rm=TRUE)
N_rooms_more_than_25pc[3,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25,na.rm=TRUE)
N_rooms_more_than_25pc[4,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25,na.rm=TRUE)
N_rooms_more_than_25pc[5,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25,na.rm=TRUE)
N_rooms_more_than_25pc[6,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25,na.rm=TRUE)
N_rooms_more_than_25pc[7,2,2] = sum(Matut_M2[[2]]$modifiedwalls[Matut_M2[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M2[[2]]$howmanywalls[Matut_M2[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25,na.rm=TRUE)

N_rooms_more_than_25pc[1,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedlivingroom,Matut_M2[[2]]$c_modifiedlivingroom,length))
N_rooms_more_than_25pc[2,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedbedroom,Matut_M2[[2]]$c_modifiedbedroom,length))
N_rooms_more_than_25pc[3,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedkitchen,Matut_M2[[2]]$c_modifiedkitchen,length))
N_rooms_more_than_25pc[4,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedstorageroom,Matut_M2[[2]]$c_modifiedstorageroom,length))
N_rooms_more_than_25pc[5,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedbathroom,Matut_M2[[2]]$c_modifiedbathroom,length))
N_rooms_more_than_25pc[6,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedanimalroom,Matut_M2[[2]]$c_modifiedanimalroom,length))
N_rooms_more_than_25pc[7,3,2] = sum(tapply(Matut_M2[[2]]$c_combinedgarage,Matut_M2[[2]]$c_modifiedgarage,length))


##M3
N_rooms_more_than_25pc[1,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[2,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[3,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[4,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[5,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[6,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[7,2,3] = sum(Matut_M3[[2]]$modifiedwalls[Matut_M3[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M3[[2]]$howmanywalls[Matut_M3[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25, na.rm=TRUE)

N_rooms_more_than_25pc[1,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedlivingroom,Matut_M3[[2]]$c_modifiedlivingroom,length))
N_rooms_more_than_25pc[2,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedbedroom,Matut_M3[[2]]$c_modifiedbedroom,length))
N_rooms_more_than_25pc[3,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedkitchen,Matut_M3[[2]]$c_modifiedkitchen,length))
N_rooms_more_than_25pc[4,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedstorageroom,Matut_M3[[2]]$c_modifiedstorageroom,length))
N_rooms_more_than_25pc[5,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedbathroom,Matut_M3[[2]]$c_modifiedbathroom,length))
N_rooms_more_than_25pc[6,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedanimalroom,Matut_M3[[2]]$c_modifiedanimalroom,length))
N_rooms_more_than_25pc[7,3,3] = sum(tapply(Matut_M3[[2]]$c_combinedgarage,Matut_M3[[2]]$c_modifiedgarage,length))


##M4
N_rooms_more_than_25pc[1,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[2,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[3,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[4,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[5,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[6,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[7,2,4] = sum(Matut_M4[[2]]$modifiedwalls[Matut_M4[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M4[[2]]$howmanywalls[Matut_M4[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25, na.rm=TRUE)

N_rooms_more_than_25pc[1,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedlivingroom,Matut_M4[[2]]$c_modifiedlivingroom,length))
N_rooms_more_than_25pc[2,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedbedroom,Matut_M4[[2]]$c_modifiedbedroom,length))
N_rooms_more_than_25pc[3,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedkitchen,Matut_M4[[2]]$c_modifiedkitchen,length))
N_rooms_more_than_25pc[4,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedstorageroom,Matut_M4[[2]]$c_modifiedstorageroom,length))
N_rooms_more_than_25pc[5,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedbathroom,Matut_M4[[2]]$c_modifiedbathroom,length))
N_rooms_more_than_25pc[6,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedanimalroom,Matut_M4[[2]]$c_modifiedanimalroom,length))
N_rooms_more_than_25pc[7,3,4] = sum(tapply(Matut_M4[[2]]$c_combinedgarage,Matut_M4[[2]]$c_modifiedgarage,length))


##M5
N_rooms_more_than_25pc[1,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[2,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[3,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[4,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[5,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[6,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[7,2,5] = sum(Matut_M5[[2]]$modifiedwalls[Matut_M5[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M5[[2]]$howmanywalls[Matut_M5[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25, na.rm=TRUE)

N_rooms_more_than_25pc[1,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedlivingroom,Matut_M5[[2]]$c_modifiedlivingroom,length))
N_rooms_more_than_25pc[2,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedbedroom,Matut_M5[[2]]$c_modifiedbedroom,length))
N_rooms_more_than_25pc[3,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedkitchen,Matut_M5[[2]]$c_modifiedkitchen,length))
N_rooms_more_than_25pc[4,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedstorageroom,Matut_M5[[2]]$c_modifiedstorageroom,length))
N_rooms_more_than_25pc[5,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedbathroom,Matut_M5[[2]]$c_modifiedbathroom,length))
N_rooms_more_than_25pc[6,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedanimalroom,Matut_M5[[2]]$c_modifiedanimalroom,length))
N_rooms_more_than_25pc[7,3,5] = sum(tapply(Matut_M5[[2]]$c_combinedgarage,Matut_M5[[2]]$c_modifiedgarage,length))



##M6
N_rooms_more_than_25pc[1,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___1 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___1 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[2,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___2 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___2 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[3,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___3 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___3 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[4,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___4 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___4 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[5,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___5 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___5 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[6,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___6 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___6 == "Checked"] > 0.25, na.rm=TRUE)
N_rooms_more_than_25pc[7,2,6] = sum(Matut_M6[[2]]$modifiedwalls[Matut_M6[[2]]$modifiedwallrooms___7 == "Checked"]/Matut_M6[[2]]$howmanywalls[Matut_M6[[2]]$modifiedwallrooms___7 == "Checked"] > 0.25, na.rm=TRUE)

N_rooms_more_than_25pc[1,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedlivingroom,Matut_M6[[2]]$c_modifiedlivingroom,length))
N_rooms_more_than_25pc[2,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedbedroom,Matut_M6[[2]]$c_modifiedbedroom,length))
N_rooms_more_than_25pc[3,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedkitchen,Matut_M6[[2]]$c_modifiedkitchen,length))
N_rooms_more_than_25pc[4,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedstorageroom,Matut_M6[[2]]$c_modifiedstorageroom,length))
N_rooms_more_than_25pc[5,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedbathroom,Matut_M6[[2]]$c_modifiedbathroom,length))
N_rooms_more_than_25pc[6,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedanimalroom,Matut_M6[[2]]$c_modifiedanimalroom,length))
N_rooms_more_than_25pc[7,3,6] = sum(tapply(Matut_M6[[2]]$c_combinedgarage,Matut_M6[[2]]$c_modifiedgarage,length))

## caution!!! check as the later months have no data so it is not a true 0

N_rooms_more_than_25pc


#######################################################
##
## TABLE 1

#TOTAL PER TRACKED MONTH  ## MOD ## added
## See xlxs of workings (IRS modification and room additions)