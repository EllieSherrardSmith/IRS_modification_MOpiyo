##########################################
## Analysis for Mercy Opiyo
## E Sherrard-Smith 

## 29 July 2020

## Remit:
## a hut trial in Mozambique where we screen mosquito entry points 
## with AVIMA screening mesh treated with Chlorfenapyr and the one 
## that is not treated.

## As part of the study, we always run monthly bioassays on the 
## chlorfenapyr mesh vs the untreated mesh. These are pieces of 
## the nets exposed to the sun and rain as explained below

## a. We have chlorfenapyr mesh 
## (ITM exposed to sun and rain =4 pieces) vs 
## untreated mesh (UM exposed to sun and rain as 
## its control =4 pieces)

## b. we also have  chlorfenapyr mesh 
## (ITM NOT exposed to sun and rain =4 pieces ) vs 
## untreated mesh (NOT UM exposed to sun and rain 
## as its control 04 pieces)

## Every month we do cone assays, with 4 replicates for each treatment 
## and the treatment and control are run in parallel.

## We monitor mortality from immediate knockdown to---- 168 h. 


## Aim: to plot the bioassay results over time 

## 1 pull out the monthly date for the 4 arms of the trial

# dataset for the Bioassay
TempMesh <- read.csv("C:\\Users\\esherrar\\Documents\\Mercy Opiyo\\EntomologiaVECECONEE_DATA_2020-07-29.csv",header=TRUE)
names(TempMesh)


datetxt <- as.Date(TempMesh$test_date)
df <- data.frame(date = datetxt,
                 day = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 year = as.numeric(format(datetxt, format = "%d")))

TempMesh$month = df$month
TempMesh$year = df$year

## Create an array with treatment, months, mortality counts
## 4 replicates usually of 5 mosquitoes per cone bioassay - sum these

unique(TempMesh$treatment) ## 4
unique(TempMesh$month)  ## 4
## 7 times (24, 48, 72, 96, 120, 144, 168) hours after exposure
MONTH = c(3,4,5,6)
mortality = array(dim=c(4,4,7))
exposed = array(dim=c(4,4,1))

for(i in 1:4){
  for(j in 1:4){
    mortality[i,j,1] = ## 24 hour mortality 
      sum(TempMesh$mortalidade_24h_1[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_24h_2[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_24h_3[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_24h_4[TempMesh$treatment == i & TempMesh$month == MONTH[j]])
    
    mortality[i,j,2] = ## 48 hour mortality 
      sum(TempMesh$mortalidade_48h_1[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_48h_2[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_48h_3[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_48h_4[TempMesh$treatment == i & TempMesh$month == MONTH[j]])
    
    mortality[i,j,3] = ## 72 hour mortality 
      sum(TempMesh$mortalidade_72h_1[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_72h_2[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_72h_3[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_72h_4[TempMesh$treatment == i & TempMesh$month == MONTH[j]])
    
    mortality[i,j,4] = ## 96 hour mortality 
      sum(TempMesh$mortalidade_96h_1[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_96h_2[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_96h_3[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_96h_4[TempMesh$treatment == i & TempMesh$month == MONTH[j]])
    
    mortality[i,j,5] = ## 120 hour mortality 
      sum(TempMesh$mortalidade_120h_1[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_120h_2[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_120h_3[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_120h_4[TempMesh$treatment == i & TempMesh$month == MONTH[j]])
    
    mortality[i,j,6] = ## 144 hour mortality 
      sum(TempMesh$mortalidade_144h_1[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_144h_2[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_144h_3[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_144h_4[TempMesh$treatment == i & TempMesh$month == MONTH[j]])
    
    mortality[i,j,7] = ## 168 hour mortality 
      sum(TempMesh$mortalidade_168h_1[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_168h_2[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_168h_3[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$mortalidade_168h_4[TempMesh$treatment == i & TempMesh$month == MONTH[j]])
   
    exposed[i,j,1] = ## 24 hour mortality 
      sum(TempMesh$exposed_mosquito_1[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$exposed_mosquito_2[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$exposed_mosquito_3[TempMesh$treatment == i & TempMesh$month == MONTH[j]],
          TempMesh$exposed_mosquito_4[TempMesh$treatment == i & TempMesh$month == MONTH[j]])
    
    
  }
}

pc_mort = array(dim=c(4,4,7))
for(i in 1:7){
  pc_mort[,,i] = 100 * mortality[,,i]/exposed[,,1]
  
}

#type of treatment translated in English
#[1]<-Untreated mesh expsoed to sun and rain 
#[2]<-Untreated mesh not exposed to sun and rain
#[3]<-Chlofenapyr treated mesh esposed to sun and rain
#[4]<-Chlofenapyr not exposed to sun and rain

## plot - (A) is with No exposure to the sun [2 and 4]
## plot - (B) is with sun exposure           [1 and 3]

## y-lab is Mosquito mortality
## x-lab is time in months (0, 1, 2)

par(mfrow=c(1,2))
time = 1:4
plot(c(pc_mort[1,,1])~time,ylim = c(0,100), xlim=c(0.5,4.5),xaxt="n",yaxt="n",
     pch="",
     ylab= "% Mosquito Mortality",
     xlab="Time in months since deployment",
     main = "Rede AVIMA tratada exposta ao sol e chuva")
axis(1,at=c(1,2,3,4),labels=c("M0","M1","M2","M3"))
axis(2,las=2,at=seq(0,100,20))
colors_hours = c("darkblue","lightblue","aquamarine3","orange","red","darkred","purple")

for(k in 1:7){
  points(c(pc_mort[1,,k])~c(time-0.1),col = colors_hours[k],pch=15) ## treatmnet 1
  points(c(pc_mort[3,,k])~c(time+0.1),col = colors_hours[k],pch=19) ## traetmet 3

  lines(c(pc_mort[1,,k])~c(time-0.1),col = colors_hours[k],lty=1)
  lines(c(pc_mort[3,,k])~c(time+0.1),col = colors_hours[k],lty=2)
}

legend("topright",legend=c("24 hours","48 hours","72 hours","96 hours",
                        "120 hours","144 hours","168 hours"),
       pch=c(15),col=colors_hours,bty="n")

## I wonder if the data are mis-entered for M2 on this first panel?
## Please check!


plot(c(pc_mort[1,,1])~time,ylim = c(0,100), xlim=c(0.5,4.5),xaxt="n",yaxt="n",
     pch="",
     ylab= "% Mosquito Mortality",
     xlab="Time in months since deployment",
     main = "Rede AVIMA nao tratada exposta ao sol e chuva")
axis(1,at=c(1,2,3,4),labels=c("M0","M1","M2","M4"))
axis(2,las=2,at=seq(0,100,20))
colors_hours = c("darkblue","lightblue","aquamarine3","orange","red","darkred","purple")

for(k in 1:7){
  points(c(pc_mort[2,,k])~c(time-0.1),col = colors_hours[k],pch=15) ## treatmnet 1
  points(c(pc_mort[4,,k])~c(time+0.1),col = colors_hours[k],pch=19) ## traetmet 3
  
  lines(c(pc_mort[2,,k])~c(time-0.1),col = colors_hours[k],lty=1)
  lines(c(pc_mort[4,,k])~c(time+0.1),col = colors_hours[k],lty=2)
}

legend("left",legend=c("Control","Treated"),
       pch=c(15,19),bty="n", lty=c(1,2))

## I wonder if the data are mis-entered for M0 on this second panel?
## Please check!
