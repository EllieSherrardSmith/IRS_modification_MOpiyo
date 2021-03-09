##########################################
##
## E Sherrard-Smith
## Wall Modification figure for Mercy Opiyo

## Data
data = read.csv("C:/Users/esherrar/Documents/Mercy Opiyo/IRS-SOC_wall_modification_questions_on_analyis/IRS-SOC Matutuine ESS.csv",header=TRUE)
head(data)


## Aim - to plota figure showing both the start month and proportions of adjusted walls

months = 0:8 ## this is October 2018 until June 2019 (as there are M4 in Feb, M6 in Jan)
set_up_blanks = rep(0,9)

par(mar=c(5,5,5,5))
plot(set_up_blanks ~ months,
     ylab="Cumulative modified houses (%)",yaxt="n",
     xlab="Months",xaxt="n",ylim=c(0,100),bty="n",pch="",
     cex.lab=1.4,cex.axis=1.4)
axis(1,at=0:8,labels=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun"))
axis(2,las=2,at=c(0,20,40,60,80,100),cex.lab=1.4,cex.axis=1.4)
axis(4,las=2,at=c(0,20,40,60,80,100),cex.lab=1.4,cex.axis=1.4)
mtext("Total houses modified (%)",side=4,line=3,cex.lab=1.4,cex.axis=1.4,cex=1.4)
## M1 November data
nov_M1 = subset(data,data$M1_month == "Nov")
polygon(c(0.8,0.9,0.9,0.8),c(0,0,nov_M1$CumPercentModified[1],nov_M1$CumPercentModified[1]),border=NA,col="blue")
polygon(c(1.8,1.9,1.9,1.8),c(0,0,nov_M1$CumPercentModified[2],nov_M1$CumPercentModified[2]),border=NA,col="blue")
polygon(c(2.8,2.9,2.9,2.8),c(0,0,nov_M1$CumPercentModified[3],nov_M1$CumPercentModified[3]),border=NA,col="blue")
polygon(c(3.8,3.9,3.9,3.8),c(0,0,nov_M1$CumPercentModified[4],nov_M1$CumPercentModified[4]),border=NA,col="blue")
polygon(c(4.8,4.9,4.9,4.8),c(0,0,nov_M1$CumPercentModified[5],nov_M1$CumPercentModified[5]),border=NA,col="blue")
polygon(c(5.8,5.9,5.9,5.8),c(0,0,nov_M1$CumPercentModified[6],nov_M1$CumPercentModified[6]),border=NA,col="blue")

## M1 December data
dec_M1 = subset(data,data$M1_month == "Dec")
polygon(c(1.9,2,2,1.9),c(0,0,dec_M1$CumPercentModified[1],dec_M1$CumPercentModified[1]),border=NA,col="orange")
polygon(c(2.9,3,3,2.9),c(0,0,dec_M1$CumPercentModified[2],dec_M1$CumPercentModified[2]),border=NA,col="orange")
polygon(c(3.9,4,4,3.9),c(0,0,dec_M1$CumPercentModified[3],dec_M1$CumPercentModified[3]),border=NA,col="orange")
polygon(c(4.9,5,5,4.9),c(0,0,dec_M1$CumPercentModified[4],dec_M1$CumPercentModified[4]),border=NA,col="orange")
polygon(c(5.9,6,6,5.9),c(0,0,dec_M1$CumPercentModified[5],dec_M1$CumPercentModified[5]),border=NA,col="orange")
polygon(c(6.9,7,7,6.9),c(0,0,dec_M1$CumPercentModified[6],dec_M1$CumPercentModified[6]),border=NA,col="orange")

## M1 Jan data
jan_M1 = subset(data,data$M1_month == "Jan")
polygon(c(3,3.1,3.1,3),c(0,0,jan_M1$CumPercentModified[1],jan_M1$CumPercentModified[1]),border=NA,col="purple")
polygon(c(4,4.1,4.1,4),c(0,0,jan_M1$CumPercentModified[2],jan_M1$CumPercentModified[2]),border=NA,col="purple")
polygon(c(5,5.1,5.1,5),c(0,0,jan_M1$CumPercentModified[3],jan_M1$CumPercentModified[3]),border=NA,col="purple")
polygon(c(6,6.1,6.1,6),c(0,0,jan_M1$CumPercentModified[4],jan_M1$CumPercentModified[4]),border=NA,col="purple")
polygon(c(7,7.1,7.1,7),c(0,0,jan_M1$CumPercentModified[5],jan_M1$CumPercentModified[5]),border=NA,col="purple")
polygon(c(8,8.1,8.1,8),c(0,0,jan_M1$CumPercentModified[6],jan_M1$CumPercentModified[6]),border=NA,col="purple")

## M1 Feb data
feb_M1 = subset(data,data$M1_month == "Feb")
polygon(c(4.1,4.2,4.2,4.1),c(0,0,feb_M1$CumPercentModified[1],feb_M1$CumPercentModified[1]),border=NA,col="aquamarine3")
polygon(c(5.1,5.2,5.2,5.1),c(0,0,feb_M1$CumPercentModified[2],feb_M1$CumPercentModified[2]),border=NA,col="aquamarine3")
polygon(c(6.1,6.2,6.2,6.1),c(0,0,feb_M1$CumPercentModified[3],feb_M1$CumPercentModified[3]),border=NA,col="aquamarine3")
polygon(c(7.1,7.2,7.2,7.1),c(0,0,feb_M1$CumPercentModified[4],feb_M1$CumPercentModified[4]),border=NA,col="aquamarine3")

## OVERALL PROPORTION OF HOUSES MODIFIED TO BE OVERLAID
Modified_counts = c(sum(data$ModifiedHouses[data$M1_month == "Nov" & data$Visit == "M1"]),
                    sum(data$ModifiedHouses[data$M1_month == "Nov" & data$Visit == "M2"] + data$ModifiedHouses[data$M1_month == "Dec" & data$Visit == "M1"]),
                    sum(data$ModifiedHouses[data$M1_month == "Nov" & data$Visit == "M3"] + data$ModifiedHouses[data$M1_month == "Dec" & data$Visit == "M2"] + data$ModifiedHouses[data$M1_month == "Jan" & data$Visit == "M1"]),
                    sum(data$ModifiedHouses[data$M1_month == "Nov" & data$Visit == "M4"] + data$ModifiedHouses[data$M1_month == "Dec" & data$Visit == "M3"] + data$ModifiedHouses[data$M1_month == "Jan" & data$Visit == "M2"]),
                    sum(data$ModifiedHouses[data$M1_month == "Nov" & data$Visit == "M5"] + data$ModifiedHouses[data$M1_month == "Dec" & data$Visit == "M4"] + data$ModifiedHouses[data$M1_month == "Jan" & data$Visit == "M3"]),
                    sum(data$ModifiedHouses[data$M1_month == "Nov" & data$Visit == "M6"] + data$ModifiedHouses[data$M1_month == "Dec" & data$Visit == "M5"] + data$ModifiedHouses[data$M1_month == "Jan" & data$Visit == "M4"]),
                    sum(0                                                                + data$ModifiedHouses[data$M1_month == "Dec" & data$Visit == "M6"] + data$ModifiedHouses[data$M1_month == "Jan" & data$Visit == "M5"]),
                    sum(0                                                                + 0                                                                + data$ModifiedHouses[data$M1_month == "Jan" & data$Visit == "M6"]))
                    

Modified_visits = c(sum(data$m1VisitedHouses[data$M1_month == "Nov" & data$Visit == "M1"]),
                    sum(data$m1VisitedHouses[data$M1_month == "Nov" & data$Visit == "M2"] + data$m1VisitedHouses[data$M1_month == "Dec" & data$Visit == "M1"]),
                    sum(data$m1VisitedHouses[data$M1_month == "Nov" & data$Visit == "M3"] + data$m1VisitedHouses[data$M1_month == "Dec" & data$Visit == "M2"] + data$m1VisitedHouses[data$M1_month == "Jan" & data$Visit == "M1"]),
                    sum(data$m1VisitedHouses[data$M1_month == "Nov" & data$Visit == "M4"] + data$m1VisitedHouses[data$M1_month == "Dec" & data$Visit == "M3"] + data$m1VisitedHouses[data$M1_month == "Jan" & data$Visit == "M2"]),
                    sum(data$m1VisitedHouses[data$M1_month == "Nov" & data$Visit == "M5"] + data$m1VisitedHouses[data$M1_month == "Dec" & data$Visit == "M4"] + data$m1VisitedHouses[data$M1_month == "Jan" & data$Visit == "M3"]),
                    sum(data$m1VisitedHouses[data$M1_month == "Nov" & data$Visit == "M6"] + data$m1VisitedHouses[data$M1_month == "Dec" & data$Visit == "M5"] + data$m1VisitedHouses[data$M1_month == "Jan" & data$Visit == "M4"]),
                    sum(0                                                                 + data$m1VisitedHouses[data$M1_month == "Dec" & data$Visit == "M6"] + data$m1VisitedHouses[data$M1_month == "Jan" & data$Visit == "M5"]),
                    sum(0                                                                + 0                                                                  + data$m1VisitedHouses[data$M1_month == "Jan" & data$Visit == "M6"]))

tot_prop_month = 100 * Modified_counts/Modified_visits
polygon(c(0.75,1.25,1.25,0.75),c(0,0,tot_prop_month[1],tot_prop_month[1]),border=NA,col=adegenet::transp("aquamarine2",0.4))
polygon(c(1.75,2.25,2.25,1.75),c(0,0,tot_prop_month[2],tot_prop_month[2]),border=NA,col=adegenet::transp("aquamarine2",0.4))
polygon(c(2.75,3.25,3.25,2.75),c(0,0,tot_prop_month[3],tot_prop_month[3]),border=NA,col=adegenet::transp("aquamarine2",0.4))
polygon(c(3.75,4.25,4.25,3.75),c(0,0,tot_prop_month[4],tot_prop_month[4]),border=NA,col=adegenet::transp("aquamarine2",0.4))
polygon(c(4.75,5.25,5.25,4.75),c(0,0,tot_prop_month[5],tot_prop_month[5]),border=NA,col=adegenet::transp("aquamarine2",0.4))
polygon(c(5.75,6.25,6.25,5.75),c(0,0,tot_prop_month[6],tot_prop_month[6]),border=NA,col=adegenet::transp("aquamarine2",0.4))
polygon(c(6.75,7.25,7.25,6.75),c(0,0,tot_prop_month[7],tot_prop_month[7]),border=NA,col=adegenet::transp("aquamarine2",0.4))
polygon(c(7.75,8.25,8.25,7.75),c(0,0,tot_prop_month[8],tot_prop_month[8]),border=NA,col=adegenet::transp("aquamarine2",0.4))


legend("topleft",legend = c("Proportion of all houses modified",
                            "Campaign begins Nov 2018 (Village 1)",
                            "Campaign begins Dec 2018 (Village 2)",
                            "Campaign begins Jan 2019 (Village 3)"),bty="n",
       col=c(adegenet::transp("aquamarine3",0.4),"blue","orange","purple"),pch=15,cex=1.4)

