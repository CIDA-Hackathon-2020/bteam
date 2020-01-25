# Script Purpose:
#This script uses 11_ModellingSummary. It helps to match following days and prepare data frame, suitable to modelling
#Following days relationship between Activities and Sleep habit


##################################
############# Setup ##############
##################################

#FIRST USER MUST RUN THIS CODE THIS TWO LINES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#NUMBERS IN THE NAME SAYS WHAT HOUR WAS CHOSEN AS CUTOFF AND HOW LONG SOME NEEDS TO REST TO BE CONSIDERED SLEEPING

#source("scripts/11_ModellingSummary.R")
#Data_frame_Spagetti_12_90 <- summary_statistics_fake_days

library(ggplot2)
library(plyr)
library(dplyr)
library(magrittr)
library(lme4)


#####################################
############# Main Code #############
#####################################

#Nothing special, just find following days for each user, and adds previous day data with label "PREVIOUS"

lls <- ddply(Data_frame_Spagetti_22_60, ~id, function(xframe){
  #Consider only a full days
  xframe <- xframe  %>% dplyr::filter(Captured_Minutes==1440)
  
  #Set 1 to ones, that had a proper previous day
  xframe$FAKE<- c(0,diff(xframe$Days_counter))
  xframe$FAKE[which(xframe$FAKE!=1)] <- 0
  
  #Calculation
  xframe[,"Previous_How_Many_Times_Resting"] <- c(NA,xframe$How_Many_Time_Resting[-length(xframe$How_Many_Time_Resting)])
  xframe[,"Previous_How_Long_Resting"] <- c(NA,xframe$How_Long_Resting[-length(xframe$How_Long_Resting)])
  xframe[,"Previous_Total_Active"] <- c(NA,xframe$Total_Active[-length(xframe$Total_Active)])
  xframe[,"Previous_Total_3_Or_Over"] <- c(NA,xframe$Total_3_Or_Over[-length(xframe$Total_3_Or_Over)])
  xframe[,"Previous_Total_Met_Rebased"] <- c(NA,xframe$Total_Met_Rebased[-length(xframe$Total_Met_Rebased)])
  
  #Filtering only those, who get previous day.
  xframe <- xframe %>% dplyr::filter(FAKE==1)
  return(xframe)
}, .progress="text")
  
lls <- lls %>%
  mutate(id=as.factor(id))

#Plots to show relationship between following days sleeping and activities.
par(mfrow=c(1,2))
plot(y=lls$How_Long_Resting, x=lls$Previous_How_Long_Resting, ylab="Total Sleeping Time", xlab="Total Sleeping Time one day before")
plot(y=lls$Total_Active, x=lls$Previous_Total_Active,ylab="Total Activity Time", xlab="Total Activity Time one day before")


#Run simple ols to see if something going on
summary(mod1 <- lm(lls$Total_Active~lls$Previous_Total_Active, data=lls))
summary(mod1 <- lm(lls$How_Long_Resting~lls$Previous_How_Long_Resting, data=lls))

#Plot how relationship looks like
lls %>%
  ggplot(aes(x=Previous_Total_Active, y=Total_Active)) +
  geom_point() +
  facet_wrap(~id)

#Random effect models

modAct <- lmer(I(scale(Total_Active))~I(scale(Previous_Total_Active)) + (I(scale(Previous_Total_Active)) | id), lls)
modSle <- lmer(I(scale(How_Long_Resting))~I(scale(Previous_How_Long_Resting)) + (I(scale(Previous_How_Long_Resting)) | id), lls)
summary(modAct)
summary(modSle)

  #MISSING A CHECK FOR A MODELS ROBUSTNESS