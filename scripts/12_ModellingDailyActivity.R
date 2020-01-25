# Script Purpose:
#This scrip below calculate hourly activity per day. Activity is defined here as sum of (mets - 1.25) per hour

#Main outputs:
#Plots for principal components between curves
#Summary_statistics_fpc - data frame that contains data about each person hourly activitie.


##################################
############# Setup ##############
##################################

library(ggplot2)  
library(plyr)
library(dplyr)
library(magrittr)
library(lme4)
library(fda.usc)

#####################################
############# Main Code #############

#Prepared data frame, where each persons hourly activitie is calculated:
  #Main collumns:
    #- Total_Captured_Minutes = how many minutes where captured in that hour
    #- Total_Met_Rebased  = sum of minute_mets-1.15
    #- Total_posture_2 = total minutes, when person spend his time in posture 2.

summary_statistics_fpc <- ldply(minbymin_raw_list, function(xframe){
  
  dates <- dmy_hms(xframe$time_new) #Convert to readable time
  
    #Fix dates
  dates_frame_x <- array(NA, dim=c(length(xframe$time_new), 4)) %>%
    set_colnames(c("Month", "Day", "Hour", "Minute")) %>%
    as_tibble %>%
    dplyr::mutate(Month=month(dates), Day=day(dates), Hour=hour(dates), Minute=minute(dates))
  
    #Check what unique day's we have data about this person
  date_frames_x_unique_labels <- dates_frame_x %>% dplyr::select(Month, Day) %>% unique()
  
    #Create labels for a day to assign Day1,.....
  date_frame_x_with_labels <- date_frames_x_unique_labels %>% 
    dplyr::mutate(Days_counter=1:length(date_frames_x_unique_labels$Month)) %>% 
    right_join(dates_frame_x,by=c("Month", "Day")) %>%
    dplyr::select(Month, Day, Hour, Minute, Days_counter) %>%
    cbind(xframe) %>%
    dplyr::mutate(time_new=dates)
  
    #Just add full nice timestamps, where day starts. Easier to get week day later
  days_counter_start <- date_frame_x_with_labels %>%
    dplyr::group_by(Days_counter) %>%
    dplyr::summarise(time_start=min(time_new)) %>%
    ungroup()
  
  
  #Goes through each label (fake days) and calculated summary statistic
  
  summary_dataframe_x <- ddply(date_frame_x_with_labels, ~Days_counter+Hour, function(xxframe){
    #Total_Captured_Minutes
    how_many_minutes_in_hour <- dim(xxframe)[1]
    #Total_Met_Rebased
    total_met_rebased <- sum(xxframe$minute_mets-1.25)
    #Total_posture_2
    total_posture_2  <- sum(xxframe$ap_posture==2)
    
      #Return
    returning_vectors <- c(how_many_minutes_in_hour, total_met_rebased, total_posture_2)
    names(returning_vectors) <- c("Total_Captured_Minutes", "Total_Met_Rebased", "Total_posture_2")
    return(returning_vectors)
  })
  
  summary_dataframe_x <- summary_dataframe_x %>% left_join(days_counter_start, by="Days_counter")
  
  #Return things
  return(summary_dataframe_x)
  
},.progress="text") %>%
  dplyr::rename(file=.id)

  #Do a data manipulation, to add week days and exclude not a full days
summary_statistics_fpc_2 <- summary_statistics_fpc %>% 
  mutate(id=parse_number(file)) %>%
  mutate(Week_Day = wday(time_start))

summary_statistics_fpc_filtering <- summary_statistics_fpc_2 %>% 
  dplyr::group_by(id, Days_counter) %>%
  dplyr::summarize(length=n()) %>%
  ungroup() %>%
  dplyr::filter(length<24) 

summary_statistics_fpc_2 <- summary_statistics_fpc_2 %>%
  anti_join(summary_statistics_fpc_filtering, by=c("id", "Days_counter"))

#-----------------START FPCA

#Make matrix form of a data for minute_mets.
summary_statistics_fpc_Met_Total <- (summary_statistics_fpc_2 %>%
  dplyr::mutate(Full_day = paste0(Days_counter,"-",id)) %>%
  dplyr::select(Full_day,Total_Met_Rebased,Hour ) %>%
  spread(Hour, Total_Met_Rebased))[,-1]


#Make matrix form of a data for sum of minute_mets for weekends
summary_statistics_fpc_Met_Total_weekend <- (summary_statistics_fpc_2 %>%
 dplyr::filter(Week_Day %in% c(6,7)) %>%
 dplyr::mutate(Full_day = paste0(Days_counter,"-",id)) %>%
 dplyr::select(Full_day,Total_Met_Rebased,Hour ) %>%
 spread(Hour, Total_Met_Rebased))[,-1]

#Make matrix form of a data for sum of minute_mets for workdays 
summary_statistics_fpc_Met_Total_workday <- (summary_statistics_fpc_2 %>%
 dplyr::filter(Week_Day %in% c(1:5)) %>%
 dplyr::mutate(Full_day = paste0(Days_counter,"-",id)) %>%
 dplyr::select(Full_day,Total_Met_Rebased,Hour ) %>%
 spread(Hour, Total_Met_Rebased))[,-1]


#PLOTS_-----------------------------------------
#Plots comparison of activities between workday and weekday

plot(colSums(summary_statistics_fpc_Met_Total_workday)/dim(summary_statistics_fpc_Met_Total_workday)[1], type="l",
     xlab="Hour", ylab="Average sum of mets beetween days") 
legend(x = 0.5,y = 16, cex = 0.5, box.col = "white", lty = c(1,1), col = c(2,1),
       legend = c("Weekends", "Workdays"))
lines(colSums(summary_statistics_fpc_Met_Total_weekend)/dim(summary_statistics_fpc_Met_Total_weekend)[1], col="red")



#Calculate FPCA for a full data
  save <- fdata(summary_statistics_fpc_Met_Total)
  pc2<-fdata2pc(save,2, norm=FALSE)
  save2 <- fdata(pc2[["u"]] %*% pc2$rotation$data)
  plot(save2, xlab="Hour",y="Hourly Met explained by the first 2 PCA's")
