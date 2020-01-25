# Script Purpose: This slide produces summary statistics across the days. It also, makes some simple plots that 
#presented at the slide 15

#Main outputs: 
  #Three plots for a sleeping patterns
  #Summary_statistics_fake_day - data frame, that contains summaries calculated according to described rules (slides).

#Warning: Everytime running, check what set-up parameters are there.
##################################
############# Setup ##############
##################################

#####################################

#Libraries
library("lubridate")
library("dplyr")
library("plyr")
  library(readr)

#SET-UP PARAMETERS

COUNT_AS_HOUR_LIMIT <- 12 #This one sets where our day starts. 
CONSIDERED_AS_SLEEPING <- 90 #This one sets how many minutes person need to rest to be consedered as resting

#####################################
############# Main Code #############

#DATA READING------------------------------ 

#minbymin_raw_list is a list of minbymin. To approach the ith persons data, just write minbymin_raw_list[[i]].
#exploratory_data is a data about persons.

  #Reading names of files in folder raw_data
  files_names <- list.files("raw_data")
  activities_files <- files_names[grepl("minbymin",files_names)]
  
  #Reading data frame into list format. 
  minbymin_raw_list <- vector(mode = "list", length = length(activities_files))
  names(minbymin_raw_list) <- activities_files

  for(i in 1:length(activities_files)){
    minbymin_raw_list[[i]] <- read.csv(file = paste0("raw_data/",activities_files[i]))
  }
  
  #Person id.22 has strange first line, so we just dropped it. 
  minbymin_raw_list[[21]] <- minbymin_raw_list[[21]][-1,]
  
  #Read data about persons
  exploratory_data <- read.csv(file="raw_data/demographics.csv")



#SUMMARY STATISTICS CALCULATOR.
  #For each person, this codes slices his measurements into bin by fake days, where day end id defined by variable
  #COUNT_AS_HOUR_LIMIT. This code return data frame, where we have variables:
    #.id = persons id
    #Days_counter = which fake day
    #Captured_Minutes = How many minutes of data we have for that fake day for person .id
    #How_Many_Time_Resting = Time events, where person was resting more or equal than 10 minutes.
    # Where must be a activity encounter to count as separate event.
    #How_Long_Resting = Total minutes, where person was resting more than 10 minutes in a row.
    #Total_Not_Sleeping = Total minutes, where persons met was more than 1.25
    #Total_Active = Total minutes, where persons "mets" was more than 1.25
    #Total_3_over = Total minutes, where persons "mets" was more or equal than 3
    #Total_6_over = Total minutes, where persons "mets" was more or equal than 6
    #First_Encountered_Moment = Moment where day started. 
  
summary_statistics_fake_days <- ldply(minbymin_raw_list, function(xframe){
  
  dates <- dmy_hms(xframe$time_new) #Convert to readable time
  
  #This part gives label for each day, to which "fake day" it gonna be included.
  
  dates_frame_x <- array(NA, dim=c(length(xframe$time_new), 4)) %>%
    set_colnames(c("Month", "Day", "Hour", "Minute")) %>%
    as_tibble %>%
    dplyr::mutate(Month=month(dates), Day=day(dates), Hour=hour(dates), Minute=minute(dates)) %>%
    dplyr::mutate(new_Month=month(dates-hours(COUNT_AS_HOUR_LIMIT)), new_Day=day(dates-hours(COUNT_AS_HOUR_LIMIT)), new_Hour=hour(dates-hours(COUNT_AS_HOUR_LIMIT)), new_Minute=minute(dates-hours(COUNT_AS_HOUR_LIMIT)))
  
  date_frames_x_unique_labels <- dates_frame_x %>% dplyr::select(new_Month, new_Day) %>% unique()
  
  date_frame_x_with_labels <- date_frames_x_unique_labels %>% 
    dplyr::mutate(Days_counter=1:length(date_frames_x_unique_labels$new_Month)) %>% 
    right_join(dates_frame_x,by=c("new_Month", "new_Day")) %>%
    dplyr::select(Month, Day, Hour, Minute, Days_counter) %>%
    cbind(xframe) %>%
    dplyr::mutate(time_new=dates)
  
  #Goes through each label (fake days) and calculated summary statistic
  
  summary_dataframe_x <- ddply(date_frame_x_with_labels, ~Days_counter, function(xxframe){
    #Calculations
      #Captured_Minutes
    number_of_minutes <- dim(xxframe)[1]
      #Total_3_Or_Over
    counter_mets_over_3 <- xxframe %>%  dplyr::filter(minute_mets>=3) %>% dplyr::select(minute_mets) %>% as.matrix() %>% length()
      #Total_6_Or_Over
    counter_mets_over_6 <- xxframe %>% dplyr::filter(minute_mets>=6) %>% dplyr::select(minute_mets) %>% as.matrix() %>% length()
      #Total_Active
    counter_mets_over_not_minimum <- xxframe %>% dplyr::filter(minute_mets>1.25) %>% dplyr::select(minute_mets) %>% as.matrix() %>% length()
      #Total_Met_Rebased
    total_met_minus_sleep <- sum(xxframe$minute_mets-1.25)
      #Total_minutes_posture0
    sitting <- sum(xxframe$ap_posture==0)
      #Total_minutes_posture1
    standing <- sum(xxframe$ap_posture==1)
      #Total_minutes_posture2
    walking <- sum(xxframe$ap_posture==2)
    
    sleeping_prepare <- xxframe %>% 
      dplyr::filter(minute_mets==1.25, ap_posture==0) %>%
      mutate(Diff=c(0,diff(time_new))-1) %>%
      dplyr::select(Diff) %>%
      mutate(if_else(Diff<=5, 0, Diff)) %>%
      cumsum() %>% 
      table()
    
    #How_Many_Time_Resting and Total_Active
    sleeping <- c(length(which(sleeping_prepare >=CONSIDERED_AS_SLEEPING)), sum(sleeping_prepare[which(sleeping_prepare >=CONSIDERED_AS_SLEEPING)]))
    
    #Return summaries  
    colnames_summary_stats <- c("Captured_Minutes", , "How_Long_Resting", "Total_Active", "Total_3_Or_Over","Total_6_Or_Over","Total_Met_Rebased", "Total_minutes_posture0","Total_minutes_posture1","Total_minutes_posture2")
    returning_vectors <- c(number_of_minutes, sleeping[1], sleeping[2],counter_mets_over_not_minimum, counter_mets_over_3, counter_mets_over_6,total_met_minus_sleep, sitting, standing, walking)
    names(returning_vectors) <- colnames_summary_stats
    return(returning_vectors)
  })
    
  #Just add timestamp of first minute included into "fake day". Gonna helps to maneuvre with data
  summary_dataframe_x[,"First_encountered_moment"] <- (date_frame_x_with_labels %>% 
    dplyr::group_by(Days_counter) %>%
    dplyr::summarize(xx = min(time_new)) %>%
    ungroup())$xx
  
  return(summary_dataframe_x)
  
},.progress="text") %>%
  dplyr::rename(file=.id)

#Delete days, where sleeping is more or equal tha 1200 minutes (considered as buggy). Also, extract id from files names

summary_statistics_fake_days <- summary_statistics_fake_days %>% 
  mutate(id=parse_number(file)) %>%
  mutate(Week_Day = wday(First_encountered_moment)) %>%
  left_join(exploratory_data, by="id") %>%
  dplyr::filter(How_Long_Resting<1200)

#-----------------------------------SHOWING SLEEPING GRAPHICS
#Recheck if filterring is done, choose only a full days. 
plotting_sleep_slide <- summary_statistics_fake_days  %>% 
  dplyr::filter(Captured_Minutes==1440) %>%
  dplyr::filter(How_Long_Resting<1200) %>%
  dplyr::mutate(weekday_name = factor(checking$Week_Day,
            labels=c("Sunday", "Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday")))

#Plot for show relationship between sleeping times, sleeping sparsity and week days

plotting_sleep_slide %>%
  ggplot(aes(y=How_Long_Resting, x=as.factor(How_Many_Time_Resting))) +
  ylab("How Long Resting per day (minutes)") +
  xlab("How Many Times Resting per day") +
  geom_point() +
  facet_wrap(~weekday_name)

#Box plot to show that weekdays does not have important role for a sleeping times
plotting_sleep_slide %>%
  ggplot(aes(y=How_Long_Resting, x=as.factor(weekday_name))) +
  xlab("Weekday") +
  ylab("How Long Resting per Day (minutes)")  +
  geom_boxplot()




