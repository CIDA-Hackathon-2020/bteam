# Script Purpose: DATA READING



##################################
############# Setup ##############
##################################

library(ggplot2)
library(dplyr)
library(magrittr)

#####################################
############# Main Code #############
#####################################
#Libraries
library("lubridate")
library("dplyr")

#Data reading - 
#minbymin_raw_list is a list of minbymin. To approach person i data, just write minbymin_raw_list[[i]].
#exploratory_data is a data about persons.

files_names <- list.files("raw_data")
activities_files <- files_names[grepl("minbymin",files_names)]

minbymin_raw_list <- vector(mode = "list", length = length(activities_files))

for(i in 1:length(activities_files)){
  minbymin_raw_list[[i]] <- read.csv(file = paste0("raw_data/",activities_files[i]))
}

exploratory_data <- read.csv(file="raw_data/demographics.csv")

#SET-UP PARAMETERS
COUNT_AS_HOUR_LIMIT <- 18





llls <- lapply(minbymin_raw_list, function(xframe){
  dates <- dmy_hms(xframe$time_new)
  dates_frame_x <- array(NA, dim=c(length(xframe$time_new), 4)) %>%
    set_colnames(c("Month", "Day", "Hour", "Minute")) %>%
    as_tibble %>%
    dplyr::mutate(Month=month(dates), Day=day(dates), Hour=hour(dates), Minute=minute(dates)) %>%
    dplyr::mutate(new_Month=month(dates-hours(COUNT_AS_HOUR_LIMIT)), new_Day=day(dates-hours(COUNT_AS_HOUR_LIMIT)), new_Hour=hour(dates-hours(COUNT_AS_HOUR_LIMIT)), new_Minute=minute(dates-hours(COUNT_AS_HOUR_LIMIT)))
  
  mutated <- dates_frame_x %>% dplyr::select(new_Month, new_Day) %>% unique()
  ffz <- mutated %>% dplyr::mutate(Days_counter=1:length(mutated$new_Month)) %>% right_join(dates_frame_x,by=c("new_Month", "new_Day")) %>%
    dplyr::select(Month, Day, Hour, Minute, Days_counter) %>%
    cbind(xframe) %>%
    dplyr::select(everything(), -time_new)
  
  ddply(ffz, ~Days_counter, function(xxframe){
    number_of_minutes <- dim(xxframe)[1]
    counter_mets_over_3 <- xxframe %>% dplyr::filter(minute_mets>=3) %>% summarize(n())
    counter_mets_over_6 <- xxframe %>% dplyr::filter(minute_mets>=6) %>% summarize(n())
    counter_mets_over_not_minimum <- xxframe %>% dplyr::filter(minute_mets>1.25) %>% summarize(n())
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  counter <- 0
  
  
  
  
  
  minipulated_dates_frame_x <- dates_frame_x %>%
    
                         
                         
                         
                         

  })
  

  
  
})



