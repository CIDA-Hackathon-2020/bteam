# Script Purpose: Loop through each subject and produce stats/visualizations



##################################
############# Setup ##############
##################################

library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

savefig <- function(name, width=10, height=10){
  ggsave(paste0(name, ".jpg"), path=file.path("graphics", "subject_stats"), device="jpg", width=width, height=height, limitsize=F)
}
theme_set(theme_minimal())

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

# remove id 24 because there's no file for it:
exploratory_data <- exploratory_data[exploratory_data$id != 24,]

#####################################
############# Main Code #############
#####################################

# loop through each subject
for(i in 1:dim(exploratory_data)[1]){
  print(i)
  # get subject information
  j <- exploratory_data$id[i] 
  age <- round(exploratory_data$age[i], 1)
  bmi <- round(exploratory_data$BMI[i], 1)
  weight_kg <- round(exploratory_data$weight_kg[i], 1)
  pbf <- round(exploratory_data$percent_bf[i], 1)
  fat <- round(exploratory_data$fatmass_g[i], 0)
  fatfree <- round(exploratory_data$fatfreemass_g[i], 0)
  race <- exploratory_data$Race[i]
  ethn <- exploratory_data$Ethnicity[i]
  title <- paste("Subject", i, j, age, bmi, weight_kg, pbf, fat, fatfree, race, ethn, sep="/")
  ts <- minbymin_raw_list[[i]]
  n <- dim(ts)[1]
    
  # extract date, time, day of week, etc. 
  ts$dt <- dmy_hms(ts$time_new)
  ts$wday_num <- wday(ts$dt)
  ts$wday <- weekdays(ts$dt)
  ts$wday2 <- paste(ts$wday_num, ts$wday, sep="_")
  ts$yday <- yday(ts$dt)
  ts$mday <- mday(ts$dt)
  ts$mon <- month(ts$dt)
  ts$hour <- hour(ts$dt)
  ts$week <- week(ts$dt)
  ts$dminute <- hour(ts$dt)*60 + minute(ts$dt)
  ts$step_diff <- c(0, diff(ts$minute_steps))
  # there's a big (negative) step change in the middle when the device is changed
  # that's the only negative step change, so just floor at zero:
  ts$step_diff <- pmax(0, ts$step_diff)
  
  # Drop the first day? (not for plots)
  #ts <- ts[ts$yday > min(ts$yday),]
  
  # Histograms for minute mets
  ggplot(ts, aes(x=minute_mets)) + geom_histogram(bins=100) + 
    labs(title=title) + 
    geom_vline(xintercept=1.25, color="blue", linetype="dashed", size=1.0) + 
    geom_vline(xintercept=1.5, color="green", linetype="dashed", size=1.0) + 
    geom_vline(xintercept=3.0, color="orange", linetype="dashed", size=1.0) +
    geom_vline(xintercept=6.0, color="red", linetype="dashed", size=1.0)
  savefig(paste("mets_hist", i, j, sep="_"), 10, 5)
  
  ggplot(ts, aes(x=minute_mets, fill=wday)) + geom_histogram(bins=100) + 
    facet_grid(wday~.) + scale_y_log10() + 
    labs(title=title) + 
    geom_vline(xintercept=1.25, color="blue", linetype="dashed", size=1.0) + 
    geom_vline(xintercept=1.5, color="green", linetype="dashed", size=1.0) + 
    geom_vline(xintercept=3.0, color="orange", linetype="dashed", size=1.0) +
    geom_vline(xintercept=6.0, color="red", linetype="dashed", size=1.0)
  savefig(paste("mets_hist_wday", i, j, sep="_"), 10, 10)
  
  ggplot(ts, aes(x=minute_mets, fill=wday)) + geom_histogram(bins=100) + 
    facet_grid(yday~.) + scale_y_log10() + 
    labs(title=title) + 
    geom_vline(xintercept=1.25, color="blue", linetype="dashed", size=1.0) + 
    geom_vline(xintercept=1.5, color="green", linetype="dashed", size=1.0) + 
    geom_vline(xintercept=3.0, color="orange", linetype="dashed", size=1.0) +
    geom_vline(xintercept=6.0, color="red", linetype="dashed", size=1.0)
  savefig(paste("mets_hist_yday", i, j, sep="_"), 10, 30)
  
  # Histograms for steps
  ggplot(ts, aes(x=step_diff)) + geom_histogram(bins=50) + scale_y_log10() + 
    labs(title=title)
  savefig(paste("steps_hist", i, j, sep="_"), 10, 5)
  
  ggplot(ts, aes(x=step_diff, fill=wday)) + geom_histogram(bins=50) + scale_y_log10() + 
    facet_grid(wday~.) + 
    labs(title=title)
  savefig(paste("steps_hist_wday", i, j, sep="_"), 10, 10)
  
  ggplot(ts, aes(x=step_diff, fill=wday)) + geom_histogram(bins=50) + scale_y_log10() + 
    facet_grid(yday~.) + 
    labs(title=title)
  savefig(paste("steps_hist_yday", i, j, sep="_"), 10, 30)
    
  # mets vs. steps, color by posture
  ggplot(ts, aes(x=minute_mets, y=step_diff, color=as.factor(ap_posture))) + geom_point(alpha=0.1) + 
    labs(title=title)
  savefig(paste("step_vs_met", i, j, sep="_"), 10, 10)
  
  
  # Plot day time series of mets
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=minute_mets), alpha=1.0) + 
    labs(title=title)
  savefig(paste("daily_met", i, j, "_"), 50, 10)
  
  # Plot day time series of mets, break up by day of week
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=minute_mets, color=as.factor(week)), alpha=0.5) + 
    facet_grid(wday2~.) + 
    labs(title=title)
  savefig(paste("daily_met_weekday", i, j, sep="_"), 10, 10)
  
  # Plot day time series of mets, break up by unique days
  ggplot(ts, aes(x=dminute, y=minute_mets, color=wday)) + 
    geom_line(aes(y=minute_mets)) + 
    facet_grid(yday~.) + 
    labs(title=title)
  savefig(paste("daily_met_daily", i, j, sep="_"), 10, 10)
  
  # Plot day time series of steps
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=step_diff), alpha=1.0) + 
    labs(title=title)
  savefig(paste("daily_step", i, j, sep="_"), 50, 5)
  
  # Plot day time series of steps, break up by day of week
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=step_diff, color=as.factor(week)), alpha=0.5) + 
    facet_grid(wday2~.) + 
    labs(title=title)
  savefig(paste("daily_step_weekday", i, j, sep="_"), 10, 10)
  
  # Plot day time series of steps, break up by unique day
  ggplot(ts, aes(x=dminute, y=minute_mets, color=wday)) + 
    geom_line(aes(y=step_diff)) + 
    facet_grid(yday~.) + 
    labs(title=title)
  savefig(paste("daily_step_daily", i, j, sep="_"), 10, 10)
  
  # Plot day time series of posture
  ggplot(ts, aes(x=dminute, y=minute_mets, color=wday)) + 
    geom_line(aes(y=ap_posture), alpha=0.5) + 
    labs(title=title)
  savefig(paste("daily_posture", i, j, sep="_"), 50, 10)
  
  # Plot day time series of posture, break up by day of week
  ggplot(ts, aes(x=dminute, y=minute_mets, color=wday)) + 
    geom_line(aes(y=ap_posture, color=as.factor(week)), alpha=0.5) + 
    facet_grid(wday2~.) +
    labs(title=title)
  savefig(paste("daily_posture_weekday", i, j, sep="_"), 10, 10)
  
  # Plot day time series of posture, break up by unique day
  ggplot(ts, aes(x=dminute, y=minute_mets, color=wday)) + 
    geom_line(aes(y=ap_posture)) + 
    facet_grid(yday~.) +
    labs(title=title)
  savefig(paste("daily_posture_daily", i, j, sep="_"), 10, 10)
}

