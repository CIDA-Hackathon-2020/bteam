# Script Purpose: Basic Descriptive Statistics and Visualizations



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

## Make sure you run the beginning of Mantautas_DataReading.R, up to 
## exploratory_data <- ...

#####################################
############# Main Code #############
#####################################

for(i in 1:dim(exploratory_data)[1]){
  print(i)
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
    
  # date time day of week, etc. 
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
  
  # compare Mets and steps
  ggplot(ts, aes(x=minute_steps)) + geom_histogram(bins=100) + 
    labs(title=title)
  savefig(paste0("minute_hist_", i), 10, 10)
  
  ggplot(ts, aes(x=step_diff)) + geom_histogram(bins=100) + scale_y_log10() + 
    labs(title=title)
  savefig(paste0("steps_hist_", i), 10, 10)
  
  ggplot(ts, aes(x=minute_mets, y=step_diff, color=as.factor(ap_posture))) + geom_point(alpha=0.1) + 
    labs(title=title)
  savefig(paste0("step_vs_met_", i), 10, 10)
  
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=minute_mets), alpha=0.5) + 
    labs(title=title)
  savefig(paste0("daily_met_", i), 50, 10)
  
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=minute_mets, color=as.factor(week)), alpha=0.5) + 
    facet_grid(wday2~.) + 
    labs(title=title)
  savefig(paste0("daily_met_weekday_", i), 10, 10)
  
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=minute_mets)) + 
    facet_grid(yday~.) + 
    labs(title=title)
  savefig(paste0("daily_met_daily_", i), 10, 10)
  
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=step_diff), alpha=0.5) + 
    labs(title=title)
  savefig(paste0("daily_step_", i), 50, 10)
  
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=step_diff, color=as.factor(week)), alpha=0.5) + 
    facet_grid(wday2~.) + 
    labs(title=title)
  savefig(paste0("daily_step_weekday_", i), 10, 10)
  
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=step_diff)) + 
    facet_grid(yday~.) + 
    labs(title=title)
  savefig(paste0("daily_step_daily_", i), 10, 10)
  
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=ap_posture), alpha=0.5) + 
    labs(title=title)
  savefig(paste0("daily_posture_", i), 50, 10)
  
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=ap_posture, color=as.factor(week)), alpha=0.5) + 
    facet_grid(wday2~.) +
    labs(title=title)
  savefig(paste0("daily_posture_weekday_", i), 10, 10)
  
  ggplot(ts, aes(x=dminute, y=minute_mets)) + 
    geom_line(aes(y=ap_posture)) + 
    facet_grid(yday~.) +
    labs(title=title)
  savefig(paste0("daily_posture_daily_", i), 10, 10)
  
}

