# Script Purpose: Loop through each subject and create descriptive statistics



##################################
############# Setup ##############
##################################

library(ggplot2)
library(lubridate)

file <- "minbymin_05.csv"

#####################################
############# Main Code #############
#####################################


{
  ts <- read.csv(file.path("raw_data", file))
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
  ts$dtime <- hms(as.character(ts$dt, format="%H:%M:%S"))
  ts$step_diff <- c(0, diff(ts$minute_steps))
  # there's a big step change in the middle when the device is changed
  # that's the only negative step change, so just floor at zero:
  ts$step_diff <- pmax(0, ts$step_diff)
}

# compare Mets and steps
ggplot(ts, aes(x=minute_steps)) + geom_histogram()
ggplot(ts, aes(x=step_diff)) + geom_histogram() + 
  scale_y_log10()
ggplot(ts, aes(x=minute_mets, y=step_diff, color=as.factor(ap_posture))) + geom_point(alpha=0.1)

# they basically tell us the same thing. 

# correlation between METS and steps?
cor(ts$minute_mets, ts$step_diff)

# really high


# Look at first day

day1 <- ts[ts$yday == min(ts$yday),]
ggplot(day1, aes(x=dminute, color=yday)) + 
  geom_line(aes(y=minute_mets), color="blue") + 
  geom_line(aes(y=step_diff), color="green")

# Look at first two days

days2 <- ts[ts$yday < min(ts$yday)+2,]
ggplot(days2, aes(x=dminute, color=yday)) + 
  geom_line(aes(y=minute_mets), color="black") + 
  facet_grid(yday~.) + 
  geom_hline(yintercept=1.25, color="red", linetype="dashed") + 
  geom_vline(xintercept=1320, color="blue", size=1.0)

# look at the first week

week1 <- ts[ts$yday < min(ts$yday) + 7,]
ggplot(week1, aes(x=dminute, color=as.factor(yday))) + 
  geom_line(aes(y=minute_mets))

ggplot(week1, aes(x=dminute)) + 
  geom_line(aes(y=minute_mets)) + 
  facet_wrap("wday2")

  
# look at whole period by week

ggplot(ts, aes(x=dminute)) + 
  geom_line(aes(y=minute_mets, color=as.factor(week))) + 
  facet_grid(wday2~.)

# look at whole period by day

ggplot(ts, aes(x=dminute, color=yday)) + 
  geom_line(aes(y=minute_mets), color="black") + 
  facet_grid(yday~.) + 
  geom_vline(xintercept=1320, color="blue", size=1.0)

ggplot(ts, aes(x=dminute, color=yday)) + 
  geom_line(aes(y=step_diff), color="black") + 
  facet_grid(yday~.) + 
  geom_vline(xintercept=1320, color="blue", size=1.0)

ggplot(ts, aes(x=dminute, color=yday)) + 
  geom_line(aes(y=ap_posture), color="black") + 
  facet_grid(yday~.) + 
  geom_vline(xintercept=1320, color="blue", size=1.0)

# look at each week





