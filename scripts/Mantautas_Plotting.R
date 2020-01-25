# Script Purpose:



##################################
############# Setup ##############
##################################

# library(ggplot2)  # for example



#####################################
############# Main Code #############
#####################################
checking <- summary_statistics_fake_days  %>% 
  dplyr::filter(Captured_Minutes==1440)

checking %>%
  ggplot(aes(x=How_Long_Resting, y=How_Many_Time_Resting)) +
  geom_point() +
  facet_wrap(~Week_Day)









