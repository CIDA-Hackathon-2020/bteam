# Script Purpose: DATA READING



##################################
############# Setup ##############
##################################

library(ggplot2)
library(dplyr)


#####################################
############# Main Code #############
#####################################
#Libraries
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

exploratory_data <- read.csv(files="raw_data/demographics.csv")

