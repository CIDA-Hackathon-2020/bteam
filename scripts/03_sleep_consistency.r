# Script Purpose: For each participant, compare the daily trajectory on different days



##################################
############# Setup ##############
##################################
library(ggplot2)
library(gplots)
library(lubridate)
library(reshape2)
library(GGally)

# convolution function for smoothing  
conv <- function(x, f){
  # note: filter f must have odd length
  n <- length(x)
  m <- length(f)
  h <- (m-1)/2
  xc <- numeric(length=n-2)
  for(i in h:(n-h)){
    xc[i-h+1] <- sum(x[(i-h+1):(i+h+1)] * f)
  }
  return(xc)
}

outpath <- file.path("graphics", "day_differences")

# which smoothing parameters to try (this is sd of gaussian filter)
sigs <- c(3, 6, 12)

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

# matrix to hold sleep irregularity scores
score_mat <- matrix(NA, dim(exploratory_data)[1], length(sigs))

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
  
  # get date time day of week, etc. 
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
  # there's a big step change in the middle when the device is changed
  # that's the only negative step change, so just floor at zero:
  ts$step_diff <- pmax(0, ts$step_diff)
  # drop the first and last days
  ts <- ts[ts$yday != min(ts$yday) & ts$yday != max(ts$yday),]
  
  # give each day its own column
  ts$ywday <- paste(ts$yday, ts$wday, sep="_")
  ts2 <- dcast(ts, dminute ~ ywday, value.var="minute_mets")
  n2 <- dim(ts2)[1]
  m2 <- dim(ts2)[2]
  
  # perform smoothing then compare different days, for multiple levels of smoothing: 
  for(l in 1:length(sigs)){
    sig <- sigs[l]
    ## Smooth functions by convolving with filter
    # gaussian filter:
    k <- 20
    f <- dnorm(seq(-k, k, 1), sd=sig)
    f <- f/sum(f)
    # manual filter:
    #f <- c(0.1, 0.2, 0.4, 0.2, 0.1)  
    
    # smoothed (convolved) version of the time series
    tss <- data.frame(sapply(ts2, function(x){conv(x, f)}))
    colnames(tss) <- colnames(ts2)
    tss2 <- tss[,2:m2]  # drop first column (it's the time of day)
  
    # figure of smoothing result 
    fpath <- file.path(outpath, paste0(paste("Smoothing", i, j, sig, sep="_"), ".jpg"))
    jpeg(fpath, width=10, height=10, units="in", res=72)
    par(mfrow=c(3,1))
    plot(f, type="p"); lines(f)
    plot(ts2$dminute, ts2[,2], type="l")
    plot(tss$dminute, tss[,2], type="l")
    dev.off()
     
    # matrix plot of smoothed trajectories 
    par(mfrow=c(1,1))
    fpath <- file.path(outpath, paste0(paste("matplot", i, j, sig, sep="_"), ".jpg"))
    jpeg(fpath, width=10, height=5, units="in", res=72)
    matplot(tss2, type="l")
    dev.off()
    
    # compute euclidean distance between days
    dists <- as.matrix(dist(t(tss2), diag=T))
      
    # heatmap of distances (with dendrograms)
    fpath <- file.path(outpath, paste0(paste("heatmap", i, j, sig, sep="_"), ".jpg"))
    hm <- heatmap.2(dists)
    dev.off()
    # compute the "sleep irregularity score" as the average distance between trajectories
    score <- round(mean(hm$carpet), 0)
    title <- paste("Subject", j, i, score, sep="_")
    score_mat[i,l] <- score
    jpeg(fpath, width=10, height=10, units="in", res=72)
    hm <- heatmap.2(dists, main = title, vline=NULL, hline=NULL, trace="none")
    dev.off()
   
    # Figure of just the dendrogram 
    fpath <- file.path(outpath, paste0(paste("dendrogram", i, j, sig, sep="_"), ".jpg"))
    jpeg(fpath, width=10, height=10, units="in", res=72)
    par(mfrow=c(1,1))
    plot(hm$rowDendrogram, main=title)
    dev.off()
    
    # reorder days according to dendrogram grouping
    new_order <- rownames(dists)[hm$rowInd]
    ts$ywday2 = factor(ts$ywday, levels=new_order)
    
    # plot daily trajectories, but order according to dendrogram
    p <- ggplot(ts, aes(x=dminute, y=minute_mets, color=wday)) + 
      geom_line(aes(y=minute_mets)) + 
      facet_grid(ywday2~.) + 
      labs(title=title)
    fname <- paste0(paste("ordered_daily_mets", i, j, sig, sep="_"), ".jpg")
    ggsave(fname, h=10, w=10, path=outpath, plot=p)
  }
}

# write scores to folder
write.csv(score_mat, file=file.path("processed_data", "score_mat.csv"))
colnames(score_mat) <- paste0("smoothing_", sigs)

# combine sleep irregularity with demographic info
ed2 <- cbind(exploratory_data, score_mat)

# look at distribution of sleep irregularities
hist(ed2$smoothing_3, breaks=10)
hist(ed2$smoothing_6, breaks=10)
hist(ed2$smoothing_12, breaks=10)

# how consistent are the three scores from three diferent levels of smoothing?
plot(ed2$smoothing_3, ed2$smoothing_6)
plot(ed2$smoothing_3, ed2$smoothing_12)
plot(ed2$smoothing_6, ed2$smoothing_12)
# very consistent

# save agumented demographics
write.csv(ed2, file=file.path("processed_data", "demographics_with_scores.csv"))

# order id's based on sleep irregularity
ed2$id[order(ed2$smoothing_3)]

# big scatterplot matrix
pairs(ed2)
ggpairs(ed2)
ggsave("scatterplot_matrix.png", path=file.path("graphics", "for_pres"), h=15, w=15)

# try to predict consistency score from demographics
m <- lm(smoothing_3 ~ age + percent_bf, data=ed2)
summary(m)


