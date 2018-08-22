# In this file, I perform anomaly detection using OMNI and baselines on minutes data of Dataport homes only.
library(xts)
library(data.table)
library(ggplot2)
library(gtools)
library(plotly)
library(TSdist)
#library(Rlof)
#library(HighDimOut) # to normalize output scores
rm(list=ls())

file1 <- "115.csv"
#path2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/default/" 
path2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/default/injected_anomalies/"
source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/nilmtk_work/R_support/support_functions_offline.R")
source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/Matrix_division/Samys_support.R") #
source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/Matrix_division/hp_support.R") #SAMY METHOD
setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/nilmtk_work/plots/")

Sys.timezone()
Sys.setenv('TZ' ="Asia/Kolkata")
df <- fread(paste0(path2,file1))
df_xts <- xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$localminute)-19800)
head(df,2)[,1]
head(df_xts,2)
# with energy data
dat <- df_xts$use
dat <- dat['2014-06-22/2014-08-30 23:59:59']
colnames(dat) <- "power"
temp = dat
# Now I create groups of days, where each group consists of consecutive 14 days. I believe I am doing because AD will takes 14 at a time and compute anomaly score
temp$day = rep(c(1:5),each=1440*14)#creating factors for grouping days, split.xts does not work perfectly
dat_month <- split(temp,f=temp$day)
dat_month <- lapply(dat_month, function(x){
  p = as.xts(x) #x is a zoo object
  q = p$power # droping day column
  return(q)
})

# dat_month <- split.xts(dat,"days",k=21)
# gp_len <- sapply(dat_month, function(x) length(x)/1440) # checking no. of days in each split
# print(gp_len)
# dat_month <- dat_month[gp_len > 5] # dropping splits with less than 5 days.

agg_score <- list()

for (i in 1:length(dat_month)) {
  #dat_month[[i]] = subset(dat_month[[i]],select=c("power"))
  dat_day <- split.xts(dat_month[[i]],"days",k=1)
  date_index <- sapply(dat_day,function(x) unique(as.Date(index(x),tz="Asia/Kolkata")))
  mat_day <- create_feature_matrix(dat_day)
  energy_anom_score <- outlierfactor(mat_day)
  print(paste0("Lof done::",i))
  energy_anom_score_xts <- xts(energy_anom_score,as.Date(date_index))
  #energy_anom_score_xts
  res <- anomaly_detection_main_1minute_data_rate(dat_month[[i]])# call to samys method
  res_samy <- xts(round(res$score,2),res$timestamp)
  print(paste0("Muliti-user done::",i))
  res_hp <- outlier_hp(mat_day)
  print(paste0("HP done::",i))
  hp_score_xts <- xts(res_hp,as.Date(date_index))
  
  agg_score[[i]] <- cbind(energy_anom_score_xts,res_samy,hp_score_xts)
  colnames(agg_score[[i]]) <- c("lof","multi_user","hp")
}

file1 = "3538.csv"
base_directory <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/nilmtk_work/inter_results/"
sub_dir <- strsplit(file1,'[.]')[[1]][1]
dir.create(file.path(base_directory, sub_dir))
agg_score <- do.call(rbind,agg_score)
write.csv(fortify(agg_score),file=paste0(base_directory,sub_dir,"/","energy_score.csv"),row.names = FALSE)


#LOGIC TO COMPUTE F-SCORE,PRECISION AND RECALL
house="3538.csv"
result <- paste0("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/nilmtk_work/inter_results/",strsplit(house,'[.]')[[1]][1],"/","energy_score.csv")
#gt_directory <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/default/ground_truth/"
gt_directory <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/nilmtk_work/inter_results/ground_truth/"
gt <- fread(paste0(gt_directory,house))
#gt <- fread(paste0(gt_directory,"490.csv"))
gt$Index <- as.Date(gt$Index,tz="Asia/Kolkata")
res_df <- fread(result)

compute_f_score(res_df,gt,threshold = 0.80)





# READ CONTEXT DATA:
# occu_data <- create_time_series_occupancydata(dat,baseline_limit = 500)
occup_path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/occupancy/"
df_occup <- fread(paste0(occup_path,file1))
df_occup_xts <- xts(df_occup[,2],fasttime::fastPOSIXct(df_occup$Index)-19800)
weather_file <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/weather/Austin2014/minute_Austinweather.csv"
df_con <- fread(weather_file)
df_con_xts <- xts(df_con[,2:3],fasttime::fastPOSIXct(df_con$localminute) - 19800)
df_sub <- df_con_xts['2014-06-01/2014-08-30']
df_sub <- cbind(df_sub,df_occup_xts[index(df_sub)])
dat_con_month <- split.xts(df_sub,"months",k=1)
temp_data <- dat_con_month[[2]]

con_anom_score_xts <- summarize_context_with_individual_features(temp_data)
con_anom_score_xts
anomaly_threshold = 0.80
f_result <- decide_final_anomaly_status(energy_anom_score_xts,con_anom_score_xts,anomaly_threshold)
savedirec <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/nilmtk_work/inter_results/"
write.csv(fortify(f_result), file = paste0(savedirec,file1),row.names = FALSE )

visualize_context_data_facet_form(temp_data,'occupancy')
dataframe_visualize_all_columns(df_xts["2014-07-03"])

dataframe_visualize_all_columns(temp_data["2014-07-03"])

plot(dat['2014-07-30'])
plot(df_xts['2014-07-01','use'])


###INJECTING  temperature SIGNATURE
df_change <- temp_data
df_change['2014-07-12 01:30:00/2014-07-12 10:00:00','TemperatureC'] <- 33
plot(df_change['2014-07-12']$TemperatureC)
# temp_data = df_change
###########################

###INJECTING  occupancy SIGNATURE
df_change <- temp_data
df_change['2014-07-12 01:30:00/2014-07-12 12:00:00','occupancy'] <- 1
plot(df_change['2014-07-12']$occupancy)
# temp_data = df_change
###########################