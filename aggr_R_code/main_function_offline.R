# In this file, I perform anomaly detection using AGGR and baselines on minutes data of Dataport homes only.
library(xts)
library(data.table)
library(ggplot2)
library(gtools)
library(plotly)
library(TSdist)
rm(list=ls())

file1 <- "115.csv"
path2 <- "path to dataset"
source("support_functions_offline.R")
source("hp_support.R") 


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

file1 = "115.csv"
base_directory <- "save directory path"
sub_dir <- strsplit(file1,'[.]')[[1]][1]
dir.create(file.path(base_directory, sub_dir))
agg_score <- do.call(rbind,agg_score)
write.csv(fortify(agg_score),file=paste0(base_directory,sub_dir,"/","energy_score.csv"),row.names = FALSE)