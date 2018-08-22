# NOW THIS CODE DOES NOT NEED TO STORE INTEDMEDIARY RESULTS. I HAVE IMPROVED IT WHEN I WAS WORKING WIHT MEGHAS PROJECT
#library(Ckmeans.1d.dp)
library(lattice)
#library(TSdist)
library(zoo)
# medoids
library(fpc)
library(cluster)
library(DMwR)
library(MASS)
#rm(list=ls())
#setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/AD-samys/")

fill_missing_data <- function(data) {
  
  #cat(paste(table(is.na(data$value)), "\n"))
  
  data_zoo <- zoo(data$value, data$timestamp)
  
  # check if the first data element is NA  
  if(is.na(data_zoo[[1]])) {
    #cat('   First element is NA.. filling it with the first non NA\n')    
    size = length(data_zoo)    
    ix = 2
    while(ix <= size) {
      if(!is.na(data_zoo[[ix]])) {
        break
      }
      ix = ix + 1
    }
    #cat(paste('   Replacing ', ix, 'th element and value ', data_zoo[[ix]], '\n'))
    # replace the NA with first non NA value
    data_zoo[[1]] = data_zoo[[ix]]    
  }
  
  data_zoo1 = na.locf(data_zoo)
  
  data_out <- data.frame(timestamp=index(data_zoo1), value=coredata(data_zoo1))  
  names(data_out) = names(data)  
  return (data_out)
}

get_rounded_data <- function(path, start.date, end.date) {
  
  data <- read.csv(path, stringsAsFactors=FALSE)
  data$timestamp = as.POSIXct(strptime(data$timestamp, "%Y-%m-%d %H:%M:%S"), tz="GMT")
  data = data[!duplicated(data$timestamp), ] # remove any duplicates based on timestamp
  
  #data = data[data$value != -1, ]
  
  data = data[ data$timestamp >= start.date, ]
  data = data[ data$timestamp <= end.date,   ]  
  
  # start.time = data[1,]$timestamp
  # end.time = data[nrow(data),]$timestamp  
  # round up the start.time to day
  # start.time = as.POSIXct(strptime(substr(start.time+(24*60*60), 1, 10), "%Y-%m-%d"), tz="GMT")
  # round down the end.time to day
  # end.time = as.POSIXct(strptime(substr(end.time-(24*60*60), 1, 10), "%Y-%m-%d"), tz="GMT") -1
  # data = data[ data$timestamp >= start.time, ]
  # data = data[ data$timestamp <= end.time,   ]  
  #  browser()
  full_dates = seq(start.date, end.date, by="1 hour")
  full_dates_frame <- data.frame(list(timestamp=full_dates))  
  data_merged <- merge(full_dates_frame, data, all=T)
  data_merged$power=na.approx(data_merged[,2]) # interpolating missing values by Haroon
  #data_merged[is.na(data_merged)] <- -1
  #data_merged = na.omit(data_merged)    
  #data_out = fill_missing_data(data_merged)  
  
  return (data_merged)
}  

# assumes that there is no missing element (timestamp) in the sequence
split_and_fill_missing_data_daily <- function(data, split_interval = 24) {  
 # browser()
  missing_threshold = split_interval / 4 # 25%
  #browser()
  #  data = data1
  sp = (0:nrow(data))%/%split_interval  + 1    # generate sequence numbers for each day
  sp = sp[1:(length(sp)-1)]         #ignore the last number
  dayList = split(data,  sp)       # split into day-day readings
  
  cat(paste("  Splitted into ", length(dayList), "days \n"))  
  
  miss = numeric(length(dayList))
  i = 1  
  #browser()
  for (day in dayList) {      
    dd = day[ is.na(day$value), ]    
    #cat(paste("     ", i, nrow(day), nrow(dd), day$timestamp[1], day$timestamp[24], "\n"))     
    miss[i] = nrow(dd)
    i = i +1
    #plot(day, type='l')
  }
  
  # consider only the days that contain less than #missing_threshold missing values
  #dayList1 = dayList[ sapply(dayList, function(x) any(x$value != -1)) ]
  missing = sapply(dayList, function(day) {
    dd = day[ is.na(day$value), ] 
    nrow(dd) < missing_threshold
  })
  
  # eliminate all the days that contain more that #missing_threshold missing values
  dayList1 = dayList[ missing ]
  
  cat(paste("  Removed #days with too many missing values ", length(dayList) - length(dayList1), "days \n"))     
  cat(paste("  Filling missing data (any) for ", length(dayList1), "days \n"))
  
  # fill the missing values of any day  
  for (i in 1:length(dayList1)) {    
    if(any(is.na(dayList1[[i]]$value))) {
      dayList1[[i]] = fill_missing_data(dayList1[[i]])
    }
  }  
  return (dayList1)
}
split_and_fill_missing_data_daily_Haroon <- function(data, split_interval = 24) {  
  # browser()
  missing_threshold = split_interval / 4 # 25%
  #browser()
  #  data = data1
  sp = (0:nrow(data))%/%split_interval  + 1    # generate sequence numbers for each day
  sp = sp[1:(length(sp)-1)]         #ignore the last number
  dayList = split(data,  sp)       # split into day-day readings
  return (dayList)
}

get_daytime_data <- function(data) {  
  hr = as.numeric(substr(data$timestamp, 12,13))  
  data1 = data
  data1 = data1[hr > 6, ]
  hr = as.numeric(substr(data1$timestamp, 12,13))
  data1 = data1[hr <= 18, ] 
  return (data1)
}

get_nighttime_data <- function(data) {  
  hr = as.numeric(substr(data$timestamp, 12,13))  
  data1 = data
  data1 = data1[hr > 6, ]
  hr = as.numeric(substr(data1$timestamp, 12,13))
  data1 = data1[hr <= 18, ]  
  hr = as.numeric(substr(data1$timestamp, 12,13))
  
  data2 = data
  data2 = data2[!data$timestamp %in% data1$timestamp, ]  
  hr = as.numeric(substr(data2$timestamp, 12,13))
  
  data3 = data2
  data3 = data3[-(1:7),]  
  return (data3)
}

get_distanct_matrix_origianl_sammy <- function(dayList) {  
  
  size = length(dayList)
  cat(paste("  Calculating distance matrix for ", size, "days.. \n"))  
  co = matrix(0, nrow=size, ncol=size)  
  names = character(size)
  
  for( row in 1:size) {
    #row=1
    x1 = dayList[[row]]    
    names[row] = paste("D", substr(x1$timestamp[1], 1, 10), sep="")
    
    for( col in 1:size) {
      #=1
      x2 = dayList[[col]]
      #browser()
      # normalize the data
      m = mean(x1$power)
      s = sd(x1$power)    
      #x1$value = (x1$value - m) / s
      
      m = mean(x2$power)
      s = sd(x2$power)    
      #x2$value = (x2$value - m) / s
      
      #cat(paste(nrow(x1), nrow(x2), "\n"))
      #co[row,col] = dtwDistance(x1$value, x2$value)      
      co[row,col] = DTWDistance(x1$power, x2$power)      
      #co[row,col] = euclideanDistance(x1$value, x2$value)            
    }
  } 
  
  rownames(co) = names
  colnames(co) = names  
  return (co)
}

get_distanct_matrix <- function(dayList) {  
  
  size = length(dayList)
  cat(paste("  Calculating distance matrix for ", size, "days.. \n"))  
  co = matrix(0, nrow=size, ncol=size)  
  names = character(size)

  for( row in 1:size) {
    #row=1
    x1 = dayList[[row]]    
    names[row] = paste("D", substr(x1$timestamp[1], 1, 10), sep="")
    for( col in 1:row) {
      x2 = dayList[[col]]
      co[row,col] = DTWDistance(x1$power, x2$power)      
    }
  }
# convert lower triangular to full matrix
  for(i in 1:NROW(co)){
    for(j in 1:NCOL(co)){
      co[i,j] = co[j,i] 
    }
  }
  #browser()
  rownames(co) = names
  colnames(co) = names  
  return (co)
}


make_cluster_distance_modified_byHaroon <- function(context) {  
  # Note: this method is a sub-part of original make_cluster_distance(), in this function I have 
  # only the code of Algorithm 1 of Samy's paper
  #browser()
  distMat =  context
  names(distMat) = NULL
  
  distMat1 <- as.matrix(distMat)
  d <- as.dist(distMat1)
  
  pamk = pamk(d,k=1:5)
  # browser()
  n = pamk$nc
  n
  pamx = pam(d, n)
  
  
  anomaly_score= find_anomaly_score_modified(distMat1, pamx)
  return(anomaly_score)
}

find_anomaly_score_modified <- function(distMat1, pamx) {  
  
  daynames <- substr(colnames(distMat1),2,11)
  dist <- data.frame(distance=numeric())
  days_count = length(pamx$clustering)
  
  for (day in 1:days_count) {  
    #day=1
    cl_no = pamx$clustering[day]    
    med = pamx$id.med[cl_no]
    medoid_data = distMat1[med, ]    
    day_data = distMat1[day, ]    
    #dis = euclideanDistance(medoid_data, day_data)    
    dis = get_anomaly_score(day, distMat1, pamx)
    # dd = data.frame(day_no=day, cluster=cl_no, mediod=med, distance=dis)
    dd = data.frame(distance=dis)
    dist = rbind(dist, dd)
  }
  
  # compute the anomaly score
  dist$timestamp <- as.Date(daynames)
  x <- dist$distance
  xx <- vector(mode="numeric",length=length(x))
  minval = min(x)
  maxval = max(x)
  for (i in 1:length(x)){
    xx[i] <- (x[i]-minval)/(maxval-minval)
  }
  #% dist <- cbind(dist,score=xx)
  score_df <- data.frame(timestamp = dist$timestamp, score = xx)
  return(score_df)
  
}

get_anomaly_score <- function(day, distMat1, pamx) {
  
  day_data = distMat1[day, ]
  
  max_cl = max(pamx$clustering)
  dis = numeric(max_cl)
  cl_size = numeric(max_cl)
  
  for (cl_no in 1:max_cl) {  
    #cl_no=2
    med = pamx$id.med[cl_no]
    medoid_data = distMat1[med, ]
    cl_size[cl_no] = pamx$clusinfo[cl_no]  # no. of objects in cluster   
    #dis[cl_no] = euclideanDistance(medoid_data, day_data)    
    dis[cl_no] = EuclideanDistance(medoid_data, day_data)    
  }  
  dis_sum = cl_size * dis
  dis_avg = mean(dis_sum)
  
  return (dis_avg)
}

# based on distance between mediod and other points in that cluster 
make_cluster_distance_byHaroon <- function(house_path, sensor_names, context, outlier) {  
  # Note: this method is a sub-part of original make_cluster_distance(), in this function I have 
  # only the code of Algorithm 1 of Samy's paper
  for (sensor_name in sensor_names) {  
    # browser()
    #sensor_name = sensor_names[3]   
    file_path = paste(house_path,sensor_name, sep="/")
    cat(paste(file_path, "\n"))  
    distMat = read.table(file_path, header = T, stringsAsFactors = F)  
    names(distMat) = NULL
    cat("\n works till state 1")
    distMat1 <- as.matrix(distMat)
    d <- as.dist(distMat1)
    cat("\n works till state 2")
    #mds <- cmdscale(d)
    #d <- mds
    
    #plot(mds.coor)
    #loff = lofactor(mds.coor, 10)
    pamk = pamk(d,k=1:5)
    # browser()
    n = pamk$nc
    n
    pamx = pam(d, n)
    cat("\n works till state 3")
    title = paste(context, sensor_name)
    try( clusplot(pamx, shade=FALSE, color=TRUE, labels=2, lines=1, main=title), silent = T)
    cat("\n works till state 4")
    write_anomaly_score_new(house_path, sensor_name, context, distMat1, pamx)
    
    
    
  }  
  #return (outlier)
}

anomaly_detection_main <- function(data) {
  
if(is.xts(data)){
  data <- fortify(data)
  colnames(data) <- c("timestamp","power")
}
  
alldays = F
weekend = TRUE
weekday = F
weekday_day = TRUE
weekday_night = TRUE

#browser()
# compute distance matrices and save them 
# MAKE SURE THERE IS NO MISSING DATA OTHERWISE UN-COMMENT BELOW STATEMENTS
  #dayList = split_and_fill_missing_data_daily(data)
  #data = do.call("rbind", dayList)

  if(alldays == TRUE) {
    # for alldays
    data1 <- data
    #write_to_file(distance_house_path, "alldays_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context alldays ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1)  
    distMat = get_distanct_matrix(dayList)
    alldays_distmat = distMat
    #write_to_file(distance_house_path, "alldays", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekend == TRUE) {
    # for weekend
    data1 <- data[weekdays(data$timestamp) %in% c('Saturday','Sunday'),]
   # write_to_file(distance_house_path, "weekend_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekend ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1)  
    distMat = get_distanct_matrix(dayList) 
    weekend_distmat  = distMat
    #browser()
   # write_to_file(distance_house_path, "weekend", sensor_name, distMat, isMatrix=T)
  }   

  if(weekday == TRUE) {
    # for weekday
    data1 <- data[!weekdays(data$timestamp) %in% c('Saturday','Sunday'),]    
    #write_to_file(distance_house_path, "weekday_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekday ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1)  
    distMat = get_distanct_matrix(dayList)  
    weekday_distmat  = distMat
    #write_to_file(distance_house_path, "weekday", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekday_day == TRUE) {
    # for weekday_day
    data1 <- data[!weekdays(data$timestamp) %in% c('Saturday','Sunday'),]    
    data1 = get_daytime_data(data1)    
    #write_to_file(distance_house_path, "weekday_day_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekday_day ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1, 12)  
    distMat = get_distanct_matrix(dayList)  
    weekday_day_distmat <- distMat
    #write_to_file(distance_house_path, "weekday_day", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekday_night == TRUE) {
    # for weekday_night
    data1 <- data[!weekdays(data$timestamp) %in% c('Saturday','Sunday'),]    
    data1 = get_nighttime_data(data1)    
    #write_to_file(distance_house_path, "weekday_night_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekday_night ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1, 12) 
    #Haroon #dayList[[length(dayList)]] = NULL # remove the last day - night data as it will contains partial data
    distMat = get_distanct_matrix(dayList)
    weekday_night_distmat <- distMat
    #write_to_file(distance_house_path, "weekday_night", sensor_name, distMat, isMatrix=T)
  }
  



weekday_day_score <- make_cluster_distance_modified_byHaroon(weekday_day_distmat) 
weekday_night_score <- make_cluster_distance_modified_byHaroon(weekday_night_distmat) 
weekday_score <- data.frame(timestamp = weekday_day_score$timestamp, score = apply(data.frame(weekday_day_score$score,weekday_night_score$score),1,max))
weekend_score <- make_cluster_distance_modified_byHaroon(weekend_distmat) 

fscore <- rbind(weekday_score,weekend_score)
fscore <- fscore[order(fscore$timestamp),]
return(fscore)
}


anomaly_detection_main_1minute_data_rate <- function(data) {
  
  if(is.xts(data)){
    data <- fortify(data)
    colnames(data) <- c("timestamp","power")
  }
  
  alldays = TRUE
  weekend = FALSE
  weekday = FALSE
  weekday_day = FALSE
  weekday_night = FALSE
  
  #browser()
  # compute distance matrices and save them 
  # MAKE SURE THERE IS NO MISSING DATA OTHERWISE UN-COMMENT BELOW STATEMENTS
  #dayList = split_and_fill_missing_data_daily(data)
  #data = do.call("rbind", dayList)
  
  if(alldays == TRUE) {
    # for alldays
    data1 <- data
    #write_to_file(distance_house_path, "alldays_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context alldays ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1,1440)  
    distMat = get_distanct_matrix(dayList)
    alldays_distmat = distMat
    #write_to_file(distance_house_path, "alldays", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekend == TRUE) {
    # for weekend
    data1 <- data[weekdays(data$timestamp) %in% c('Saturday','Sunday'),]
    # write_to_file(distance_house_path, "weekend_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekend ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1,1440)  
    distMat = get_distanct_matrix(dayList) 
    weekend_distmat  = distMat
    #browser()
    # write_to_file(distance_house_path, "weekend", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekday == TRUE) {
    # for weekday
    data1 <- data[!weekdays(data$timestamp) %in% c('Saturday','Sunday'),]    
    #write_to_file(distance_house_path, "weekday_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekday ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1,1440)  
    distMat = get_distanct_matrix(dayList)  
    weekday_distmat  = distMat
    #write_to_file(distance_house_path, "weekday", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekday_day == TRUE) {
    # for weekday_day
    data1 <- data[!weekdays(data$timestamp) %in% c('Saturday','Sunday'),]    
    data1 = get_daytime_data(data1)    
    #write_to_file(distance_house_path, "weekday_day_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekday_day ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1,1440/2)  
    distMat = get_distanct_matrix(dayList)  
    weekday_day_distmat <- distMat
    #write_to_file(distance_house_path, "weekday_day", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekday_night == TRUE) {
    # for weekday_night
    data1 <- data[!weekdays(data$timestamp) %in% c('Saturday','Sunday'),]    
    data1 = get_nighttime_data(data1)    
    #write_to_file(distance_house_path, "weekday_night_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekday_night ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1, 1440/2) 
    #Haroon #dayList[[length(dayList)]] = NULL # remove the last day - night data as it will contains partial data
    distMat = get_distanct_matrix(dayList)
    weekday_night_distmat <- distMat
    #write_to_file(distance_house_path, "weekday_night", sensor_name, distMat, isMatrix=T)
  }
  
  
  if(alldays == TRUE){
    fscore <- make_cluster_distance_modified_byHaroon(alldays_distmat) 
    fscore <- fscore[order(fscore$timestamp),]
  }else{
  weekday_day_score <- make_cluster_distance_modified_byHaroon(weekday_day_distmat) 
  weekday_night_score <- make_cluster_distance_modified_byHaroon(weekday_night_distmat) 
  weekday_score <- data.frame(timestamp = weekday_day_score$timestamp, score = apply(data.frame(weekday_day_score$score,weekday_night_score$score),1,max))
  weekend_score <- make_cluster_distance_modified_byHaroon(weekend_distmat) 
   fscore <- rbind(weekday_score,weekend_score)
  fscore <- fscore[order(fscore$timestamp),]
  }
  return(fscore)
}


anomaly_detection_main_10_minute_data_rate <- function(data,readings_per_day) {
  
  if(is.xts(data)){
    data <- fortify(data)
    colnames(data) <- c("timestamp","power")
  }
  
  alldays = TRUE
  weekend = FALSE
  weekday = FALSE
  weekday_day = FALSE
  weekday_night = FALSE
  
  #browser()
  # compute distance matrices and save them 
  # MAKE SURE THERE IS NO MISSING DATA OTHERWISE UN-COMMENT BELOW STATEMENTS
  #dayList = split_and_fill_missing_data_daily(data)
  #data = do.call("rbind", dayList)
  
  if(alldays == TRUE) {
    # for alldays
    data1 <- data
    #write_to_file(distance_house_path, "alldays_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context alldays ", nrow(data1), "\n"))
    #browser()
    dayList = split_and_fill_missing_data_daily(data1,readings_per_day)  
    distMat = get_distanct_matrix(dayList)
    alldays_distmat = distMat
    #write_to_file(distance_house_path, "alldays", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekend == TRUE) {
    # for weekend
    data1 <- data[weekdays(data$timestamp) %in% c('Saturday','Sunday'),]
    # write_to_file(distance_house_path, "weekend_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekend ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1,1440)  
    distMat = get_distanct_matrix(dayList) 
    weekend_distmat  = distMat
    #browser()
    # write_to_file(distance_house_path, "weekend", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekday == TRUE) {
    # for weekday
    data1 <- data[!weekdays(data$timestamp) %in% c('Saturday','Sunday'),]    
    #write_to_file(distance_house_path, "weekday_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekday ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1,1440)  
    distMat = get_distanct_matrix(dayList)  
    weekday_distmat  = distMat
    #write_to_file(distance_house_path, "weekday", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekday_day == TRUE) {
    # for weekday_day
    data1 <- data[!weekdays(data$timestamp) %in% c('Saturday','Sunday'),]    
    data1 = get_daytime_data(data1)    
    #write_to_file(distance_house_path, "weekday_day_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekday_day ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1,1440/2)  
    distMat = get_distanct_matrix(dayList)  
    weekday_day_distmat <- distMat
    #write_to_file(distance_house_path, "weekday_day", sensor_name, distMat, isMatrix=T)
  }   
  
  if(weekday_night == TRUE) {
    # for weekday_night
    data1 <- data[!weekdays(data$timestamp) %in% c('Saturday','Sunday'),]    
    data1 = get_nighttime_data(data1)    
    #write_to_file(distance_house_path, "weekday_night_data", sensor_name, data1)
    
    #plot(data1, type='h', main=sensor_name)
    #cat(paste(" Processing context weekday_night ", nrow(data1), "\n"))
    
    dayList = split_and_fill_missing_data_daily(data1, 1440/2) 
    #Haroon #dayList[[length(dayList)]] = NULL # remove the last day - night data as it will contains partial data
    distMat = get_distanct_matrix(dayList)
    weekday_night_distmat <- distMat
    #write_to_file(distance_house_path, "weekday_night", sensor_name, distMat, isMatrix=T)
  }
  
  
  if(alldays == TRUE){
    fscore <- make_cluster_distance_modified_byHaroon(alldays_distmat) 
    #fscore <- fscore[order(fscore$timestamp),]
    fscore <- last(fscore$score)
  }else{
    weekday_day_score <- make_cluster_distance_modified_byHaroon(weekday_day_distmat) 
    weekday_night_score <- make_cluster_distance_modified_byHaroon(weekday_night_distmat) 
    weekday_score <- data.frame(timestamp = weekday_day_score$timestamp, score = apply(data.frame(weekday_day_score$score,weekday_night_score$score),1,max))
    weekend_score <- make_cluster_distance_modified_byHaroon(weekend_distmat) 
    fscore <- rbind(weekday_score,weekend_score)
    fscore <- fscore[order(fscore$timestamp),]
  }
  return(fscore)
}


Samys_outlier_score <- function(house_name){
  #function used to combine outlier scores  and present scors as outlier score per day basis
  dir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aug15-Nov15/SamyHourlyProcessResults/"
  fils <- list.files(path=paste0(dir,house_name),pattern="anomaly_*",recursive=T)
  ff <- lapply(fils, function(y){
    fil <- read.csv(paste0(dir,house_name,"/",y),sep=",")
    x <- fil$distance
    xx <- vector(mode="numeric",length=length(x))
    minval = min(x)
    maxval = max(x)
    for (i in 1:length(x)){
      xx[i] <- (x[i]-minval)/(maxval-minval)
    }
    fil <- cbind(fil,score=xx)
    fil$timestamp <- as.POSIXct(strptime(fil$timestamp,"%Y-%m-%d"))
    fil
    # as.data.frame(fil)
  })
  weekday_data <- merge(ff[[1]],ff[[2]], by = "timestamp")
  
  score <- vector(mode="logical",length <- dim(weekday_data)[1])
  for(i in 1:length(score)){
    score[i]= max(weekday_data[i,7],weekday_data[i,13])
  }
  weekday <- data.frame(timestamp=weekday_data$timestamp,score=score)
  weekday$timestamp <- as.POSIXct(strptime(weekday$timestamp,"%Y-%m-%d"))
  month_data <- rbind(weekday,data.frame(timestamp=ff[[3]]$timestamp, score=ff[[3]]$score)) 
  month_data <- month_data[order(month_data$timestamp),]
  #month_data$score
  return(month_data)
}



# based on silhoutte distance -- error
make_cluster_distance <- function(outlier) {  
  
  for (sensor_name in sensor_names) {  
    
    file_path = paste(house_path,sensor_name, sep="/")
    cat(paste(file_path, "\n"))  
    distMat = read.table(file_path, header = FALSE)  
    names(distMat) = NULL
    
    distMat1 <- as.matrix(distMat)
    d <- as.dist(distMat1)
    
    #distMat1 <- cmdscale(as.dist(distMat))
    #d = distMat1
    
    #plot(mds.coor)
    #loff = lofactor(mds.coor, 10)
    
    pamk = pamk(d)
    n = pamk$nc
    n
    pamx = pam(d, n)
    #clusplot(pamx, shade=FALSE, color=TRUE, labels=2)
    
    sil = pamx$silinfo$widths
    sild = data.frame(sil, row.names = NULL)
    sild = cbind(point=as.numeric(row.names(sil)), sild, row.names = NULL)
    
    
    percent_list = c(0.01, 0.05, 0.1)
    
    for (percent in percent_list) {
      
      avg_sum = 0  
      max_cl = max(pamx$clustering)
      for (cl_no in 1:max_cl) {
        
        sil_clus = sild[sild$cluster == cl_no, ] 
        # find the avg distance of that cluster
        med = pamx$medoids[cl_no]
        medoid_data = distMat1[med, ]
        size = nrow(sil_clus)
        dist = numeric(size)
        for( i in 1:size ) {  
          ss = sil_clus[i,]  
          point = ss$point  
          point_data = distMat1[point, ]  
          # dist[i] = euclideanDistance(medoid_data, point_data)  
          dist[i] = EuclideanDistance(medoid_data, point_data)  
        }
        
        # get 10% of outliers
        nn = ceiling( nrow(sil_clus) * percent )
        #out_clus = sil_clus[sil_clus$sil_width <= 0.3, ] 
        out_clus = tail(sil_clus, nn)
        
        sil_clus_new = sil_clus[! sil_clus$point %in% out_clus$point,]
        
        # find the avg distance of that cluster
        size_new = nrow(sil_clus_new)
        dist_new = numeric(size_new)
        for( i in 1:size_new ) {  
          ss = sil_clus_new[i,]  
          point = ss$point  
          point_data = distMat1[point, ] 
          #dist_new[i] = euclideanDistance(medoid_data, point_data) 
          dist_new[i] = EuclideanDistance(medoid_data, point_data)  
        }
        
        m1 = mean(dist)
        m2 = mean(dist_new)  
        avg_sum = avg_sum + (m1-m2)/m1
        
        cat(paste("Cluster avg differnece ", percent, cl_no, size, size_new, 
                  m1, m2, (m1-m2)/m1, avg_sum, "\n"))
      }
      
      avg_red = (avg_sum/max_cl) * 100
      
      cat(paste("Cluster avg differnece ", percent, cl_no, size, size_new, 
                m1, m2, avg_sum, avg_red, max_cl, "\n\n"))
      
      dd = data.frame(house = house_name, sensor = sensor_name, 
                      context = context, percent= percent, avg_red = avg_red )  
      
      outlier = rbind(outlier, dd) 
    } # percent
  }  
  return (outlier)
}
