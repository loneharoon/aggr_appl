normalizedata <- function(x){
  xx<- vector(mode="numeric",length=length(x))
  minval= min(x)
  maxval= max(x)
  for (i in 1:length(x)){
    xx[i] <- (x[i]-minval)/(maxval-minval)
  }
  return (xx)
}

create_feature_matrix <- function(tdata) {
  #daydata <- split.xts(tdata,f="days",k=1)
  #sapply(daydata)
  matdata <- as.data.frame(sapply(tdata,function(x) coredata(x$power)))
  mat_xts <- xts(matdata,index(tdata[[1]]))
  return(mat_xts)
}

summarize_context_with_individual_features <- function(temp_data){
  # in this we take each of the features and run anomaly detection algorithm
  data <- temp_data
  keep = c("TemperatureC","occupancy")
  data <-  data[,keep]
  features <- colnames(data)
  scores <- sapply(features, function(x){
    df  <- data[,x]
    # browser()
    colnames(df) <- "power" # acco. to create_feature_matrix function
    dat_day <- split.xts(df,"days",k=1)
    mat_day <- create_feature_matrix(dat_day)
    res <- outlierfactor(mat_day)
    return(res) 
  })
 # browser()
  f_scores<- apply(scores,1,max)
  date_index <- sapply(temp_data[,1],function(x) unique(as.Date(index(x),tz="Asia/Kolkata")))
  f_scores_xts <- xts(f_scores,as.Date(date_index))
  return (f_scores_xts)
  
}

create_time_series_occupancydata <- function(power_data,baseline_limit){
  if(dim(power_data)[2] == 1){
    occupancy = ifelse(power_data > baseline_limit,1,0)
  } else if("power" %in% colnames(power_data)) {
    occupancy = ifelse(power_data > baseline_limit,1,0)
  } else if("use" %in% colnames(power_data)){
    occupancy = ifelse(power_data$use > baseline_limit,1,0)
  } else {
    stop("No related column found")
  }
  occupancy_xts <- xts(occupancy,index(power_data))
  colnames(occupancy_xts) <- "occupancy"
  return(occupancy_xts)
}

outlierfactor <- function(daymat){
  library(Rlof)
  library(HighDimOut)
  # all results in the paper have been presented using this function
  daymat <- daymat[complete.cases(daymat),] # removes rows containing NAs
  # daymat1 <- as.xts(t(apply(daymat,1, function(x) abs(fft(x-mean(x)))^2/length(x))) ) # form 1
  # daymat2 <- as.xts(t(apply(daymat,1, function(x) abs(fft(x-mean(x)))/length(x))) )  # form 2
  # daymat1 <- as.xts(t(apply(daymat,1, function(x) Mod(Re(fft(x-mean(x)))))) )   #form 3
  #daymat <- daymat
  # browser()
  #dis_mat <- dist(t(daymat)) 
  dis_mat <- compute_dtw_distance_matrix(daymat)
  # fit <- cmdscale(dis_mat, eig = TRUE, k = 2)
  # x <- scale(fit$points[, 1])
  # y <- scale(fit$points[, 2]);  # mymdsplot(x,y,"abc.pdf")
  # daymat <- data.frame(x,y)
  
  #df.lof2 <- lof(daymat,c(5:8),cores = 2)
  df.lof2 <- lof(dis_mat,c(5:10),cores = parallel::detectCores()-1)
  #browser()
  #df.lof2 <- apply(df.lof2,2,normalizedata)
  df.lof2 <- apply(df.lof2,2,function(x) Func.trans(x,method = "FBOD"))
  # browser()
  # anom_mean <- apply(df.lof2,1,function(x) round(mean(x,na.rm=TRUE),2) )
  anom_max <-  apply(df.lof2,1,function(x) round(max(x,na.rm = TRUE),2) )#feature bagging for outlier detection
  #df=data.frame(day= c(format(start(daymat),"%d"):format(end(daymat),"%d")), score=anom_max,month=format(index(daymat[5,]),"%Y-%m"))
  # anom_sum1 <-  apply(df.lof2,1,function(x) round(sum(x),2) )
  # return(df)
  return(anom_max)
}


decide_final_anomaly_status <- function(energy_anom_score_xts,con_anom_score_xts,anomaly_threshold){
  
  for(i in 1:length(energy_anom_score_xts)){
    if(energy_anom_score_xts[i] >= anomaly_threshold & con_anom_score_xts[i] >= anomaly_threshold) {
      print (paste0("Contextually non-anomalous: ",index(energy_anom_score_xts[i])))
    } else if(energy_anom_score_xts[i] >= anomaly_threshold & con_anom_score_xts[i] <= anomaly_threshold){
      print (paste0("anomaly on:) ",index(energy_anom_score_xts[i])) )
      if(!exists("anom_vec")){
        anom_vec <- energy_anom_score_xts[i]
      } else{
        anom_vec <- rbind(anom_vec,energy_anom_score_xts[i])
      }
    }
  }
  if(exists("anom_vec")){
    return(anom_vec)
  }else{
    return (0)}
}

compute_dtw_distance_matrix  <- function(data_mat){
  # this computes distance w.r.t columns
  library(TSclust)
  cols = dim(data_mat)[2]
  dis_mat = matrix(0,nrow=cols,ncol=cols)
  # compute only lower traingular matrix
  for(row in 1:cols){
    ref_col = data_mat[,row]
    for(col in 1:row){
      comp_col = data_mat[,col]
      dis_mat[row,col] = DTWDistance(ref_col, comp_col)
    }
  }
  # convert lower_triangular to full_symmetric matrix
  for(i in 1:NROW(dis_mat)){
    for(j in 1:NCOL(dis_mat)){
      dis_mat[i,j] = dis_mat[j,i] 
    }
  }
  colnames(dis_mat) <- colnames(data_mat)
  row.names(dis_mat) <- colnames(data_mat)
  return(dis_mat)
}


compute_f_score <- function(res_df,gt,threshold){
  if(is.xts(res_df)) {
    res_df_xts =  res_df
  }else{
    res_df_xts <- xts(res_df[,2:NCOL(res_df)],as.Date(res_df$Index,tz="Asia/Kolkata"))
  }
  res_df_xts <- res_df_xts["2014-07-01/2014-08-30 23:59:59"]
  print("Only retaining july and Aug res")
  #threshold = 0.8
  f_score <- vector(mode="numeric")
  precise <- vector(mode="numeric")
  recal <- vector(mode="numeric")
  for (i in 1:NCOL(res_df_xts)){
    dat <- res_df_xts[,i]
    dat <- dat[dat >= threshold]
    f_dates <- index(dat)
    a_dates <- gt$Index
    tp <- f_dates[f_dates %in% a_dates]
    fp <- f_dates[!f_dates %in% a_dates]
    fn <- a_dates[!a_dates %in% f_dates]
    precision = length(tp)/(length(tp)+length(fp))
    recall =  length(tp)/(length(tp)+length(fn))
    f_score[i] <- round( 2*(precision*recall)/(precision+recall),2)
    precise[i] <- round(precision,2)
    recal[i] <- round(recall,2)
    #browser()
  }
  l <- rbind(f_score,precise,recal)
  #colnames(l) <- colnames(res_df[,2:NCOL(res_df)])
  #print(l)
  return(l)
}

compute_f_score_REDDandIawe <- function(res_df,gt,threshold){
  # this function is same as that of compute_f_score. It does subseet the dataset as is done in the mentioned ones
  if(is.xts(res_df)) {
    res_df_xts =  res_df
  }else{
    res_df_xts <- xts(res_df[,2:NCOL(res_df)],as.Date(res_df$Index,tz="Asia/Kolkata"))
  }
  #res_df_xts <- res_df_xts["2014-07-01/2014-08-30 23:59:59"]
  #print("Only retaining july and Aug res")
  #threshold = 0.8
  f_score <- vector(mode="numeric")
  precise <- vector(mode="numeric")
  recal <- vector(mode="numeric")
  for (i in 1:NCOL(res_df_xts)){
    dat <- res_df_xts[,i]
    dat <- dat[dat >= threshold]
    f_dates <- index(dat)
    a_dates <- gt$Index
    tp <- f_dates[f_dates %in% a_dates]
    fp <- f_dates[!f_dates %in% a_dates]
    fn <- a_dates[!a_dates %in% f_dates]
    precision = length(tp)/(length(tp)+length(fp))
    recall =  length(tp)/(length(tp)+length(fn))
    f_score[i] <- round( 2*(precision*recall)/(precision+recall),2)
    precise[i] <- round(precision,2)
    recal[i] <- round(recall,2)
   # browser()
  }
  l <- rbind(f_score,precise,recal)
  #colnames(l) <- colnames(res_df[,2:NCOL(res_df)])
  #print(l)
  return(l)
}

