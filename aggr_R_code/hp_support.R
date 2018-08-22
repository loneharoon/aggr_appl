
outlier_hp <- function(daymat){
  #browser()
  daymat <- daymat[complete.cases(daymat),] # removes rows containing NAs
  daymat1 <- apply(daymat,2, function(x) abs(fft(x-mean(x)))^2/length(x)) 
  #daymat1 <- as.xts(t(apply(daymat,1, function(x) abs(fft(x-mean(x)))^2/length(x))) )
  # daymat1 <- as.xts(t(apply(daymat,1, function(x) Mod(Re(fft(x))))) )
  daymat<- daymat1
  dis_mat=dist(t(daymat))
  fit <- cmdscale(dis_mat, eig = TRUE, k = 2)
  x <- scale(fit$points[, 1])
  y <- scale(fit$points[, 2]); # mymdsplot(x,y,"xx_august15.pdf")
  scale_mds_mat<-cbind(x,y)
  dist_mat=dist(scale_mds_mat)
  dist_mat=as.matrix(dist_mat)
  den_x = vector("numeric",nrow(dist_mat))
  prob_x=vector("numeric",nrow(dist_mat))# for probability at each point
  k=6 # calculated from paper
  for(j in 1:nrow(dist_mat))
  {
    radius<-sort(dist_mat[j,])[k+1] ##CAREFUL ABOUT SECOND PARAMETER, IT DEFINES NO. OF NEIGHBOURS REQUIRED,referred in papers as k 
    den_x[j] =  k/(pi*radius^2)
    #den_x[j] = k/radius
  }
  for(h in 1:nrow(dist_mat))
  {
    prob_x[h] = round(1-(den_x[h]/max(den_x)),2)
  }

  return(prob_x)
}

