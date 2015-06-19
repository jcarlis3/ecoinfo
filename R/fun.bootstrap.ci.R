# Function that accepts a vector, screens NAs, and returns
# bootstrap-generated CIs of the mean (default is 95% CIs)


boot.mean <- function(vec, b, ci=0.95){
  
  vec <- vec[!is.na(vec)]
  meanvec <- 1:b
  
  
  for (i in 1:b){
    samp <- sample(vec, length(vec), replace=TRUE)
    meanvec[i] <- mean(samp)
  }
  
  return(quantile(meanvec, c(1-((ci/2)+.5), ((ci/2)+.5)), na.rm=TRUE))
}