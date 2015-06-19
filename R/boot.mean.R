#' Bootstrap CI of the mean
#'
#' This function accepts a vector, removes NAs, and returns a bootstrap-generated CI of the mean.
#'
#' @param vec Vector, must be numeric.
#' @param b Scalar, the number of bootstrap iterations to perform.
#' @param ci Scalar, the level of confidence interval (CI),  Default is 95\% CI.
#' @return A named numeric vector giving the lower and upper confidence limits.
#' @author Jason D. Carlisle, University of Wyoming, <jason.d.carlisle@@gmail.com>
#' @examples
#' set.seed(333)
#' x <- runif(50, min=0, max=10)
#' boot.mean(vec=x, b=100, ci=0.95)


boot.mean <- function(vec, b, ci=0.95){

  vec <- vec[!is.na(vec)]
  meanvec <- 1:b


  for (i in 1:b){
    samp <- sample(vec, length(vec), replace=TRUE)
    meanvec[i] <- mean(samp)
  }

  quantile(meanvec, c(1-((ci/2)+.5), ((ci/2)+.5)), na.rm=TRUE)
}
