#' Bootstrap CI of the mean with hierarchical data
#'
#' This function applies a non-parametric bootstrapping procedure suited to hierarchically structured data, sometimes called a multi-stage bootstrap.
#' For example, random sites are selected, then multiple sub-plots are surveyed at each site.  Resampling is done first at the top level, then at the sub-level
#' measurements associated with that top level stratum.  There is some debate about whether resampling at each level should be done with or without
#' replacement.  Here, we sample with replacement at both levels.
#' accepts a data.frame where one column is top-level strata, one column is values of interest  and returns a bootstrap-generated CI of the mean.
#'
#' @param x Name of column (in quotes) containing numeric sub-level measurements.
#' @param strata Name of column (in quotes) containing factors that identify top-level strata.
#' @param ci Level of confidence interval (CI).  Default is 95\% CI.
#' @param data Data.frame containing the above columns.
#' @param B Number of bootstrap replicates.  Default is 100.
#' @param plot Logical, should a histogram of the bootstrap replicates and CI be plotted.  Default is TRUE.
#' @return A named numeric vector giving the observed mean and lower and upper confidence limits.
#' @seealso boot.mean
#' @author Jason D. Carlisle, University of Wyoming


hier.boot.mean <- function(x, strata, data, B=100, ci=0.95, plot=TRUE){
  
  # Pull columns from dataframe
  (d <- data.frame(strata = data[, strata], x = data[, x]))
  
  # Placeholder for top-level means
  (top.mean <- rep(NA, B))
  
  for(i in 1:B){
    
    # Random sample of top level (w/replacement)
    (new.tops <- sample(unique(d$strata), replace=TRUE))
    
    # Placeholder for sub-level means
    (sub.mean <- rep(NA, length(new.tops)))
    
    for(j in 1:length(new.tops)){
      
      # Select the counts for the cluster
      (sub.counts <- d[d$strata==new.tops[j], "x"])
      
      # Random sample of sub-level (w/replacement)
      (new.sub.counts <- sample(sub.counts, replace=TRUE))
      
      # Mean of new sub-level counts
      (sub.mean[j] <- mean(new.sub.counts))
      
    }  # end j loop
    
    # Mean of sub-level means
    (top.mean[i] <- mean(sub.mean))
    
  }  # end i loop
  
  # Calculate CI with percentile method
  out <- c(mean(d$x),  # observed mean
           quantile(top.mean, (0.5 - (ci/2))),  # boot lower CI
           quantile(top.mean, ((ci/2) + 0.5)))  # boot upper CI
  
  names(out) <- c("obsmean", "low", "upp")
  
  return(out)
  
  
  if(plot==TRUE){
    
    hist(top.mean, breaks=20, col="grey", main="Distribution of bootstrap replicates", xlab="Top-level mean")
    
    abline(v=out[1], lwd=2)
    abline(v=out[2], lty="dashed", lwd=2)
    abline(v=out[3], lty="dashed", lwd=2)
    
    legend("topright", c("Observed mean", "Bootstrap CI for mean"), lty=c("solid", "dashed"), lwd=c(2, 2), col=c("black", "black"))
    
    
  }  # end plot
  
}  # end function