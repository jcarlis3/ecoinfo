#' Create random, available points for RSF/SDM
#'
#' This function creates random, available (i,e, pseudo-absence) points that are within a specified polygon.
#' The created points conform to a user-specified proximity rule, i.e., points are not created within a specified
#' minimum distance from each other or the original used points.
#'
#' @param used SpatialPointsDataFrame, the used (i.e., presence) points.
#' @param poly SpatialPolygonsDataFrame, the area within which random, available points are generated.
#' @param dist Scalar, passed to ecoinfo::remove.near.  The minimum distance output points can be from the used points, and from each other.  Unit is meters for projected coordinate systems.
#' @param mult Scalar, the ratio of available points to used points.  1 for a balanced design, 2 for twice as many available points.
#' @return A SpatialPointsDataFrame of random, available (i.e., pseudo-absence) points.
#' @author Shannon E. Albeke and Jason D. Carlisle, University of Wyoming


avail.pts <- function(used, poly, dist=NULL, mult=1){

  # Create area available for random, available points
  # If min dist rule is applied, remove areas around used points from consideration
  if(!is.null(dist)){
    buff <- rgeos::gBuffer(used, width=dist)
    avail <- rgeos::gDifference(poly, buff)
  }else{
    avail <- poly
  }

  # Create availability points
  # If min dist rule is applied, some points will likely be too close, so oversample (5x) to start
  RandPtCount <- (mult * nrow(used))
  RandPts <- sp::spsample(x=avail, n=(5 * RandPtCount), type="random", iter=200)
  sp::proj4string(RandPts) <- sp::proj4string(used)

  # Make RandPts a SPDF
  df <- data.frame(availpt.id=1:length(RandPts))
  RandPts <- sp::SpatialPointsDataFrame(sp::coordinates(RandPts), data=df)

  # If min dist rule is applied, remove available points too close to other available points
  if(!is.null(dist)){
    RandRetainedPts <- ecoinfo::remove.near(pts=RandPts, dist=dist)
  }else{
    RandRetainedPts <- RandPts
  }

  # Sample down to appropriate number of points
  RandRetainedPts <- RandRetainedPts[sample(1:nrow(RandRetainedPts), size=RandPtCount),]
  length(RandRetainedPts)

  return(RandRetainedPts)
}
