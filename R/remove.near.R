#' Remove spatial points based on proximity
#'
#' Removes points from pts that do not meet a user-specified proximity rule, i.e., tosses
#' out points that are within some specified distance of another point in pts.
#' Note, this function is not necessarily optimized, meaning it may be possible to remove
#' points in a different order that would result in fewer points having to be removed.
#'
#' @param pts SpatialPointsDataFrame, the input points to thin based on proximity.
#' @param dist Scalar, the minimum distance features in pts can be from each other.
#' @return A SpatialPointsDataFrame with equal or fewer features than pts.
#' @author Shannon E. Albeke and Jason D. Carlisle, University of Wyoming, <jason.d.carlisle@@gmail.com>


remove.near <- function(pts, dist){

  #Create a temp.id column used later in data.frame manipulation
  pts@data$temp.id <- 1:length(pts)

  #Create a complete distance matrix
  d <- gDistance(pts, byid=TRUE) #uses rgeos package
  #Remove the 0s on the diagonal (distance of point to itself)
  d[row(d) == col(d)] <- NA

  # If some points are too close, start thinning,
  # Else, end
  if(min(d, na.rm=TRUE) < dist){
    #Loop through each point and determine which points are within Dist
    outDF <- data.frame()
    for(i in 1:nrow(d)){
      d.match <- unique(which(d[i,] < Dist))
      if(length(d.match) > 0){
        outDF <- rbind(outDF, data.frame(inPt=i, closePt=paste(d.match, collapse=",")))
      }#close if statement
    }#close for loop

    #Loop through each close point and remove points as needed
    outDF$closePt <- as.character(outDF$closePt)     #Adjust data type in outDF
    retain <- pts                    #Copy pts to retain

    for(i in 1:nrow(outDF)){
      #First check to see if current outDF point still exists in the retain dataset
      if(!is.na(match(outDF$inPt[i], retain$temp.id))){
        #Point still exists in the dataset, so remove the other points that are too close
        kill <- which(retain@data$temp.id %in% unlist(strsplit(outDF[i, 2], ",")))
        if(length(kill) > 0){
          retain <- retain[-kill,]
        }#close inner if statement
      }#close outer if statement
    }#close for loop

  }else{
    retain <- pts
  }


  retain@data$temp.id <- NULL             #Remove temp.id column
  return(retain)

}

