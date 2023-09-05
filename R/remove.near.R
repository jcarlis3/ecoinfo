#' Remove spatial points based on proximity
#'
#' This function removes points from a set (pts) based on a user-defined proximity rule.
#' It discards points that are within a specified distance of another point
#' in the set. It's important to note that this function is not optimized, 
#' meaning it may not remove points in the most efficient order, 
#' potentially resulting in a higher number of points being removed.
#'
#' @param pts sf data frame, the input points to thin based on proximity.
#' @param dist Scalar, the minimum distance features in pts can be from each other.  Unit is meters for projected coordinate systems.
#' @return A sf spatial object with equal or fewer features than pts.
#' @author Shannon E. Albeke and Jason D. Carlisle, University of Wyoming, Camila Pacheco-Riaño University of Gothenburg


remove.near <- function(pts, dist){
  
  #Create a temp.id column used later in data.frame manipulation
  pts$temp.id <- 1:nrow(pts)
  
  #Create a complete distance matrix
  d <-sf::st_distance(pts,tolerance = dist)
  #d <- rgeos::gDistance(pts, byid=TRUE) #uses rgeos package
  #Remove the 0s on the diagonal (distance of point to itself)
  d[row(d) == col(d)] <- NA
  dist <- units::set_units(dist, "meters")
  # If some points are too close, start thinning,
  # Else, end
  if(min(d, na.rm=TRUE) < dist){
    #Loop through each point and determine which points are within dist
    outDF <- data.frame()
    for(i in 1:nrow(d)){
      d.match <- unique(which(d[i,] < dist))
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
        kill <- which(retain$temp.id %in% unlist(strsplit(outDF[i, 2], ",")))
        if(length(kill) > 0){
          retain <- retain[-kill,]
        }#close inner if statement
      }#close outer if statement
    }#close for loop
    
  }else{
    retain <- pts
  }
  
  retain$temp.id <- NULL             #Remove temp.id column
  return(retain)
  
}
