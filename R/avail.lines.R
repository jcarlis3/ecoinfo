#' Create random, available lines
#'
#' This function creates random, available (i,e, pseudo-absence) lines that are within the features of a specified polygon.
#' The created lines conform to a user-specified proximity rule, i.e., lines are not created within a specified
#' minimum distance from each other (rule enforced within a feature, but not between overlapping features).
#' The direction of each line is random.  All lines have the same length.  Each line are fully contained within the polygon.
#' Suited for input polygons with one or multiple features.
#'
#' @param poly SpatialPolygonsDataFrame, the area within which random, available lines are generated.
#' @param n Scalar, the number of lines to create.
#' @param length Scalar, the length of each line.  Unit is meters for projected coordinate systems.
#' @param dist Scalar, the minimum distance output lines can be from each other.  Unit is meters for projected coordinate systems.
#' @return A SpatialLinesDataFrame object of random, available (i.e., pseudo-absence) lines.
#' @author Jason D. Carlisle, University of Wyoming
#' @details Suited for input polygons with one or multiple features.  For example, if n=5 and poly has 3 features, the output will
#' have 15 total lines, 5 in each of the 3 polygon features.  The output object will have a @data slot with columns that identify
#' the polygon, a unique lineID, the coordinates of the line end-points, and the line length.  Lengths may be slightly different
#' from what is specified in the length argument.  This is due to circles created by rgeos::gBuffer being imperfectly smooth. 


avail.lines <- function(poly, n, length, dist){
  
  rand.lines <- list()  # Each feature's rand.lines will be a list component

  for(i in 1:length(poly)){
    
    # Subset poly to one feature
    p <- poly[i, ]
    # plot(p)
    
    # 1) Make the first new random line
    j=1  # For unique line ID
    
    # Create random start point
    pt <- sp::spsample(x=p, n=1, type="random", iter=100)  # random point 1
    # plot(pt, add=TRUE)
    
    # Create a temporary line to drop the endpoint on
    circ <- rgeos::gBuffer(pt, width=length)  # buffer
    cl <- as(circ, "SpatialLines")  # convert to line
    # plot(cl, add=TRUE, col="red")
    cl.in <- rgeos::gIntersection(cl, p)  # clip to polygon
    # plot(cl.in, add=TRUE, col="red", lwd=3)
    pt2 <- sp::spsample(x=cl.in, n=1, type="random", iter=100)  # random point 2
    # plot(pt2, add=TRUE)
    
    # Turn two points into the line of interest (SpatialLines object)
    beg.coords <- data.frame(x=sp::coordinates(pt)[1], y=sp::coordinates(pt)[2])
    end.coords <- data.frame(x=sp::coordinates(pt2)[1], y=sp::coordinates(pt2)[2])
    rand.lines[[i]] <- sp::SpatialLines(list(sp::Lines(list(sp::Line(rbind(beg.coords, end.coords))), ID=as.character(paste(i, j, sep=".")))))
    sp::proj4string(rand.lines[[i]]) <- sp::proj4string(p)
    # plot(rand.lines[[i]], col="blue", add=TRUE, lwd=2)

    # Turn into SLDF
    # Store polygon ID, line ID, coords of line ends, and length of line
    rand.lines[[i]] <- sp::SpatialLinesDataFrame(rand.lines[[i]], data.frame(polyID=i, lineID=paste(i, j, sep="."),
                                                                             pt1x=beg.coords[[1]], pt1y=beg.coords[[2]],
                                                                             pt2x=end.coords[[1]], pt2y=end.coords[[2]],
                                                                             length=rgeos::gLength(rand.lines[[i]]),
                                                                             row.names=paste(i, j, sep=".")))
    
    # 2) add other lines
    while(length(rand.lines[[i]]) < n){
      j <- (j + 1)  # To insure each line gets a unique ID
      
      # Create random start point
      pt <- sp::spsample(x=p, n=1, type="random", iter=100)  # random point 1
      # plot(pt, add=TRUE)
      
      # Create a temporary line to drop the endpoint on
      circ <- rgeos::gBuffer(pt, width=length)  # buffer
      cl <- as(circ, "SpatialLines")  # convert to line
      # plot(cl, add=TRUE, col="red")
      cl.in <- rgeos::gIntersection(cl, p)  # clip to polygon
      # plot(cl.in, add=TRUE, col="red", lwd=3)
      pt2 <- sp::spsample(x=cl.in, n=1, type="random", iter=100)  # random point 2
      # plot(pt2, add=TRUE)
      
      # Turn two points into the line of interest (SpatialLines object)
      beg.coords <- data.frame(x=sp::coordinates(pt)[1], y=sp::coordinates(pt)[2])
      end.coords <- data.frame(x=sp::coordinates(pt2)[1], y=sp::coordinates(pt2)[2])
      new.line <- sp::SpatialLines(list(sp::Lines(list(sp::Line(rbind(beg.coords, end.coords))), ID=as.character(paste(i, j, sep=".")))))
      sp::proj4string(new.line) <- sp::proj4string(p)
      # plot(new.line, col="blue", add=TRUE, lwd=2)
      
      # Test if new line is too close to other lines, if so, next.  If not, append.
      if(rgeos::gIntersects(new.line, rgeos::gBuffer(rand.lines[[i]], width=dist))){
        next
      }  # end if
      
      # Turn into SLDF
      # Store polygon ID, line ID, coords of line ends, and length of line
      new.line <- sp::SpatialLinesDataFrame(new.line, data.frame(polyID=i, lineID=paste(i, j, sep="."),
                                                                 pt1x=beg.coords[[1]], pt1y=beg.coords[[2]],
                                                                 pt2x=end.coords[[1]], pt2y=end.coords[[2]],
                                                                 length=rgeos::gLength(new.line),
                                                                 row.names=paste(i, j, sep=".")))
      
      # Append new line to those already made
      rand.lines[[i]] <- rbind(rand.lines[[i]], new.line)
      
    }  # end while
    
  }  # end for loop of poly features
  
  # Each feature has a rand.lines.  rbind them all together
  # IDs for each line must be unique
  all.rand.lines <- do.call(rbind, rand.lines)
  
  # Return the random, available lines
  return(all.rand.lines)
  
}  # end avail.lines function