#' Create random, available lines
#'
#' This function creates random, available (i,e, pseudo-absence) lines that are within a specified polygon.
#' The created lines conform to a user-specified proximity rule, i.e., lines are not created within a specified
#' minimum distance from each other.  The direction of each line is random.  All lines have the same length.
#' Each line must be fully contained within the polygon.  Currently works best when input polygon is one feature.
#'
#' @param poly SpatialPolygonsDataFrame, the area within which random, available lines are generated.
#' @param n Scalar, the number of lines to create.
#' @param length Scalar, the length of each line.  Unit is meters for projected coordinate systems.
#' @param dist Scalar, the minimum distance output lines can be from each other.  Unit is meters for projected coordinate systems.
#' @return A SpatialLines object of random, available (i.e., pseudo-absence) lines.
#' @author Jason D. Carlisle, University of Wyoming


avail.lines <- function(poly, n, length, dist){
  
  # 1) Make the first new random line
  i=1  # For unique line ID
  
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
  rand.lines <- SpatialLines(list(Lines(list(Line(rbind(beg.coords, end.coords))), ID=as.character(i))))
  proj4string(rand.lines) <- proj4string(p)
  # plot(rand.lines, col="blue", add=TRUE, lwd=2)
  
  
  # 2) add other lines
  while(length(rand.lines) < n){
    i <- (i + 1)  # To insure each line gets a unique ID
    
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
    new.line <- SpatialLines(list(Lines(list(Line(rbind(beg.coords, end.coords))), ID=as.character(i))))
    proj4string(new.line) <- proj4string(p)
    # plot(new.line, col="blue", add=TRUE, lwd=2)

    # Test if new line is too close to other lines, if so, next.  If not, append.
    if(rgeos::gIntersects(new.line, rgeos::gBuffer(rand.lines, width=dist))){
      next
    }  # end if
    
    # Append new line to those already made
    rand.lines <- rbind(rand.lines, new.line)
    
  }  # end while
  
  # Return the random, available lines
  return(rand.lines)
  
}  # end avail.lines function