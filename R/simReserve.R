
#' Simulate randomly sited reserve
#'
#' This function is used to generate a randomly sited reserve, more specifically,
#' an aggregate of multiple reserve units (distinct polygons).
#'
#' The reserve is built by randomly siting circular polygons of the specified size.  The polygons are allowed to overlap.  The radius of each circle is drawn from a uniform distribution with min and max values specified.
#' Circles are iteratively added until the specified area is reached, then a smaller circle is trimmed out if
#' the simulated reserve overshot the specified area.  The user can specify a tolerance for how much bigger/smaller
#' than the specified area is acceptable.
#'
#' @param target.poly A polygon within which all simulated reserves are to be contained.  The availability mask.
#' @param buff.width A vector of length 2, giving the minimum and maximum lengths of the radii of the randomly sited circles to be used.  Radius values will be drawn from a uniform distribution between these values.
#' @param total.area The target size of the simulated reserve.
#' @param wiggle How much bigger/smaller can the simulated reserves be than \code{total.area}
#' (in same units as \code{total.area})
#'
#' @return Object of class SpatialPolygons, the simulated reserve.
#'
#' @author Jason D. Carlisle, University of Wyoming and WEST, Inc., <jason.d.carlisle@@gmail.com>


# data(demo.msk, package="umbrella")
# target.poly <- demo.msk
# rm(demo.msk)
# total.area <- 2000
# wiggle <- 100
# buff.width <- c(5, 10)


simReserve <- function(target.poly, buff.width, total.area, wiggle){

  # Given the largest possible buffwidth, calc number of (unique) circles needed
  # (if no overlap of circles, but there likely will be), less one.
  start.num <- trunc((total.area / (pi*(buff.width[2]^2))) - 1)
  
  # For small total.area or big buff.width, start.num could be 0.  Start with 1 in this case
  if(start.num == 0){
    start.num <- 1
  }

  # plot(target.poly, lwd=2, add=TRUE)


  # Create simulated protected area using randomly sited circles
  # Because some likely overlap and because we're assuming all have the max radius length, more than (start.num) will be needed
  rand.poly <- sp::spsample(target.poly, n=start.num, type="random", iter=1000)
  # plot(rand.poly, add=TRUE)
  rand.poly <- rgeos::gBuffer(rand.poly, byid=TRUE,
                              width=runif(length(rand.poly), buff.width[1], buff.width[2]))
  
  # plot(rand.poly, add=TRUE)
  
  # Dissolve and crop
  rand.poly <- rgeos::gUnaryUnion(rand.poly)
  rand.poly <- rgeos::gIntersection(rand.poly, target.poly)
  
  # plot(rand.poly, add=TRUE, col="red")
  

  # Append circles one at a time until total area exceeds target total.area
  repeat{
    # Add one more random circle polygon
    p <- sp::spsample(target.poly, n=1, type="random", iter=1000) # select 1 point
    p <- rgeos::gBuffer(p, width=runif(length(p), buff.width[1], buff.width[2])) # buffer that point
    
    # plot(p, add=TRUE, col="orange")
    
    p <- rgeos::gIntersection(p, target.poly)  # crop to target.poly
    p <- sp::spChFIDs(obj=p, x="2") # change ID in order to have unique IDs for rbind
    
    rand.poly <- rbind(rand.poly, p) # append to the polys we have
    rand.poly <- rgeos::gUnaryUnion(rand.poly) # dissolve
    
    # plot(rand.poly, add=TRUE, col="red")

    # Stop once total area exceeds target size
    if(rgeos::gArea(rand.poly) >= total.area) break()
  }


  # Trim rand.poly down to exact size (+/- wiggle room)
  if(rgeos::gArea(rand.poly) > total.area){

    repeat{
      # Calc how much area to remove
      to.cut <- rgeos::gArea(rand.poly) - total.area

      # Calc radius of a circle this size
      rad <- sqrt((to.cut/pi))

      # Add one random polygon inside rand.poly (that won't reach outside rand.poly) to erase
      # temp <- rgeos::gBuffer(rand.poly, width=-rad)
      # x <- sp::spsample(temp, n=1, type="random", iter=1000) # select 1 point
      # Changed, now the random polygon to cut can rech beyond rand.poly
      x <- sp::spsample(rand.poly, n=1, type="random", iter=1000) # select 1 point
      x <- rgeos::gBuffer(x, width=rad) # buffer that point
      
      # plot(x, add=TRUE, col="blue")

      # Erase from rand.poly
      rand.poly <- rgeos::gDifference(spgeom1=rand.poly, spgeom2=x, byid=FALSE)
      
      # plot(target.poly, lwd=2, add=TRUE); plot(rand.poly, add=TRUE, col="red")

      # Stop once areas match close enough
      if(abs(total.area - rgeos::gArea(rand.poly)) <= wiggle) break()
    }
  }
  return(rand.poly)

}
