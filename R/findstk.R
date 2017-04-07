#' Optimize stkmod function within depth interval
#' 
#' @param profin \code{data.frame} o
#' @param plot logical if the model is plotted
#' 
#' @export
#' 
#' @import dplyr
#' 
#' @details This function finds the optimal value for \code{strt} in \code{\link{stkmod}} that minimizes RMSE for the stick model.  The function will return the maximum depth value in \code{profin} if the range of seawater densities is less than \code{sigdiff}, implying no stratification. 
#' 
#' The \code{profin} data frame must have two columns for depth (\code{binmd}) and seawater density (\code{sig}).
#' 
#' @return The depth of the mixing layer if \code{plot = FALSE}, otherwise a plot of the stick model.
#' 
findstk <- function(profin, sigdiff = 3, plot = FALSE) {
  
  # get relevant columns, remove NA
  profin <- select(profin, binmd, sig) %>% 
    na.omit

  # do not evaluate if less than two obs  
  if(nrow(profin) <= 2) return(NA)

  # linearly interpolate to avoid sample size issues
  xint <- range(profin$binmd)
  xint <- seq(xint[1], xint[2], length = 500)
  yint <- with(profin, approx(binmd, sig, xout = xint)$y)
  ints <- data.frame(binmd = xint, sig = yint)
  
  # interpolated depths and length
  binmd <- ints$binmd
  binln <- length(binmd)
  
  # interval ranges for optimizer
  rngs <- diff(binmd) / 2
  rngs <- binmd[-binln] + rngs
  rngs <- c(rngs[1], rngs[length(rngs)])
   
  # sig of linear trend top to bottom
  # mod <- lm(sig ~ binmd, profin) %>% 
  #   summary %>% 
  #   coefficients %>% 
  #   .[2, 4]
  
  sgdff <- range(profin$sig) %>% 
    diff
  
  # mixing depth is to bottom if density rng is insufficient
  if(sgdff < sigdiff) {
    
    brk <- max(profin$binmd)

  # otherwise optimize stickmodel within the interval    
  } else {
  
    brk <- optimize(stkmod, interval = rngs, profin = ints)$minimum
  
  }
  
  # return the optimum if no plot
  if(!plot) return(brk)
    
  # create plot
  ests <- stkmod(brk, ints, est = T)
  
  plot(sig ~ binmd, profin)
  with(ests, {
    segments(x0 = xout[1], x1 = xout[2], y0 = yout[1])
    segments(x0 = xout[2], x1 = xout[3], y0 = yout[2], y1 = yout[3])
    })
        
}  