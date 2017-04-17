#' Optimize stkmod function within depth interval
#' 
#' @param profin \code{data.frame} of a single depth profile
#' @param sigdiff numeric of density difference beyond which the model is estimated
#' @param expand numeric for expansion factor to increase resolution of search
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
findstk <- function(profin, sigdiff = 3, expand = 100, plot = FALSE) {
  
  # get relevant columns, remove NA
  profin <- select(profin, binmd, sig) %>% 
    na.omit

  # do not evaluate if less than two obs  
  if(nrow(profin) <= 2) return(NA)

  # linearly interpolate to avoid sample size issues
  xint <- range(profin$binmd)
  xint <- seq(xint[1], xint[2], length = expand)
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
  ests <- stkmod(brk, ints, est = T) %>% 
    data.frame
  
  p1 <- ggplot2::ggplot(profin, ggplot2::aes(x = binmd, y = sig)) + 
    ggplot2::geom_point(size = 2, alpha = 0.6) + 
    ggplot2::geom_path(data = ests, ggplot2::aes(x = xout, y = yout)) + 
    ggplot2::geom_point(data = ests[2, ], ggplot2::aes(x = xout, y = yout), size = 4, fill = 'lightgreen', pch = 21) + 
    ggplot2::scale_x_reverse('Depth (m)') +
    ggplot2::scale_y_continuous('Sigma') + 
    ggplot2::coord_flip() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle(label = NULL, subtitle = paste('Zmix =', round(ests[2, 1], 3)))
  return(p1)
        
}  