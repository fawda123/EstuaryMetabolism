#' Get RMSE of stick model for density stratification
#' 
#' @param strt numeric for depth of mixing layer
#' @param profin \code{data.frame} of a single depth profile
#' @param est logical if model estimates are returned instead of RMSE
#' 
#' @export
#' 
#' @details The stick model is fit to a plot of seawater density by depth to estimate the depth of the mixing layer.  The model assumes mean density at depths above the value for \code{strt} and a linear model at depths deeper than \code{strt}.  The model is joined at the mean density value above \code{strt} and the regression fit through the same value at \code{strt}.  The function returns RMSE of the fit for single interval optimization with \code{\link{findstk}}
#' .  
#' The \code{profin} data frame must have two columns for depth (\code{binmd}) and seawater density (\code{sig}).
#' 
#' @return The RMSE value if \code{est = FALSE}, otherwise a two-element list of x and y values corresponding to the beginning, join, and ending values for depth and density for the model. 
#' 
#' @import dplyr
#' 
stkmod <- function(strt, profin, est = FALSE) {
  
  # cut depth vector above/below strt depth
  cts <- profin$binmd %>%
    cut(breaks = c(-Inf, strt, Inf))

  # separate interpolated data by cts
  ptsabv <- filter(profin, binmd < strt)
  ptsbel <- filter(profin, binmd >= strt)

  # average density above strt
  aveabv <- mean(ptsabv$sig, na.rm = T)

  # format pts below for fixed origin model at strt and aveabv
  ptsbel <- mutate(ptsbel,
    binmd = binmd - strt, 
    sig = sig - aveabv
    )
  
  # fit model, get predictions, translate back to original scale
  lmbel <- lm(sig ~ 0 + binmd, ptsbel)
  lmbel <- predict(lmbel) + aveabv
  
  # return estimates if true
  if(est){
    
    xout <- c(strt, range(profin$bin)) %>% 
      sort
    yout <- c(rep(aveabv, 2), lmbel[length(lmbel)])
    out <- list(xout = xout, yout = yout)
    
    return(out)

  } 
  
  # otherwise return rmse of model fit  
  errabv <- ptsabv$sig - aveabv
  errbel <- ptsbel$sig + aveabv - lmbel
  out <- sum(errabv^2, errbel^2) %>%
    sqrt
  
  return(out)

}