######
#' Get midpoints and widths of depth bins
#'
#' @param dat data.frame in long format with rows as one sample time and depth bin
#' @param maxd numeric for maximum depth of the site, see details
#'
#' @details If \code{maxd} is not supplied, the site depth is estimated as the maximum bin midpoint plus half the average binwidth. 
#' 
#' All depth values are positive, surface is assumed to be zero depth. 
#' 
#' @import dplyr tibble
#' 
#' @export
#' 
#' @return A data.frame with two columns, \code{binmd} for midpoint depth (m) and \code{binwd} for midpoint width
depsd <- function(dat, maxd = NULL){
    
  # center of depth bins
  deps <- dat$depth %>% 
    unique %>% 
    sort
  
  # if not supplied, estimate maximum depth as deepest center plus half the average difference between bins
  if(is.null(maxd))
    maxd <- mean(diff(deps))/2 + max(deps)
  
  # width of each bin
  depsd <- diff(deps) / 2 
  depsd <- depsd + deps[-length(deps)]
  depsd <- c(0, depsd, maxd) %>% 
    diff
  
  # value is width of bin, name is midpoint depth of bind
  names(depsd) <- as.character(deps)
  depsd <- data.frame(binwd = depsd) %>% 
    mutate(
      binmd = deps
    ) %>% 
    select(binmd, binwd)
  
  return(depsd)

}