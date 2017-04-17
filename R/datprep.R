#' Prep input data
#' 
#' Prepare input data for estimating metabolic rates
#' 
#' @param dat input \code{data.frame}
#' @param wqcols chr string of column names to inteprolate across depths, water quality variables only
#' @param ... additional arguments passed  to \code{\link{binterp}}
#' 
#' @details Input data are linearly interpolated across depth bins and time to fill missing values and increase resolution. This is a wrapper to \code{\link{binterp}}, more details therein. This function processes the water quality variables in \code{dat} by passing the observations as nested data frames to \code{\link{binterp}}.
#' 
#' The time vector in the input data must have equal time steps. 
#' 
#' @return The expanded \code{data.frame}
#' 
#' @export
#' 
#' @import dplyr tidyr
#' 
#' @examples
#' \dontrun{
#' datprep(mblight)
#' }
datprep <- function(dat, wqcols = c('do_mgl', 'sal', 'temp'), ...){

  # check if ts is continuous
  stp <- unique(dat$datetimestamp) %>% 
    diff %>% 
    unique
  if(length(stp) > 1)
    stop('Time series is not continuous')
  
  # make complete cases for time series and depth
  dat <- complete(dat, datetimestamp, depth) %>% 
    arrange(datetimestamp, depth)
  
  # get wx data
  wx <- dat[, c('datetimestamp', 'atemp', 'bp', 'wspd')] %>% 
    unique
   
  # get wq data
  wq <- dat[, c('datetimestamp', 'depth', wqcols)]

  # interpolate wq data by continuous chunks
  wq <- wq %>% 
    gather('var', 'val', -datetimestamp, -depth) %>% 
    group_by(var) %>% 
    nest %>% 
    mutate(int = purrr::map(data, ~ binterp(., ...))) %>% 
    select(-data) %>% 
    unnest %>% 
    spread(var, val)
  
  # combine with wq to get complete time series, return
  out <-  full_join(wq, wx, by = 'datetimestamp') %>% 
    arrange(datetimestamp, depth)
  out <- out[, names(dat)]

  return(out)
  
}
      
