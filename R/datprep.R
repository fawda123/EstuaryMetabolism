#' Prep input data
#' 
#' Prepare input data for estimating metatolic rates
#' 
#' @param dat input \code{data.frame}
#' @param ... additional arguments passed  to \code{\link{binterp}}
#' 
#' @details Input data are linearly interpolated across depth bins and time to fill missing values and inrease resolution. This is a wrapper to \code{\link{binterp}}.
#' 
#' Currently assumes the time vector in the input data has equal time steps. 
#' 
#' @return The expanded \code{data.frame}
#' 
#' @export
#' 
#' @import dplyr tidyr
#' 
#' @examples
#' datprep(mblight)
datprep <- function(dat, ...){

  # interpolate wq data, recombine with wq
  out <- dat %>% 
    gather('var', 'val', -datetimestamp, -depth) %>% 
    group_by(var) %>% 
    nest %>% 
    mutate(int = purrr::map(data, ~ binterp(., ...))) %>% 
    select(-data) %>% 
    unnest %>%
    spread(var, val) %>% 
    select(datetimestamp, depth, everything())
  
  return(out)
    
}
      
