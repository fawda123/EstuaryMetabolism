#' Prep input data
#' 
#' Prepare input data for estimating metatolic rates
#' 
#' @param dat input \code{data.frame}
#' @param n numeric expansion factor for depth bins
#' @param wqcols chr string names columns with water quality parameters
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
datprep <- function(dat, n = 15, wqcols = c('do_mgl', 'sal', 'temp')){

  # get date vector
  dts <- dat$datetimestamp %>% 
    unique
  
  # sanity check
  if(length(unique(diff(dts))) > 1)
    stop('Time steps are unequal')
  
  # create depth values to interpolate
  deps <- unique(dat$depth)
  deps <- seq(min(deps), max(deps), length = n)

  # get wx data
  wx <- dat[, c('datetimestamp', 'atemp', 'bp', 'wspd')] %>% 
    unique
  
  # get wq data
  wq <- dat[, c('datetimestamp', 'depth', wqcols)]
  
  # expansion grid to interpolate
  new_grd <- expand.grid(dts, deps) %>% 
    rename(
      datetimestamp = Var1, 
      depth = Var2
    ) %>% 
    arrange(datetimestamp, depth)

  # interpolate wq data, recombine with wq
  out <- wq %>% 
    gather('var', 'val', -datetimestamp, -depth) %>% 
    spread(depth, val) %>% 
    group_by(var) %>% 
    nest %>% 
    mutate(int = purrr::map(data, ~ binterp(., new_grd = new_grd))) %>% 
    select(-data) %>% 
    unnest %>%
    spread(var, val) %>% 
    select(datetimestamp, depth, everything()) %>% 
    left_join(., wx, by = 'datetimestamp')
  
  return(out)
    
}
      
