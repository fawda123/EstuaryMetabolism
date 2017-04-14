#' Identify gap sizes in time series of depth profiles
#'
#' @param x \code{data.frame} of profile time series in long format
#'
#' @return A \code{data.frame} with three columns for start and end times and gap size.
#' 
#' @details A gap is defined as missing values for all depths for a single date.
#' 
#' @import dplyr
#' 
#' @export
gapsz <- function(x){
  
  # chk complete missing rows
  chk <- group_by(x, datetimestamp) %>% 
    summarize(
      mval = !any(!is.na(val))
    )
  
  # data frame of strt, end dates of gaps
  gaps <- data.frame(
    gapstr = with(chk, datetimestamp[diff(mval) == 1]),
    gapend = with(chk, datetimestamp[which(diff(mval) == -1) - 1])
    )
  
  # number of observations in each gap
  n <- sapply(1:nrow(gaps), function(dts){
    filter(chk, datetimestamp >= gaps[dts, 1] & datetimestamp <= gaps[dts, 2]) %>% 
      nrow
    })
  
  # combine n obs with gap start/end
  gaps <- mutate(gaps, n = n)
  
  return(gaps)
  
}