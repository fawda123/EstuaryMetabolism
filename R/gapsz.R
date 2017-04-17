#' Identify gaps in time series of depth profiles
#'
#' @param x \code{data.frame} of profile time series in long format
#'
#' @return A \code{data.frame} with one row per time step and three columns indicating if all profiles are missing for the time step (\code{misi}, \code{TRUE} or \code{FALSE}, a unique identifier for the chunk (\code{mval}, lower-case if chunk is complete data, upper-case if missing data), and number of time observations in the chunk (\code{n}).
#' 
#' @details A gap is defined as missing values for all depths for a single date.
#' 
#' @import dplyr stringi zoo
#' 
#' @export
gapsz <- function(x){

  # chk complete missing rows
  # mval is vector of str and end values for endpoints of missing observations
  chk <- group_by(x, datetimestamp) %>% 
    summarize(
      misi = !any(!is.na(val))
    ) %>% 
    ungroup %>% 
    mutate(
      mval = c(0, diff(misi)), 
      mval = ifelse(mval == 0, NA, mval),
      mval = factor(mval, levels = c(-1, 1), labels = c('end', 'str')),
      mval = as.character(mval)
      )

  # create unique labels for each continuous chunk 
  # upper-case is missing chunk, lower-case is complete chunk
  tosmp <- table(chk$mval)['str']
  chk[chk$mval %in% 'str', 'mval'] <- stri_rand_strings(tosmp, 5, pattern = '[A-Z]')
  chk[chk$mval %in% 'end', 'mval'] <- stri_rand_strings(tosmp, 5, pattern = '[a-z]')
  
  # repeat unique labels in each chunk
  chk <- mutate(chk,
    mval = na.locf(mval, na.rm = F),
    mval = ifelse(is.na(mval), stri_rand_strings(1, 5, pattern = '[a-z]'), mval),
    mval = forcats::as_factor(mval)
    )
  
  # length of each chunk
  gaps <- group_by(chk, mval) %>% 
    mutate(n = length(misi)) %>% 
    ungroup

  return(gaps)
  
}