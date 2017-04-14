#' Identify gap sizes in time series of depth profiles
#'
#' @param x \code{data.frame} of profile time series in long format
#'
#' @return A \code{data.frame} with three columns for start and end times and gap size.
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
  chk <- mutate(chk, 
    mval = ifelse(mval %in% 'str', stri_rand_strings(tosmp, 5, pattern = '[A-Z]'), mval),
    mval = ifelse(mval %in% 'end', stri_rand_strings(tosmp, 5, pattern = '[a-z]'), mval),
    mval = na.locf(mval, na.rm = F),
    mval = ifelse(is.na(mval), stri_rand_strings(1, 5, pattern = '[a-z]'), mval)
    )
  
  # length of each chunk
  gaps <- group_by(chk, mval) %>% 
    mutate(n = length(misi)) %>% 
    ungroup
  
  return(gaps)
  
}