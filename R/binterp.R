#' Bilinear interpolation function
#'
#' @param x input \code{data.frame} in wide format, first column is datetimestamp, column names are depth bins, z-values are to be interpolated
#' @param ny numeric for number of observations to expand in the y direction (depth)
#' @param gapsize numeric for maximum gap size beyond which chunks are not processed
#' @param new_grd expanded matrix of values to interpolate
#' 
#' @return The expanded \code{data.frame}
#'
#' @import akima stringi zoo
#' 
#' @export
binterp <- function(x, new_grd, ny = 15, gapsize = NULL) {

  set.seed(5)
  # break data by gaps 
  if(!is.null(gapsize)){
      
    # find gap sizes
    gaps <- gapsz(x) %>% 
      mutate(
        int = ifelse(misi & n >= gapsize, TRUE, FALSE),
        int = c(0, diff(int)), 
        int = ifelse(int == 0, NA, int),
        int = factor(int, levels = c(-1, 1), labels = c('end', 'str')),
        int = as.character(int)
      )
    
    # create unique labels for each continuous chunk 
    # upper-case is missing chunk, lower-case is complete chunk
    tosmp <- table(gaps$int)['str']
    gaps <- mutate(gaps, 
      int = ifelse(int %in% 'str', stri_rand_strings(tosmp, 5, pattern = '[A-Z]'), int),
      int = ifelse(int %in% 'end', stri_rand_strings(tosmp, 5, pattern = '[a-z]'), int),
      int = na.locf(int, na.rm = F),
      int = ifelse(is.na(int), stri_rand_strings(1, 5, pattern = '[a-z]'), int)
      ) %>% 
      select(datetimestamp, int)
    
    # combine with x, remove the gaps, add month, group by continuous and month
    x <- left_join(x, gaps, by = 'datetimestamp') %>% 
      filter(grepl('[a-z]', int)) %>% 
      split(.$int)
        
  }
  
  # chunk wise interpolation
  x <- lapply(x, function(chnk){
    
    cat(unique(chnk$int), '\n')
    
    # no missing values for interpolation
    chnk <- na.omit(chnk)
    
    # no extrap on nx
    nx <- length(unique(chnk$datetimestamp))

    # akima fails if values to interp vary in scale
    browser()
    # interpolate
    newvals <- interp(
      x = chnk$datetimestamp,
      # y = scale(chnk$depth, # try rescaling this in range of x,
      z = chnk$val, 
      nx = nx, ny = ny
    )
    
    # new z vals are in matrix, format as vector
    val <- t(newvals$z) %>%
      as.numeric

    # format output
    out <- expand.grid(newvals$y, newvals$x) %>%
      rename(
        datetimestamp = Var2,
        depth = Var1
      ) %>%
      mutate(
        datetimestamp = as.POSIXct(datetimestamp, origin = '1970-01-01', tz = attr(x$datetimestamp, 'tzone')),
        val = val
        ) %>%
      select(datetimestamp, everything())

    return(out)

    })

  stop('recombine the chunks dumbass')
  
  return(x)
  
}