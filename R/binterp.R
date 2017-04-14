#' Bilinear interpolation function
#'
#' @param x input \code{data.frame} in wide format, first column is datetimestamp, column names are depth bins, z-values are to be interpolated
#' @param new_grd expanded matrix of values to interpolate
#' 
#' @return The expanded \code{data.frame}
#'
#' @import akima
#' 
#' @export
binterp <- function(x, new_grd, ny = 15, nx = NULL, gapsize = NULL) {

  # get gaps larger than gapsize
  gaps <- gapsz(x)

  # no missing values in x
  # add mo/yr column for separate interpolation
  x <- na.omit(x) %>% 
    mutate(
      moyr = as.Date(datetimestamp, tz = attr(x$datetimestamp, 'tzone')),
      moyr = as.character(moyr),
      moyr = gsub('-[0-9]*$', '', moyr)
    ) %>% 
    split(.$moyr) %>% 
    lapply(., function(moyr){

      
      # no extrap on nx if not provided
      if(is.null(nx))
        nx <- length(unique(moyr$datetimestamp))

      # interpolate
      newvals <- interp(
        x = moyr$datetimestamp, 
        y = moyr$depth,
        z = moyr$val, 
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
  
      })
  browser()
  # reinsert gaps
  if(!is.null(gapsize)){
    
    gaps <- filter(gaps, n >= gapsize)
    
    browser()  
  }
  
  return(out)
  
}