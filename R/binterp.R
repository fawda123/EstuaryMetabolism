#' Bilinear interpolation function
#'
#' @param x input \code{data.frame} in wide format, first column is datetimestamp, column names are depth bins, z-values are to be interpolated
#' @param new_grd expanded matrix of values to interpolate
#' 
#' @return The expanded \code{data.frame}
#'
#' @import fields
#' 
#' @export
binterp <- function(x, new_grd) {

  # get interped values
  int_val <- interp.surface(
    obj = list(
      x = as.numeric(unique(new_grd$datetimestamp)),
      y = as.numeric(names(x)[-1]),
      z = as.matrix(x[,-1])
      ),
    loc = new_grd
    )

  # combine coords with interp values
  out <- data.frame(new_grd, val = int_val)
  
  return(out)
  
}