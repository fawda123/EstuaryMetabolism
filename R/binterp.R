#' Bilinear interpolation function with missing chunk preservation
#'
#' @param x input \code{data.frame} in wide format, first column is datetimestamp, column names are depth bins, z-values are to be interpolated
#' @param ny numeric for number of observations to expand in the y direction (depth)
#' @param gapsize numeric for maximum gap size beyond which chunks are not processed
#' 
#' @details This function expands a variable on the depth axis using \code{\link[akima]{interp}}.  A two-dimensional interpolation of time and depth is used to expand the data on the depth axis.  This is also useful to interpolate values in time when depth data are insufficient to interpolate vertically.  The depth bins are expanded to the divisions provided by \code{ny} and the original time step is preserved in the output data.  
#' 
#' The \code{gapsize} argument specifies the number of observations in the native time step beyond which values are not interpolated.  For example, a gapsize of ten will interpolate all values in the dataset except those separated by ten or more time steps.  This is useful because interpolation errors increase with size of the missing chunk.  A gap is considered one or more continuous time steps where all values are missing across the enire depth profile.  The data are interpolated in separate chunks, where the chunks are separated by missing values defined by \code{gapsize}. 
#' 
#' @return The expanded \code{data.frame}
#'
#' @import akima stringi zoo
#' 
#' @export
binterp <- function(x, ny = 15, gapsize = 1) {

  # find gap sizes
  # id strt/end of continuous chunks, which may include gaps if less than gapsize
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
  
  # do not label separately if no missing values
  if(is.na(tosmp)){
    
    gaps$int <- stri_rand_strings(1, 5, pattern = '[a-z]')
    
  # otherwise labels are separate
  } else {
    
    # unique upper/lower chr strings for chunk labels
    gaps[gaps$int %in% 'str', 'int'] <- stri_rand_strings(tosmp, 5, pattern = '[A-Z]')
    gaps[gaps$int %in% 'end', 'int'] <- stri_rand_strings(tosmp, 5, pattern = '[a-z]')
    
    # repeat unique labels in each chunk
    gaps <- mutate(gaps, 
      int = na.locf(int, na.rm = F),
      int = ifelse(is.na(int), stri_rand_strings(1, 5, pattern = '[a-z]'), int)
      )
    
  }
  
  # chunk label as factor
  gaps <- mutate(gaps, 
    int = forcats::as_factor(int)
    ) %>% 
    select(datetimestamp, int)

  # combine with x, remove the gaps, split by chunk
  tointerp <- left_join(x, gaps, by = 'datetimestamp') %>% 
    filter(grepl('[a-z]', int)) %>% 
    mutate(
      int = as.character(int),
      int = forcats::as_factor(int)
      ) %>% 
    split(.$int)
        
  # chunk wise interpolation
  chnks <- lapply(tointerp, function(chnk){
    
    cat('Chunk', unique(chnk$int), '\n')
    
    # no missing values for interpolation
    chnk <- na.omit(chnk)
    
    # no extrap on nx
    nx <- length(unique(chnk$datetimestamp))

    # akima fails if values to interp vary in scale
    # rescale values similarly
    xin <- chnk$datetimestamp %>% 
      as.numeric
    yin <- chnk$depth %>% 
      scales::rescale(., to = range(xin))

    # interpolate
    newvals <- interp(
      x = xin,
      y = yin,
      z = chnk$val, 
      nx = nx, ny = ny,
      linear = FALSE
    )
    
    # new z vals are in matrix, format as vector
    val <- newvals$z %>%
      as.numeric
    
    # depth vals, do not back-transform in function
    depth <- seq(min(chnk$depth), max(chnk$depth), length = ny)

    # format output for expanded values
    chnkout <- expand.grid(newvals$x, depth) %>%
      rename(
        datetimestamp = Var1,
        depth = Var2
      ) %>%
      mutate(
        datetimestamp = as.POSIXct(datetimestamp, origin = '1970-01-01', 
          tz = attr(x$datetimestamp, 'tzone')),
        val = val
        ) %>%
      select(datetimestamp, everything())

    return(chnkout)

    })

  # recombine the chunks
  out <- do.call('rbind', chnks) %>% 
    tibble::remove_rownames(.) %>% 
    arrange(datetimestamp, depth)
 
  return(out)
  
}