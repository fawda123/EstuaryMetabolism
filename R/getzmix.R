#' Estimate depth of mixing layer 
#'
#' @param dat \code{data.frame} of profile time series
#' @param parallel logical if data are processed in parallel
#' @param ... other arguments passed to \code{\link{findstk}}
#' 
#' @details This function applies \code{\link{findstk}} and \code{\link{stkmod}} to all profiles in the dataset for convenience.
#' 
#' @return The input dataset with an additional column for \code{zmix}
#' 
#' @import dplyr parallel tidyr
#' 
#' @export
getzmix <- function(dat, parallel = TRUE, ...) {

  # sanity check
  if(!'sig' %in% names(dat))
    stop('sig not present in input data')

  # tz
  tz <- attr(dat$datetimestamp, 'tzone')
  
  # split dataset by time stamp
  splts <- split(dat, datest$datetimestamp)
  
  # process by lapply if not parallel
  if(!parallel){
  
    ests <- lapply(splts, findstk, ...)
    
  # otherwise parallel
  } else {
    
    nc <- detectCores() - 1
    cl <- makeCluster(nc)
    clusterEvalQ(cl, library(dplyr))
    clusterExport(cl, c('findstk', 'stkmod'))
    ests <- parLapply(cl, splts, findstk, ...)
    stopCluster(cl)
        
  }
  
  # format estimates of zmix for combining with dat
  ests <- do.call('rbind', ests) %>% 
    data.frame(zmix = .) %>% 
    rownames_to_column %>% 
    mutate(rowname = as.POSIXct(rowname, format = '%Y-%m-%d %H:%M:%S', tz = tz)) %>% 
    rename(datetimestamp = rowname)
  
  # join with input data, return
  dat <- left_join(dat, ests, by = 'datetimestamp')
  return(dat)

}