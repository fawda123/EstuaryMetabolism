#' Get DO flux between layers from eddy diffusivity (Dv)
#'
#' @param dat input data.frame pre-processed with \code{\link{getwithin}} and \code{\link{getn2kv}}
#'
#' @details DO flux between depth layers is estimated using eqn 8 in Staehr et al. 2012. Negative values are fluxes in and positive values are fluxes out of a depth layer.
#' 
#' @return Input data frame with one additional column for \code{dv} (mmol O2 m-3 hr-1)
#'   
#' @export
#' 
#' @import dplyr tibble
getdv <- function(dat){
  
  # sanity check
  if(!'kv' %in% names(dat))
    stop('kv not in data')
  
  # split by bin
  dat <- split(dat, dat$binmd)
  bins <- names(dat)
    
  # get eddy diffusion in a layer
  for(bin in seq_along(bins)){
    
    # get current bin kv, do, height
    bini <- dat[[bin]]
    doi <- select(bini, do)
    kvi <- select(bini, kv)
    h <- select(bini, binwd) %>% 
      unique %>% 
      as.numeric
  
    # dv change i - 1
    if(bin == 1){ 
      
      dvup <- 0
      
    } else {
    
      # do up
      binup <- dat[[bin - 1]]
      doup <- select(binup, do)
      
      # dv up
      dvup <- kvi * (doi - doup)
      
    }
     
    # dv change i + 1
    if(bin == length(bins)){
      
      dvdn <- 0
      
    } else {
      
      # do, kv down
      bindn <- dat[[bin + 1]]
      dodn <- select(bindn, do)
      kvdn <- select(bindn, kv)
      
      # dv down
      dvdn <- kvdn * (doi - dodn)
      
    }
    
    # combined dv, mmol 02 m3 hr-1
    # h is m, so square it to get mmol m3 hr-1
    # dvup and dvdn are mmol m-1 hr-1
    dv <- (dvup + dvdn) / (h ^ 2)
    names(dv) <- 'dv'
    
    # append to output for bin
    dat[[bin]] <- data.frame(bini, dv)
      
  }
  
  # format output
  out <- do.call('rbind', dat) %>% 
    remove_rownames %>% 
    arrange(datetimestamp, binmd)
  
}
  