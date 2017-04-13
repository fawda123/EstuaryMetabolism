#' Get DO flux between layers from air-sea gas exchange, (Ds), eddy diffusivity (Dv) and mixed-layer deepening (Dz)
#'
#' @param dat input data.frame pre-processed with \code{\link{getwithin}}, \code{\link{getn2kv}}, and \code{\link{getzmix}}
#'
#' @details DO flux between depth layers from air-sea gas exchange is esimated from eqn 9, eddy diffusivity is estimated using eqn 8, and DO flux from mixed-layer deepening is estimated from eqn 10 in in Staehr et al. 2012. Estimates of \code{ds} are only applied if the depth layer is shallower than the mixing depth.
#' 
#' @return Input data frame with three additional columns for \code{ds}, \code{dv}, and \code{dz} as DO flux from air-sea gas exchange, flux between layers from eddy diffusivity, and flux from mixed-layer deepening (mmol O2 m-3 hr-1 for all)
#'   
#' @export
#' 
#' @import dplyr tibble
getdsdvdz <- function(dat){
  
  # sanity check
  if(!'kv' %in% names(dat))
    stop('kv not in data, use getn2kv function')
  
  if(!'dzmix' %in% names(dat))
    stop('dzmix not in data, use getzmix function')
  
  # split by bin
  dat <- split(dat, dat$binmd)
  bins <- names(dat)
    
  # get eddy diffusion in a layer
  for(bin in seq_along(bins)){
    
    # get current bin kv, do, height
    bini <- dat[[bin]]
    doi <- select(bini, do)
    kvi <- select(bini, kv)
    dzmixi <- select(bini, dzmix)
    h <- select(bini, binwd) %>% 
      unique %>% 
      as.numeric
  
    # dv change i - 1
    if(bin == 1){ 
      
      # dv one layer up
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
      
      # dv one layer down
      dvdn <- 0
      
      # dz
      dz <- 0
      
    } else {
      
      # do, kv down
      bindn <- dat[[bin + 1]]
      dodn <- select(bindn, do)
      kvdn <- select(bindn, kv)
      
      # dv down
      dvdn <- kvdn * (doi - dodn)
      
      # dz, mmol o2 m-3 hr-1 (do not square h)
      dz <- dzmixi * (doi - dodn) / h
        
    }
    
    # combined dv, mmol 02 m3 hr-1
    # h is m, so square it to get mmol m3 hr-1
    # dvup and dvdn are mmol m-1 hr-1
    dv <- (dvup + dvdn) / (h ^ 2)
    names(dv) <- 'dv'
    names(dz) <- 'dz'
    
    # append to output for bin
    dat[[bin]] <- data.frame(bini, dv, dz)
      
  }
  
  # format output, get ds
  # from Thiebault mmol o2 m-3 hr-1
  # ds only applies if bin is shallower than mixing depth
  out <- do.call('rbind', dat) %>% 
    remove_rownames %>% 
    arrange(datetimestamp, binmd) %>% 
    mutate(
      ds = ka * (dosat - do), 
      ds = ifelse(binmd <= zmix, ds, NA) 
    )
  
  return(out)
  
}
  