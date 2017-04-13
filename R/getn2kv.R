#' Get Brunt-Vaisala frequency (N2), vertical turbulent diffusivity (Kv)
#'
#' @param dat input data.frame pre-processed with \code{\link{getwithin}}
#' @param g gravity constant, m2 s-1
#'
#' @details Brunt-Vaisala frequency and the vertical turbulent diffusivity are appended for each row where estimable in \code{dat}. Estimates are based on equations 3 and 4 in Staehr et al. 2012.  Estimated values for each bin are in reference to the current bin and the bin immediately below.  As such, values are not estimated for the deepest depth bin.
#'
#' @return Input data frame with two additional columns for \code{n2} (s-2) and \code{kv} (m2 hr-1)
#' 
#' @export
#' 
#' @references
#' 
#' Staehr, P.A., Christensen, J.P.A., Batt, R.D., Read, J.S. 2012. Ecosystem metabolism in a stratified lake. 57(5):1317-1330.
#' 
#' @import dplyr tibble
getn2kv <- function(dat, g = 9.7963){
  
  # sanity check
  if(!'sig' %in% names(dat))
    stop('density sig not in data')
  
  # split by bin
  dat<- split(dat, dat$binmd)
  bins <- names(dat)
  
  # get brunt-vaisala buoyancy frequencies between layers
  for(bin in seq_along(bins)){
    
    # get current bin sw density
    isig <- dat[[bin]] %>% 
      select(sig)
  
    # get current bin midpoint
    binimd <- names(dat)[bin] %>% 
      as.numeric
    
    # get bin one layer down
    if(bin == length(bins)){
      
      n2 <- NA
      
    } else {
      
      # density one bin down
      dnsig <- dat[[bin + 1]] %>% 
        select(sig)
      
      # midpoint one bin down
      bindnmd <- names(dat)[bin + 1] %>% 
        as.numeric
      
      # brunt valsala freq, s-2
      n2 <- -1 * (g / isig) * ((isig - dnsig) / (bindnmd - binimd))
      
    }
    
    # vertical turbulent diffusivity
    kv <- 2.941e-3 * (1e-6 ^ 0.56) * (n2 ^ -0.43)
  
    # append to output for bin
    ests <- data.frame(cbind(n2, kv))
    names(ests) <- c('n2', 'kv')
    dat[[bin]] <- data.frame(dat[[bin]], ests)
      
  }
  
  # format output
  out <- do.call('rbind', dat) %>% 
    remove_rownames %>% 
    arrange(datetimestamp, binmd)
  
  return(out)
  
}

  
  