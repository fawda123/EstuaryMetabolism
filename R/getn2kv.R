#' Get Brunt-Vaisala freq (N2), vertical turbulent diffusivity (Kv)
#'
#' @param dat input data.frame pre-processed with \code{\link{getwithin}}
#' @param g gravity constant, m2/s
#'
#' @details Brunt-Vaisala frequency and the vertical turbulent diffusivity are appended for each row where estimable in \code{dat}. Estimates are based on equations 3 and 4 in Staehr et al. 2012
#'
#' @return Input data frame with two additional columns for \code{n2} (s-2) and \code{kv} (m2 hr-1)
#' 
#' @export
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
      
      ni2 <- NA
      
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

  
  