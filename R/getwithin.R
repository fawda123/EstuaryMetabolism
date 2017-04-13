######
#' Get relevant parameters for integrated metabolism estimates within a single depth bin
#'
#' @param dat data frame for one depth bin
#' @param binmid numeric of midpoint depth of bin
#' @param binwid numeric of bin width
#' 
#' @details Input data must have seven columns with \code{datetimestamp}, dissolved oxygen (\code{do_mgl}, mg L-1), salinity (\code{sal}, psu), water temprature (\code{temp}, C), air temperature (\code{atemp}, C), barometric pressur (\code{bp}, mb), and wind speed (\code{wspd}, m s-1). The \code{binmid} and \code{binwid} arguments are for the midpoint and width of the depth bin for the corresponding data (m). 
#' 
#' The input data are first transposed to estimate the midpoint values of all observations within each depth bin.  That is, an hour time step on the hour will be returned as an hourly time step every half hour minus one observation.  
#' 
#' The function continues to estimate values for delta DO, DO at saturation, the oxygen mass transfer coefficient, the volumetric reaeration coefficient, and water density.
#' 
#' @return The input data minus one observation (times the number of depth bins) with additional columns for delta DO (\code{ddo}, mmol o2 m-3 hr-1), DO at saturation (\code{dosat}, mmol o2), the mass transfer coefficient (\code{kl}, m d-1), the volumetric reaeration coefficient (\code{ka}, h-1), and water density (\code{sig}, set for 10 decibars, output kg m-3).
#' 
#' @export
#' 
#' @import oce SWMPr
#' 
getwithin <- function(dat, binmid, binwid){

  # do from mg/l to mmol/m3
  # get do at saturation, temp as C, salinity as psu
  dat <- mutate(dat, 
    do = do_mgl / 32 * 1000
    ) %>% 
    select(-do_mgl)
  
  # get change in do per hour, as mmol m^-3 hr^-1
  # difference between time steps, divided by time (in mins) between time steps, multiplied by 60
  ddo <- with(dat, diff(do) / as.double(diff(datetimestamp), units = 'hours'))
  
  # take diff of each column, divide by 2, add original value
  # this gets interpolated values at midpoint between each time step
  # done to match with ddo
  datetimestamp <- diff(dat$datetimestamp)/2 + dat$datetimestamp[-c(nrow(dat))]
  dat <- apply(
    dat[,2:ncol(dat), drop = FALSE],
    2,
    function(x) diff(x)/2 + x[1:(length(x) -1)]
    )
  dat <- data.frame(datetimestamp, dat, ddo = ddo)
  
  # get do saturation, volumetric reaeration coefficient, Ds
  dat <- dat %>% 
    mutate(
      dosat = oxySol(temp, sal, bp / 1013.25), # bp in atm (mb / 1013.25)
      dosat = dosat / 32 * 1000,
      kl = calckl(temp, sal, atemp, wspd, bp), # m d-1
      ka = kl / 24 / binwid, # binwid is specific to the bin
      sig = swRho(sal, temp, 10) # set for 10 decibars, output kg m-3
    )
  
  return(dat)

}
