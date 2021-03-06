######
#' Water quality and weather data from Mobile Bay, Middle Bay Lighthouse station
#'
#' One year of hourly observations from the Mobile Bay, Middle Bay Lighthouse station were pre-processed for use with this package.  The data include combined water quality and weather data from 2015. The water quality data were taken at approximate 0.5 m depth intervals from the surface (0.5 shallow, 3 deep) and the weather data are repeated for each unique time step across depth bins.  
#'  
#' Note that all cases in the data are complete. That is, all time steps have row entries for all even if the actual observations are missing. 
#' 
#' @format A \code{\link[dplyr]{tibble}} obejct with 52560 observations and 8 variables:
#' \describe{
#'   \item{\code{datetimestamp}}{POSIXct, hourly time step of CTD casts}
#'   \item{\code{depth}}{num depth bin of the CTD cast, m}
#'   \item{\code{do_mgl}}{num dissolved oxygen, mg L-1}
#'   \item{\code{sal}}{num salinity, psu}
#'   \item{\code{temp}}{num, water temperature, C}
#'   \item{\code{atemp}}{num, air temperature, C}
#'   \item{\code{bp}}{num, barometric pressure, mb}
#'   \item{\code{wspd}}{num, wind speed, m s-1}
#' }
#' 
#' @source \url{http://www.mymobilebay.com/stationdata/stationinfomiddlebay.asp?stationid=188}
#'
#' @examples 
#' data(mblight)
"mblight"