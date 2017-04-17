#' Plot observations and flux estimates for one step
#'
#' @param dat Processed hourly metabolic data
#' @param obs chr string of datetimestamp to plot
#' @param widths numeric for widths to assign for each plot, passed to \code{\link[gridExtra]{grid.arrange}}
#' @param colpal chr string of color palette for flux plots
#'
#' @details Dissolved oxygen, salinity, temperature, water density, change in do per hour, surface O2 flux, eddy diffusivity flux, and deepening of mixed layer flux are plotted. 
#'  
#' @export
#'
#' @importFrom gridExtra grid.arrange
#' 
#' @return Three ggplot objects combined with \code{\link[gridExtra]{grid.arrange}}.
#' 
hrplot <- function(dat, obs, widths = c(0.7, 0.2, 1), colpal = 'Spectral'){

  # time zone and observation to plot
  tzone <- attr(datest$datetimestamp, 'tzone')
  obs <- as.POSIXct(obs, format = '%Y-%m-%d %H:%M', tz = tzone)
  
  # filter data by observation
  flxplo <- filter(datest, datetimestamp %in% obs)
  
  # default theme
  thm <- ggplot2::theme_bw() + 
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 7),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
      )
  
  ##
  # observed data, do, sal, temp
  
  toplo1 <- select(flxplo, binmd, do, sal, temp) %>% 
    gather('var', 'val', do:temp) %>% 
    mutate(
      Depth = factor(round(binmd, 2)),
      Depth = forcats::fct_rev(Depth)
      )
  
  p1 <- ggplot2::ggplot(toplo2, ggplot2::aes(x = Depth, y = val, group = var)) + 
    ggplot2::geom_point(size = 3) + 
    ggplot2::geom_path() +
    ggplot2::coord_flip() + 
    ggplot2::facet_wrap(~var, ncol = 3, scales = 'free_x') + 
    thm + 
    ggplot2::scale_y_continuous('Depth (m)')

  ##
  # estimate of mixing layer depth
  
  p2 <- findstk(flxplo, plot = T) + 
    thm + 
    theme(axis.title.y = ggplot2::element_blank())
  
  ##
  # O2 flux plots 

  toplo3 <- select(flxplo, binmd, ddo, dv, dz, ds) %>% 
    gather('var', 'val', ddo:ds) %>% 
    mutate(
      Depth = factor(round(binmd, 2)),
      Depth = forcats::fct_rev(Depth)
      )
  
  p3 <- ggplot2::ggplot(toplo3, ggplot2::aes(x = Depth, y = val, fill = val)) + 
    ggplot2::geom_bar(stat = 'identity', col = 'black') + 
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::coord_flip() + 
    ggplot2::facet_wrap(~var, ncol = 4, scales = 'free_x') + 
    thm +
    theme(axis.title.y = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous('mmol O2 m-3 hr-1') + 
    ggplot2::scale_fill_distiller(palette = colpal)
  
  # combine all three
  grid.arrange(p1, p2, p3, ncol = 3, widths = widths, top = grid::textGrob(obs))

}
  