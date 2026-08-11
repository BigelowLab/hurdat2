#' Build a map of the hurricane paths colored by 50 epoch
#' 
#' @export
#' @param x HURDAT sf object
#' @param coast sf geometry of the coast
#' @return ggplot2 object
map_hurdat = function(x = read_hurdat(form = "linestring"),
                      coast = read_coast()){
  
  if (FALSE){
    library(hurdat2)
    library(ggplot2)
    library(colorspace)
    library(ggokabeito)
    x = read_hurdat(form = "linestring")
    coast = read_coast()
  }

  code_epoch = function(y = c(1851, 1922, 1877, 2025)){
    s = seq(from = 1850, to = 2050, by = 50)
    s2 = seq(from = 1900, to = 2100, by = 50)
    ix = findInterval(y, s)
    paste(s, s2, sep = "-")[ix]
  }
  
  x = x |>
    dplyr::mutate(epoch = format(.data$start, "%Y") |>
                    as.numeric() |>
                    code_epoch() )
  
  ggplot2::ggplot() + 
    ggplot2::geom_sf(data = x,
                     #mapping = ggplot2::aes(color = .data$epoch),
                     alpha = 0.1) + 
    #scale_fill_discrete_qualitative(palette = "Dark 3") + 
    #scale_fill_okabe_ito(aesthetics = "fill", order = 1:9) + 
    ggplot2::geom_sf(data = coast, color = "orange") + 
    ggplot2::coord_sf(xlim = c(-130, 63),
                      ylim = c(6, 84)) + 
    labs(x = NULL, y = NULL, title = "HURDAT2") +
    facet_wrap(~epoch)
}