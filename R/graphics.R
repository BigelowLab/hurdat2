#' Build a map of the hurricane paths colored by 50 epoch
#' 
#' @export
#' @param x HURDAT sf object
#' @param epoch num, the number of consecutive years to group by
#' @param coast sf geometry of the coast
#' @return ggplot2 object
map_hurdat = function(x = read_hurdat(form = "storms"),
                      epoch = 50,
                      coast = read_coast() |>sf::st_crop(x)){
  
  if (FALSE){
    library(hurdat2)
    library(ggplot2)
    library(colorspace)
    library(ggokabeito)
    x = read_hurdat(form = "linestring")
    coast = read_coast()
    epoch = 50
  }

  code_epoch = function(y = c(1851, 1922, 1877, 2025),
                        epoch = c(10, 25, 50, 100)[50]){
    s = seq(from = 1850, to = 2050, by = epoch)
    s2 = seq(from = 1850 + epoch - 1, to = 2050 + epoch - 1, by = epoch)
    ix = findInterval(y, s)
    paste(s, s2, sep = "-")[ix]
  }
  
  x = x |>
    dplyr::mutate(epoch = format(.data$start, "%Y") |>
                    as.numeric() |>
                    code_epoch(epoch = epoch) )
  
  ggplot2::ggplot() + 
    ggplot2::geom_sf(data = x,
                     mapping = ggplot2::aes(color = .data$wind_max_sus),
                     alpha = 1) + 
    colorspace::scale_color_continuous_sequential(palette = "Purples 3") + 
    ggplot2::geom_sf(data = coast, color = "orange") + 
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank()) + 
    #ggplot2::coord_sf(xlim = c(-130, 63),
    #                  ylim = c(6, 84)) + 
    ggplot2::labs(x = NULL, y = NULL, title = "HURDAT2") +
    ggplot2::facet_wrap(~epoch)
}