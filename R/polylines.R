#' Convert POINT collection to LINESTRING by storm
#' 
#' @export
#' @param x hurdat POINT data for one or more storms
#' @return LINESTRING object, one per storm
as_linestring = function(x = read_hurdat()){
  
  x |> 
    dplyr::group_by(.data$id) |>
    dplyr::group_map(
      function(grp, key){
        n = nrow(grp)
        z = sf::st_combine(grp) |>
          sf::st_cast("LINESTRING") |>
          sf::st_as_sf() |>
          sf::st_set_geometry("geom") |>
          dplyr::mutate(id = grp$id[1],
                        name = grp$name[1],
                        start = grp$datetime[1],
                        end = grp$datetime[n],
                        wind_max_sus = median(grp$wind_max_sus, na.rm = TRUE),
                        min_pres = median(grp$min_pres,na.rm = TRUE),
                        .before = 1)
          
      }, .keep = TRUE) |>
    dplyr::bind_rows()
}