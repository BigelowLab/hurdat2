#' Convert POINT collection to LINESTRING by storm
#' 
#' @export
#' @param x hurdat POINT data for one or more storms
#' @return LINESTRING object, one per storm
as_storms = function(x = read_hurdat()){
  
  x |> 
    dplyr::group_by(.data$id) |>
    dplyr::group_map(
      function(grp, key){
        n = nrow(grp)
        
        s = sf::st_drop_geometry(grp) |>
          dplyr::summarise(dplyr::across(where(is.numeric), 
                                         \(x) median(x, rm.na = TRUE)))
          
        z = sf::st_combine(grp) |>
          sf::st_cast("LINESTRING") |>
          sf::st_as_sf() |>
          sf::st_set_geometry("geom") |>
          dplyr::mutate(id = grp$id[1],
                        name = grp$name[1],
                        start = grp$datetime[1],
                        end = grp$datetime[n], 
                        duration = (as.Date(end) - as.Date(start)) |> 
                          as.numeric() + 1,
                        n = n,
                        .before = 1) |>
          dplyr::bind_cols(s)
      }, .keep = TRUE) |>
    dplyr::bind_rows()
}

# Private function to build and save the storm data
make_storms = function(){
  r = as_storms() |>
    saveRDS(file = "inst/extdata/storms.rds")
  r
}