# Use this to generate the rnaturalearth coastline

library(hurdat2)
library(sf)
library(rnaturalearth)

x = read_hurdat()
y = st_geometry(x)
coast = ne_coastline(scale = "medium", returnclass = "sf") |>
  st_geometry() |>
  write_sf("inst/extdata/coastline.gpkg")
