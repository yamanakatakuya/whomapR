# R/add_marker.R

#' `add_marker()` is used in combination with `whomapper()` and adds a secondary
#' marker located at the centroid of listed countries and territories.
#'
#' @param iso3 A vector of ISO3 country codes.
#' @param shape Marker shape.
#' @param col Marker colour.
#' @param size Marker size.
#' @param lab Marker label.
#' @return A ggplot2 function.
#' @author Takuya Yamanaka, adapted from scripts of whomap developed by Philippe Glaziou.
#' @import ggplot2
#' @export
#' @examples
#' brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'), var=1:5)
#' whomap(brics, legend.title='BRICS', legend.pos=c(0.14, 0.34)) +
#'    add_marker(c('BRA','RUS'), lab='Subnational\ndata')
#'
add_marker <- function(iso3 = NA_character_,
                       shape = 17,
                       col = 'red',
                       size = 3,
                       alpha = 1,
                       projection = "robin",
                       offset = 10.8) {
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Warning for invalid iso3 ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  
  if (!all(iso3 %in% world$iso3))
    stop("iso3 should only include valid ISO3 country codes")
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Renaming datasets ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  iso3_list <- iso3
  data <- world
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Call map layers and WHO disclaimer from map_builder.R ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Call map layers
  layers <- build_map_layers(data, projection, offset)
  # unpack a list
  list2env(layers, envir = environment())
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # plotting an output ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # marker points adjustment
  marker_points <- data_trans |>
    dplyr::filter(!is.na(CENTER_LON), !is.na(CENTER_LAT)) |>
    dplyr::group_by(iso3) |>
    dplyr::mutate(row_number = dplyr::row_number()) |>
    dplyr::filter(
      (iso3 == "RUS" & row_number == 2) |
        (iso3 != "RUS" & row_number == 1)
    ) |>
    dplyr::select(-row_number) |>
    dplyr::ungroup() |>
    dplyr::mutate(geometry = sf::st_point_on_surface(geometry)) |>
    sf::st_as_sf() |>
    filter(iso3 %in% iso3_list)
  
  # Plotting marker points
  list(
    ggplot2::geom_sf(data = marker_points,
                     size = size,
                     shape = shape,
                     color = col,
                     fill = col,
                     alpha = alpha
    )
  )
  
}