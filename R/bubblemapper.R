# R/bubblemapper.R

#' Bubble world maps 2025 version
#' `bubblemapper()` prints a bubble world map based on the latest WHO geoJSON files
#'  It requires ggplot2, ggpattern, sf and here
#'
#' @param df A dataframe with two columns: 'iso3' (character or factor WHO country codes) and 'size' (numeric values to map as bubble sizes).
#' @param colours A vector of colour values for each category in "var", excepting missing values.
#' @param low_col First value of a gradient of colours.
#' @param high_col Last value of a gradient of colours.
#' @param line_col Colour of country border lines.
#' @param map_title Map title.
#' @param legend_title Legend title.
#' @param water_col Colour of oceans and lakes.
#' @param na_label Legend lable for missing values.
#' @param na_col Colour of countries with missing values.
#' @param disclaimer A boolean, inserts a standard WHO disclaimer.
#' @param legend_pos A vector of two numbers, positions the legend.
#' @return A ggplot2 plot.
#' @source Modified from WHO GIS (https://gis-who.hub.arcgis.com/)
#' @author Takuya Yamanaka, adapted from scripts of whomap developed by Philippe Glaziou.
#' @import ggplot2
#' @import scales
#' @import ggpattern
#' @import sf
#' @import dplyr
#' @examples
#' bubblemapper(data.frame(iso3 = NA, size = NA))
#' @export
bubblemapper <- function (X = data.frame(iso3 = NA, size = NA),
                       projection = "eck4",
                       offset = NULL,
                       bubble_col = 'dodgerblue',
                       bubble_alpha = 0.4,
                       scale_breaks,
                       scale_limits,
                       scale_labels,
                       line_col = 'black',
                       line_width = 0.3,
                       map_title = "",
                       legend_title = "",
                       water_col = 'white',
                       na_label = 'No data',
                       na_col = 'white',
                       disclaimer = FALSE,
                       legend_pos = c(0.17,0.42),
                       zoom = "Global"
)
{
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Data check and definition ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (!all(c("iso3", "size") %in% names(X)))
    stop("X must have two columns: 'iso3' and 'size'")
  
  X <- as.data.frame(X[!is.na(X$size) & X$size != "",])
  if (!is.numeric(X$size))
    X$size <- as.numeric(X$size)
  
  # leftjoin a dataset with the base world map
  data <- world |>
    dplyr::left_join(X, by = c("iso3"))
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Call map layers and WHO disclaimer from map_builder.R ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Call zoom setting for regional map
  zoom_info <- region_zoom(zoom)
  # if user didn't supply offset, use zoom default
  if (is.null(offset)) {
    offset <- zoom_info$offset
  }
  
  # Call map layers
  layers <- build_map_layers(data, projection, offset)
  # unpack a list
  list2env(layers, envir = environment())
  
  # Call WHO disclaimer
  if (disclaimer) {
    disclaim <- get_who_disclaimer()
  }
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # plotting an output ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # bubble points adjustment
  bubble_points <- data_trans |>
    # dplyr::filter(!is.na(size), !is.na(CENTER_LON), !is.na(CENTER_LAT)) |>
    dplyr::group_by(iso3) |>
    dplyr::mutate(row_number = dplyr::row_number()) |>
    dplyr::filter(
      (iso3 == "RUS" & row_number == 2) |
        (iso3 != "RUS" & row_number == 1) # due to offsetting, two bubbles will be produced for RUS. These lines are to remove a bubble around the east end of RUS.
    ) |>
    dplyr::select(-row_number) |>
    dplyr::ungroup() |>
    dplyr::mutate(geometry = sf::st_point_on_surface(geometry)) |>
    sf::st_as_sf()

  # plot the base world map with bubble layer using bubble_points
  p <- ggplot2::ggplot() + 
    ggplot2::geom_sf(data=data_trans,  col=line_col, fill = "white", linewidth = line_width) +
    ggplot2::geom_sf(data = bubble_points,
                        aes(
                            size = size
                          ),
                          shape = 21,
                          color = bubble_col,
                          fill = bubble_col,
                          alpha = bubble_alpha
                        ) +
    ggplot2::scale_size_area(
      name = legend_title,
      limits = scale_limits,
      breaks = scale_breaks,
      labels = scale_labels,
      max_size = 25
    ) +
    # legend
    ggplot2::guides(
      size = guide_legend(override.aes = list(color = NA, fill = bubble_col, alpha = bubble_alpha))  # remove outline in legend
    ) 
  
  #- - - - -plot colour trick start- - - - -#
  china_color <- "white"
  korea_color <- "white"
  sudan_color <- "white"
  palestine_color <- "white"
  #- - - - -plot colour trick end- - - - -#
  
  #- - calling common disputed border and theme settings from map_builder.R - -#
  p <- common_disputed_border(
    p = p,
    layers,
    map_title,
    disclaimer,
    legend_pos,
    line_col,
    line_width,
    water_col,
    china_color,
    korea_color,
    sudan_color,
    palestine_color,
    disclaim,
    zoom_info
  )
  
  return(p)
  
}


