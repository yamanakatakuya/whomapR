# R/whomapper.R

#' Choropleth world maps 2025 version
#' `whomapper()` prints a choropleth world map based on the latest WHO geoJSON files
#'  It requires ggplot2, ggpattern, sf and here
#'
#' with standard WHO ISO3 country codes.The categorical variable to be
#' mapped should be named "var" (see examples).
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
#' @param X A dataframe with two columns: 'iso3' (character or factor WHO country codes) and 'var' (categorical variable to map).
#' @source Modified from WHO GIS (https://gis-who.hub.arcgis.com/)
#' @author Takuya Yamanaka, adapted from scripts of whomap developed by Philippe Glaziou.
#' @import ggplot2
#' @import scales
#' @import ggpattern
#' @import sf
#' @import dplyr
#' @examples
#' whomapper(data.frame(iso3 = NA, var = NA))
#' @export 

whomapper <- function (X = data.frame(iso3 = NA, var = NA),
                    colours = NULL,
                    projection = "robin",
                    offset = 10.8,
                    low_col = '#BDD7E7',
                    high_col = '#08519C',
                    line_col = 'black',
                    line_width = 0.3,
                    map_title = "",
                    legend_title = "",
                    water_col = 'white',
                    na_label = 'No data',
                    na_col = 'white',
                    disclaimer = FALSE,
                    legend_pos = c(0.17,0.42)
)
{
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Data check and definition ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # required data frame
  if (is.data.frame(X) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "var") %in% names(X)) == FALSE)
    stop("X must have two variables named 'iso3' and 'var'")
  
  X <- as.data.frame(X[!is.na(X$var) & X$var != "",])
  if (is.factor(X$var) &
      !is.na(match('', levels(X$var))))
    X <- droplevels(X[!grepl("^\\s*$", X$var), , drop = FALSE])
  if (!is.factor(X$var))
    X$var <- as.factor(X$var)
  
  # colour definition ---
  if (is.null(colours)) {
    xc <- seq(0, 1, length = length(levels(data[["var"]])))
    col <- scales::seq_gradient_pal(low_col, high_col)(xc)
  } else {
    col <- colours
  }
  
  col2 <- c(col, na_col, 'grey60')
  
  # leftjoin a dataset with the base world map
  data <- world |>
  dplyr::left_join(X, by = c("iso3"))
  
  # --- Sync SJM with NOR, and GUF with FRA ---
  # Get values from 'var' for NOR and FRA
  norway_value <- data$var[data$iso3 == "NOR"]
  france_value <- data$var[data$iso3 == "FRA"]
  # Assign to SJM and GUF
  data$var[data$iso3 == "SJM"] <- norway_value
  data$var[data$iso3 == "GUF"] <- france_value
  
  # Ensure var is a factor with explicit NA
  data$var <- forcats::fct_na_value_to_level(data$var, level = na_label)
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Call map layers and WHO disclaimer from map_builder.R ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
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
  # Create one dummy sf object for both legend entries ---
  dummy_legend <- sf::st_sf(
    var = factor(
      c(na_label, "Not applicable"),
      levels = c(levels(data$var),  "Not applicable")
    ),
    geometry = sf::st_sfc(st_geometrycollection(), st_geometrycollection()),
    crs = sf::st_crs(data)
  )
  
  # plot the base world map
  p <- ggplot2::ggplot() + 
    ggplot2::geom_sf(data=data_trans,  col=line_col, aes(fill = var), linewidth = line_width) +
    # Dummy data for legend entries
    ggplot2::geom_sf(data = dummy_legend, aes(fill = var), show.legend = TRUE) +
    # legend
    ggplot2::scale_fill_manual(legend_title, values = col2) +
    ggplot2::guides(
    fill = guide_legend(override.aes = list(color = NA))  # remove outline in legend
    ) 

  #- - - - -plot colour trick start- - - - -#

    # Aksai Chin colour trick
  # 1. Check China's value
  china_status <- data$var[data$iso3 == "CHN"]
  
  if (!is.na(china_status)) {
    # 2. Assign names to color vector
    names(col2) <- levels(data$var)
    # 3. Get the color applied to China
    china_color <- col2[as.character(china_status)]
  } else {
    china_color <- na_col}
  
  # Korean DMZ colour trick
  # 1. Check Korea's value
  korea_status <- data$var[data$iso3 == "KOR"]
  
  if (!is.na(korea_status)) {
    # 2. Assign names to color vector
    names(col2) <- levels(data$var)
    # 3. Get the color applied to Korea
    korea_color <- col2[as.character(korea_status)]
  } else {
    korea_color <- na_col}
  
  # Sudan vs Egypt border colour trick
  # 1. Check Sudan's value
  sudan_status <- data$var[data$iso3 == "SDN"] |> na.omit() |> head(1)
  
  if (!is.na(sudan_status)) {
    # 2. Assign names to color vector
    names(col2) <- levels(data$var)
    # 3. Get the color applied to Sudan
    sudan_color <- col2[as.character(sudan_status)]
  } else {
    sudan_color <- na_col}
  
  # Palestine border colour trick
  # 1. Check Palestine's value
  palestine_status <- data$var[data$iso3 == "PSE"]
  
  if (!is.na(palestine_status)) {
    # 2. Assign names to color vector
    names(col2) <- levels(data$var)
    # 3. Get the color applied to Palestine
    palestine_color <- col2[as.character(palestine_status)]
  } else {
    palestine_color <- na_col}
  
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
    disclaim
  )
  
  return(p)
  }


