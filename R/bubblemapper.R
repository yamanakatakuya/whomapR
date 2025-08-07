# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Importing latest WHO geoJSON files (in ./shape folder)
# the latest WHO shape/geoJSON files are available at https://gis-who.hub.arcgis.com/
# Takuya Yamanaka, August 2025
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
#' bubblemapper(data.frame(iso3 = NA, var = NA))
#' @export
bubblemapper <- function (df = data.frame(iso3 = NA, size = NA),
                       projection = "moll",
                       offset = 10.8,
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
                       legend_pos = c(0.17,0.42)
)
{
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Data check and definition ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  if (is.data.frame(df) == FALSE)
    stop("df must be a dataframe")
  if (!all(c("iso3", "size") %in% names(df)))
    stop("df must have two columns: 'iso3' and 'size'")
  
  df <- as.data.frame(df[!is.na(df$size) & df$size != "",])
  if (!is.numeric(df$size))
    df$size <- as.numeric(df$size)
  
  # leftjoin a dataset with the base world map
  data <- world |>
    dplyr::left_join(df, by = c("iso3"))
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Call map layers and WHO disclaimer from map_builder.R ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Call map layers
  layers <- build_map_layers(data, projection, offset)
  # unpack a list
  list2env(layers, envir = environment())
  
  # Call WHO disclaimer
  if (disclaimer) {
    disclaim <- get_who_disclaimer() # from map_builder.R
  }
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # plotting an output ----
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # bubble points adjustment
  bubble_points <- data_trans |>
    filter(!is.na(size), !is.na(CENTER_LON), !is.na(CENTER_LAT)) |>
    group_by(iso3) |>
    mutate(row_number = row_number()) |>
    filter(
      (iso3 == "RUS" & row_number == 2) |
        (iso3 != "RUS" & row_number == 1)
    ) |>
    select(-row_number) |>
    ungroup() |>
    mutate(geometry = sf::st_point_on_surface(geometry)) |>
    sf::st_as_sf()

    
  # plot the base world map
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
  
  # add AC layer and other layers
  p <- p +
    # Stripe pattern for AC filled with China colour
    ggpattern::geom_sf_pattern(data = disa_ac_trans,
                               fill = "white",
                               col = "grey80",           # outline color
                               linewidth = line_width,          # outline thickness
                               pattern = "stripe",
                               pattern_fill = "grey80",  # stripe color
                               pattern_colour = "grey80",
                               pattern_size = 0.050,     # stripe thickness
                               pattern_angle = 45,
                               pattern_density = 0.3,
                               pattern_spacing = 0.002) +
    # fill grey for other disputed areas
    ggplot2::geom_sf(data=disa_nlake_nac_trans,  col="grey80", fill="grey80",
                     linewidth = line_width) +
    # fill white for lakes
    ggplot2::geom_sf(data=disa_lake_trans,  col=line_col, fill=water_col,
                     linewidth = line_width) +
    # black dashed lines for Kenya/Sudan Kosovo etc
    ggplot2::geom_sf(data=disb_dashed_black_trans,  col=line_col, fill="grey50",
                     linewidth = line_width,
                     linetype = "dashed") +
    # black dashed lines where there is already black solid lines from base world map: Korean DMZ, Palestine, Egypt/Sudan
    ggplot2::geom_sf(data=disb_dashed_kor_trans,  col=line_col, fill="grey50",
                     linewidth = line_width,
                     linetype = "dashed") +
    ggplot2::geom_sf(data=disb_dashed_sdn_trans,  col=line_col, fill="grey50",
                     linewidth = line_width,
                     linetype = 4) + # dotdash
    ggplot2::geom_sf(data=disb_dashed_pse_trans,  col=line_col, fill="grey50",
                     linewidth = line_width,
                     linetype = "dashed") +
    # grey dashed lines for J&K
    ggplot2::geom_sf(data=disb_dashed_grey_trans,  col="grey50", fill="grey50",
                     linewidth = line_width,
                     linetype = "dashed") +
    # black solid line for Arunachal Pradesh, Western Sahara, AC, Egypt Claim
    ggplot2::geom_sf(data=disb_solid_trans,  col=line_col, fill="grey50",
                     linewidth = line_width,
                     linetype = "solid") +
    # grey dotted lines for J&K control line 
    ggplot2::geom_sf(data=disb_dotted_grey_trans,  col="grey50", fill="grey50",
                     linewidth = line_width,
                     linetype = "dotted") +
    # black dotted lines for Abyei
    ggplot2::geom_sf(data=disb_dotted_black_trans,  col=line_col, fill="grey50",
                     linewidth = line_width,
                     linetype = "dotted") +
    # adjusting background/axis settings
    ggplot2::theme(
      panel.background = element_rect(fill = water_col, color = NA),
      plot.background = element_rect(fill = water_col, color = NA)
    ) +
    ggplot2::theme(axis.title.x = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank()) +
    ggplot2::theme(panel.grid = element_blank()) +
    # map title
    ggplot2::labs(title = map_title) +
    # adjusting legend settings
    ggplot2::theme(
      legend.key.size = unit(0.4, "cm"),
      legend.key = element_rect(fill = "white", color = "white"),
      legend.text = element_text(size = 6),
      legend.justification = c(0.5, 1),
      legend.title = element_text(size = 6),
      legend.background = element_rect(fill = NA, color = NA),
      legend.position = legend_pos
    )
  
  # disclaimer option
  if (disclaimer == FALSE)
    p
  else
  {
    p +
      ggplot2::labs(caption = disclaim) +
      ggplot2::theme(plot.caption.position = 'plot',
                     plot.caption = element_text(size = 6,
                                                 hjust = 0.5))
  }
}


