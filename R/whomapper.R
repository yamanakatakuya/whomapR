# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Importing latest WHO geoJSON files (in ./shape folder)
# the latest WHO shape/geoJSON files are available at https://gis-who.hub.arcgis.com/
# Takuya Yamanaka, August 2025
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' Choropleth world maps 2025 version
#' `whomapper()` prints a choropleth world map based on the latest WHO geoJSON files
#'  It requires ggplot2, ggpattern, sf and here
#'
#' @param df a dataframe. It must contain a variable "iso" (factor)
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
#' @format An `sf` object with one row per country and at least a column `iso3`.
#' @source Modified from WHO GIS (https://gis-who.hub.arcgis.com/)
#' @author Takuya Yamanaka, adapted from scripts of whomap developed by Philippe Glaziou.
#' @import ggplot2
#' @import scales
#' @import ggpattern
#' @import sf
#' @import dplyr
#' @examples
#' whomapper(data.frame(iso3 = NA, var = NA))
#' @export whomapper()

whomapper <- function (df = data.frame(iso3 = NA, var = NA),
                    colours = NULL,
                    moll = FALSE,
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
                    legend_pos = c(0.2,0.40)
)
{
  # required data
  if (is.data.frame(df) == FALSE)
    stop("X must be a dataframe")
  if (all(c("iso3", "var") %in% names(df)) == FALSE)
    stop("X must have two variables named 'iso3' and 'var'")
  
  df <- as.data.frame(df[!is.na(df$var) & df$var != "",])
  if (is.factor(df$var) &
      !is.na(match('', levels(df$var))))
    df <- droplevels(df[!grepl("^\\s*$", df$var), , drop = FALSE])
  if (!is.factor(df$var))
    df$var <- as.factor(df$var)

  # leftjoin a dataset with the base world map
  data <- world |>
  dplyr::left_join(df, by = c("iso3"))

  # option to switch Plate Carrée (Equirectangular projection) and Mollweide projection
  crs_plot <- if (moll) "+proj=moll" else sf::st_crs(data) # option to choose Plate Carrée (Equirectangular projection) or Mollweide projection
 
  # option to switch Plate Carrée (Equirectangular projection) and Mollweide projection 
  # Ensure var is a factor with explicit NA
  data$var <- forcats::fct_explicit_na(data$var, na_level = na_label)

  # data transformation to switch Plate Carrée (Equirectangular projection) and Mollweide projection
  data_trans      <- sf::st_transform(data, crs_plot)
  disa_ac_trans   <- sf::st_transform(disa_ac, crs_plot)
  disa_lake_trans   <- sf::st_transform(disa_lake, crs_plot)
  disa_nlake_nac_trans   <- sf::st_transform(disa_nlake_nac, crs_plot)
  disb_su_trans   <- sf::st_transform(disb_su, crs_plot)
  disb_ar_trans   <- sf::st_transform(disb_ar, crs_plot)
  disb_nsu_trans   <- sf::st_transform(disb_nsu, crs_plot)

  # 2. Create one dummy sf object for both legend entries ---
  dummy_legend <- sf::st_sf(
  var = factor(
    c(na_label, "Not applicable"),
    levels = c(levels(data$var),  "Not applicable")
  ),
  geometry = sf::st_sfc(st_geometrycollection(), st_geometrycollection()),
  crs = sf::st_crs(data)
  )

  # 2. colour definition ---
  if (is.null(colours)) {
  xc <- seq(0, 1, length = length(levels(data[["var"]])))
  col <- scales::seq_gradient_pal(low_col, high_col)(xc)
  } else
  col <- colours

  col2 <- c(col, na_col, 'grey60')

  # disclaimer
  disclaim <- paste(
  "\uA9 World Health Organization",
  format(Sys.Date(), "%Y"),
  ". All rights reserved.
  The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of
  the World Health Organization concerning the legal status of any country, territory, city or area or of its authorities,or concerning the delimitation
  of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate borderlines for which there may not yet be full agreement."
  )

  
  # plotting an output
  # plot the base world map
  p <- ggplot2::ggplot() + 
    ggplot2::geom_sf(data=data_trans,  col=line_col, aes(fill = var), linewidth = line_width) +
    # Dummy data for legend entries
    ggplot2::geom_sf(data = dummy_legend, aes(fill = var), show.legend = TRUE) +
    # legend
    ggplot2::scale_fill_manual(legend.title, values = col2) +
    ggplot2::guides(
    fill = guide_legend(override.aes = list(color = NA))  # remove outline in legend
    ) 

  # Aksai Chin colour trick
  # 1. Check China's value
  china_status <- data$var[data$iso3 == "CHN"]
  # 2. Assign names to color vector
  names(col2) <- levels(data$var)
  # 3. Get the color applied to China
  china_color <- col2[as.character(china_status)]

  # plot AC layer and other layers
  p <- p +
  # Stripe pattern for AC fillin with China colour
    ggpattern::geom_sf_pattern(data = disa_ac_trans,
                  fill = china_color,
                  col = "grey80",           # outline color
                  linewidth = 0.3,          # outline thickness
                  pattern = "stripe",
                  pattern_fill = "grey80",  # stripe color
                  pattern_colour = "grey80",
                  pattern_size = 0.017,     # stripe thickness
                  pattern_angle = 45,
                  pattern_density = 0.3,
                  pattern_spacing = 0.001) +
  # fill grey for other disputed areas
    ggplot2::geom_sf(data=disa_nlake_nac_trans,  col="grey80", fill="grey80",
          linewidth = line_width) +
  # fill white for lakes
    ggplot2::geom_sf(data=disa_lake_trans,  col="black", fill="white",
          linewidth = line_width) +
  # grey dashed lines for Sudan/South Sudan  boundaries
    ggplot2::geom_sf(data=disb_su_trans,  col="grey50", fill="grey50",
          linewidth = line_width,
          linetype = "dashed") +
  # black solid line for Arunachal Pradesh
    ggplot2::geom_sf(data=disb_ar_trans,  col="black", fill="grey50",
          linewidth = line_width,
          linetype = "solid") +
  # grey solid lines for other boundaries (this is not 100% following LEG SOP)
    ggplot2::geom_sf(data=disb_nsu_trans,  col="grey50", fill="grey50",
          linewidth = line_width,
          linetype = "solid") +
  # adjusting background/axis settings
    ggplot2::theme(
    panel.background = element_rect(fill = "white", color = NA),
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
      legend.key.size = unit(0.5, "cm"),
      legend.key = element_rect(fill = "white", color = "white"),
      legend.text = element_text(size = 7),
      legend.justification = c(0.5, 1),
      legend.title = element_text(size = 7),
      legend.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
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


