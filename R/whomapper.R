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
#' @param df A dataframe with two columns: 'iso3' (character or factor WHO country codes) and 'var' (categorical variable to map).
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

whomapper <- function (df = data.frame(iso3 = NA, var = NA),
                    colours = NULL,
                    projection = "moll",
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
  if (is.data.frame(df) == FALSE)
    stop("df must be a dataframe")
  if (all(c("iso3", "var") %in% names(df)) == FALSE)
    stop("df must have two variables named 'iso3' and 'var'")
  
  df <- as.data.frame(df[!is.na(df$var) & df$var != "",])
  if (is.factor(df$var) &
      !is.na(match('', levels(df$var))))
    df <- droplevels(df[!grepl("^\\s*$", df$var), , drop = FALSE])
  if (!is.factor(df$var))
    df$var <- as.factor(df$var)
  
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
  dplyr::left_join(df, by = c("iso3"))
  
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
    disclaim <- get_who_disclaimer() # from map_builder.R
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
  
  # add AC layer and other layers
  p <- p +
  # Stripe pattern for AC filled with China colour
    ggpattern::geom_sf_pattern(data = disa_ac_trans,
                  fill = china_color,
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
  # coloured dashed lines where there is already black solid lines from base world map: Korean DMZ, Palestine, Egypt/Sudan
    ggplot2::geom_sf(data=disb_dashed_kor_trans,  col=korea_color, fill="grey50",
                     linewidth = line_width,
                     linetype = "dashed") +
    ggplot2::geom_sf(data=disb_dashed_sdn_trans,  col=sudan_color, fill="grey50",
                     linewidth = line_width,
                     linetype = 4) +
    ggplot2::geom_sf(data=disb_dashed_pse_trans,  col=palestine_color, fill="grey50",
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


