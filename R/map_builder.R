# R/map_builder.R

#' Build base map layers and transformed geometries
#'
#' @param data The merged map data with iso3, size, etc.
#' @param projection Projection string (e.g., "moll", "bonne +lat_1=45")
#' @param offset Central meridian for wrapping map
#' @return A list with transformed sf objects (world, disa/disb layers, crs string)
build_map_layers <- function(data, 
                             projection, 
                             offset) {
  
  # Define valid/available map projections
  valid_projs <- c(
    "eqc", "moll", "robin", "eck1", "eck2", "eck3", "eck4", "eck5", "eck6",
    "hammer", "goode", "sinu", "aitoff", "bonne +lat_1=45", "bonne +lat_1=90"
  )
  # Validate and set projection. If invalid, robin
  if (!projection %in% valid_projs) {
    warning(paste0("Invalid projection '", projection, "'. Defaulting to 'robin'."))
    projection <- "robin"
  }
  
  # Construct CRS string
  crs_plot <- paste0("+proj=", projection, " +lon_0=", offset, " +datum=WGS84 +units=m +no_defs")
  
  # 'break' any polygons that cross offset point
  data <- data |> sf::st_break_antimeridian(lon_0 = offset)
  
  # data transformation according to map projection and CRS
  list(
    crs_plot = crs_plot,
    data_trans = sf::st_transform(data, crs_plot),
    disa_ac_trans = sf::st_transform(disa_ac, crs_plot),
    disa_lake_trans = sf::st_transform(disa_lake, crs_plot),
    disa_nlake_nac_trans = sf::st_transform(disa_nlake_nac, crs_plot),
    disb_dashed_black_trans = sf::st_transform(disb_dashed_black, crs_plot),
    disb_dashed_kor_trans = sf::st_transform(disb_dashed_kor, crs_plot),
    disb_dashed_sdn_trans = sf::st_transform(disb_dashed_sdn, crs_plot),
    disb_dashed_pse_trans = sf::st_transform(disb_dashed_pse, crs_plot),
    disb_dashed_grey_trans = sf::st_transform(disb_dashed_grey, crs_plot),
    disb_solid_trans = sf::st_transform(disb_solid, crs_plot),
    disb_dotted_grey_trans = sf::st_transform(disb_dotted_grey, crs_plot),
    disb_dotted_black_trans = sf::st_transform(disb_dotted_black, crs_plot)
  )
}

#' WHO-style map disclaimer
get_who_disclaimer <- function() {
  paste0(
    "\uA9 World Health Organization ",
    format(Sys.Date(), "%Y"),". All rights reserved.\n",
    "The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of\n",
    "the World Health Organization concerning the legal status of any country, territory, city or area or of its authorities,\n",
    "or concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate borderlines\n",
    "for which there may not yet be full agreement."
  )
}


# Common map layers
common_disputed_border <- function(p,
                                     layers,
                                     map_title,
                                     disclaimer,
                                     legend_pos,
                                     line_col,
                                     line_width,
                                     water_col,
                                     disclaim) {
  
  p <- p +
    # black dashed lines for Kenya/Sudan Kosovo etc
    ggplot2::geom_sf(data = layers$disb_dashed_black, col = line_col, fill = "grey50", linewidth = line_width, linetype = "dashed") +
    # grey dashed lines for J&K
    ggplot2::geom_sf(data = layers$disb_dashed_grey, col = "grey50", fill = "grey50", linewidth = line_width, linetype = "dashed") +
    # black solid line for Arunachal Pradesh, Western Sahara, AC, Egypt Claim
    ggplot2::geom_sf(data = layers$disb_solid, col = line_col, fill = "grey50", linewidth = line_width, linetype = "solid") +
    # grey dotted lines for J&K control line 
    ggplot2::geom_sf(data = layers$disb_dotted_grey, col = "grey50", fill = "grey50", linewidth = line_width, linetype = "dotted") +
    # black dotted lines for Abyei
    ggplot2::geom_sf(data = layers$disb_dotted_black, col = line_col, fill = "grey50", linewidth = line_width, linetype = "dotted") +
    # adjusting background/axis/legend settings
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = water_col, color = NA),
      plot.background = ggplot2::element_rect(fill = water_col, color = NA),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.key.size = grid::unit(0.4, "cm"),
      legend.key = ggplot2::element_rect(fill = "white", color = "white"),
      legend.text = ggplot2::element_text(size = 6),
      legend.title = ggplot2::element_text(size = 6),
      legend.justification = c(0.5, 1),
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.position = legend_pos
    ) +
    # map title
    ggplot2::labs(title = map_title)
  
  # WHO disclaimer option
  if (isTRUE(disclaimer)) {
    p <- p +
      ggplot2::labs(caption = disclaim) +
      ggplot2::theme(
        plot.caption.position = 'plot',
        plot.caption = ggplot2::element_text(size = 6, hjust = 0.5)
      )
  }
  
  return(p)
  
}
  