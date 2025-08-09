# R/map_builder.R

#' Build base map layers and transformed geometries
#'
#' @param data The merged map data with iso3, size, etc.
#' @param projection Projection string (e.g., "moll", "bonne +lat_1=45")
#' @param offset Central meridian for wrapping map
#' @author Takuya Yamanaka
#' @return A list with transformed sf objects (world, disa/disb layers, crs string)

#' Map projection setting and CRS adjustment
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
  
  quiet_st_transform <- function(x, crs) {
    suppressWarnings(sf::st_transform(x, crs))
  }
  
  list(
    crs_plot = crs_plot,
    data_trans = quiet_st_transform(data, crs_plot),
    disa_ac_trans = quiet_st_transform(disa_ac, crs_plot),
    disa_lake_trans = quiet_st_transform(disa_lake, crs_plot),
    disa_nlake_nac_trans = quiet_st_transform(disa_nlake_nac, crs_plot),
    disb_dashed_black_trans = quiet_st_transform(disb_dashed_black, crs_plot),
    disb_dashed_kor_trans = quiet_st_transform(disb_dashed_kor, crs_plot),
    disb_dashed_sdn_trans = quiet_st_transform(disb_dashed_sdn, crs_plot),
    disb_dashed_pse_trans = quiet_st_transform(disb_dashed_pse, crs_plot),
    disb_dashed_grey_trans = quiet_st_transform(disb_dashed_grey, crs_plot),
    disb_solid_trans = quiet_st_transform(disb_solid, crs_plot),
    disb_dotted_grey_trans = quiet_st_transform(disb_dotted_grey, crs_plot),
    disb_dotted_black_trans = quiet_st_transform(disb_dotted_black, crs_plot)
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

#' Zoom setting
region_zoom <- function(zoom) {
  
  # zoom and offset settings in lon/lat (EPSG:4326)
  zoom_specs <- list(
    Global = list(x = c(-180, 180), y = c(-90, 90), offset = 10.8), 
    WPR    = list(x = c(  60, 215), y = c(-50, 55), offset = 150),
    EMR    = list(x = c( -14, 77), y = c(-2, 47), offset = 10.8),
    EUR    = list(x = c( -60, 180), y = c( 28, 85), offset = 10.8),
    AFR    = list(x = c( -25, 55), y = c(-40, 40), offset = 10.8),
    SEA    = list(x = c(  62, 130), y = c(-10, 36), offset = 80),
    AMR    = list(x = c(-175, -35), y = c(-60, 85), offset = 203)
  )
  if (!zoom %in% names(zoom_specs)) {
    stop(sprintf("%s is not on my list of zoom level options: %s",
                 zoom, paste(names(zoom_specs), collapse = ", ")))
  }
  
  zoom_specs[[zoom]]
  
}

#' Common map layers
common_disputed_border <- function(p,
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
                                   zoom_info = zoom_info) {
  
  # add AC fill colour, other disbuted area fill colour and coloured dashed line for Korean DMZ, Palestine, Egypt/Sudan
  p <- p +
    # Stripe pattern for AC filled with China colour
    ggpattern::geom_sf_pattern(data = layers$disa_ac_trans,
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
    ggplot2::geom_sf(data=layers$disa_nlake_nac_trans,  col="grey80", fill="grey80",
                     linewidth = line_width) +
    # fill white for lakes
    ggplot2::geom_sf(data=layers$disa_lake_trans,  col=line_col, fill=water_col,
                     linewidth = line_width) +
    # coloured dashed lines where there are already black solid lines from base world map: Korean DMZ, Palestine, Egypt/Sudan
    ggplot2::geom_sf(data=layers$disb_dashed_kor_trans,  col=korea_color, fill="grey50",
                     linewidth = line_width,
                     linetype = "dashed") +
    ggplot2::geom_sf(data=layers$disb_dashed_sdn_trans,  col=sudan_color, fill="grey50",
                     linewidth = line_width,
                     linetype = 4) +
    ggplot2::geom_sf(data=layers$disb_dashed_pse_trans,  col=palestine_color, fill="grey50",
                     linewidth = line_width,
                     linetype = "dashed") +
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
    # zooming for regional map: default is Global
    ggplot2::coord_sf(
      default_crs = sf::st_crs(4326),
      xlim        = zoom_info$x,
      ylim        = zoom_info$y
    ) +
    # adjusting background/axis/legend settings
    ggplot2::theme(
      panel.border = element_blank(),
      panel.background = ggplot2::element_rect(fill = water_col, color = NA),
      plot.background = ggplot2::element_rect(fill = water_col, color = NA),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
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
  