# R/helpers.R

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