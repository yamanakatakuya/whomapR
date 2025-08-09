# R/add_annotate.R

#' `add_annotate()` is used in combination with `whomapper()` and adds optional
#' text and shapes, with adjustment for CRS setting.
#'
#' @param iso3 A vector of ISO3 country codes.
#' @param shape Marker shape.
#' @param col Marker colour.
#' @param size Marker size.
#' @param lab Marker label.
#' @return A ggplot2 function.
#' @author Takuya Yamanaka.
#' @import ggplot2
#' @export
#' @examples
#' add_annotate("text", x = -50, y = 10, lab = "Example", size = 4, col = "blue")

add_annotate <- function(
    geom,
    x = NULL, y = NULL,
    xend = NULL, yend = NULL,
    xmin = NULL, ymin = NULL,
    xmax = NULL, ymax = NULL,
    linewidth = NULL,
    size = NULL,
    color = "black",
    label = NULL,
    include_coord = FALSE,
    default_crs = 4326
) {
  
  
  # Build args for annotate()
  args <- list(
    geom  = geom,
    x     = x,    y    = y,
    xend  = xend, yend = yend,
    xmin  = xmin, ymin = ymin,
    xmax  = xmax, ymax = ymax,
    color = color
  )

  # other options dependent of geom
  lineish <- geom %in% c("segment", "curve", "path", "rect")
  if (lineish && is.null(linewidth)) linewidth <- 0.6
  
  if (grepl("text|label", geom)) args$label <- label
  if (lineish && !is.null(linewidth)) args$linewidth <- linewidth
  if (!lineish && !is.null(size))     args$size      <- size
  
  # calling annotate
  annotate_layer <- do.call(ggplot2::annotate, args)
  
  # common theme settings
  theme_clean <- ggplot2::theme(
    axis.ticks  = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
  )
  
  # only the first annotate requires default crs
  if (isTRUE(include_coord)) {
    list(
      annotate_layer,
      ggplot2::coord_sf(default_crs = sf::st_crs(default_crs)),
      theme_clean
    )
  } else {
    list(annotate_layer, theme_clean)
  }
}