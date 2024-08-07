#' @importFrom stats aggregate complete.cases is.ts ppoints quantile qunif runif time
#' @importFrom grDevices rgb2hsv hsv boxplot.stats col2rgb contourLines hsv rgb2hsv
#' @importFrom pryr named_dots
#' @importFrom utils head tail
NULL

#' rbokeh: R interface for Bokeh
#'
#' R interface for creating plots in Bokeh.  Bokeh by Continuum Analytics, \url{https://docs.bokeh.org/en/latest/}
#'
#' For full documentation on the package, visit \url{https://hafen.github.io/rbokeh}
#' @name rbokeh-package
#' @aliases rbokeh
NULL

#' "Periodic Table" dataset
#'
#' @name elements
#' @description
#' Data for periodic table of the elements
#' @usage elements
#' @keywords data
#' @example man-roxygen/ex-elements.R
NULL

#' Flight frequency dataset
#'
#' @name flightfreq
#' @description
#' Daily counts of domestic flights in the U.S. from 1999 to mid-2008
#' @usage flightfreq
#' @keywords data
#' @example man-roxygen/ex-flightfreq.R
NULL

#' Hexagon binned counts of NYC taxi pickup locations
#'
#' @name nyctaxihex
#' @description
#' Counts of NYC taxi pickups by location for January 2013, obtained from \href{https://chriswhong.com/open-data/foil_nyc_taxi/}{here}.
#' @usage nyctaxihex
#' @keywords data
#' @examples
#' \dontrun{
#' gmap(title = "NYC taxi pickups January 2013",
#'   lat = 40.74, lng = -73.95, zoom = 11,
#'   map_type = "roadmap", width = 1000, height = 800) %>%
#'   ly_hexbin(nyctaxihex, alpha = 0.5,
#'     palette = "Spectral10", trans = log, inv = exp)
#' }
NULL

#' Pipe figures
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs a Bokeh figure
#' @param rhs a layer to add to the figure
NULL
