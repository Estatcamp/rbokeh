% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbokeh-package.R
\name{nyctaxihex}
\alias{nyctaxihex}
\title{Hexagon binned counts of NYC taxi pickup locations}
\usage{
nyctaxihex
}
\description{
Counts of NYC taxi pickups by location for January 2013, obtained from \href{https://chriswhong.com/open-data/foil_nyc_taxi/}{here}.
}
\examples{
\dontrun{
gmap(title = "NYC taxi pickups January 2013",
  lat = 40.74, lng = -73.95, zoom = 11,
  map_type = "roadmap", width = 1000, height = 800) \%>\%
  ly_hexbin(nyctaxihex, alpha = 0.5,
    palette = "Spectral10", trans = log, inv = exp)
}
}
\keyword{data}
