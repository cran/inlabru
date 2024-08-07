% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.toypoints.R
\docType{data}
\name{toypoints}
\alias{toypoints}
\title{Simulated 2D point process data}
\format{
The data are a list that contains these elements:
\describe{
\item{\code{points}}{An \code{sf} object of point locations and and \code{z} measurements}
\item{\code{mesh}}{An \code{fm_mesh_2d} object}
\item{\code{boundary}}{An \code{sf} polygon denting the region of interest}
\item{\code{pred_locs}}{A \code{sf} object with prediction point locations}
}
}
\usage{
data(toypoints)
}
\description{
This data set serves as an example for basic inlabru.
}
\examples{
if (require("ggplot2")) {
  ggplot() +
    fmesher::geom_fm(data = toypoints$mesh, alpha = 0) +
    geom_sf(data = toypoints$boundary, fill = "blue", alpha = 0.1) +
    geom_sf(data = toypoints$points, aes(color = z)) +
    scale_color_viridis_c()
}
}
\keyword{datasets}
