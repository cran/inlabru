% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatstat.R
\name{spatial.to.ppp}
\alias{spatial.to.ppp}
\title{Convert SpatialPoints and boundary polygon to spatstat ppp object}
\usage{
spatial.to.ppp(points, samplers)
}
\arguments{
\item{points}{A \code{SpatialPoints[DataFrame]} object describing the point pattern.}

\item{samplers}{A \code{SpatialPolygons[DataFrame]} object describing the observation window.}
}
\value{
A spatstat \code{spatstat} \code{ppp} object
}
\description{
Spatstat point pattern objects consist of points and an observation windows. This
function uses a SpatialPoints object and a SpatialPolygon object to generate the points
and the window. Lastly, the ppp() function is called to create the \code{ppp} object.
}
\examples{
\donttest{
if (require("spatstat.geom") &&
  bru_safe_sp() &&
  require("sp")) {
  # Load Gorilla data

  data("gorillas", package = "inlabru")

  # Use nest locations and survey boundary to create a spatstat ppp object

  gp <- spatial.to.ppp(gorillas$nests, gorillas$boundary)
  class(gp)

  # Plot it

  plot(gp)
}
}

}
