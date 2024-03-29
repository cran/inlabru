% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.seals.R
\docType{data}
\name{seals}
\alias{seals}
\alias{seals_sp}
\title{Seal pups}
\format{
The data contain these objects:
\describe{
\item{\code{points}:}{ A \code{SpatialPointsDataFrame} Center locations of the photos}
\item{\code{mesh}:}{ An \code{fm_mesh_2d} enclosing the plane's transect}
\item{\code{ice.data}:}{ An \code{SpatialPointsDataFrame} with MODIS ice concentration estimates}
\item{\code{ice.cv}:}{ An \code{covdata} object with interpolated ice coverage data}
}
}
\source{
Martin Jullum \email{Martin.Jullum@nr.no}
}
\usage{
data(seals_sp)
}
\description{
This is a single transect of an aereal photo seal pup survey in the Greenland Sea
}
\examples{
if (require(ggplot2, quietly = TRUE)) {
  ggplot() +
    geom_fm(data = seals_sp$mesh) +
    gg(seals_sp$points)
}
}
\references{
Oigard, T. A. (2013) From pup production to quotas: current status of harp seals in the Greenland Sea.
ICES Journal of Marine Science, doi.10.1093/icesjms/fst155.

Oigard, T. A. (2014) Current status of hooded seals in the Greenland Sea. Victims of climate change and predation?,
Biological Conservation , 2014, 172, 29 - 36.
}
\keyword{datasets}
