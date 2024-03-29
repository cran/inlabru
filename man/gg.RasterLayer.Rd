% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot.R
\name{gg.RasterLayer}
\alias{gg.RasterLayer}
\title{Geom for RasterLayer objects}
\usage{
\method{gg}{RasterLayer}(
  data,
  mapping = ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["layer"]]),
  ...
)
}
\arguments{
\item{data}{A RasterLayer object.}

\item{mapping}{aesthetic mappings created by \code{aes}. These are passed on to \code{geom_tile}.}

\item{...}{Arguments passed on to \code{geom_tile}.}
}
\value{
An object returned by \code{geom_tile}
}
\description{
This function takes a RasterLayer object, converts it into a
\code{SpatialPixelsDataFrame} and uses \code{geom_tile} to plot the data.
}
\details{
This function requires the \code{raster} and \code{ggplot2} packages.
}
\examples{
\dontrun{
# Some features require the raster and spatstat.data packages.
if (require("spatstat.data", quietly = TRUE) &&
  require("raster", quietly = TRUE) &&
  require("ggplot2", quietly = TRUE)) {
  # Load Gorilla data
  data("gorillas", package = "spatstat.data")

  # Convert elevation covariate to RasterLayer

  elev <- as(gorillas.extra$elevation, "RasterLayer")

  # Plot the elevation

  ggplot() +
    gg(elev)
}
}
}
\seealso{
Other geomes for Raster data: 
\code{\link{gg}()},
\code{\link{gm}()}
}
\concept{geomes for Raster data}
