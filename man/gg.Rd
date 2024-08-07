% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot.R
\name{gg}
\alias{gg}
\title{ggplot2 geomes for inlabru related objects}
\usage{
gg(data, ...)
}
\arguments{
\item{data}{an object for which to generate a geom.}

\item{...}{Arguments passed on to the geom method.}
}
\value{
The form of the value returned by gg depends on the class of its argument. See the documentation of the particular methods for details of what is produced by that method.
}
\description{
gg is a generic function for generating geomes from various kinds of spatial objects, e.g. Spatial* data,
meshes, Raster objects and inla/inlabru predictions. The function invokes particular methods which depend
on the \link{class} of the first argument.
}
\examples{
if (require("ggplot2", quietly = TRUE)) {
  # Load Gorilla data

  data(gorillas, package = "inlabru")

  # Invoke ggplot and add geomes for the Gorilla nests and the survey boundary

  ggplot() +
    gg(gorillas$boundary) +
    gg(gorillas$nests)
}
}
\seealso{
Other geomes for inla and inlabru predictions: 
\code{\link{gg.bru_prediction}()},
\code{\link{gg.data.frame}()},
\code{\link{gg.matrix}()},
\code{\link{gm}()}

Other geomes for spatial data: 
\code{\link{gg.SpatRaster}()},
\code{\link{gg.SpatialGridDataFrame}()},
\code{\link{gg.SpatialLines}()},
\code{\link{gg.SpatialPixels}()},
\code{\link{gg.SpatialPixelsDataFrame}()},
\code{\link{gg.SpatialPoints}()},
\code{\link{gg.SpatialPolygons}()},
\code{\link{gg.sf}()},
\code{\link{gm}()}

Other geomes for meshes: 
\code{\link{gg.fm_mesh_1d}()},
\code{\link{gg.fm_mesh_2d}()},
\code{\link{gm}()}

Other geomes for Raster data: 
\code{\link{gg.RasterLayer}()},
\code{\link{gm}()}
}
\concept{geomes for Raster data}
\concept{geomes for inla and inlabru predictions}
\concept{geomes for meshes}
\concept{geomes for spatial data}
