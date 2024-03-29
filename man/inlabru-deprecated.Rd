% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R, R/integration.R, R/mappers.R,
%   R/mesh.R
\name{inlabru-deprecated}
\alias{inlabru-deprecated}
\alias{mesh_triangle_integration}
\alias{bru_mapper_offset}
\alias{is.inside}
\alias{vertices.inla.mesh}
\title{Deprecated functions in inlabru}
\usage{
mesh_triangle_integration(mesh, tri_subset = NULL, nsub = NULL)

bru_mapper_offset(...)

is.inside(mesh, loc, mesh.coords = NULL)

vertices.inla.mesh(...)
}
\arguments{
\item{mesh}{an inla.mesh object.}

\item{tri_subset}{Optional triangle index vector for integration on a subset
of the mesh triangles (Default \code{NULL})}

\item{nsub}{number of subdivision points along each triangle edge, giving
\code{(nsub + 1)^2} proto-integration points used to compute
the vertex weights
(default \code{NULL=9}, giving 100 integration points for each triangle)}

\item{\dots}{Usually passed on to other methods}

\item{loc}{Points in space stored either as data.frame, a two-column matrix
of x and y coordinates or a SpatialPoints object.}

\item{mesh.coords}{Coordinate names of the mesh. Use only if loc is a
data.frame with respective column names.}
}
\value{
\itemize{
\item \code{mesh_triangle_integration} returns a \code{list} with elements \code{loc}
and \code{weight} with integration points for the mesh
}

\code{is.inside()}: Single column matrix of Boolean values indicating if a point is
inside the mesh.
}
\description{
These functions still attempt to do their job, but will be removed in a
future version.
}
\section{Functions}{
\itemize{
\item \code{mesh_triangle_integration()}: Integration scheme for mesh triangle interiors

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Use \code{\link[=fm_int_mesh_2d_core]{fm_int_mesh_2d_core()}} instead.

\item \code{bru_mapper_offset()}: Creates a \code{\link[=bru_mapper_const]{bru_mapper_const()}} mapper.

\item \code{is.inside()}: Find out which points are inside a mesh.
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} in favour of \code{\link[=fm_is_within]{fm_is_within()}}.
Replace \code{is.inside(mesh, loc)} with \code{fm_is_within(loc, mesh)}.

\item \code{vertices.inla.mesh()}: Extract vertex locations from an \code{inla.mesh}.
Converts the vertices of an \code{inla.mesh} object into a \code{SpatialPointsDataFrame}.
Deprecated in favour of \code{\link[=fm_vertices]{fm_vertices()}}

}}
\seealso{
\code{\link[=fm_is_within]{fm_is_within()}}
}
\author{
Finn Lindgren \email{finn.lindgren@gmail.com}
}
\keyword{internal}
