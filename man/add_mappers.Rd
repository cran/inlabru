% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effect.R
\name{add_mappers}
\alias{add_mappers}
\alias{add_mappers.bru_comp}
\alias{add_mappers.bru_comp_list}
\title{Add component input/latent mappers}
\usage{
add_mappers(...)

\method{add_mappers}{bru_comp}(component, lhoods, inputs = NULL, lh_data = NULL, ...)

\method{add_mappers}{bru_comp_list}(components, lhoods, inputs = NULL, lh_data = NULL, ...)
}
\arguments{
\item{\dots}{Parameters passed on to other methods}

\item{component}{A \link{component} object}

\item{lhoods}{A \link{bru_obs_list} object}

\item{inputs}{A precomputed list of inputs, as returned by
\code{\link[=bru_input.bru_comp_list]{bru_input.bru_comp_list()}}}

\item{lh_data}{A list of data object, one for each likelihood in \code{lhoods}
that is used to determine the mapper(s). If \code{NULL}, the data is
or inputs are used instead.}

\item{components}{A \code{bru_comp_list} object}
}
\value{
A \code{component} object with completed mapper information
}
\description{
Add missing mappers between input data and latent variables,
based on likelihood data

Equip component(s) with mappers for subcomponents that do not
have predefined mappers. When needed, the data in \code{lhoods} is used to
determine the appropriate mapper(s).
}
\examples{
\dontrun{
if (interactive() && bru_safe_inla()) {
}
}
}
\keyword{internal}
