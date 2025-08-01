% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/used.R
\name{bru_used_update}
\alias{bru_used_update}
\alias{bru_used_update.bru_obs_list}
\alias{bru_used_update.bru_obs}
\alias{bru_used_update.bru_used}
\title{Update used_component information objects}
\usage{
bru_used_update(x, labels, ...)

\method{bru_used_update}{bru_obs_list}(x, labels, ...)

\method{bru_used_update}{bru_obs}(x, labels, ...)

\method{bru_used_update}{bru_used}(x, labels, ...)
}
\arguments{
\item{x}{Object to be updated}

\item{labels}{character vector of component labels}

\item{...}{Unused}
}
\value{
An updated version of \code{x}
}
\description{
Merge available component labels information with used components
information.
}
\seealso{
Other bru_used: 
\code{\link{bru_used}()},
\code{\link{bru_used_vars}()}
}
\concept{bru_used}
\keyword{internal}
