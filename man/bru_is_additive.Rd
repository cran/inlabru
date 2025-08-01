% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bru.inference.R
\name{bru_is_additive}
\alias{bru_is_additive}
\alias{bru_is_additive.data.frame}
\alias{bru_is_additive.character}
\alias{bru_is_additive.expression}
\alias{bru_is_additive.formula}
\title{Check for predictor expression additivity}
\usage{
bru_is_additive(x, ...)

\method{bru_is_additive}{data.frame}(x, root_id = 0, ..., verbose = FALSE)

\method{bru_is_additive}{character}(x, ...)

\method{bru_is_additive}{expression}(x, ...)

\method{bru_is_additive}{formula}(x, ...)
}
\arguments{
\item{x}{A predictor \code{expression}, \code{formula}, or parse information
\code{data.frame}.}

\item{\dots}{Arguments passed on recursively.}

\item{verbose}{logical; if \code{TRUE}, print diagnostic parsing information.}
}
\value{
\code{TRUE} if the expression is detected to be additive, \code{FALSE}
otherwise.
}
\description{
Checks if a predictor expression is additive or not
}
\keyword{internal}
