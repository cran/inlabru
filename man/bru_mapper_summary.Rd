% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mappers.R
\name{summary.bru_mapper}
\alias{summary.bru_mapper}
\alias{summary.bru_mapper_multi}
\alias{summary.bru_mapper_pipe}
\alias{summary.bru_mapper_collect}
\alias{print.summary_bru_mapper}
\alias{print.bru_mapper}
\title{mapper object summaries}
\usage{
\method{summary}{bru_mapper}(object, ..., prefix = "", initial = prefix, depth = 1)

\method{summary}{bru_mapper_multi}(object, ..., prefix = "", initial = prefix, depth = 1)

\method{summary}{bru_mapper_pipe}(object, ..., prefix = "", initial = prefix, depth = 1)

\method{summary}{bru_mapper_collect}(object, ..., prefix = "", initial = prefix, depth = 1)

\method{print}{summary_bru_mapper}(x, ...)

\method{print}{bru_mapper}(x, ..., prefix = "", initial = prefix, depth = 1)
}
\arguments{
\item{object}{\code{bru_mapper} object to summarise}

\item{\dots}{Unused arguments}

\item{prefix}{character prefix for each line. Default \code{""}.}

\item{initial}{character prefix for the first line. Default \code{initial=prefix}.}

\item{depth}{The recursion depth for multi/collection/pipe mappers.
Default 1, to only show the collection, and not the contents of the sub-mappers.}

\item{x}{Object to be printed}
}
\description{
mapper object summaries
}
\examples{
mapper <-
  bru_mapper_pipe(
    list(
      bru_mapper_multi(list(
        A = bru_mapper_index(2),
        B = bru_mapper_index(3)
      )),
      bru_mapper_index(2)
    )
  )
summary(mapper, depth = 2)
}
