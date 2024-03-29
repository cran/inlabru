% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot.R
\name{multiplot}
\alias{multiplot}
\title{Multiple ggplots on a page.}
\source{
\url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
}
\usage{
multiplot(..., plotlist = NULL, cols = 1, layout = NULL)
}
\arguments{
\item{...}{Comma-separated \code{ggplot} objects.}

\item{plotlist}{A list of \code{ggplot} objects - an alternative to the comma-separated argument above.}

\item{cols}{Number of columns of plots on the page.}

\item{layout}{A matrix specifying the layout. If present, 'cols' is ignored. If the layout is
something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left,
2 will go in the upper right, and 3 will go all the way across the bottom.}
}
\description{
Renders multiple ggplots on a single page.
}
\examples{
if (require("ggplot2", quietly = TRUE)) {
  df <- data.frame(x = 1:10, y = 1:10, z = 11:20)
  pl1 <- ggplot(data = df) +
    geom_line(mapping = aes(x, y), color = "red")
  pl2 <- ggplot(data = df) +
    geom_line(mapping = aes(x, z), color = "blue")
  multiplot(pl1, pl2, cols = 2)
}
}
\author{
David L. Borchers \email{dlb@st-andrews.ac.uk}
}
