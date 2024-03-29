% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effect.R
\name{input_eval}
\alias{input_eval}
\alias{input_eval.component}
\alias{input_eval.component_list}
\alias{input_eval.bru_input}
\title{Obtain component inputs}
\usage{
input_eval(...)

\method{input_eval}{component}(component, data, ...)

\method{input_eval}{component_list}(components, data, ...)

\method{input_eval}{bru_input}(input, data, env = NULL, null.on.fail = FALSE, ...)
}
\arguments{
\item{...}{Unused.}

\item{component}{A component.}

\item{data}{A \code{data.frame}, \code{tibble}, \code{sf}, \code{list}, or \verb{Spatial*} object of
covariates and/or point locations.
If \code{NULL}, return the component's map.}
}
\value{
An list of mapper input values, formatted for the full component mapper
(of type \code{bru_mapper_pipe})
}
\description{
Obtain component inputs
}
\section{Methods (by class)}{
\itemize{
\item \code{input_eval(bru_input)}: Attempts to evaluate a component input (e.g. \code{main},
\code{group}, \code{replicate}, or \code{weight}), and process the results:
\enumerate{
\item If \code{eval()} failed, return NULL or map everything to 1
(see the \code{null.on.fail} argument). This should normally not
happen, unless the component use logic is incorrect,
(e.g. via \code{include}/\code{exclude})
leading to missing columns for a certain likelihood in a
multi-\code{like()} model.
\item If we obtain a function, apply the function to the data object
\item If we obtain an object supported by \code{\link[=eval_spatial]{eval_spatial()}}, extract the values
of that data frame at the point locations
\item Else we obtain a vector and return as-is. This happens when input
references a column of the data points, or some other complete expression
}

}}
\section{Simple covariates and the map parameter}{


It is not unusual for a random effect act on a transformation of a covariate. In other frameworks this
would mean that the transformed covariate would have to be calculated in advance and added to the
data frame that is usually provided via the \code{data} parameter. inlabru provides the option to do
this transformation automatically. For instance, one might be interested in the effect of a covariate
\eqn{x^2}. In inla and other frameworks this would require to add a column \code{xsquared} to the
input data frame and use the formula

\itemize{\item{\code{formula = y ~ f(xsquared, model = "linear")},}}

In inlabru this can be achieved in several ways of using the \code{main} parameter
(\code{map} in version 2.1.13 and earlier), which does not need to be named.

\itemize{
\item{\code{components = y ~ psi(main = x^2, model = "linear")}}
\item{\code{components = y ~ psi(x^2, model = "linear")}}
\item{\code{components = y ~ psi(mySquareFun(x), model = "linear")},}
\item{\code{components = y ~ psi(myOtherSquareFun, model = "linear")},}

}

In the first example inlabru will interpret the map parameter as an expression to be evaluated within
the data provided. Since \eqn{x} is a known covariate it will know how to calculate it. The second
example is an expression as well but it uses a function called \code{mySquareFun}. This function is
defined by user but has to be accessible within the work space when setting up the components.
The third example provides the function \code{myOtherSquareFun}. In this case,
inlabru will call the function as \code{myOtherSquareFun(.data.)}, where \code{.data.}
is the data provided via the \code{\link[=like]{like()}} \code{data} parameter.
The function needs to know what parts of the data to use to construct the
needed output. For example,

\if{html}{\out{<div class="sourceCode">}}\preformatted{myOtherSquareFun <- function(data) \{
  data[ ,"x"]^2
\}
}\if{html}{\out{</div>}}
}

\section{Spatial Covariates}{


When fitting spatial models it is common to work with covariates that depend on space, e.g. sea
surface temperature or elevation. Although it is straightforward to add this data to the input
data frame or write a covariate function like in the previous section there is an even more
convenient way in inlabru. Spatial covariates are often stored as \code{SpatialPixelsDataFrame},
\code{SpatialPixelsDataFrame} or \code{RasterLayer} objects. These can be provided directly via
the input expressions if they are supported by \code{\link[=eval_spatial]{eval_spatial()}}, and
the \code{\link[=like]{like()}} data is an \code{sf} or \code{SpatialPointsDataFrame} object.
\code{inlabru} will then automatically
evaluate and/or interpolate the covariate at your data locations when using code like

\if{html}{\out{<div class="sourceCode">}}\preformatted{components = y ~ psi(mySpatialPixels, model = "linear")
}\if{html}{\out{</div>}}

For more precise control, use the the \code{layer} and \code{selector} arguments (see \code{\link[=component]{component()}}),
or call \code{eval_spatial()} directly, e.g.:

\if{html}{\out{<div class="sourceCode">}}\preformatted{components = y ~ psi(eval_spatial(mySpatialPixels, where = .data.), model = "linear")
}\if{html}{\out{</div>}}
}

\section{Coordinates}{


A common spatial modelling component when using inla are SPDE models. An important feature of
inlabru is that it will automatically calculate the so called A-matrix (a component model matrix)
which maps SPDE
values at the mesh vertices to values at the data locations. For this purpose, the input
can be set to \code{coordinates}, which is the \code{sp} package function that extracts point
coordinates from the \code{SpatialPointsDataFrame} that was provided as input to \code{\link[=like]{like()}}. The code for
this would look as follows:

\if{html}{\out{<div class="sourceCode">}}\preformatted{components = y ~ field(coordinates, model = inla.spde2.matern(...))
}\if{html}{\out{</div>}}

Since \code{coordinates} is a function from the \code{sp} package, this results in
evaluation of \code{sp::coordinates(.data.)}, which loses any CRS information
from the data object.

For \code{sf} data with a geometry column (by default named \code{geometry}), use

\if{html}{\out{<div class="sourceCode">}}\preformatted{components = y ~ field(geometry, model = inla.spde2.matern(...))
}\if{html}{\out{</div>}}

Since the CRS information is part of the geometry column of the \code{sf} object,
this retains CRS information, so this is more robust, and allows the model
to be built on a different CRS than the observation data.
}

\seealso{
\code{\link[=component]{component()}}
}
\author{
Fabian E. Bachl \email{bachlfab@gmail.com}, Finn Lindgren \email{finn.lindgren@gmail.com}
}
\keyword{internal}
