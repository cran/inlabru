#' @name Poisson3_1D
#' @title 1-Dimensional NonHomogeneous Poisson example.
#' @docType data
#' @description Point data and count data, together with intensity function and expected counts for
#' a multimodal nonhomogeneous 1-dimensional Poisson process example. Counts are given for two
#' different gridded data interval widths.
#'
#' @aliases lambda3_1D E_nc3a E_nc3b pts3 countdata3a countdata3b
#'
#' @usage data(Poisson3_1D)
#'
#' @format The data contain the following `R` objects:
#'  \describe{
#'    \item{`lambda3_1D`:}{ A function defining the intensity function of a
#'    nonhomogeneous Poisson process. Note that this function is only defined on
#'    the interval (0,55).}
#'    \item{`E_nc3a`}{ The expected counts of gridded data for the wider bins (10 bins).}
#'    \item{`E_nc3b`}{ The expected counts of gridded data for the wider bins (20 bins).}
#'    \item{`pts3`}{ The locations of the observed points (a data frame with one column, named `x`).}
#'    \item{`countdata3a`}{ A data frame with three columns, containing the count data for the
#'    10-interval case:}
#'    \item{`countdata3b`}{ A data frame with three columns, containing the count data for the
#'    20-interval case:}
#'    \describe{
#'      \item{`x`}{ The grid cell midpoint.}
#'      \item{`count`}{ The number of detections in the cell.}
#'      \item{`exposure`}{ The width of the cell.}
#'    }
#'  }
#'
#' @examples
#' \donttest{
#' if (require("ggplot2", quietly = TRUE)) {
#'   data(Poisson3_1D)
#'   # first the plots for the 10-bin case:
#'   p1a <- ggplot(countdata3a) +
#'     geom_point(data = countdata3a, aes(x = x, y = count), col = "blue") +
#'     ylim(0, max(countdata3a$count, E_nc3a)) +
#'     geom_point(
#'       data = countdata3a, aes(x = x), y = 0, shape = "+",
#'       col = "blue", cex = 4
#'     ) +
#'     geom_point(
#'       data = data.frame(x = countdata3a$x, y = E_nc3a),
#'       aes(x = x), y = E_nc3a, shape = "_", cex = 5
#'     ) +
#'     xlab(expression(bold(s))) +
#'     ylab("count")
#'   ss <- seq(0, 55, length.out = 200)
#'   lambda <- lambda3_1D(ss)
#'   p2a <- ggplot() +
#'     geom_line(
#'       data = data.frame(x = ss, y = lambda), aes(x = x, y = y),
#'       col = "blue"
#'     ) +
#'     ylim(0, max(lambda)) +
#'     geom_point(data = pts3, aes(x = x), y = 0.2, shape = "|", cex = 4) +
#'     xlab(expression(bold(s))) +
#'     ylab(expression(lambda(bold(s))))
#'   multiplot(p1a, p2a, cols = 1)
#'
#'   # Then the plots for the 20-bin case:
#'   p1a <- ggplot(countdata3b) +
#'     geom_point(data = countdata3b, aes(x = x, y = count), col = "blue") +
#'     ylim(0, max(countdata3b$count, E_nc3b)) +
#'     geom_point(
#'       data = countdata3b, aes(x = x), y = 0, shape = "+",
#'       col = "blue", cex = 4
#'     ) +
#'     geom_point(
#'       data = data.frame(x = countdata3b$x, y = E_nc3b),
#'       aes(x = x), y = E_nc3b, shape = "_", cex = 5
#'     ) +
#'     xlab(expression(bold(s))) +
#'     ylab("count")
#'   ss <- seq(0, 55, length.out = 200)
#'   lambda <- lambda3_1D(ss)
#'   p2a <- ggplot() +
#'     geom_line(
#'       data = data.frame(x = ss, y = lambda), aes(x = x, y = y),
#'       col = "blue"
#'     ) +
#'     ylim(0, max(lambda)) +
#'     geom_point(data = pts3, aes(x = x), y = 0.2, shape = "|", cex = 4) +
#'     xlab(expression(bold(s))) +
#'     ylab(expression(lambda(bold(s))))
#'   multiplot(p1a, p2a, cols = 1)
#' }
#' }
NULL
