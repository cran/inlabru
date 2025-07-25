% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mappers.R
\name{bm_const}
\alias{bm_const}
\alias{bru_mapper_const}
\alias{ibm_n.bm_const}
\alias{ibm_values.bm_const}
\alias{ibm_jacobian.bm_const}
\alias{ibm_eval.bm_const}
\title{Constant mapper}
\usage{
bm_const()

bru_mapper_const()

\method{ibm_n}{bm_const}(mapper, ...)

\method{ibm_values}{bm_const}(mapper, ...)

\method{ibm_jacobian}{bm_const}(mapper, input, ...)

\method{ibm_eval}{bm_const}(mapper, input, state = NULL, ...)
}
\arguments{
\item{mapper}{A mapper S3 object, inheriting from \code{bru_mapper}.}

\item{...}{Arguments passed on to other methods}

\item{input}{Data input for the mapper.}

\item{state}{A vector of latent state values for the mapping,
of length \code{ibm_n(mapper, inla_f = FALSE)}}
}
\description{
Create a constant mapper
}
\examples{
m <- bm_const()
ibm_eval2(m, input = 1:4)

}
\seealso{
\link{bru_mapper}, \link{bru_mapper_generics}

Other mappers: 
\code{\link{bm_aggregate}()},
\code{\link{bm_collect}()},
\code{\link{bm_factor}()},
\code{\link{bm_fmesher}()},
\code{\link{bm_harmonics}()},
\code{\link{bm_index}()},
\code{\link{bm_linear}()},
\code{\link{bm_logsumexp}()},
\code{\link{bm_marginal}()},
\code{\link{bm_matrix}()},
\code{\link{bm_mesh_B}()},
\code{\link{bm_multi}()},
\code{\link{bm_pipe}()},
\code{\link{bm_repeat}()},
\code{\link{bm_scale}()},
\code{\link{bm_shift}()},
\code{\link{bm_sum}()},
\code{\link{bm_taylor}()},
\code{\link{bru_get_mapper}()},
\code{\link{bru_mapper}()},
\code{\link{bru_mapper.fm_mesh_1d}()},
\code{\link{bru_mapper.fm_mesh_2d}()},
\code{\link{bru_mapper_generics}}
}
\concept{mappers}
