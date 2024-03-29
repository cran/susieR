% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/susie_trendfilter.R
\name{susie_trendfilter}
\alias{susie_trendfilter}
\title{Apply susie to trend filtering (especially changepoint
  problems), a type of non-parametric regression.}
\usage{
susie_trendfilter(y, order = 0, standardize = FALSE, use_mad = TRUE, ...)
}
\arguments{
\item{y}{An n-vector of observations ordered in time or space
(assumed to be equally spaced).}

\item{order}{An integer specifying the order of trend filtering.
The default, \code{order = 0}, corresponds to "changepoint"
problems (\emph{i.e.}, piecewise constant \eqn{mu}). Although
\code{order > 0} is implemented, we do not recommend its use; in
practice, we have found problems with convergence of the algorithm
to poor local optima, producing unreliable inferences.}

\item{standardize}{Logical indicating whether to standardize the X
variables ("basis functions"); \code{standardize = FALSE} is
recommended as these basis functions already have a natural scale.}

\item{use_mad}{Logical indicating whether to use the "median
absolute deviation" (MAD) method to the estimate residual
variance. If \code{use_mad = TRUE}, susie is run twice, first by
fixing the residual variance to the MAD value, then a second time,
initialized to the first fit, but with residual variance estimated
the usual way (by maximizing the ELBO). We have found this strategy
typically improves reliability of the results by reducing a
tendency to converge to poor local optima of the ELBO.}

\item{...}{Other arguments passed to \code{\link{susie}}.}
}
\value{
A "susie" fit; see \code{\link{susie}} for details.
}
\description{
Fits the non-parametric Gaussian regression model
  \eqn{y = mu + e}, where the mean \eqn{mu} is modelled as \eqn{mu =
  Xb}, X is a matrix with columns containing an appropriate basis,
  and b is vector with a (sparse) SuSiE prior. In particular, when
  \code{order = 0}, the jth column of X is a vector with the first j
  elements equal to zero, and the remaining elements equal to 1, so
  that \eqn{b_j} corresponds to the change in the mean of y between
  indices j and j+1. For background on trend filtering, see
  Tibshirani (2014). See also the "Trend filtering" vignette,
  \code{vignette("trend_filtering")}.
}
\details{
This implementation exploits the special structure of X,
  which means that the matrix-vector product \eqn{X^Ty} is fast to
  compute; in particular, the computation time is \eqn{O(n)} rather
  than \eqn{O(n^2)} if \code{X} were formed explicitly. For
  implementation details, see the "Implementation of SuSiE trend
  filtering" vignette by running
  \code{vignette("trendfiltering_derivations")}.
}
\examples{
set.seed(1)
mu = c(rep(0,50),rep(1,50),rep(3,50),rep(-2,50),rep(0,200))
y = mu + rnorm(400)
s = susie_trendfilter(y)
plot(y)
lines(mu,col = 1,lwd = 3)
lines(predict(s),col = 2,lwd = 2)

# Calculate credible sets (indices of y that occur just before
# changepoints).
susie_get_cs(s)

# Plot with credible sets for changepoints.
susie_plot_changepoint(s,y) 

}
\references{
R. J. Tibshirani (2014). Adaptive piecewise polynomial
  estimation via trend filtering. \emph{Annals of Statistics}
  \bold{42}, 285-323.
}
