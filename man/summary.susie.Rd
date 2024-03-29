% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.susie.R
\name{summary.susie}
\alias{summary.susie}
\alias{print.summary.susie}
\title{Summarize Susie Fit.}
\usage{
\method{summary}{susie}(object, ...)

\method{print}{summary.susie}(x, ...)
}
\arguments{
\item{object}{A susie fit.}

\item{\dots}{Additional arguments passed to the generic \code{summary}
or \code{print.summary} method.}

\item{x}{A susie summary.}
}
\value{
\code{summary.susie} returns a list containing a data frame
  of variables and a data frame of credible sets.
}
\description{
\code{summary} method for the \dQuote{susie} class.
}
