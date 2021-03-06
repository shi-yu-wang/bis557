% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge_reg.R
\name{ridge_reg}
\alias{ridge_reg}
\title{Fit a ridge regression model}
\usage{
ridge_reg(formula, lambda, data)
}
\arguments{
\item{formula}{a formula}

\item{lambda}{a numeric parameter}

\item{data}{a data.frame}
}
\value{
An ridge_reg object
}
\description{
This function passes parameters to the ridge_reg function.
}
\examples{
fit <- ridge_reg(Sepal.Length ~., 1.5,iris)
}
