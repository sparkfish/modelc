% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelc.r
\name{apply_linkinverse}
\alias{apply_linkinverse}
\title{Wrap the model SQL in the appropriate link function inverse to return scaled predictions}
\usage{
apply_linkinverse(model, sql)
}
\arguments{
\item{model}{A list with the same signature as the output of \code{lm} or \code{glm}}

\item{sql}{A character string representing the SQL to be wrapped in the link inverse}
}
\value{
A character string representing a SQL model formula
}
\description{
Wrap the model SQL in the appropriate link function inverse to return scaled predictions
}
