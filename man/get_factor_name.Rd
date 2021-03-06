% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelc.r
\name{get_factor_name}
\alias{get_factor_name}
\title{Extract the factor name from an R model}
\usage{
get_factor_name(parameter, model)
}
\arguments{
\item{parameter}{A parameter name.}

\item{model}{A list with the same signature as the output of \code{lm} or \code{glm}}
}
\value{
A character string representing the factor name
}
\description{
Extract the factor name from an R model
}
