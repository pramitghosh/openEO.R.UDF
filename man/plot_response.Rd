% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{plot_response}
\alias{plot_response}
\title{Plots stars object from POST response}
\usage{
plot_response(json_out, method, file = FALSE)
}
\arguments{
\item{json_out}{String representing JSON or path to JSON}

\item{method}{Either "raw" or "binary" depending on which endpoint the response is of}

\item{file}{Logical. Needs to be TRUE if \code{json_out} represents the path to a JSON file}
}
\description{
Plots stars object from POST response
}
