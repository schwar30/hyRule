% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf_functions.R
\name{get_in_between}
\alias{get_in_between}
\title{Gets list of indices between specified indicies}
\usage{
get_in_between(vector, indices, include_start = T, include_end = T)
}
\arguments{
\item{vector}{Desired vector to get}

\item{indices}{Specific indices to calculate in between values}

\item{include_start}{Includes the first index, even if not in the selected indices}

\item{include_end}{Includes the last index, even if not in the selected indices}
}
\description{
This is a function that will get all in between values of what needs to be a vector.
    Maybe at some point I'll figure out how to include other object types, but maybe that's not all that
    essential anyway.
}
\details{
The power of this function is getting sequencing between selected values. In the past,
    I've used this to sequence things such as pdf's which often have different titles that I either
    need to remove or append.

    Keep in mind that this currently only works with vectors and only exports lists of what the sequences
    actually are.
}
