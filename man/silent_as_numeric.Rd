% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general.R
\name{silent_as_numeric}
\alias{silent_as_numeric}
\title{Converts to Numeric}
\usage{
silent_as_numeric(x)
}
\arguments{
\item{x}{A value or string to be converted to numeric}
}
\description{
This function converts a value into class numeric. The main difference 
    between this function and the standard [as.numeric] function is that this function
    suppresses the warning associated with values being converted into NA.
}
\details{
RSelenium often crashes when there is an incorrect chrome association, so this
is a great helper function that allows us to more quickly and accurately guess chrome
versions without too much pain.
}
