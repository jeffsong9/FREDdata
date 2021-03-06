% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{closest_date}
\alias{closest_date}
\title{Obtain the closest date entry in the FRED data set}
\usage{
closest_date(x)
}
\arguments{
\item{x}{Initial date that one wishes to match with the closest date on FRED data set. \code{x} must be in ymd format, e.g., "1988-09-23".}
}
\value{
Return the FRED data with the date that is closest to the initial date \code{x}.
}
\description{
Obtain the closest date entry in the FRED data set
}
\examples{
convert_values("2017-03-30", 100, "1988-09-23")
}
