\name{get_CME_aggregate}
\alias{get_CME_aggregate}
\title{Compute regional child mortality aggregates}
\usage{
get_CME_aggregate(region_iso)
}
\arguments{
  \item{region_iso}{A \code{data.frame} or \code{data.table} containing at least
  \code{Region} and \code{ISO3Code}.}
}
\value{
  A \code{data.table} in long format with \code{Region}, \code{Region_Code} when
  available, \code{Shortind}, \code{Sex}, \code{Year}, and \code{Median}.
}
\description{
  Runs the packaged aggregate pipeline on an ad hoc region membership table and
  returns the same long-format output used by the app download.
}
