% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mykrige.R
\name{myidw}
\alias{myidw}
\alias{myidw0}
\title{IDW interpolation wrapper}
\usage{
myidw(formula, data, nmax = Inf)

myidw0(formula, data, nmax = Inf)
}
\arguments{
\item{formula, data}{See \code{\link[gstat:krige]{gstat::idw()}}.}

\item{nmax}{See \code{\link[gstat:krige]{gstat::idw()}}}
}
\value{
\code{myidw()} returns a list with the formula and data, i.e. an object of class \code{myidw}.
}
\description{
IDW interpolation wrapper
}
\section{Functions}{
\itemize{
\item \code{myidw0}: IDW without trend variables
}}

