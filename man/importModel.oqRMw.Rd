% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importModel.oqRMw.R
\name{importModel.oqRMw}
\alias{importModel.oqRMw}
\title{Import disaggregation hazard (R-MW-p) from openquake}
\usage{
importModel.oqRMw(path, ITo, vref)
}
\arguments{
\item{path}{Path to the folder containing the hazard and quantile curves}

\item{ITo}{Numeric. Investigation Time}

\item{vref}{Numeric. Reference Vs30 in m/s}
}
\value{
A data.table with the following columns:
}
\description{
Import disaggregation hazard (R-MW-p) from openquake
}
