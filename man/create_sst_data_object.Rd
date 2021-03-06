% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sst_data_class.R
\name{create_sst_data_object}
\alias{create_sst_data_object}
\title{Crate a sst_data object}
\usage{
create_sst_data_object(data, coords, B = NULL, varB = NULL)
}
\arguments{
\item{data}{a tibble of sst data}

\item{coords}{the coordinates of the data}

\item{B}{a matrix of expected bias (if included)}

\item{varB}{a matrix of bias variance (if included)}
}
\value{
Returns a sst_data object
}
\description{
This creates the object that holds the sst_data results, i.e., the Z
}
