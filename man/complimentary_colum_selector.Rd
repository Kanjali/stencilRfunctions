% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlPipelineAndAlgorithmFunctions.R
\name{complimentary_colum_selector}
\alias{complimentary_colum_selector}
\title{Used to get the columns which need to be predict during algorithm runs}
\usage{
complimentary_colum_selector(data, columns)
}
\arguments{
\item{data}{is a dataframe}

\item{columns}{unnecessary columns vector}
}
\value{
ReqColumns a vector having columns which need to be predicted
}
\description{
Used to get the columns which need to be predict during algorithm runs
}
