% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_file.R
\name{write_file}
\alias{write_file}
\title{write_file}
\usage{
write_file(src, file = NULL, outdir = "./", overwrite = FALSE, ...)
}
\arguments{
\item{src}{download link}

\item{file}{file name}

\item{outdir}{output directory}

\item{...}{other parameters to \code{\link[httr:GET]{httr::GET()}}}
}
\description{
download web file through web link src
}
