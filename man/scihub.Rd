% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scihub.R
\name{scihub}
\alias{scihub}
\title{download pdf by doi}
\usage{
scihub(doi, outdir = ".", overwrite = FALSE, col = "DOI", return = FALSE, ...)
}
\arguments{
\item{doi}{Character, Digital Object Identifier, like
"10.1175/JHM-D-15-0157.1", URLencoding format is also supported, i.e.
"10.1175\\%2FJHM-D-15-0157.1".
Based on doi, \code{srcFUN} find corresponding paper and download it.}

\item{outdir}{Output file directory}

\item{...}{other parameters pass to \code{\link[httr:GET]{httr::GET()}}}

\item{.download}{If true, it will will download pdf directly, and return
pdf src. If false, only pdf src returned, without downlaoding pdf.}
}
\description{
download pdf by doi
}
\details{
supported:
\itemize{
\item www.sciencedirect.com
\item agupubs.onlinelibrary.wiley.com
\item link.springer.com
\item nature.com
\item iopscience.iop.org
\item journals.ametsoc.org
\item www.hydrol-earth-syst-sci.net
\item scihub.do
}
}
\examples{
\dontrun{
src_wiley("10.1002/2016WR020175", overwrite = T)
scihub("10.1016/j.scitotenv.2021.145381", overwrite = T) # scidirect
scihub("10.1002/2016WR020175", overwrite = T)            # wiley

scihub("10.3390/rs13071263")         # mpdi
scihub("10.1007/s00704-019-02913-w") # springer
scihub("10.1038/s41467-020-14688-0") # nature
}
}
