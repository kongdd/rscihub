#' @param url pdf url
#' @param urls pdf urls 
#' @rdname srcFUN
#' @export
src_URL <- function(url) url

#' @title srcFUN
#' @name srcFUN
#' 
#' @description
#' *  `src_URL` simplest srcFUN, just treat the input url as download link.
#' *  `src_wiley` wiley library. Journal like GRL, JGR, WRR, HP, 
#'  all in the database. Compared with other srcFUNs, this one is quite 
#'  complicated. It relies on previous web page identidy authentication. Hence, 
#'  it can't download simply by pdf urls, likes other database.   
#' *  `src_SciDirect.doi` access elsevier database trough doi.
#' *  `src_SciDirect.url` access elsevier database trough url.
#' *  `src_AMS` American Meteorological Society.
#' *  `src_Springer` Springer.
#' *  `src_SciReps` Scientific Reports.
#' *  `src_IOP` IOPscience database.
#' *  `src_hess` HESS.
#' *  `src_SciReps` Scientific Reports.
#' 
#' If just src returned, you need to download with [download_httr()]
#' 
#' @param doi Character, Digital Object Identifier, like 
#' "10.1175/JHM-D-15-0157.1", URLencoding format is also supported, i.e. 
#' "10.1175\%2FJHM-D-15-0157.1".  
#' Based on doi, `srcFUN` find corresponding paper and download it. 
#' @param DOIs Character vectors, multiple `doi`.
#' @param outdir Output file directory
#' @param .download If true, it will will download pdf directly, and return 
#' pdf src. If false, only pdf src returned, without downlaoding pdf.
#' @param ... other parameters pass to [httr::GET()]
NULL

#' @rdname srcFUN
#' @export
src_AMS <- function(doi){
  paste0("http://journals.ametsoc.org/doi/pdf/", doi)
}

#' @rdname srcFUN
#' @export
src_Springer <- function(doi){
  paste0("https://link.springer.com/content/pdf/", doi, ".pdf")
}

#' @rdname srcFUN
#' @export
src_SciReps <- function(doi){
  gsub("10.1038/", "", doi) %>% 
    paste0("https://www.nature.com/articles/", ., ".pdf")
} 

#' @rdname srcFUN
#' @export
src_IOP <- function(doi){
  paste0("http://iopscience.iop.org/article/", doi, "/pdf")
}

#' @rdname srcFUN
#' @export
src_hess <- function(doi){
  # doi %<>% {strsplit(., "/")[[1]][2]}
  doi <- strsplit(doi, "/") %>% laply(function(x) x[2])
  # replaceString <- strsplit(doi[1], "/")[[1]][1]
  #  	doi %<>% gsub(paste0(replaceString, "/"), "", .)
  paste0("http://www.hydrol-earth-syst-sci.net/", 
         gsub("hess-", "", doi) %>% gsub("-", "/", .), "/", 
         doi, ".pdf")
}
