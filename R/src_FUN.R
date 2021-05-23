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
#'  all in the database. 
#' 
#' If just src returned, you need to download with [download_httr()]
#' 
#' @param doi Character, Digital Object Identifier, like
#' "10.1175/JHM-D-15-0157.1", URLencoding format is also supported, i.e.
#' "10.1175\%2FJHM-D-15-0157.1".
#' Based on doi, `srcFUN` find corresponding paper and download it.
#' @param outdir Output directory
#' @param .download If true, it will will download pdf directly, and return
#' pdf src. If false, only pdf src returned, without downlaoding pdf.
#' @param ... other parameters pass to [httr::GET()]
NULL

#' @export
url_scihub <- function(doi, scihub = "https://sci-hub.do") {
    url <- sprintf("%s/%s", scihub, doi)
    p <- GET(url) %>% content()
    xml_find_first(p, "//div[@id='article']/iframe") %>%
        xml_attr("src") %>%
        gsub("^//", "https://", .)
}

#' @rdname srcFUN
#' @export
url_AMS <- function(doi) {
    paste0("http://journals.ametsoc.org/doi/pdf/", doi)
}


#' @rdname srcFUN
#' @export
url_IOP <- function(doi) {
    paste0("http://iopscience.iop.org/article/", doi, "/pdf")
}

#' @rdname srcFUN
#' @export
url_hess <- function(doi) {
    # doi %<>% {strsplit(., "/")[[1]][2]}
    doi <- strsplit(doi, "/") %>% laply(function(x) x[2])
    # replaceString <- strsplit(doi[1], "/")[[1]][1]
    #  	doi %<>% gsub(paste0(replaceString, "/"), "", .)
    paste0(
        "http://www.hydrol-earth-syst-sci.net/",
        gsub("hess-", "", doi) %>% gsub("-", "/", .), "/",
        doi, ".pdf"
    )
}

#' @rdname srcFUN
#' @export
url_springer <- function(doi) {
    paste0("https://link.springer.com/content/pdf/", doi, ".pdf")
}

#' @rdname srcFUN
#' @export
url_nature <- function(doi) {
    file <- str_extract(doi, "(?<=/).*")
    paste0("https://www.nature.com/articles/", file, ".pdf")
}

#' @import glue
#' @export
url_wiley <- function(doi) {
    sprintf("https://agupubs.onlinelibrary.wiley.com/doi/pdfdirect/%s?download=true", doi)
}
