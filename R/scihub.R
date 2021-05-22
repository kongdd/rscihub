#' download pdf by doi
#'
#' @param doi Character, Digital Object Identifier, like
#' "10.1175/JHM-D-15-0157.1", URLencoding format is also supported, i.e.
#' "10.1175\%2FJHM-D-15-0157.1".
#' Based on doi, `srcFUN` find corresponding paper and download it.
#' @param outdir Output file directory
#' @param .download If true, it will will download pdf directly, and return
#' pdf src. If false, only pdf src returned, without downlaoding pdf.
#' @param ... other parameters pass to [httr::GET()]
#' 
#' @details supported:
#' - www.sciencedirect.com
#' - agupubs.onlinelibrary.wiley.com
#' - link.springer.com
#' - nature.com
#' - iopscience.iop.org
#' - journals.ametsoc.org
#' - www.hydrol-earth-syst-sci.net
#' 
#' @example R/examples/ex-scihub.R
#' @export
scihub <- function(doi, outdir = ".", overwrite = TRUE, ...) {
    param <- parse_doi(doi)
    server <- param$server

    if (server == "www.sciencedirect.com") {
        url <- url_scidirect(param$content)
    } else if (server == "agupubs.onlinelibrary.wiley.com") {
        url <- url_wiley(doi)
    } else if (server == "www.mdpi.com") {
        url <- paste0(param$url, "/pdf")
    } else if (server == "link.springer.com") {
        url <- url_springer(doi)
    } else if (server == "www.nature.com") {
        url <- url_nature(doi)
    } else if (server == "iopscience.iop.org") {
        url <- url_IOP(doi)
    } else if (server == "journals.ametsoc.org") {
        url <- url_AMS(doi)
    } else if (server == "www.hydrol-earth-syst-sci.net") {
        url <- url_hess(doi)
    } else {
        stop(sprintf("not support: %s", server))
    }
    write_webfile(url, param$outfile, outdir, overwrite = overwrite, ...)
}

## 1. redirect by DOI: "//meta[@http-equiv='REFRESH']"
# get refreshed URL from https://doi.org/
getRefreshUrl_DOI <- function(p){
    tryCatch({
        url = content(p, encoding = "UTF-8") %>% 
            xml_find_first("//meta[@http-equiv='REFRESH']") %>% xml_attr("content") %>% 
            str_extract("(?<=Redirect=).*(?='$)") %>% URLdecode()
        if (url == "NA") return(NA_character_)
        url
    }, error = function(e) {
        NA_character_
    })
}

#' @export
parse_doi <- function(doi) {
    p <- POST("https://doi.org/",
        encode = "form",
        body = list(hdl = doi)
    )
    url <- getRefreshUrl_DOI(p)
    if (!is.na(url)) p <- GET(url) 
    
    listk(doi, 
        server = url_parse(p$url)$server,
        url = p$url,
        outfile = paste0(doi, ".pdf") %>% gsub("/", "-", .),
        content = p %>% content(encoding = "UTF-8"))
}
