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
#' @example R/examples/ex-scihub.R
#' @export
scihub <- function(doi, outdir = ".", overwrite = FALSE, ...) {
    param <- parse_doi(doi)
    server <- param$server

    if (server == "www.sciencedirect.com") {
        src_scidirect(param$content, param$outfile, outdir, overwrite = overwrite, ...)
    } else if (server == "agupubs.onlinelibrary.wiley.com") {
        src_wiley(doi, param$outfile, outdir, overwrite = overwrite, ...)
    } else {
        message("not support")
    }
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
