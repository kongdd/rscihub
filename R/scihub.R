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
#' - scihub.do
#' 
#' @example R/examples/ex-scihub.R
#' @importFrom readxl read_xls
#' @importFrom stringr str_replace_all
#' @import future
#' @export
scihub <- function(doi, outdir = ".", overwrite = FALSE, col = "DOI", return = FALSE, ...) {
    if (grepl("*.xls$", doi[1])) doi = read_xls(doi[1])[[col]]
    # plan(multisession)
    ans <- sapply(doi, scihub_one, outdir = outdir, overwrite = overwrite, return = return, ...)
    if (return) ans else invisible() 
}

journal_miss <- c("10.1111")

scihub_one <- function(doi, outdir = ".", overwrite = FALSE, return = FALSE, ...) {
    if (is.na(doi)) return()
    doi %<>% URLencode(reserved = TRUE)
    outfile = gsub("/", "-", doi) %>% paste0(outdir, "/", ., ".pdf")
    if (file.exists(outfile) && !overwrite) return()
    
    param <- parse_doi(doi)
    server <- param$server

    tryCatch({
        if (dirname(doi) %!in% journal_miss) {
            if (server == "www.sciencedirect.com") {
                url <- url_scidirect(param$content)
            } else if (grepl("onlinelibrary.wiley.com", server)) {
                url <- param$url %>%
                    gsub("abs/|full/", "pdfdirect/", .) %>%
                    paste0("?download=true")
            } else if (grepl("ieee.org", server)) {
                url <- basename(param$url) %>%
                    paste0("https://ieeexplore.ieee.org/stampPDF/getPDF.jsp?tp=&arnumber=", .)
            } else if (server == "www.mdpi.com") {
                url <- paste0(param$url, "/pdf")
            } else if (server == "link.springer.com") {
                url <- url_springer(doi)
            } else if (server == "www.nature.com") {
                url <- url_nature(doi)
            } else if (server == "iopscience.iop.org") {
                url <- url_IOP(doi)
            } else if (server == "journals.ametsoc.org") {
                url <- param$url %>% gsub(".xml$", ".pdf", .)
                # url <- url_AMS(doi)
            } else if (server == "www.hydrol-earth-syst-sci.net") {
                url <- url_hess(doi)
            } else if (server == "www.pnas.org") {
                url <- param$url %>%
                    paste0(".pdf") %>%
                    str_replace_all(c("content" = "content/pnas"))
            } else url <- url_scihub(doi)
        } else {
            url <- url_scihub(doi)
        }

        write_webfile(url, param$outfile, outdir, overwrite = overwrite, ...)
        if (return) return(url)
    }, error = function(e) {
        message(sprintf('%s', e$message))
        message(sprintf("[e] not support: %s, doi = %s\n", server, doi))
        # second chance
        url <- url_scihub(doi)
        status = write_webfile(url, param$outfile, outdir, overwrite = overwrite, ...)        
    })
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
    tryCatch({
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
            content = p %>% content(encoding = "UTF-8")
        )
    }, error = function(e) {
        message(sprintf('%s', e$message))
        NULL
    })
}
