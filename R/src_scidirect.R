# get refreshed download URL from SciDirect
getRefreshUrl_scidirect <- function(p) {
    tryCatch({
        content(p, encoding = "UTF-8") %>% 
            xml_find_first('//div[@id="redirect-message"]/p/a') %>% xml_attr("href")
    }, error = function(e) {
        p$url
    })
}

url_scidirect <- function(x) {
    if (is.character(x)) {
        if (is_httr(x)) {
            p <- GET(x) %>% content(encoding = "UTF-8") # if url
        } else {
            p <- parse_doi(x)$content # if doi
        }
    } else p <- x
    json <- xml_find_first(p, "//script[@type='application/json']") %>% xml_text()

    if (is.na(json)) {
        # "//div[@class='PdfDropDownMenu']"
        src <- xml_find_first(p, "//a[@id='pdfLink']") %>% xml_attr("href")
    } else {
        param <- fromJSON(json)$article$pdfDownload$urlMetadata %>% {
            c(.[-1], .$queryParams)
        }
        root <- "https://www.sciencedirect.com/science/article/pii"
        src <- with(param, glue::glue("{root}/{pii}{pdfExtension}?md5={md5}&pid={pid}"))
        # if (is.null(outfile)) outfile = param$pid
    }
    ## 2. redirect by ScienceDirect
    p <- GET(src)
    src <- getRefreshUrl_scidirect(p)
    src
    # write_file(src, outfile, outdir, ...)
}

# #' @rdname srcFUN
# #' @export
# src_scidirect <- function(DOIs, outdir = "./", .download = TRUE, ...) {
#     DOIs %<>% check_doi(outdir)
#     srcs <- character()
#     for (i in seq_along(DOIs)) {
#         cat(sprintf("[%d]: downloading %s\n", i, DOIs[i]))
#         type = ifelse(is_httr(DOIs[i]), "url", "doi")
#         .SciDirect(DOIs[i], type, .download, outdir, ...)
#     }
#     # return(srcs)
# }
