# get refreshed download URL from SciDirect
getRefreshUrl_scidirect <- function(p) {
    tryCatch({
        xml_find_first(p, '//div[@id="redirect-message"]/p/a') %>% xml_attr("href")
    }, error = function(e) {
        NA
    })
}

.SciDirect <- function(url, type = c("url", "doi"), .download = TRUE, outdir, ...) {
    outfile = NULL
    tryCatch({
        if (type[1] == "doi") {
            outfile = paste0(url, ".pdf") %>% gsub("/", "-")
            p <- POST("https://doi.org/",
                encode = "form",
                body = list(hdl = url)
            ) %>% content(encoding = "UTF-8")

            url <- getRefreshUrl_DOI(p)
            if (!is.na(url)) {
                p <- GET(url) %>% content(encoding = "UTF-8")
            }
        } else if (type[1] == "url") {
            p <- GET(url) %>% content(encoding = "UTF-8")
        }

        json <- xml_find_first(p, "//script[@type='application/json']") %>% xml_text()

        if (is.na(json)) {
            src <- xml_find_first(p, "//a[@id='pdfLink']") %>% xml_attr("href")
            # or "//div[@class='PdfDropDownMenu']"
        } else {
            param <- fromJSON(json)$article$pdfDownload$urlMetadata %>% {
                c(.[-1], .$queryParams)
            }
            root <- "https://www.sciencedirect.com/science/article/pii"
            src <- with(param, glue::glue("{root}/{pii}{pdfExtension}?md5={md5}&pid={pid}"))
            if (is.null(outfile)) outfile = param$pid
        }

        ## 2. redirect by ScienceDirect
        p <- GET(src) %>% content(encoding = "UTF-8")
        src_new <- getRefreshUrl_scidirect(p)

        if (!is.na(src_new)) src <- src_new
        if (.download) write_webfile(src, outdir, outfile, ...)
        # src
    },
    error = function(e) {
        message(e)
    })
    invisible()
}

#' @rdname srcFUN
#' @export
src_scidirect <- function(DOIs, outdir = "./", .download = TRUE, ...) {
    DOIs %<>% check_doi(outdir)
    srcs <- character()
    for (i in seq_along(DOIs)) {
        cat(sprintf("[%d]: downloading %s\n", i, DOIs[i]))
        type = ifelse(is_httr(DOIs[i]), "url", "doi")
        srcs[i] <- .SciDirect(DOIs[i], type, .download, outdir, ...)
    }
    # return(srcs)
}
