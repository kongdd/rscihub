src_wiley_I <- function(DOIs, outdir = "./", .download = TRUE, ...) {
    #   DOIs %<>% check_doi(outdir)
    # urls <- paste0("http://onlinelibrary.wiley.com/doi/", DOIs, "/pdf")
    # for every single url; modified to support batch download model
    temp = GET("http://onlinelibrary.wiley.com/")
    FUN <- function(doi) {
        tryCatch(
            {
                # p <- GET(doi, add_headers(`User-Agent` = header)) %>% content()
                # src <- xml_find_all(p, "//iframe[@id='pdfDocument']") %>% xml_attr("src")
                src <- url_wiley(doi)
                # file_pdf <- str_extract(src, ".*pdf") %>% basename %>% paste0(outdir, .)
                print(src)
                if (.download) write_file(src, ...)
                return(src)
            },
            error = function(e) {
                message(e)
                return("")
            }
        )
    }
    sapply(DOIs, FUN, USE.NAMES = FALSE) # return srcs
}
