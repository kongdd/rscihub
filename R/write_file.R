# httpheader: used to cheat web server
header <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0"

#' write_file
#' 
#' download web file through web link src
#' 
#' @param src download link
#' @param outdir output directory
#' @param file file name
#' @param ... other parameters to [httr::GET()]
#' @export
write_file <- function(src, file = NULL, outdir = "./", overwrite = TRUE, ...){
    # extract pdf filename from src, and combine with outdir
    if (is.null(file)) file = guess_filename(src)
    file <- paste0(outdir, "/", basename(file))

    if (file.exists(file) && !overwrite) return()
    tryCatch({
        p <- GET(src, add_headers(`User-Agent` = header),
            write_disk(file, overwrite = TRUE), progress(), ...); cat("\n")
        # if not success
        content_type = p$header$`content-type`
        if (grepl("html", content_type)) {
            file_new = gsub(".pdf$", ".html", file)
            file.rename(file, file_new)
            message(sprintf("[e] '%s' failed, pls check!", src))
        }
        #offset the deficiency of progress (without newline at the end)
        TRUE
    }, error = function(e) {
        message(e$message)
        FALSE
    })
}
