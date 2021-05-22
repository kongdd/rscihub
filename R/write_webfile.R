# httpheader: used to cheat web server
header <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.94 Safari/537.36"

#' write_webfile
#' 
#' download web file through web link src
#' 
#' @param src download link
#' @param outdir output directory
#' @param file file name
#' @param ... other parameters to [httr::GET()]
#' @export
write_webfile <- function(src, file = NULL, outdir = "./", overwrite = FALSE, ...){
    # extract pdf filename from src, and combine with outdir
    if (is.null(file)) file = guess_filename(src)
    file <- paste0(outdir, "/", basename(file))

    if (file.exists(file) && !overwrite) return()
    tryCatch({
        GET(src, add_headers(`User-Agent` = header),
            write_disk(file, overwrite = TRUE), progress(), ...)
        cat("\n") #offset the deficiency of progress (without newline at the end)
    }, 
    error = function(e) {
        message(e$message)
    })
}
