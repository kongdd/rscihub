#' download using httr
#'
#' Download using httr package by hadley
#'
#' @inheritParams download_aria2
#'
#' @examples
#' # First, you need to get doi;
#' # Second, you need to select the suitable srcFUN of corresponding database. If
#' #   this package have not yet, you can consider to extend the srcFUN, or
#' #   contact me directly.
#' \dontrun{
#' download_httr("10.1175%2FJHM-D-15-0157.1", outdir = ".", srcFUN = src_AMS)
#' }
#' @export
download_httr <- function(DOIs, srcFUN = NULL, outdir = ".", ...) {
    # if (!dir.exists(outdir)) dir.create(outdir)
    DOIs <- check_doi(DOIs, outdir = outdir)
    # Convert DOIs to pdf download urls using srcFUN. If srcFUN is NULL, then set urls
    #   equal to DOIs
    if (is.null(srcFUN)) srcFUN <- src_URL

    for (i in seq_along(DOIs)) {
        cat(sprintf("[%d]: downloading %s\n", i, DOIs[i]))
        src <- srcFUN(DOIs[i], ...)
        write_file(src, outdir = paste0(outdir, "/"))
    }
}
