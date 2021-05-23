#' download using aria2
#'
#' download papers batchly using aria2. `srcFUN` function is used
#' to generate pdf download links. You can also construct src function personally
#' to support more.
#'
#' @inheritParams srcFUN
#' @param outdir outdir name, string used to make new directory to save papers
#' @param srcFUN function used to generate pdf download links according to DOIs.
#' If srcFUN is null, then this function will treat input parameter DOIs as download urls.
#' @param n maximum number of parallel downloads. We used aria2c to download
#' papers batchly. In this function we set --max-concurrent-downloads, --split and
#' --max-connection-per-server all equal to n. Detailed information about this
#' parameters could be find in aria2c document <https://aria2.github.io/manual/en/html/>.
#' @param Rshell whether execute aria2c command in R shell. If false, it will save
#' command string to clipboard, and then you can paste this command in cmd OR other
#' shells which can execute aria2c command
#' @param ... other parameter pass to srcFUN.
#'
#' @author Dongdong Kong <kongdd@live.cn>
#'
#' @examples
#' # First, you need to get doi;
#' # Second, you need to select the suitable srcFUN of corresponding database. If
#' #   this package have not yet, you can consider to extend the srcFUN, or
#' #   contact me directly.
#' \dontrun{
#' DOIs <- rep("10.1175%2FJHM-D-15-0157.1", 4) # test aria2 parallel download
#' download_aria2(DOIs, outdir = "JHM", srcFUN = src_AMS, n = 4, Rshell = TRUE)
#' }
#' @export
download_aria2 <- function(DOIs, srcFUN = NULL, outdir = ".", n = 8, Rshell = FALSE, ...) {
    # if (!dir.exists(outdir)) dir.create(outdir)
    DOIs <- check_doi(DOIs, outdir = outdir)
    # Convert DOIs to pdf download urls using srcFUN. If srcFUN is NULL, then set urls
    #   equal to DOIs
    if (is.null(srcFUN)) {
        urls <- DOIs
    } else {
        urls <- srcFUN(DOIs, ...)
    }

    write_urls(urls, paste0(outdir, ".txt"))

    # --header "%s"
    cmd <- sprintf("aria2c -x%d -s%d -j%d -k1M -c -i %s.txt -d %s", n, n, n, outdir, outdir)
    if (.Platform$OS.type == "windows") writeLines(cmd, "clipboard")
    if (Rshell) system(cmd)
}
