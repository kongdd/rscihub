#' check outdir and doi
#'
#' Check whether outdir exist, if not then will create it. If doi was URLencoded,
#' then decode it.
#'
#' @inheritParams src_wiley_I
#'
#' @return URLdecode doi
#' @export
check_doi <- function(doi, outdir) {
    if (!dir.exists(outdir)) dir.create(outdir)
    sapply(doi, URLdecode, USE.NAMES = F)
}

is_httr <- function(x) {
    substr(x, 1, 4) == "http"
}

#' write_urls
#'
#' write character vectors of urls into text file, in order to the use of
#' subsequent aria2 download
#'
#' @param urls pdf downloading urls.
#' @param file urls are written into file.
#'
#' @importFrom utils URLdecode write.table
#' @export
write_urls <- function(urls, file) {
    # data.table::fwrite(data.frame(urls), file, col.names = F)
    write.table(data.frame(urls), file, col.names = F, row.names = F, quote = F)
}

#' get DOIs from endnote export xml files
#'
#' @param xmlfile Endnote exported xml file path, DOI information must be included.
#' @export
getDOIs_endnote <- function(xmlfile) {
    doc <- read_xml(xmlfile)
    titles <- xml_find_all(doc, "//title") %>% xml_text()
    dois <- xml_find_all(doc, "//electronic-resource-num") %>%
        xml_text() %>%
        gsub("\r\n| ", "", .)

    data.frame(title = titles, DOI = dois, stringsAsFactors = F) # quickly return
}
