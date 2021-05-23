#' check outdir and doi
#'
#' Check whether outdir exist, if not then will create it. If doi was URLencoded,
#' then decode it.
#'
#' @inheritParams srcFUN
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

#' guess_filename
#' @examples
#' guess_filename("https://agupubs.onlinelibrary.wiley.com/doi/pdfdirect/10.1002/2016WR020175?download=true")
#' 
#' @export
guess_filename <- function(x) {
    file = str_extract(x, "[-\\w\\.]*\\.pdf")[[1]][1]
    if (is.na(file)) {
        # for wiley
        file = parse_url(x)$path %>%
            gsub("doi/|pdfdirect/", "", .) %>%
            gsub("/", "-", .) %>% paste0(".pdf")
    }
    file
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

listk <- function (...) 
{
    cols <- as.list(substitute(list(...)))[-1]
    vars <- names(cols)
    Id_noname <- if (is.null(vars)) 
        seq_along(cols)
    else which(vars == "")
    if (length(Id_noname) > 0) 
        vars[Id_noname] <- sapply(cols[Id_noname], deparse)
    x <- setNames(list(...), vars)
    return(x)
}

`%!in%` <- function(x, y) !(`%in%`(x, y))
