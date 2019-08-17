# httpheader: used to cheat web server
header <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.94 Safari/537.36"

# http://www.sciencedirect.com/science/article/pii/S0034425701002310/pdfft?md5=2e6dfdaa5b680d49fbe09360b5bed6b4&pid=1-s2.0-S0034425701002310-main.pdf
# has an error: for above url

#' write_webfile
#' 
#' download web file through web link src
#' 
#' @param src download link
#' @param outdir output directory
#' @param file file name
#' @param ... other parameters to [httr::GET()]
#' @export
write_webfile <- function(src, outdir = "./", file = NULL, ...){
  # extract pdf filename from src, and combine with outdir
  if (is.null(file)) {
    # strs <- strsplit(src, "\\?")[[1]]
    # file <- str_extract(basename(strs[1]), ".*pdf$") %>% paste0(outdir, .)

    # match '-', 'alphabet and number', '\\.'
    file <- str_extract(src, "[-\\w\\.]*\\.pdf")[[1]][1] %>% paste0(outdir, .)
  }else{
    #make sure outdir is correct, the last character of outdir, should be '/'
    file <- paste0(outdir, basename(file)) 
  }
  
  # IF file exist then break out the function
  if (!file.exists(file)){
    tryCatch({
      GET(src, add_headers(`User-Agent` = header),
          write_disk(file, overwrite = TRUE), progress(), ...)
      cat("\n") #offset the deficiency of progress (without newline at the end)
    }, 
    error = function(e) {
      message(e)
      return(e)
    })
  }
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
write_urls <- function(urls, file){
  # data.table::fwrite(data.frame(urls), file, col.names = F)
  write.table(data.frame(urls), file, col.names = F, row.names = F, quote=F)
}

#' get DOIs from endnote export xml files
#' 
#' @param xmlfile Endnote exported xml file path, DOI information must be included.
#' @export
getDOIs_endnote <- function(xmlfile) {
    doc    <- read_xml(xmlfile)
    titles <- xml_find_all(doc, "//title") %>% xml_text()
    dois   <- xml_find_all(doc, "//electronic-resource-num") %>% xml_text() %>% 
        gsub("\r\n| ", "", .)

    data.frame(title = titles, DOI = dois, stringsAsFactors = F)#quickly return
}

#' check outdir and doi
#' 
#' Check whether outdir exist, if not then will create it. If doi was URLencoded, 
#' then decode it.
#' 
#' @inheritParams src_wiley_I
#' 
#' @return URLdecode doi 
#' @export
Init_Check <- function(doi, outdir){
  if (!dir.exists(outdir)) dir.create(outdir)
  sapply(doi, URLdecode, USE.NAMES = F)
}

# download_AMS_I <- function(doi, outdir, ...){
#   doi %<>% Init_Check(outdir)
#   url <- paste0("http://journals.ametsoc.org/doi/pdf/", doi)
  
#   file_pdf <- paste0(outdir, doi, ".pdf") 
#   write_webfile(src, file_pdf, ...)
# }

# downlaod paper from springer
# download_Springer_I <- function(doi, outdir, ...){
#   doi %<>% Init_Check(outdir)
#   src <- paste0("https://link.springer.com/content/pdf/", doi, ".pdf")

#   file_pdf <- paste0(outdir, doi, ".pdf") 
#   write_webfile(src, file_pdf, ...)
# }
