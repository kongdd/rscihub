
getInfo <- function(x) {
    doi <- xml_find_all(x, "div/div[3]/span[@name='doi']") %>% xml_text()
    journal <- xml_find_all(x, "div/div[3]/span[2]/a/span/value") %>% xml_text()
    title <- xml_find_first(x, "div/div[1]/div/a/value") %>% xml_text()
    author <- xml_find_all(x, "div/div[2]") %>%
        xml_text() %>%
        gsub("\n|作者: ", "", .)
    date <- xml_find_all(x, "div/div[3]/span[@class='data_bold'][last()]/value") %>%
        xml_text() %>%
        str_extract("\\d{4}")
    # date = xml_find_all(x, "div/div[3]/span[11]/value") %>% xml_text()
    listk(author, date, journal, title, doi) %>%
        check_null() %>%
        as.data.frame()
}

check_null <- function(x) {
    id_null <- which(sapply(x, function(x) {
        is.null(x) || length(x) == 0
    }))
    x[id_null] <- NA
    x
}
