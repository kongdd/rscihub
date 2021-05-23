
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

param <- list(
    fieldCount                       = "2",
    action                           = "search",
    product                          = "WOS",
    search_mode                      = "GeneralSearch",
    SID                              = "8D7XNKLinyEE2TbhQgO",
    max_field_count                  = "25",
    max_field_notice                 = "注意:+无法添加另一字段。",
    input_invalid_notice             = "检索错误:+请输入检索词。",
    exp_notice                       = "检索错误:+专利检索词可以在多个家族中找到+(",
    input_invalid_notice_limits      = "+<br/>注意:+滚动框中显示的字段必须至少与一个其他检索字段相组配。",
    sa_params                        = "WOS||8D7XNKLinyEE2TbhQgO|http://apps.webofknowledge.com|'",
    formUpdated                      = "true",
    `value(input1)`                  = "aerosol",
    `value(select1)`                 = "TS",
    `value(hidInput1)`               = "",
    `value(bool_1_2)`                = "AND",
    `value(input2)`                  = "evapotranspiration",
    `value(select2)`                 = "TS",
    `value(hidInput2)`               = "",
    limitStatus                      = "collapsed",
    ss_lemmatization                 = "On",
    ss_spellchecking                 = "Suggest",
    SinceLastVisit_UTC               = "",
    SinceLastVisit_DATE              = "",
    period                           = "Range+Selection",
    range                            = "ALL",
    startYear                        = "1982",
    endYear                          = "2021",
    editions                         = "SCI",
    editions                         = "SSCI",
    editions                         = "ISTP",
    editions                         = "CCR",
    editions                         = "IC",
    update_back2search_link_param    = "yes",
    ssStatus                         = "display:none",
    ss_showsuggestions               = "ON",
    ss_numDefaultGeneralSearchFields = "1",
    ss_query_language                = "",
    rs_sort_by                       = "PY.D;LD.D;SO.A;VL.D;PG.A;AU.A"
)
