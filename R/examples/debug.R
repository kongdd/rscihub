url <- "https://apps.webofknowledge.com/Search.do?product=WOS&SID=7CzWKRwuYwfJfIdZcEx&search_mode=GeneralSearch&prID=4918640a-06f1-4dec-a69a-516562448d69"
r <- GET(url) %>% content()
nodes <- xml_find_all(r, "//div[@class='search-results-content']")
df <- lapply(nodes, getInfo) %>% do.call(rbind, .)
