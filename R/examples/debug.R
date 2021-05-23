url <- "https://apps.webofknowledge.com/Search.do?product=WOS"
r <- GET(url) %>% content()
write_html(r, "a.html")

nodes <- xml_find_all(r, "//div[@class='search-results-content']")
df <- lapply(nodes, getInfo) %>% do.call(rbind, .)


p <- POST("http://apps.webofknowledge.com/WOS_GeneralSearch.do", body = param)

p <- GET("https://www.webofscience.com/wos/woscc/advanced-search")
