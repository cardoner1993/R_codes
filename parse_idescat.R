install.packages("")

require(XML)



airline = 'https://www.idescat.cat/emex/?id=259046#h7fffffffffff'
airline.table = readHTMLTable(airline, header=T, which=1,stringsAsFactors=F)


?readHTMLTable

tables <- getNodeSet(htmlParse(airline), "//table")

?htmlParse


library("rvest")

url <- "http://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population"
population <- airline %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="t81"]/tfoot') %>%
  html_table()

population <- population[[1]]

html
?html

airline %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="t81"]/tbody/tr[2]/td[1]') %>% 
  html_table(fill=TRUE)
  

?html_nodes
  



 ff<- 'https://www.idescat.cat/emex/?id=259046#h7fffffffffff'

read_html %>%
  xml_find_all('td') %>%
  html_text()

?html_nodes


lego_movie <- read_html("http://www.imdb.com/title/tt1490017/",encoding = 'UTF-8')
ff %>%
  xml_node("table") %>%
  html_text() %>%
  as.numeric()



library(XML)

url <- "http://www.bls.gov/web/empsit/cesbmart.htm"

# read in HTML data
tbls_xml <- readHTMLTable(ff)

typeof(tbls_xml)
## [1] "list"

length(tbls_xml)


library(RCurl)

# parse url
url_parsed <- htmlParse(getURL(ff), asText = TRUE,encoding = 'UTF-8')



tableNodes <- getNodeSet(url_parsed, c('//*[@id="g168"]'))

bls_table2 <- readHTMLTable(tableNodes[[1]])


require(dplyr)

names(bls_table2)[1] <- "atributs" 
bls_table_final <- bls_table2 %>% filter(!is.na(Catalunya))

head(bls_table_final)

dim(bls_table_final)


municipis <- c('170209','170216')

output <- list()
for ( i in 1:length(municipis)) {
  # parse url
  url_parsed <- htmlParse(getURL(municipis[i]), asText = TRUE,encoding = 'UTF-8')
  
}


