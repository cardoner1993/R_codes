# Exists a nice package for make change in .js
# look pluralize.js => only english version 

substitute.all <- function(pattern, replacement, x) {
  idx <- match(x, pattern)
  return(ifelse(is.na(idx), x, replacement[idx]))
}

## BCN open data 
## some rows of textdata
require(tidyr)

canvis <- c("espais","publics")
canvi <- c("espai","public")

bigram_filtered <- textdata[21:30,] %>%
  
  mutate(TEXT = iconv(TEXT,from="UTF-8",to="ASCII//TRANSLIT")) %>% 
  unnest_tokens(bigram, TEXT, token= "ngrams", n=2) %>%
  separate(bigram, c("word1","word2"), sep=" ") %>%
  mutate(word1=substitute.all(canvis,canvi,word1),
         word2=substitute.all(canvis,canvi,word2)) %>% 
  # mutate(word1=wordStem(word1),
  #        word2=wordStem(word2)
  filter(!word1 %in% stop$word,
         !word2 %in% stop$word) 
#  count(word1, word2, sort=TRUE)
