## http://varianceexplained.org/r/tidytext-gender-plots/
## https://www.r-bloggers.com/the-animals-of-actuallivingscientists/


# td <- textdata[1:20,]
# 
# names(td)

stop

require(SnowballC)

# pluralize.js ?? english version only()
require("stringr")
canvis <- c("espais","publics")
canvi <- c("espai","public")

substitute.all <- function(pattern, replacement, x) {
  idx <- match(x, pattern)
  return(ifelse(is.na(idx), x, replacement[idx]))
}

require(tidyr)
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

bigram_filtered

# tt <- title %>%  mutate(word=wordStem(word))
bigrams_united <- bigram_filtered %>%
   unite(bigram, word1, word2, sep=" ")
  # mutate(bigram=wordStem(bigram,"spanish")) 

head(bigrams_united)
# %>% aes(reorder(x=bigram,+n),y=n)
bigrams_united %>% group_by(bigram,DISTRICTE) %>% count(sort=TRUE) %>%
  ungroup() %>% group_by(DISTRICTE) %>%  
  top_n(5) %>%  arrange(DISTRICTE) %>% 
  ggplot(aes(reorder(x=bigram,+n),y=n))+geom_bar(stat="identity",
                                               fill="gray73") + coord_flip() +
  xlab("") + facet_wrap(~DISTRICTE,ncol=5,scales="free") +
  theme_minimal()  + theme(strip.background = element_rect(fill="orange")) 


require(dplyr)
require(ggplot2)
require(plotly)

ggplotly(a)
