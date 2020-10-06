### per bigrams 2 a 2
bigram_filtered2 <- textdata %>% rowwise() %>% 
  filter(as.vector(length(str_split_fixed(TEXT, " ",n=Inf)))<3) %>%
  head(2)
  mutate(TEXT = iconv(TEXT,from="UTF-8",to="ASCII//TRANSLIT")) %>% 
  unnest_tokens(bigram, TEXT, token= "ngrams", n=2) %>%
  separate(bigram, c("word1","word2"), sep=" ") %>%
  mutate(word1=substitute.all(canvis,canvi,word1),
         word2=substitute.all(canvis,canvi,word2)) %>% 
  # mutate(word1=wordStem(word1),
  #        word2=wordStem(word2)
  filter(!word1 %in% stop$word,
         !word2 %in% stop$word) 
