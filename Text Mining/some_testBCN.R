require(tidytext)
require(tidyr)
require(dplyr)
require(ggplot2)
require(stringr)
require(readr)


## Read data
inci <- read_csv("D:/Users/str_aml/Desktop/text mining OPEN DATA BCN/2017_bcn.csv")

# na_count <-sapply(inci, function(y) sum(length(which(is.na(y))))) %>% as.data.frame() %>% 
#   add_rownames("Columna")  %>% arrange(desc(.))
# 
# aux <- inci %>%  filter(COORDENADA_X != '',COORDENADA_Y!= '') %>%
#   filter(TIPUS == 'SUGGERIMENT') %>%  as.data.frame() 


textdata<- inci %>% 
  filter(TIPUS == 'INCIDENCIA',DISTRICTE != '') %>%
  select(c("DETALL","DISTRICTE")) %>% 
  rename(TEXT = DETALL) %>% as.data.frame()

## 1 a 1 

gram1 <- textdata %>% 
  unnest_tokens(word,TEXT,format = "text")


## stop words
# "incidencies"
stop <- data_frame(word = c("per","pal","neta","a","al","de","la","o","/",
                            "i","u","en","meva","d'"))


gram1 <- gram1 %>% 
  anti_join(stop,by="word")  

# head(gram1)


gram1 %>% group_by(DISTRICTE,word) %>% 
  count(sort = TRUE)

# reorder(x=word,+n),y=n)
gram1 %>% group_by(DISTRICTE,word) %>% count(sort=TRUE) %>% 
  ungroup() %>% group_by(DISTRICTE) %>% arrange(desc(DISTRICTE)) %>%
  top_n(4)  %>%
  ggplot(aes(reorder(x=word,+n),y=n))+geom_bar(stat="identity") + coord_flip() +
  xlab("") + facet_wrap(~DISTRICTE,ncol=5,scales = 'free')

#########


gram1 %>% group_by(word,DISTRICTE) %>% count(sort=TRUE) %>% 
  filter(n>150) %>% 
  ggplot(aes(reorder(x=word,+n),y=n))+geom_bar(stat="identity",
                                               fill="gray73") + coord_flip() +
  xlab("") + facet_wrap(~DISTRICTE,ncol=5,scales="free") +
  theme_minimal()  + theme(strip.background = element_rect(fill="orange")) 


###################

###################

# require("stringr")
canvis <- c("espais","publics")
canvi <- c("espai","public")

substitute.all <- function(pattern, replacement, x) {
  idx <- match(x, pattern)
  return(ifelse(is.na(idx), x, replacement[idx]))
}


## change values!
pap <- c("papereres")
pap2 <- c("paperera incidencia")


### bigrams amb 2 paraules
bigram_filtered2 <- textdata %>% mutate(TEXT = tolower(TEXT)) %>% rowwise() %>% 
  filter(as.vector(length(str_split_fixed(TEXT, " ",n=Inf)))<3) %>%
  mutate(TEXT=substitute.all(pap,pap2,TEXT)) %>% 
  mutate(TEXT = iconv(TEXT,from="UTF-8",to="ASCII//TRANSLIT")) %>% 
  separate(TEXT, c("word1","word2"), sep=" ") %>%  
  as.data.frame()



bigram_filtered_plus2 <- textdata %>% mutate(TEXT = tolower(TEXT)) %>% 
                  rowwise() %>% 
  filter(!as.vector(length(str_split_fixed(TEXT, " ",n=Inf)))<3) %>%
   ungroup() %>% 
  mutate(TEXT = iconv(TEXT,from="UTF-8",to="ASCII//TRANSLIT")) %>% 
  mutate(TEXT=tolower(TEXT)) %>% 
  unnest_tokens(bigram, TEXT, token= "ngrams", n=2) %>%
  separate(bigram, c("word1","word2"), sep=" ") %>%
  mutate(word1=substitute.all(canvis,canvi,word1),
         word2=substitute.all(canvis,canvi,word2)) %>% 
  # mutate(word1=wordStem(word1),
  #        word2=wordStem(word2)
  filter(!word1 %in% stop$word,
         !word2 %in% stop$word)  %>%  as.data.frame()


# unir tot
bigrams_united <- bigram_filtered_plus2 %>% full_join(bigram_filtered2) %>% 
  unite(bigram, word1, word2, sep=" ") %>% as.data.frame()



# count(sort=TRUE)
# aes(reorder(x=bigram,+n),y=n)
# mutate(order = row_number()) %>%
tsss<-  bigrams_united %>% group_by(DISTRICTE,bigram) %>% tally() %>%
    ungroup() %>% group_by(DISTRICTE) %>% 
    top_n(3,n) %>% arrange(DISTRICTE,n) %>%   
    mutate(order=row_number()) %>% as.data.frame()
   
  ggplot(tsss,aes(x=order,y=n))+geom_bar(stat="identity",
                                                 fill="gray73") + coord_flip() +
  xlab("") + facet_wrap(~DISTRICTE,ncol=5,scales="free") +
  
  scale_x_continuous(  # This handles replacement of .r for x
        # notice need to reuse data frame
    breaks=tsss$order,
    labels = tsss$bigram )
   
  
  theme_minimal()  + theme(strip.background = element_rect(fill="orange")) 

View(bigrams_united)
?order_by


dat <- bigrams_united %>% group_by(DISTRICTE,bigram) %>% tally() %>%
  ungroup() %>% group_by(DISTRICTE) %>% 
  top_n(3,n) %>%  arrange(n) %>%  as.data.frame()



ggplot(ts,aes(x=reorder(bigram,+n),y=n))+geom_bar(stat="identity")+ coord_flip()+
  facet_wrap(~DISTRICTE,scales="free")


# https://stackoverflow.com/questions/34001024/ggplot-order-bars-in-faceted-bar-chart-per-facet
# https://stackoverflow.com/questions/40608870/order-by-y-value-in-facet-plot

#create list of plots
myplots <- lapply(split(dat,dat$DISTRICTE), function(x){
  #relevel factor partei by wert inside this subset
  x$bigram <- factor(x$bigram, levels=x$bigram[order(x$n,decreasing=F)])
  
  #make the plot
  p <- ggplot(x, aes(x = bigram, y = n, fill = DISTRICTE, width=0.75)) +
    geom_bar(stat = "identity") +
    scale_fill_discrete(drop=F)+ #to force all levels to be considered, and thus different colors
    theme_bw()+
    theme(legend.position="none")+
    labs(y="", x="", title=unique(x$kat))+
    coord_flip()
})

do.call(grid.arrange,(c(myplots, ncol=3)))


tss <- bigrams_united %>% group_by(DISTRICTE,bigram) %>% tally() %>%
  ungroup() %>% group_by(DISTRICTE) %>% 
  top_n(3,n) %>%  
  mutate(order=row_number()) %>% as.data.frame()

ggplot(tss,aes(x=order,y=n))+geom_bar(stat="identity",
                                  fill="gray73") + coord_flip() +
  xlab("") + facet_wrap(~DISTRICTE,ncol=5,scales="free") +
  
  scale_y_discrete(  # This handles replacement of .r for x
    breaks = tss$order,     # notice need to reuse data frame
    labels = tss$DISTRICTE )


theme_minimal()  + theme(strip.background = element_rect(fill="orange")) 

