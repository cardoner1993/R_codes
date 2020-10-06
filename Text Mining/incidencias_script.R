## Data from link
## http://opendata-ajuntament.barcelona.cat/data/es/dataset/iris/resource/3544d426-dda7-49ae-9378-13689113b065?inner_span=True
## https://github.com/mjockers/syuzhet
## https://www.kaggle.com/headsortails/personalised-medicine-eda-with-tidy-r/notebook
## https://books.google.es/books?id=qNcnDwAAQBAJ&pg=PA111&lpg=PA111&dq=unnest+tokens+regex+R&source=bl&ots=Q0uU8mI_s_&sig=hH1YmroFMSugsifjaErNYa3srNs&hl=es&sa=X&ved=0ahUKEwiioqvu_aLWAhXHORQKHQmtDRMQ6AEIXjAH#v=onepage&q=unnest%20tokens%20regex%20R&f=false


## https://github.com/hrbrmstr/pluralize



library("dplyr")
library("stringr")
library("tidytext")
library("readr")

## Read data


inci <- read_csv("D:/Users/str_aml/Desktop/text mining OPEN DATA BCN/2017.csv")
# inci2015 <- red_csv("D:/Users/str_aml/Desktop/text mining OPEN DATA BCN/2015.csv")
#View(inci)

# sapply(inci, is.numeric)
na_count <-sapply(inci, function(y) sum(length(which(is.na(y))))) %>% as.data.frame() %>% 
  add_rownames("Columna")  %>% arrange(desc(.))

names(na_count)[2] <- "NAs"
head(na_count)




 install.packages("leaflet")
 require(leaflet)
 names(inci)

 # Show first 20 rows from the `quakes` dataset
table(inci$TIPUS)

aux <- inci %>%  filter(COORDENADA_X != '',COORDENADA_Y!= '') %>%
   filter(TIPUS == 'SUGGERIMENT') %>%  as.data.frame() 
 
dim(aux)
 
leaflet(data = aux) %>% addTiles() %>%
   addMarkers(~LONGITUD, ~LATITUD,
              popup = ~TIPUS, label = ~CANALS_RESPOSTA,
              clusterOptions=markerClusterOptions())
  


names(inci)
# table(inci$TIPUS)
## table(inci$DISTRICTE)
## creem dataframe amb la idea de veure diferencies entre DISTRICTES
require(dplyr)
textdata<- inci %>% 
              filter(TIPUS == 'INCIDENCIA',DISTRICTE != '') %>%
              select(c("DETALL","DISTRICTE")) %>% 
              rename(TEXT = DETALL) %>% as.data.frame()


dim(textdata)


td <- textdata[1:20,]

library(tidyr)
# token = "ngrams",n=2
title <- textdata %>% 
  unnest_tokens(word,TEXT,format = "text")

title

# tt <- title %>%  mutate(word=wordStem(word))
# 
# title1 <- textdata %>% 
#   unnest_tokens(word,TEXT,token = stringr::str_split, pattern = "/")



# stop_words <- data.frame(stop=c("a","/","o"))

# stopwords<-title %>%
#   count(word, sort = TRUE) %>% head(15) %>%  as.data.frame()

# "incidencies"
stop <- data_frame(word = c("per","pal","neta","a","al","de","la","o","/",
                            "i","u","en","meva","d'"))


# documents = gsub('[[:punct:]]', '', documents) #remove punctuation



title_minus <- title %>% 
  anti_join(stop,by="word")  



?nest

# title_minus %>%
#   count(word, sort = TRUE)


title_minus %>% group_by(DISTRICTE,word) %>% 
  count(sort = TRUE)

require("ggplot2")
title_minus %>% group_by(word,DISTRICTE) %>% count(sort=TRUE) %>% 
  ungroup() %>% group_by(DISTRICTE) %>% top_n(4) %>%  
  ggplot(aes(reorder(x=word,+n),y=n))+geom_bar(stat="identity") + coord_flip() +
  xlab("") + facet_wrap(~DISTRICTE,ncol=5,scales = 'free') 



title_minus %>% group_by(word,DISTRICTE) %>% count(sort=TRUE) %>% 
  filter(n>150) %>% 
  ggplot(aes(reorder(x=word,+n),y=n))+geom_bar(stat="identity",
                                               fill="gray73") + coord_flip() +
  xlab("") + facet_wrap(~DISTRICTE,ncol=5,scales="free") +
  theme_minimal()  + theme(strip.background = element_rect(fill="orange")) 


# theme_rob <- function (base_size = 12, base_family = "sans") 
# {
#   (theme_foundation(base_size = base_size, base_family = base_family) + 
#      theme(line = element_line(colour = "grey60"), rect = element_rect(fill = "grey90", 
#                                                                        linetype = 0, colour = NA), text = element_text(colour = "grey20"), 
#            axis.title = element_text(colour = "grey30"), axis.text = element_text(), 
#            axis.ticks = element_blank(), axis.line = element_blank(), 
#            legend.background = element_rect(), legend.position = "bottom", 
#            legend.direction = "horizontal", legend.box = "vertical", 
#            panel.grid = element_line(colour = NULL), panel.grid.major = element_line(colour = "grey60"), 
#            panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0, 
#                                                                          size = rel(1.5), face = "bold"), plot.margin = unit(c(1, 
#                                                                                                                                1, 1, 1), "lines"), strip.background = element_rect()))
# }
# 
# 
# require(pluralize)
# install.packages("pluralize")
# singularize(c("boats", "houses", "cats", "rivers"))
# 
# library(ggthemes)
# install.packages("ggthemes")
