## as.factor
df <- df %>% mutate_if(is.character,as.factor)

## lapply & sapply
DF[sapply(DF, is.character)] <- lapply(DF[sapply(DF, is.character)], 
                                       as.factor)
