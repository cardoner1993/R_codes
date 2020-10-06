library(dplyr)
df <- rnorm(1000,10,2) 
df <- data.frame(x = df)
res <- df %>% mutate(category=cut(x, breaks=c(-Inf, 7, 9, Inf),
                                  labels=c("low","middle","high")))

# -inf, 7) => low
# [7 < x < 9) middle
# [9,inf) => high

