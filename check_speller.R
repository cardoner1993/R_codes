## dominis

### paraules


# (1) extreure domini

correu <- c("adafa@gmail.com")
correu_domini <- gsub(".*@|\\..*", "", correu)


dictio_words <- c("pedro011","gmail","gmeil","hotmail","yopmail","outlook")
dictio_words_s <- sort(dictio_words, decreasing = TRUE)

correct <- function(word) {
  # Calculate the edit distance between the word and all other words in sorted_words.
  edit_dist <- adist(word, dictio_words_s)
  # Calculate the minimum edit distance to find a word that exists in big.txt 
  # with a limit of two edits.
  min_edit_dist <- min(edit_dist, 3)
  # Generate a vector with all words with this minimum edit distance.
  # Since sorted_words is ordered from most common to least common, the resulting
  # vector will have the most common / probable match first.
  proposals_by_prob <- c(dictio_words_s[ edit_dist <= min(edit_dist, 3)])
  # In case proposals_by_prob would be empty we append the word to be corrected...
  proposals_by_prob <- c(proposals_by_prob, word)
  # ... and return the first / most probable word in the vector.
  proposals_by_prob[1]
}

correu <- c("fafafa@pedro0123.es","adafa@gmail.com","adafa@gmoil.com","pedro@gggmail.com","samanta@outliik.com",
            "afa@gmeill.com","susi@yahu.es","fa@hotmoil.com")

#correu_domini <- gsub(".*@|\\..*", "", correu)

output <- character(length(correu))
for (i in 1:length(correu)) {
  correu_domini <- gsub(".*@|\\..*", "", correu[i])
  output[i] <- correct(correu_domini)
}

output

