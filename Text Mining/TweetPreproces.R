TweetPreproces <- lapply(p, function(x) {

  x = gsub('http\\S+\\s*', '', x) ## Remove url's

  x = gsub('\\b+RT', '', x) ## Remove RT

  x = gsub('#\\S+', '', x) ## Remove Hashtags

  x = gsub('@\\S+', '', x) ## Remove Mentions

  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters

  x = gsub("\\d", '', x) ## Remove Controls and special characters

  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations

  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces

  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces

  x = gsub(' +',' ',x) ## Remove extra whitespaces

})
