fromto <- read.table(text="
from to
š s
œ oe
ž z
ß ss
þ y
à a
á a
â a
ã a
ä a
å a
æ ae
ç c
è e
é e
ê e
ë e
ì i
í i
î i
ï i
ð d
ñ n
ò o
ó o
ô o
õ o
ö o
ø oe
ù u
ú u
û u
ü u
ý y
ÿ y
ğ g",header=TRUE)
Then the function:

replaceforeignchars <- function(dat,fromto) {
  for(i in 1:nrow(fromto) ) {
    dat <- gsub(fromto$from[i],fromto$to[i],dat)
  }
  dat
}

test <- c("Sølvsten", "Günther")
replaceforeignchars(test,fromto)
#[1] "Soelvsten" "Gunther"
