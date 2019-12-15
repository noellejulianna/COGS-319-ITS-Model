library(stringr)
library(dplyr)
library(tidytext)
library(readr)
library(tm)
library(widyr)
library(sotu)

reviews <- read_lines("finefoods.txt") 
reviews <- reviews[str_sub(reviews, 1, 12) == "review/text:"] #only take the text from actual reviews
reviews <- str_sub(reviews, start = 14) #filter out the phrase "review/text:"
tblReviews <- tibble(reviews) #transform into tibble

custom <- add_row(stop_words, word = "br", lexicon = "custom") #add br to stopword list

tblReviews %>%
  unnest_tokens(output="word", input = reviews, token = "words") %>% #take from column reviews, prase into words, output in column called "word"
  anti_join(custom) %>% #remove stop_words and added stop words
  count(word,sort=TRUE) #count frequency of top 10 words in column "word" of reviews | note: column for words on stop_word is called 'word', so must match output

data("acq")
acq[[1]]$meta
acq[[1]]$content

acq_tibble <- tidy(acq)

corpus <- VCorpus(VectorSource(acq_tibble$text))
meta(corpus, 'Author') <- acq_tibble$author
meta(corpus, 'Places') <- acq_tibble$places
head(meta(corpus))

ids <- c()
for(i in c(1:50)){
  ids <- append(ids, acq[[i]]$meta$id)
}

acq_counts <- acq_tibble %>%
  unnest_tokens(word, text) %>%
  count(word, sort = T) %>%
  anti_join(stop_words)

#ACQ article similarity by word count
acq_counts <- acq_tibble %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = T) %>%
  anti_join(stop_words)

comparisons <- acq_counts %>%
  pairwise_similarity(id,word,n) %>%
  arrange(desc(similarity))

comparisons %>%
  summarize(mean = mean(similarity))

#ACQ article similarity by TF_IDF
acq_tfidfs <- acq_tibble %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(id, word, sort = T) %>%
  bind_tf_idf(id, word, n)

acq_tfidfs %>%
  pairwise_similarity(id, word, tf_idf) %>%
  arrange(desc(similarity))

# mike-lawrence wikiBEAGLE?

library(stats)

vectorLength <- 2^10

normalize <- function(x){
  # Normalize a vector to length 1.
  return(x/sum(a^2.0)^0.5)
}

cconv <- function(x, y){
  # Compute the circular convolution of the (real-valued) vectors a and b
  xyArray <- fft(x, inverse = FALSE) * fft(y, inverse = FALSE)
  return(fft(xyArray, inverse=TRUE))
}

ordConv <- function(a, b, p1, p2){
  # Performs ordered (non-commutative circular convolution on 
  # the vectors a and b by first permuting them according to 
  # the index vectors p1 and p2.)
  return(cconv(a[p1], b[p2]))
}

seqOrdConv <- function(l, p1, p2){
  # Given a list of vectors, iteratively convolves them into a single vector
  # (i.e., binds them together: (((1+2)+3)+4)+5 ). Used to combine 
  # characters in ngrams.
  return(Reduce(function(a,b) return(normalize(ordConv(a, b, p1, p2)))), l)
}

getOpenNGrams <- function(word, charVecList, charPlaceholder){
  # Modified to use number vectors instead of characters
  ngrams <- vector()
  sizes <- seq(length(word))[2:length(word)] # ******* Should change to 3 because R starts from 1?
  append(sizes,length(word))
  for(size in sizes){
    for(i in seq(from=1, to=length(word))){
      if(i+size > length(word)){break}
      tmp <- vector()
      for(char in word[i:i+size]){
        append(tmp,charVecList[char])
      }
      append(ngrams,tmp)
      if(i+size == length(word)){continue}
      for(b in seq(from=1, to=size)){
        for(e in seq(from=1, to=length(word)-i-size+1)){
          tmp <- vector()
          for(char in word[i:(i+b)]){
            append(tmp, charVecList[char])
          }
          append(tmp,charPlaceholder)
          for(char in word[(i+b+e):(i+e+size)]){
            append(tmp,charVecList[char])
          }
          append(ngrams,tmp)
        }
      }
    }
  }
  return(ngrams)
}

# initializing

set.seed(12604)
chars <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '`', "'", '-', ',', ';', ':', '.', '!', '?')
charVecList <- vector()
for(char in chars){
  charVecList[char] <- normalize(rnorm(vectorLength) * vectorLength^-0.5)
}

charPlaceholder <- normalize(rnorm(vectorLength) * vectorLength^-0.5)
wordPlaceholder <- normalize(rnorm(vectorLength) * vectorLength^-0.5)
perm1 <- sample(vectorLength)
perm2 <- 
