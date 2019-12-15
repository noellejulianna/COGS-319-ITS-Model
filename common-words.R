library(sotu)
library(tidytext)
library(tm)
library(dplyr)
library(stringr)

sotu.text <- sotu_text %>%
  gsub(pattern = '[0-9]+', replacement = '') %>%
  gsub(pattern = "Mr.", replacement = "Mr") %>%
  gsub(pattern = "Ms.", replacement = "Ms") %>%
  gsub(pattern = "Mrs.", replacement = "Mrs")

# This is a separate space to determine what words to use for a probe since probe words have
# to be existing words in memory

# Find common words ACROSS PRESIDENTS to observe trends in word associations across their corpora:
# 1. Create a unique.tbl for each president by changing the president name in line 21
# 2. Change variable name 'unique.tbl' in line 42 to [presidentname].unique.tbl

# Run to analyze SOTU speeches of a particular president
speeches <- which(sotu_meta$president == 'William J. Clinton') # Change string for different presidents
sotu.tibble <- tibble(sotu.text[speeches]) # Transform SOTU speeches into a tibble of each speech
sotu.tibble <- rename(sotu.tibble, text = 'sotu.text[speeches]')

sotu.words <- sotu.tibble %>% # Deconstruct SOTU into words
  unnest_tokens(word, text, to_lower = TRUE) %>%
  anti_join(stop_words) %>% # Remove stop words
  count(word, sort=T) # Count word frequency

clinton.unique.tbl <- unique(sotu.words) # Take all unique words in SOTU 

# Once there is a unique.tbl for each president, run the code below to get all the common words across presidents
common <- intersect(obama.unique.tbl$word, bush.unique.tbl$word) %>%
  intersect(clinton.unique.tbl$word) %>% # For additional presidents, copy/paste this line below like so:
  # intersect(reagan.unique.tbl$word) %>%
  common <- tibble(common)

# The following lines add the word counts of each common word per president
common$obama_n <- obama.unique.tbl$n[match(common$common, obama.unique.tbl$word)]
common$bush_n <- bush.unique.tbl$n[match(common$common, bush.unique.tbl$word)]
common$clinton_n <- clinton.unique.tbl$n[match(common$common, clinton.unique.tbl$word)]
# For additional presidents, copy/paste this format like so:
# common$raegan_n <- raegan.unique.tbl$n[match(common$common, raegan.unique.tbl$word)]

# Find common words ACROSS PARTIES to observe trends in word associations across their corpora:
# 1. Create a unique.tbl for each party by changing the party name in line 51
# 2. Change variable name 'unique.tbl' in line 42 to [presidentname].unique.tbl
speeches <- which(sotu_meta$party == 'Republican') # Change string for different presidents
sotu.tibble <- tibble(sotu.text[speeches]) # Transform SOTU speeches into a tibble of each speech
sotu.tibble <- rename(sotu.tibble, text = 'sotu.text[speeches]')

sotu.words <- sotu.tibble %>% # Deconstruct SOTU into words
  unnest_tokens(word, text, to_lower = TRUE) %>%
  anti_join(stop_words) %>% # Remove stop words
  count(word, sort=T) # Count word frequency

rep.unique.tbl <- unique(sotu.words) # Take all unique words in SOTU 

# Once there is a unique.tbl for each party, run the code below to get all the common words across parties
common <- intersect(dem.unique.tbl$word, rep.unique.tbl$word) %>%
  tibble()

# The following lines add the word counts of each common word per president
common$dem_n <- dem.unique.tbl$n[match(common$., dem.unique.tbl$word)]
common$rep_n <- rep.unique.tbl$n[match(common$., rep.unique.tbl$word)]