# # This is a version of ITS with BEAGLE's original method of storing context information
# To test this storage method with the rest of ITS:
#   1. Run lines 1-65 in ITS.R
#   2. Run the code below
#   3. Run lines 83 onwards in ITS.R

memory <- matrix(
  data=0, 
  nrow=nrow(unique.tbl), # Create a new row for every word in the text
  ncol=n.dimensions,
  byrow=TRUE)
for(unique.word in 1:nrow(unique.tbl)){ # For every unique word in the corpus
  for(sentence in 1:nrow(sotu.sentences)){
    sentence.words <- sotu.sentences[sentence,2] %>%
      unnest_tokens(word, text)
    if(unique.tbl[[unique.word,1]] %in% sentence.words$word){ # Check if the unique word is a sentence of the corpus
      for(word in 1:nrow(sentence.words)){ # Add all the environmental vectors of the words that appear in that sentence and store it in the memory of that unique word
        memory[unique.word,] <- memory[unique.word,] + env.vectors[sotu.sentences[[word,1]],] 
      }
    }
  }
}
rownames(memory) <- unique.tbl$word