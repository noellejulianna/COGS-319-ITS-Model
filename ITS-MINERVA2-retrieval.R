# This is a version of ITS with MINERVA2's original implementation (FORMULA 5 in Jamieson et al.)
# To test this retrieval method with the rest of ITS:
#   1. Run lines 1-119 in ITS.R
#   2. Run the code below
#   3. Run lines 131 onwards in ITS.R

make.echo <- function(word, wordvecs, mem, tau){
  for(row in 1:nrow(mem)){
    a <- cosine(wordvecs[word,], mem[row,])^tau # Get the cosine similarity^tau between the probe word and every context in memory
    if(row == 1){
      echo <- a * mem[row,] # If it's the first row, make the variable echo and multiply the activation of that context by the context vector and store in echo
    }else{
      echo <- echo + (a * mem[row,]) # Add the traces for the rest of the rows echo
    }
    return(echo)
  }
}

semantic.subset <- function(probe, wordvecs, mem, tau){ # Retrieve weighted sum of the traces
  subset <- matrix(0, length(probe), ncol(mem))
  for(i in 1:length(probe)){
    probe.words <- unlist(strsplit(probe[i], split="/"))
    if(all(probe %in% rownames(wordvecs)) == TRUE){
      for(word in 1:length(probe.words)){ # Get the individual echo for every probe word
        echo <- make.echo(probe.words[word], wordvecs, mem, tau)
      }
      subset[i,] <- echo
    }
  }
  rownames(subset) <- probe
  return(subset)
}