library(sotu)
library(tidytext)
library(tm)
library(dplyr)
library(stringr)
library(ggcorrplot)

set.seed(12604) # !!! Makes sure that the random numbers generated in rnorm are always the same !!!

# Pre-tokenizing cleaning to remove symbols and punctuation that could interfere with tokenizing
# into sentences
sotu.text <- sotu_text %>%
   gsub(pattern = '[0-9]+', replacement = '') %>%
   gsub(pattern = "Mr.", replacement = "Mr") %>%
   gsub(pattern = "Ms.", replacement = "Ms") %>%
   gsub(pattern = "Mrs.", replacement = "Mrs") %>%
   gsub(pattern = "\\[.*?\\]", replacement = "")

# Run to analyze SOTU speeches of a particular president
speeches <- which(sotu_meta$president == 'George W. Bush') # Change string for different presidents
sotu.tibble <- tibble(sotu.text[speeches]) # Transform SOTU speeches into a tibble of each speech
sotu.tibble <- rename(sotu.tibble, text = 'sotu.text[speeches]')

# Run to analyze SOTU speeches of a particular party
speeches <- which(sotu_meta$party == 'Democratic') # Change string for different presidents
sotu.tibble <- tibble(sotu.text[speeches]) # Transform SOTU speeches into a tibble of each speech
sotu.tibble <- rename(sotu.tibble, text = 'sotu.text[speeches]')

# Run to analyze all SOTU speeches
sotu.tibble <- tibble(sotu.text) # Transform SOTU into a tibble of each speech
sotu.tibble <- rename(sotu.tibble, text = 'sotu.text')

# Set parameters
n.dimensions <- 300
tau <- 3

# Get unique words in the corpus
sotu.words <- sotu.tibble %>% # Deconstruct SOTU into words
  unnest_tokens(word, text, to_lower = TRUE) %>%
  anti_join(stop_words) # Remove stop words

unique.tbl <- unique(sotu.words) # Take all unique words in SOTU 
unique.words <- pull(unique.tbl,word)

# Create the environmental word vectors for all unique words from a Gaussian distribution
# with mean = 0 and sd = 1/sqrt(n)
env.vectors <- matrix(
                data=rnorm(length(unique.words)*n.dimensions, 0, 1/sqrt(n.dimensions)), # Generate distribution
                nrow=length(unique.words), # 1 row for every word
                ncol=n.dimensions, # 1 column for every dimension
                byrow = TRUE) # Input data by row 
rownames(env.vectors) <- unique.words # Assign names of words to each row

# Get all contexts in the speeches
sotu.sentences <- sotu.tibble %>% # Deconstruct SOTU into sentences
  unnest_tokens(sentence, text, token="sentences")

sotu.sentences$number <- as.numeric(rownames((sotu.sentences))) # Add a column of row names (important for regrouping sentences after filtering out stop words)

sotu.sentences <- sotu.sentences %>% 
  unnest_tokens(word, sentence, to_lower = TRUE) %>%
  anti_join(stop_words) %>% # Remove stop words from each sentences
  group_by(number) %>% # Regroup by sentences
  summarize(text = str_c(word, collapse = " ")) %>% # Regroup all words together into their sentences
  ungroup() # Return to character format

# Construct semantic memory matrix where every row represents ONE CONTEXTUAL OCCURENCE
# of the words in a sentence. This corresponds to FORMULA 2 in Jamieson et al.
memory <- matrix(
            data=0, # Fill every cell with 0 to start
            nrow=nrow(sotu.sentences),
            ncol=n.dimensions,
            byrow=TRUE)
for(sentence in 1:nrow(sotu.sentences)){ # Iterate through all sentences
  words <- sotu.sentences[sentence,2] %>% # Separate each sentence into words
    unnest_tokens(word, text)
  for(i in 1:nrow(words)){ # Add the environmental vector of every word in the sentence to one row of context
      memory[sentence,] <- memory[sentence,] + env.vectors[words[[i,1]],] # FORMULA 1 in Jamieson et al.
  }
}
rownames(memory) <- sotu.sentences$text # Assign names by sentences

# Cosine similarity function, when the output of this is cubed, this corresponds to FORMULA 3 in Jamieson et al.
cosine <- function(a,b){
  cos <- 0
  if(sum(abs(a)) != 0 & sum(abs(b)) != 0){
    cos <- sum(a*b) / sqrt(sum(a^2) * sum(b^2))
  }
  return(cos)
} 

# Get cosine similarity table for all echoes retrieved from memory based on the probe, coresponds to FORMULA 4 in Jamieson et al.
cosine.table <- function(semspace){
  cos.table <- matrix(0, nrow(semspace), nrow(semspace))
  for(i in 1:nrow(semspace)){
    for(j in 1:nrow(semspace)){
      if(sum(abs(semspace[i,])) != 0 & sum(abs(semspace[j,])) != 0)
        cos.table[i,j] <- cosine(semspace[i,],semspace[j,]) # Get the cosine similarity between the echoes of each word
    }
  }
  rownames(cos.table) <- rownames(semspace)
  colnames(cos.table) <- rownames(semspace)
  return(cos.table)
}

# Get semantic vectors (probe = list of probe words, wordvecs = words matrix, mem = memory matrix, tau = retrieval gradient)
semantic.subset <- function(probe, wordvecs, mem, tau){ # Retrieve weighted sum of the traces
  subset <- matrix(0, length(probe), ncol(mem)) 
  for(i in 1:length(probe)){
    probe.word <- unlist(strsplit(probe[i], split="/")) # Take the nth word in the probe
    if(grepl("\\s", probe.word)){ # Check if the word is a phrase (has a space)
      probe.word <- unlist(strsplit(probe.word, split=" ")) # Split the phrase in probe.word into separate strings
    }
    for(row in 1:nrow(mem)){ # Get the activation for every row in memory
      a <- 1.0
      for(word in 1:length(probe.word)){ # If length = 1, loops once and gets a single activation, if lenth > 1 loops as many times as the words in the phrase
        a <- a * cosine(wordvecs[probe.word[word],], mem[row,])^tau # FORMULA 4 OR FORMULA 6 in Jamieson et al., depending on word or phrase input
        }
      subset[i,] <- subset[i,] + a * mem[row,] # Multiply the activation by each row in memory then sum all the traces to make an echo for the probe word
    }
  }
  rownames(subset) <- probe # Add row names corresponding to probe words
  return(subset)
}

# Politically significant words in common with more than 100 occurences for dep_n and rep_n in common,
# except for race, god, and bless
parties <- c("government","war","power","peace","business","free", 
              "economic","military","duty","jobs","education","children",
              "justice","debt","law","policy","foreign","progress","future",
              "security","land","americans","race","women","fail","labor",
              "public","hope","strong","god","bless")

# Themes from the Bush era and phrases mentioned by Lakoff
bush <- c("tax","relief","terrorists","war","iraqi","freedom","security",
          "permission","slip","defend","america","enemy","love","children",
          "god","evil","violence","foreign","cuts","oil","saddam",
          "afghanistan")

# Metaphor test words, as a phrase
metaphor.phrases <- c("deepen inequality",
                     "increase prosperity", "embrace success",
                     "provide security", "cultivate safety",
                     "question authority", "attack power")

metaphor.words <- c("deepen", "inequality", 
                      "increase","prosperity","embrace","success",
                      "provide","security","cultivate","safety",
                      "question","authority","attack","power")

# Get semantic space
probe.list <- parties
semantic.space <- semantic.subset(probe = probe.list,
                                  wordvecs = env.vectors,
                                  mem = memory,
                                  tau = 3)

sem.space.table <- cosine.table(semantic.space)

# Get the difference between Democratic and Republican speeches by saving their respective
# sem.space.tables and getting the absolute value between them
diff.space.table <- abs(dem.space.table - rep.space.table)

# Plot the results as a heatmap of cosine similarity between each word
ggcorrplot(sem.space.table,
           method = "circle",
           legend.title = "Cosine Similarity",
           lab = TRUE,
           lab_size = 2,
           outline.color = "white",
           colors = c("#FE4A49", "#FED766", "#00B77D"),
           hc.order = TRUE,
           type = "lower"
)

# Plot the distances between echoes in the semantic space
distances <- dist(semantic.space) #Get distance between vectors for each word
distance.matrix <- as.matrix(distances) # To see distance values for context 
two.d.distances <- cmdscale(distances) # Scale the 300 dimensions to 2 dimensions for easy visualization

x <- two.d.distances[,1] # Get x-coordinates
y <- two.d.distances[,2] # Get y-coordinates

# Plot the word vectors in a 2d space, commented x and y limits are for zooming into clusters of plot
plot(x, y, 
     xlab="Dimension 1", ylab="Dimension 2",
     # xlim = c(0,0.01),
     # ylim = c(0.0042,0.0055),
     main="Distances between words in SOTU", 
     type="n")
text(x, y, labels = row.names(semantic.space), cex=.7)