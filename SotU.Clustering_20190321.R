# This script runs through the process of clustering SotU speeches. 
# Steps:
#     1. Clean SotU speech text
#     2. Define similarity (distance) matrix comparing all speeches, Jaro-Winkler
#     3. Cluster speeches using Affinity Propagation

# R Libs
library(sotu) # loads SotU speech data, sotu_text and sotu_meta
library(tm) # tm = text manipulations, using it here for stopwords()
library(RecordLinkage) # used for a string distance function called jarowinkler()
library(apcluster) # used for the Affinity Propagation clustering algorithm apcluster()
#####
#     1. Clean SotU speech text

# Remove common words & misc. characters, from tm library
clean.speeches <- 
  removeWords(
    words = stopwords(kind = "en"),
    x = tolower(
      removePunctuation(
        x = gsub(
          pattern = "\n", 
          replacement = "",
          x = sotu_text
        ))))

# Create labels for the SotU speeches with sotu_meta data
s.labels <- 
  paste(1:nrow(sotu_meta), "_", # unique numeric
        sotu_meta$president, "_", # pres name
        sotu_meta$year, # speech year
        sep = "")

#####
#     2. Define similarity matrix comparing all speeches
#
# Below we define a function that takes the SotU speeches and labels as input
#   and returns a labeled similarity matrix with disatnce values comparing each speech text

my.speech.dist <- function(speech.text, speech.labels = NA, sel=NA) {
  # speech.text = a character vector with each element as a speech
  # speech.labels = the labels to call each speech (default label is speech text values)
  # sel = subset of speeches, as numeric indicies of the speech vector (returns rect. matrix)
  
  # Set up the speech.label, if NA
  if (any(is.na(speech.labels))) {
    speech.labels <- 1:length(speech.text)
  }
  # Check for valid speech.label length
  if(!(length(speech.text) == length(speech.labels))) {
    stop("The length of speech.labels must equal the length of x.")
  }
  
  # For square matrix...
  if (any(is.na(sel))) {
    L <- length(speech.text)
    m <- matrix(nrow = L, ncol = L)
    # For the i'th column of matrix m --> the Jaro-Winkler distance between
    #   each row's speech and the single speech of column i
    for (i in 1:L) {
      m[, i] <- jarowinkler(str1 = speech.text[i], str2 = speech.text)
    }
    
    # label the rows and columns of m
    row.names(m) <- speech.labels
    colnames(m) <- speech.labels
  } else {  # For rectangular matrix...
    R <- length(speech.text)
    C <- length(sel)
    m <- matrix(nrow = R, ncol = C)
    # the i'th column of matrix m is the Jaro-Winkler distance between
    #   each row's speech and the single speech of column i
    #     sel is the subset of columns chosen by the user
    for (i in 1:C) {
      m[, i] <- jarowinkler(str1 = speech.text[sel[i]], str2 = speech.text)
    }
    
    # label the rows and columns of m
    row.names(m) <- speech.labels
    colnames(m) <- speech.labels[sel]
  }
  return(m)
}

# Run the above as a command to define the function in R...

#   okay, now lets run it on our cleaned SotU data, 
#   but first see the runtime WARNING below the command

N <- 10 # N = How many speeches are being compared?
sim.matrix <- 
  my.speech.dist(
    speech.text = clean.speeches[1:N], 
    speech.labels = s.labels[1:N], 
    sel = NA)

# WARNING: Runtime for all speeches at once (N = 236) is ~2 hours (w/ 16GB RAM)
#   If you create sim.matrix for less speeches at once it will run faster
#   Benchmarks:
#     N = 10 ... 3.33 sec
#     N = 20 ... 8.66 sec
#     N = 40 ... 85.25 sec
#     N = 80 ... 1528.7 sec = ~25 minutes

#####
#     3. Cluster speeches using Affinity Propagation
#
# Plug the similarity matrix in as the argument 's' to the
#   Affinity Propagation clustering algorithm apcluster()

speech.clusters <-apcluster(s = sim.matrix)

# Let's explore the clusters, some tips:

# This will print all of the clusters and some stats about the algorithm
speech.clusters

# The cluster result is a list object; each cluster is indexed like this:
speech.clusters[[1]]

# There are also attribute that can be called, like the list of exemplars
speech.clusters@exemplars   # exemplars are the elements of each cluster that
                            # are most similar to other elemnts of the cluster
                            # think min dist w/i cluster, central element
