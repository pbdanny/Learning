library(tm)
library(ggplot2)

# Setup data directory -----
spam.path <- "/Users/Danny/Downloads/spam/" 
spam2.path <- "/Users/Danny/Downloads/spam_2/" 
easyham.path <- "/Users/Danny/Downloads/easy_ham/" 
easyham2.path <- "/Users/Danny/Downloads/easy_ham2/" 
hardham.path <- "/Users/Danny/Downloads/hard_ham/" 
hardham2.path <- "/Users/Danny/Downloads/hard_ham_2/"

# Define function get msg ----
get.msg <- function(file_name) {
  # Open file connection 
  con <- file(file_name, open = "rt", encoding = "latin1")
  text <- readLines(con)
  # The message always begins after the first full line break 
  # which return index of first == "" , start with next line , then fetch to the last line
  msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
  close(con)
  # paste with collapse, collapsed all vector of charactor to 1 vector of charactor
  return(paste(msg, collapse="\n"))
}

# Reading all spam mail ----
# dir list all files in path
spam.docs <- dir(spam.path)
# exclude filename = cmds from file list
spam.docs <- spam.docs[spam.docs != "cmds"]
# Apply get.msg to extract data from files
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path,p,sep="")))

# Read vector of charactor and transform to corpus , TDM ----
# TDM term doccument matrix = frequency table of words i = # words, j = # doccument 

get.tdm <- function(doc.vec) {
  # VectorSource convert vector of charactor to object VectorSource 
  # Corpus convert object VectorSource to object Corpus
  doc.corpus <- Corpus(VectorSource(doc.vec))
  # Create control parameter for object TDM
  control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE,
                  minDocFreq=2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

spam.tdm <- get.tdm(all.spam)
# Convert TDM to matrix
spam.matrix <- as.matrix(spam.tdm)
# Count frequency of each words 
spam.counts <- rowSums(spam.matrix)
# Convert matrix to dataframe
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)), stringsAsFactors=FALSE) 
names(spam.df) <- c("term","frequency") 
# Convert charactor to numeric
spam.df$frequency <- as.numeric(spam.df$frequency)
# % word occured compared with all other words
spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i) {length(which(spam.matrix[i,] > 0))/ncol(spam.matrix)})
# Density = frequency / sum frequency
spam.density <- spam.df$frequency/sum(spam.df$frequency)
# Add density and occurance to spam.df
spam.df <- transform(spam.df, density=spam.density, occurrence=spam.occurrence)

head(spam.df[order(-spam.df$occurrence),])
