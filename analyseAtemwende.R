# analyseAtemwende.R
#
# Purpose:  Sample code to read and analyse Atemwende poems.
#           
# Preconditions: Poems exist as textfiles in the ../data/Atemwende folder.
#                                
#                
# Postcondition: ...
#                
## 
#
# V 0.6
# Date:     June 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo:     - Refactor!
#           - Ensure lemmata are also tokens in lemma dictionary
#           - Complete code for lemmatization dictionary
#           - Handle case
#           - Review WP corpus (some residual markup still present)
#
# V 0.6     - add stemming for frequency analysis,
#           - create lemmatization dictionary (incomplete)           
# V 0.5.1   print enrichment analysis into nice table
# V 0.5     add enrichment analysis
# V 0.4     add frequency analysis from samples with equal numbers of words
# V 0.3     proces Wikipedia reference corpus
# V 0.2     merge hyphenated words
# V 0.1     first code
#
# ==============================================================================

# confirm that the working directory is the correct one - it should be
# the Wenji-project directory.
getwd()


# ==== PACKAGES ================================================================

# XML parses HTML files
if (!require(XML)) {
  install.packages("XML")
  library(XML)
}

# string handling functions, e.g. str_trim
if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

# various Unicode functions for validation
if (!require(Unicode)) {
  install.packages("Unicode")
  library(Unicode)
}

# SnowballC for stemming
if (!require(SnowballC)) {
  install.packages("SnowballC")
  library(SnowballC)
}

# tm for managing a corpus
if (!require(tm)) {
  install.packages("tm")
  library(tm)
}


# ==== DEFINITIONS =============================================================


# ...

# ==== FUNCTIONS ===============================================================

inFile <- "../data/morphy/morphy-mapping-20110717.csv"


makeLemmaDict <- function(fileName) {
  # Creates a lemma dictionary from data found in inFile. inFile 
  # must be a tab separated text document that contains (optionally) comment 
  # lines prefixed with a "#", no header, and data lines in which the first column is a
  # full-form and the second column contains lemmata. The full form is treated as case
  # sensitive. In cases where the full form is ambiguous, we replace it with a
  # mixed lemma: eg:
  #    abduzierte   abduzieren
  #    abduzierte   abduziert
  # is replaced with the unique entry
  #    abduzierte   abduzier(t/en)
  # Since all full-forms are then unique, we can use them as rownames for
  # efficient lookup of lemmata.
  # 
  tmp <- read.csv(fileName,
                  comment.char = "#",
                  blank.lines.skip = TRUE,
                  header = FALSE,
                  sep = "\t",
                  stringsAsFactors = FALSE)
  
  # The Morphy dictionary seems to contain erroneous mappings, in which the
  # token is upper-case but the lemma is lower case. Perhaps these are
  # sentence-initial forms? Whatever the reason, if the lemma is lowercase then
  # we convert the token to lowercase.
  ucTokens <- grep("^[A-ZÄÖÜ]", tmp[ , 1])  # find all noun-case tokens
  ucLcMixed <- ucTokens[grepl("^[a-zäöü]", tmp[ucTokens, 2])] # which of these
                                                              # have lowercase
                                                              # lemmas?
  tmp[ucLcMixed, 1] <- tolower(tmp[ucLcMixed, 1])   # make these mixed entries
                                                    # lowercase
  
  # find index of all duplicated tokens
  dupIdx <- which(duplicated(tmp[ , 1]))
  
  # make a copy of tmp containing only unique tokens
  tmpU <- tmp[- dupIdx, ]
  
  # make a list of unique duplicates (i.e. triplicates and higher are collapsed)
  dupTokens <- unique(tmp[dupIdx, 1])
  
  for (token in dupTokens) {
    # Note: this loop takes about 20 minutes on the original Morphy dictionary
    # retrieve all lemmata for this token
    lemmata <- tmp[grep(token, tmp[ , 1]), 2]
    
    # replace the lemma in the unique-tokens-dataframe with a poly lemma
    # constructed from the lemmata
    tmpU[tmpU[ , 1] == token, 2] <- makePolyLemma(lemmata)
  }

  # finally make a new, single column dataframe in which rownames are tokens
  # and the entries are lemmata
  myDict  <- data.frame(tmpU[ , 2], stringsAsFactors = FALSE)
  rownames(myDict)  <- tmpU[ , 1]
  names(myDict) <- "Lemma"
  
  return(myDict)
}

makePolyLemma <- function(x) {
  # Convert tokens in x into a poly lemma.
  # 

  if (length(x) == 0) { return("") }

  x <- unique(x) # collapse duplicated lemmata 
  if (length(x) == 1) { return(x[1]) }
  
  l <- max(nchar(x))  # length of longest word
  # put all words into a character matrix and pad to l with "" 
  charMat <- character()
  for (i in 1:length(x)) {
    charMat <- rbind(charMat, c(unlist(strsplit(x[i], "")),
                                character(l - nchar(x[i]))))
  }
  
  # find the longest common prefix
  prefix <- character()
  for (i in 1:l) {
    if (length(unique(charMat[ , i])) > 1) { 
      break
    }
  }
  if (i > 1) {
    prefix <- paste(charMat[1, 1:(i-1)], collapse = "")
  } else {
    prefix <- ""
  }
  
  # collect all specific suffixes using index i from above
  suffixes <- character()
  for (j in 1:length(x)) {
    suffixes[j] <- paste(charMat[j, i:l], collapse = "")
  }
  # order by length, shortest first
  suffixes <- suffixes[order(nchar(suffixes), decreasing = FALSE)]

  # assemble the polylemma, collapsing suffixes into a "/" separated string
  lemma <- sprintf("%s(%s)", prefix, paste(suffixes, collapse = "/"))
  
  return(lemma)
}

myLemmaDict <- makeLemmaDict(inFile)
save(myLemmaDict, file = "../data/myLemmaDict.1.0.RData")
load(file = "../data/myLemmaDict.1.0.RData")
myLemmaDict["spielte", 1]
#


# ==== PROCESS =================================================================


# get a vector of filenames to process
filenames <- list.files("../data/Atemwende", full.names = TRUE)

# add the contents one by one to the vector AW
AW <- character()
for (i in 1:length(filenames)) {
  AW <- c(AW, readLines(filenames[i]))
}

# drop all empty lines
AW <- AW[AW != ""]

# Merge hyphenated words: we define a hyphenated word as one where
# a hyphen appears as the last character, directly attached to a letter
# and the first character of the next line is lowercase. When we find such
# a situation, we merge the two lines and delete the hyphen. We also remove the
# second line.

endHyphens <- grep("\\w-$", AW)
for (i in 1:length(endHyphens)) {
  iH <- endHyphens[i]
  if (length(grep("^[a-zäöü]", AW[iH + 1])) > 0) {

#    print(paste(AW[iH], AW[iH + 1]))

#    pre <- substr(AW[iH], 1, nchar(AW[iH]) - 1)
#    post <- AW[iH + 1]
#    print(paste(pre, post, sep = "|"))

    AW[iH] <- paste(substr(AW[iH], 1, nchar(AW[iH]) - 1),
                    AW[iH + 1],
                    sep = "")
    AW[iH + 1] <- ""
  }
}


# Case-handling logic to go here, for now, we lowercase everything
AW <- tolower(AW)

# Strsplit this into words
AWwords <- unlist(strsplit(AW, "\\W+"))

# Remove all empty words
AWwords <- AWwords[AWwords != ""]

# how many words are there?
length(AWwords)  # 3556

# make a copy with stemmed words
AWstemmedWords <- wordStem(AWwords, language="de")


# tabulate word frequencies and sort
AWfreq <- sort(table(AWwords), decreasing = TRUE)
AWstemmedFreq <- sort(table(AWstemmedWords), decreasing = TRUE)

# how many unique words?
length(AWfreq)  # 1765
length(AWstemmedFreq)  # 1765


# calculate TTR (Type Token Ratio)
cat(sprintf("\n\n\n === TTR for Atemwende is %7.5f (stemmed:  %7.5f) ===\n\n\n\n", 
            length(AWfreq) / length(AWwords),
            length(AWstemmedFreq) / length(AWwords)))



# look at the top 100
head(AWfreq, 100)

# look at the bottom 100
tail(AWfreq, 100)

# plot log rank vs. log frequency (Zipf plot)
plot(log(1:length(AWfreq)), log(as.numeric(AWfreq)), col="seagreen")
# compare stemmed frequencies
points(log(1:length(AWstemmedFreq)), log(as.numeric(AWstemmedFreq)), cex = 0.7, col="firebrick")


# look at all words that appear exactly twice
AWfreq[AWfreq == 2]

# ==== Constructing a reference dataset:
# Download and unzip the sample Wikipedia article corpus from IDS, place it in
# the data folder. Then use a texteditor to briefly inspect it. You will notice
# that the sentences (which we are interested in) are marked-up with <s> ...
# </s> tags and that there is a lot of extra junk.
# 
# Let's read in the corpus while splitting it into sentences, then get rid of
# the junk.
# 
# First we open a connection to a file, while specifying the correct encoding
# ...
FH <- file("../data/wpd13_sample.i5.xml", encoding="iso-8859-1")

# ... then we use readLines() to read the whole thing and paste it into a single
# huge string.
txt <- paste(readLines(FH), collapse = " ")

# ... and we close the connection.
close(FH)

# Then we strsplit on sentence closing tags ...
chunks <- unlist(strsplit(txt, "</s>"))

# ... remove txt to free the memory
rm(txt) 

# Each chunk now contains a <s> tag, followed by the contents of the sentence.
# ... Except for the last chunk. We drop the last chunk.

chunks <- chunks[-length(chunks)]

# 
#  
# We use strsplit to split the chunk into two parts on the <s> element, then
# overwrite the chunk with the second element of the strsplit result.

for (i in 1:length(chunks)) {
  chunks[i] <- strsplit(chunks[i], "<s>")[[1]][2]
}

# Many chunks contain reference information that we should remove. It is
# demarcated like : &lt;ref ... /ref&gt; We can get rid of all these references
# AND the html markup tags AND the html special characters AND  and all other
# non-word characters AND numbers AND leading and trailing blanks with regular
# expressions

for (i in 1:length(chunks)) {
  chunks[i] <- gsub("&lt;ref.*?/ref&gt;", "", chunks[i])  # references
  chunks[i] <- gsub("<.+?>", "", chunks[i])  # tags
  chunks[i] <- gsub("&.+?;", "", chunks[i])  # HTML character codes
  chunks[i] <- gsub("\\W", " ",  chunks[i])  # replace \\W with " "
  chunks[i] <- gsub("[0-9]", "", chunks[i])  # numbers
  chunks[i] <- gsub("^\\s+", "", chunks[i])  # leading whitespace
  chunks[i] <- gsub("\\s+$", "", chunks[i])  # trailing whitespace
}

# To remove further anomalies, we drop all empty elements, and retain only 
# elements that contain more than one word (the single words are usually
# Wikipedia section headings).

chunks <- chunks[! is.na(chunks)]
chunks <- chunks[grep("\\w\\s+\\w", chunks)]  # character-space-character: 
                                              # defines "more than one word"

# Finally, we strsplit on non-word characters and put the results into a
# word-vector and count numbers and frequencies.

chunks <- tolower(chunks)
WPwords <- unlist(strsplit(chunks, "\\W+"))
WPwords <- WPwords[nchar(WPwords) > 1]  # remove blanks and single characters
length(WPwords)

# create a stemmed version
WPstemmedWords <- wordStem(WPwords, language = "de")

WPfreq <- sort(table(WPwords), decreasing = TRUE)
length(WPfreq)

WPstemmedFreq <- sort(table(WPstemmedWords), decreasing = TRUE)
length(WPstemmedFreq)

# Calculate the TTR for the WP corpus
cat(sprintf("\n\n\n === TTR for WP articles is %7.5f ===\n\n\n\n", 
            length(WPfreq) / length(WPwords)))

cat(sprintf("\n\n\n === TTR for WP articles is %7.5f (stemmed:  %7.5f) ===\n\n\n\n", 
            length(WPfreq) / length(WPwords),
            length(WPstemmedFreq) / length(WPwords)))



# Show a Zipf plot
plot(log(1:length(WPfreq)), log(as.numeric(WPfreq)), col = "skyblue")
# compare stemmed version
points(log(1:length(WPstemmedFreq)), log(as.numeric(WPstemmedFreq)),
       cex = 0.7,
       col = "firebrick")

# compare Celan
points(log(1:length(AWfreq)), log(as.numeric(AWfreq)), col = "seagreen")


# ==============================================================================
# ANALYSIS VERSION 1: AS-IS
# ==============================================================================

# ==== Frenquency analysis with corpora of matched size
# 

# A: make a random sample from the WP corpus
# 

corpusSize <- length(AWwords)
RSwords <- sample(WPwords, corpusSize)

# analyze
RSfreq <- sort(table(RSwords), decreasing = TRUE)

# plot
plot(log(1:length(RSfreq)), 
     log(as.numeric(RSfreq)),
     xlab = "log(Rank)",
     ylab = "log(Frequency)",
     xlim = c(0, 8),
     ylim = c(0, 5.2),
     col = "seagreen")
points(log(1:length(AWfreq)), log(as.numeric(AWfreq)), col = "firebrick")

# repeat this many times and do a density plot of the sampled numbers
# 

N <- 20
RLogRanks <- numeric()
RLogFreq  <- numeric()

for (i in 1:N){
  RSwords <- sample(WPwords, corpusSize)
  RSfreq  <- sort(table(RSwords), decreasing = TRUE)
  RLogRanks <- c(RLogRanks, log(1:length(RSfreq)))
  RLogFreq  <- c(RLogFreq,  log(as.numeric(RSfreq)))
}

# plot this as a density plot
plot(RLogRanks, 
     RLogFreq,
     xlab = "log(Rank)",
     ylab = "log(Frequency)",
     xlim = c(0, 8),
     ylim = c(0, 5.2),
     col=densCols(RLogRanks,RLogFreq),
     pch=19,
     cex=1.5)
points(log(1:length(AWfreq)),
       log(as.numeric(AWfreq)),
       pch = 19,
       col = "firebrick",
       cex = 0.5)


# ==== Relative frequencies
# 

# extract the first N words from AW corpus
N <- 100
AWrelFreq <- AWfreq[1:N] / length(AWwords)

# compile their matching positions in the WP frequency table
# and compute the relative frequencies of these words in the WP corpus
WPmatches <- numeric()
WPrelFreq <- numeric()
for (i in 1:N) {
  M <- which(names(WPfreq) == names(AWfreq)[i])
  if (length(M) == 1) {
    WPmatches[i] <- M
    WPrelFreq[i] <- WPfreq[WPmatches[i]] / length(WPwords)
  } else {
    WPmatches[i] <- 0
    WPrelFreq[i] <- 0
  }
}

# check this:
# AWfreq[7]
# WPfreq[WPmatches[7]]


# print the tabulated results
printEnrichmentTable <- function(N, X, Y) {
  # N - number of words
  # X, Y - words list of two corpora
  N <- 100
  Xfreq <- sort(table(X), decreasing = TRUE)
  Yfreq <- sort(table(Y), decreasing = TRUE)
  XrelFreq <- Xfreq[1:N] / length(X)
  
  # compile their matching positions in the WP frequency table
  # and compute the relative frequencies of these words in the WP corpus
  Ymatches <- numeric()
  YrelFreq <- numeric()
  for (i in 1:N) {
    M <- which(names(Yfreq) == names(Xfreq)[i])
    if (length(M) == 1) {
      Ymatches[i] <- M
      YrelFreq[i] <- Yfreq[Ymatches[i]] / length(Y)
    } else {
      Ymatches[i] <- 0
      YrelFreq[i] <- 0
    }
  }

  cat("\n\n")
  cat(" rank |   rank |              |        |        |          \n")
  cat("    X |      Y |         word |     fX |    fY  | log-ratio\n")
  cat("------|--------|--------------|--------|--------|----------\n")
  logR <- log10(XrelFreq / YrelFreq)
  for (i in order(abs(logR), decreasing = TRUE)) {
    x <- nchar(names(Xfreq)[i])
    cat(sprintf("  %3d | %6d | %s%s | %5.4f | %5.4f | %7.4f\n",
                i,
                Ymatches[i],
                sprintf("%*s", 12 - x, " "),
                names(Xfreq)[i],
                XrelFreq[i],
                YrelFreq[i],
                logR[i]
    ))
  }
}

printEnrichmentTable(100, AWwords, WPwords)


printEnrichmentTable(100, WPwords, AWwords)

# ==============================================================================
# ANALYSIS VERSION 2: STEMMED WORDS
# ==============================================================================

# ==== Frenquency analysis with corpora of matched size
# 

# A: make a random sample from the WP corpus
# 

corpusSize <- length(AWstemmedWords)
RSwords <- sample(WPstemmedWords, corpusSize)

# analyze
RSfreq <- sort(table(RSwords), decreasing = TRUE)

# plot
plot(log(1:length(RSfreq)), 
     log(as.numeric(RSfreq)),
     xlab = "log(Rank)",
     ylab = "log(Frequency)",
     xlim = c(0, 8),
     ylim = c(0, 5.2),
     col = "seagreen")
points(log(1:length(AWstemmedFreq)), log(as.numeric(AWstemmedFreq)), col = "firebrick")

# repeat this many times and do a density plot of the sampled numbers
# 

N <- 20
RLogRanks <- numeric()
RLogFreq  <- numeric()

for (i in 1:N){
  RSwords <- sample(WPstemmedWords, corpusSize)
  RSfreq  <- sort(table(RSwords), decreasing = TRUE)
  RLogRanks <- c(RLogRanks, log(1:length(RSfreq)))
  RLogFreq  <- c(RLogFreq,  log(as.numeric(RSfreq)))
}

# plot this as a density plot
plot(RLogRanks, 
     RLogFreq,
     xlab = "log(Rank)",
     ylab = "log(Frequency)",
     xlim = c(0, 8),
     ylim = c(0, 5.2),
     col=densCols(RLogRanks,RLogFreq),
     pch=19,
     cex=1.5)
points(log(1:length(AWstemmedFreq)),
       log(as.numeric(AWstemmedFreq)),
       pch = 19,
       col = "firebrick",
       cex = 0.5)


# ==== Relative frequencies
# 

# extract the first N words from AW corpus
N <- 100
AWrelFreq <- AWstemmedFreq[1:N] / length(AWstemmedWords)

# compile their matching positions in the WP frequency table
# and compute the relative frequencies of these words in the WP corpus
WPmatches <- numeric()
WPrelFreq <- numeric()
for (i in 1:N) {
  M <- which(names(WPstemmedFreq) == names(AWstemmedFreq)[i])
  if (length(M) == 1) {
    WPmatches[i] <- M
    WPrelFreq[i] <- WPstemmedFreq[WPmatches[i]] / length(WPstemmedwords)
  } else {
    WPmatches[i] <- 0
    WPrelFreq[i] <- 0
  }
}

# check this:
# AWfreq[7]
# WPfreq[WPmatches[7]]


# print the tabulated results
printEnrichmentTable <- function(N, X, Y) {
  # N - number of words
  # X, Y - words list of two corpora
  N <- 100
  Xfreq <- sort(table(X), decreasing = TRUE)
  Yfreq <- sort(table(Y), decreasing = TRUE)
  XrelFreq <- Xfreq[1:N] / length(X)
  
  # compile their matching positions in the WP frequency table
  # and compute the relative frequencies of these words in the WP corpus
  Ymatches <- numeric()
  YrelFreq <- numeric()
  for (i in 1:N) {
    M <- which(names(Yfreq) == names(Xfreq)[i])
    if (length(M) == 1) {
      Ymatches[i] <- M
      YrelFreq[i] <- Yfreq[Ymatches[i]] / length(Y)
    } else {
      Ymatches[i] <- 0
      YrelFreq[i] <- 0
    }
  }
  
  cat("\n\n")
  cat(" rank |   rank |              |        |        |          \n")
  cat("    X |      Y |         word |     fX |    fY  | log-ratio\n")
  cat("------|--------|--------------|--------|--------|----------\n")
  logR <- log10(XrelFreq / YrelFreq)
  for (i in order(abs(logR), decreasing = TRUE)) {
    x <- nchar(names(Xfreq)[i])
    cat(sprintf("  %3d | %6d | %s%s | %5.4f | %5.4f | %7.4f\n",
                i,
                Ymatches[i],
                sprintf("%*s", 12 - x, " "),
                names(Xfreq)[i],
                XrelFreq[i],
                YrelFreq[i],
                logR[i]
    ))
  }
}

printEnrichmentTable(100, AWstemmedWords, WPstemmedWords)


printEnrichmentTable(100, WPstemmedWords, AWstemmedWords)


#    
# ==== TESTS ===================================================================




# [END]