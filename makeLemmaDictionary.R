# makeLemmaDictionary.R
# 
# Purpose:  Create a dictionary for lemmatization based on the Morphy dictionary
# with corrections.
# 
# Preconditions: The input file Morphy Mapping must exist. The input file of 
# corrections must exist.
# 
# Postcondition: A lemma dictionary which contains a lemma in each row with the 
# token (full-form) as the row name. This R object is named lemmaDict and saved 
# to an .RData file.
# 
# Notes: This code creates a lemma dictionary from data found in morphyFile, a
# tab separated text document that contains (optionally) comment lines prefixed
# with a "#", no header, and data lines in which the first element is a token 
# (Vollform) and the second element is a lemma (Grundform).
# 
# The source dictionary was downloaded from 
# http://www.danielnaber.de/morphologie/ - it has a Creative Commons - By 
# Attribution license.
# 
# The tokens (Vollform) are case sensitive i.e. upper-case forms are nouns.
# 
# == POLYLEMMATA == In cases where the token is not unique, we replace the
# original lemma with a "polylemma": eg the two rows: abduzierte   abduzieren
# (i.e. the lemma is a verb) abduzierte   abduziert    (i.e. the lemma is an
# adjective) are replaced with the unique token -> polylemma entry: abduzierte
# abduzier(t/en)
# 
# Since all tokens are unique after using these "polylemmata", we can use the 
# tokens as rownames in a dataframe for efficient lookup.
# 
# == CORRECTIONS == In many cases the original dictionary "overlemmatizes" for
# our purposes. E.g. all forms of personal pronouns are lemmatized to first
# person: deiner -> ich
# 
# We employ the general principle that a separate token should exist whenever 
# the Chinese language has a separate character. I.e. we preserve person, but 
# not gender, number and case, and we distinguish posessive- and relative 
# pronouns etc. We feel this is important to conserve stylistic features of the 
# text. For details, see the corrections file.
# 
# Atemwende contains a fair number of words that can't be lemmatized but should 
# be (e.g "schritt", "Aug"). We enter these - and others we may encounter in the
# future - through a list of additions.
# 
# ToDo: Compile a list of missing lemmata from high to medium frequency tokens
# in the reference corpus and add to the dictionary; this is a prerequisite in
# case an measurement of lemmatized hapax frequencies is to be undertaken.
# 
# 
# Version:  1.2 
# Date:     August 2016 
# Author:   Boris Steipe and Yi Chen
# 
# History:
#   V 1.2  Add additions 
#   V 1.1  Add corrections 
#   V 1.0  First code
# 
# ==============================================================================

# ==== INITIALIZATIONS =========================================================

morphyFile   <- "../data/morphy/morphy-mapping-20110717.csv"
corrFile     <- "../data/morphy/grundform-korrekturen.csv"
addFile      <- "../data/morphy/ergänzungen.csv"
lemmaFile    <- "../data/lemmaDict.1.2.RData"

# ==== FUNCTIONS ===============================================================

makePolyLemma <- function(x) {
  # Convert tokens in x into a polylemma.
  # 
  # x: a vector of strings
  # return: the longest common prefix, followed by all unique suffixes in
  #         parentheses, sorted by length, separated by "/"
  #         
  # Example:
  #         x <- c("abbestellen", "abbestellt", "abbestellung")
  #         makePolyLemma(x)
  #         [1] "abbestell(t/en/ung)"  
  
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


# ==== PART I: CREATE THE DICTIONARY ===========================================

# == READ THE ORIGINAL MORPHY DICTIONARY == 
tmp <- read.csv(morphyFile,
                comment.char = "#",
                blank.lines.skip = TRUE,
                header = FALSE,
                sep = "\t",
                stringsAsFactors = FALSE)

# Some tokens look like typos and have a questionmark as a lemma. Drop these.
tmp <- tmp[! tmp[ ,2] == "?", ]

# The Morphy dictionary seems to contain erroneous mappings, in which the
# token is upper-case but the lemma is lower case. Perhaps these are
# sentence-initial forms? Whatever the reason, if the lemma is lowercase then
# we convert the token to lowercase.

# find all tokens that begin with an uppercase character
ucTokens <- grep("^[A-ZÄÖÜ]", tmp[ , 1])  

# find all of these entries which have a lowercase lemma
ucLcMixed <- ucTokens[grepl("^[a-zäöü]", tmp[ucTokens, 2])]

# Convert the tokens for these mixed cases to lowercase
tmp[ucLcMixed, 1] <- tolower(tmp[ucLcMixed, 1])   # make these mixed entries


# == MAKE CORRECTIONS ==
# 
# Read the corrections data.
corr <- read.csv(corrFile,
                 comment.char = "#",
                 blank.lines.skip = TRUE,
                 header = FALSE,
                 sep = "\t",
                 stringsAsFactors = FALSE)

# Apply each correction by replacing the lemma if the token exists, or appending
# the token/lemma row if the token does not yet exist.
for (i in 1:nrow(corr)) {
  tokenRows <- which(tmp[ , 1] == corr[i, 1])
  if (length(tokenRows) > 0) {
    tmp[tokenRows, 2] <- corr[i, 2]
  } else {
    tmp[nrow(tmp) + 1, ] <- corr[i, ]
  }
}

# == ADD ADDITIONS ==

add <- read.csv(addFile,
                comment.char = "#",
                blank.lines.skip = TRUE,
                header = FALSE,
                sep = "\t",
                stringsAsFactors = FALSE)

tmp <- data.frame("token" = c(tmp[ , 1], add[ , 1]),
                  "lemma" = c(tmp[ , 2], add[ , 2]),
                  stringsAsFactors = FALSE)


# == ADD LEMMATA TO THE LIST OF TOKENS ==
# 
# The original Morphy dictionary does not contain about a third of the lemmata 
# as tokens ("Vollform"). This makes sense for some processing - if a token is 
# not found, it is returned unchanged, but for more precise analysis it will be 
# desirable to distinguish the case that a token was not found because it is 
# itself a lemma, or because it is completely unknown - a hapax. We simply add 
# all lemmata to the dictionary, wherever this creates duplicates, these will be
# removed in the next step.
# 
# However, this procedure is not complete. A large number of German tokens are 
# not included in the dictionary because the token is always the same as the 
# lemma. For example the tokens "mit", "für", "gegen", "weil", "demnach" are not
# present.

uniqueLemmata <- unique(tmp[ , 2])
tmp <- data.frame("token" = c(tmp[ , 1], uniqueLemmata),
                  "lemma" = c(tmp[ , 2], uniqueLemmata),
                  stringsAsFactors = FALSE)


# == MAKE TOKENS UNIQUE ==

# Find the index of all duplicated tokens.
dupIdx <- which(duplicated(tmp[ , 1]))

# Make a copy of tmp containing only unique tokens.
tmpU <- tmp[- dupIdx, ]

# make a list of unique duplicates (i.e. if these are triplicates or higher the token should not appear more than once in the list of duplicates.)
dupTokens <- unique(tmp[dupIdx, 1])

for (token in dupTokens) {
  # Note: this loop takes about 15 minutes on Boris' machine

  # collect all lemmata for these tokens 
  lemmata <- tmp[token == tmp[ , 1], 2]
  
  # replace the lemma in the unique-tokens-dataframe with a poly lemma
  # constructed from the lemmata
  tmpU[tmpU[ , 1] == token, 2] <- makePolyLemma(lemmata)
}

# Make a new, single column dataframe in which rownames are unique tokens
# and the entries are lemmata (resp. polylemmata).
lemmaDict  <- data.frame("lemma" = tmpU$lemma, stringsAsFactors = FALSE)
rownames(lemmaDict)  <- tmpU$token

# order by rowname
x <- data.frame("lemma" = lemmaDict[order(rownames(lemmaDict)), 1],
           stringsAsFactors = FALSE)
rownames(x) <- rownames(lemmaDict)[order(rownames(lemmaDict))]

lemmaDict <- x

# == SAVE LEMMA DICTIONARY ==
save(lemmaDict, file = lemmaFile)

# == CLEAN UP WORKSPACE ==
# 
rm(list = c(
  "corr",
  "tmp",
  "tmpU",
  "x",
  "dupIdx",
  "dupTokens",
  "ucLcMixed",
  "ucTokens",
  "uniqueLemmata"
  ))

rm(lemmaDict)

# Done.
# 
# == PART II: DEFINE MISSING ENTRIES ==

missingFile <- "xLem.tsv"

# == make a word-table from Atemwende and reference corpora.

AWX <- read_xml("../data/Atemwende/Atemwende.xml")
txt <- xml_text(xml_find_all(xml_children(AWX)[[1]], ".//s"))

# get reference corpora from mk2
MK2X <- read_xml("../data/mk/mk1.i5.xml")

# Max Frisch: Homo Faber (1957)
txt <- c(txt, xml_text(xml_find_all(xml_children(MK2X)[[4]], ".//s")))

# Heinrich Böll: Ansichten eines Clowns (1963)
txt <- c(txt, xml_text(xml_find_all(xml_children(MK2X)[[2]], ".//s")))

# Thomas Mann: Die Betrogene (1953)
txt <- c(txt, xml_text(xml_find_all(xml_children(MK2X)[[7]], ".//s")))

# BZ is Bild Zeitung, ausgewählte Artikel, Juli 1967
txt <- c(txt, xml_text(xml_find_all(xml_children(MK2X)[[29]], ".//s")))

# load these functions from analyseAtemwende.R if necessary
txt <- collapseHyphenation(txt)
txt <- removePunctuation(txt)
txt <- normalizeWhitespace(txt)
txt <- expandContractions(txt)

words <- unlist(strsplit(txt, " "))
wt <- table(words)
wf <- data.frame("word" = names(wt),
                 "freq"  = as.numeric(wt),
                 stringsAsFactors = FALSE)

# drop all words that begin with a numeral
wf <- wf[- grep("^\\d", wf$word), ]

# drop all words that have a length of less than 2
wf <- wf[! nchar(wf$word) < 2, ]


# Lemmatize (30 sec for 25,000 words)
wl <- data.frame("word" = wf$word,
                 "freq"  = wf$freq,
                 "lemma" = lemmaDict[wf$word, 1],
                 stringsAsFactors = FALSE)

# select missing entries with frequency > 1
wsel <- wl[ (wl$freq > 1) & (is.na(wl$lemma)), ]

# write to file. 
write.table(wsel[order(wsel$freq, decreasing = TRUE), ],
            file = missingFile,
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE,
            sep = "\t")

# Subsequently the relevant entries were manually lemmatized and added to
# ergänzungen.csv

#    
# ==== TESTS ===================================================================

# load(file = lemmaFile)
#
# retrieve lemma for one word
# lemmaDict["spielte", 1]
# 
# retrieve all tokens for one lemma
# rownames(lemmaDict)[lemmaDict$lemma == "spielen"]
# 
# lemmatize a sentence
# 
# x <- unlist(strsplit("du darfst mich getrost mit Schnee bewirten", " "))
# lemmaDict[x, 1]
# 
# To replace the NA with the token:
# y <- lemmaDict[x, 1]
# idxNA <- which(is.na(y))
# y[idxNA] <- x[idxNA]
# y

# [END]
