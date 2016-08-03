# makeLemmaDictionary.R
# 
# Purpose:  Create a dictionary for lemmatization based on the Morphy dictionary
#           with corrections.
#           
# Preconditions: The input file Morphy Mapping must exist.
#                The input file of corrections must exist.
#                                
# Postcondition: A lemma dictionary which contains a lemma in each row with
#                the token (full-form) as the row name. This R object is
#                named lemmaDict and saved to an .RData file.
#                
#
# Notes: The source dictionary was downloaded from  
#           http://www.danielnaber.de/morphologie/ - it has a Creative
#           Commons - By Attribution license.
#
# Creates a lemma dictionary from data found in morphyFile, a tab separated text
# document that contains (optionally) comment lines prefixed with a "#", no
# header, and data lines in which the first element is a token (Vollform) and
# the second element is a lemma (Grundform). The full form is case sensitive. In
# cases where the token is not unique, we replace it with a mixed lemma: eg:
#    abduzierte   abduzieren
#    abduzierte   abduziert
# is replaced with the unique entry:
#    abduzierte   abduzier(t/en)
#    
# Since all tokens are then unique, we can use the tokens as rownames for
# efficient lookup of lemmata.
# 
# V 1.0
# Date:     June 2016
# Author:   Boris Steipe and Yi Chen
#
# ==============================================================================

# ==== INITIALIZATIONS =========================================================

morphyFile   <- "../data/morphy/morphy-mapping-20110717.csv"
corrFile     <- "../data/morphy/grundform-korrekturen.csv"
lemmaFile    <- "../data/lemmaDict.1.0.RData"

# ==== FUNCTIONS ===============================================================

makePolyLemma <- function(x) {
  # Convert tokens in x into a polylemma.
  # 
  # x: a vector of strings
  # return: the longest common prefix, followed by all unique suffixes in
  #         parentheses, sorted by length, separated by "/"
  #         
  # Example:
  #         > x
  #         [1] "abbestellen"  "abbestellt"   "abbestellung"
  #         > makePolyLemma(x)
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


# ==== PROCESS =================================================================

  
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
ucTokens <- grep("^[A-ZÄÖÜ]", tmp[ , 1])  # find all noun-case tokens
ucLcMixed <- ucTokens[grepl("^[a-zäöü]", tmp[ucTokens, 2])] # which of these
# have lowercase
# lemmas?
tmp[ucLcMixed, 1] <- tolower(tmp[ucLcMixed, 1])   # make these mixed entries
# lowercase

# We provide a number of corrections
corr <- read.csv(corrFile,
                 comment.char = "#",
                 blank.lines.skip = TRUE,
                 header = FALSE,
                 sep = "\t",
                 stringsAsFactors = FALSE)

for (i in 1:nrow(corr)) {
  tokenRows <- which(tmp[ , 1] == corr[i, 1])
  if (length(tokenRows) > 0) {
    tmp[tokenRows, 2] <- corr[i, 2]
  } else {
    tmp[nrow(tmp) + 1, ] <- corr[i, ]
  }
}


#=========

# Make all tokens unique:
# find index of all duplicated tokens
dupIdx <- which(duplicated(tmp[ , 1]))

# make a copy of tmp containing only unique tokens
tmpU <- tmp[- dupIdx, ]

# make a list of unique duplicates (i.e. triplicates and higher are collapsed)
dupTokens <- unique(tmp[dupIdx, 1])

for (token in dupTokens) {
  # Note: this loop takes about 2 minutes on the original Morphy dictionary
  lemmata <- tmp[token == tmp[ , 1], 2]
  
  # replace the lemma in the unique-tokens-dataframe with a poly lemma
  # constructed from the lemmata
  tmpU[tmpU[ , 1] == token, 2] <- makePolyLemma(lemmata)
}

# make a new, single column dataframe in which rownames are tokens
# and the entries are lemmata
lemmaDict  <- data.frame("lemma" = tmpU$lemma, stringsAsFactors = FALSE)
rownames(lemmaDict)  <- tmpU$token

# should be 394193

#=========
#
# The original Morphy dictionary does not contain about a third of the lemmata
# as full-form tokens. We simply add all lemmata to the dictionary - duplicates
# will be removed in the next step.

uniqueLemmata <- unique(tmp[ , 2])
tmp <- data.frame("token" = c(tmp[ , 1], uniqueLemmata),
                  "lemma" = c(tmp[ , 2], uniqueLemmata),
                  stringsAsFactors = FALSE)

# Make all tokens unique:
# find index of all duplicated tokens
dupIdx <- which(duplicated(tmp[ , "token"]))

# make a copy of tmp containing only unique tokens
tmpU <- tmp[- dupIdx, ]

# make a list of unique duplicates (i.e. triplicates and higher are collapsed)
dupTokens <- unique(tmp[dupIdx, "token"])

for (token in dupTokens) {
  # Note: this loop takes about 15 minutes
  lemmata <- tmp[token == tmp[ , "token"], "lemma"]
  
  # replace the lemma in the unique-tokens-dataframe with a poly lemma
  # constructed from the lemmata
  tmpU[tmpU[ , "token"] == token, "lemma"] <- makePolyLemma(lemmata)
}

# order by token

x <- tmpU[order(tmpU$token), ]


missingLemmata <- uniqueLemmata[is.na(lemmaDict[uniqueLemmata,1])]





save(lemmaDict, file = lemmaFile)

# Done.

#    
# ==== TESTS ===================================================================

# load(file = "../data/lemmaDict.1.0.RData")
#
# myLemmaDict["spielte", 1]
# rownames(myLemmaDict)[myLemmaDict$Lemma == "spielen"]



# [END]
