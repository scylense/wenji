# analyseAtemwende.R
#
# Purpose:  Sample code to read and analyse Atemwende poems.
#           
# Preconditions: Poems exist as textfiles in the ../data/Atmewende folder.
#                                
#                
# Postcondition: ...
#                
## 
#
# V 0.1
# Date:     June 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo      ...
#           
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


# ==== DEFINITIONS =============================================================

# ...

# ==== FUNCTIONS ===============================================================


#


# ==== PROCESS =================================================================


# get a vector of filenames to process
filenames <- list.files("../data/Atemwende", full.names = TRUE)

# add the contents one by one to the vector AW
AW <- ""
for( i in 1:length(filenames)) {
  AW <- c(AW, readLines(filenames[i]))
}

# drop all empty lines
AW <- AW[AW != ""]

# make everything lower-case
AW <- tolower(AW)

# Strsplit this into words
AWwords <- unlist(strsplit(AW, "\\W+"))
AWwords <- AWwords[AWwords != ""]


# how many words are there?
length(AWwords)  # 3654

# tabulate word frequencies and sort
AWfreq <- sort(table(AWwords), decreasing = TRUE)

# how many unique words?
length(AWfreq)

# look at the top 100
head(AWfreq, 100)

# look at the bottom 100
tail(AWfreq, 100)

# plot log rank vs. log frequency (Zipf plot)
plot(log(1:length(AWfreq)), log(AWfreq))

# look at all words that appear exactly twice
AWfreq[AWfreq == 2]



#    
# ==== TESTS ===================================================================




# [END]