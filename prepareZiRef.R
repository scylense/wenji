# prepareZiref.R
#
# Purpose:  prepare (global) object ziRef with character information
#           
# Precondition: The cedict dictionary source file exists in
#               "../cedict/cedict_ts.u8.txt"
#               ziFreq has been loaded
#                
# Postcondition:  (global) object ziRef exists and has been saved to
#                 "../data/ziRef.RData"
#
# Notes: 
# 
#
# V 1.0
# Date:     November 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo      
#           
# V 1.0     First code 
#
# ==============================================================================

setwd(WENJIDIR)

# ==== PACKAGES ================================================================


# ==== DEFINITIONS =============================================================

cedictFile <- "../cedict/cedict_ts.u8.txt"

# ==== FUNCTIONS ===============================================================


# ==== PROCESS =================================================================

cedict <- readLines(cedictFile)
cedict <- cedict[- grep("^#", cedict)]  # discard comments
cedict <- cedict[substr(cedict, 2, 2) == " "]  # keep single character entries
# cedict <- cedict[- grep(" \\[[A-Z]", cedict)]  # drop surnames (capital PY)
# cedict <- cedict[- grep("variant of", cedict)] # drop variants


l <- length(ziFreq)
ziRef <- data.frame(S = character(l),
                    T = character(l),
                    PY = character(l),
                    def = character(l),
                    stringsAsFactors = FALSE)


for (i in 1:l) {
  if (! i %% 500) { print(i) }
  s <- cedict[substr(cedict, 3, 3) == names(ziFreq)[i]]
  if (length(s) == 0) { # character not found - is it a traditional one?
    s <- cedict[substr(cedict, 1, 1) == names(ziFreq)[i]]
  }

  if (length(s) > 0) { # more than one entry - attempt ordering
    priority <- rep(1L, length(s))
    priority[grep(" \\[[A-Z]", s)] <- 0   # capital pinyin = surname
    priority[grep("/\\(classical\\)", s)] <- 0   # old use
    priority[grep("/\\(archaic\\)", s)] <- 0   # old use
    priority[grep("/\\(arch.\\)", s)] <- 0   # old use
    priority[grep("/\\(onom.\\)", s)] <- 0   # onomatopoetic, not meaning
    priority[grep("/variant", s)] <- 0   
    priority[grep("/old variant", s)] <- 0   
    priority[grep("/erroneous variant", s)] <- 0   
    priority[grep("/see ", s)] <- 0   
    s <- s[order(priority, decreasing = TRUE)]
  }
    
  if (length(s) > 0) { # process it
    ziRef$S[i] <- substr(s[1], 3, 3)
    ziRef$T[i] <- substr(s[1], 1, 1)
    m <- regexec("(\\[.+?\\])", s[1])
    ziRef$PY[i] <- unlist(regmatches(s[1], m))[1]
    m <- regexec("(/.+/$)", s[1])
    ziRef$def[i] <- unlist(regmatches(s[1], m))[1]
    if (length(s) > 1) {
      ziRef$def[i] <- paste(c(ziRef$def[i], s[-1]),
                            collapse = "+")
    }
  } else { #not found
    ziRef$S[i] <- names(ziFreq)[i]
    ziRef$T[i] <- names(ziFreq)[i]
    ziRef$PY[i] <- "[?]"
    ziRef$def[i] <- "/?/"
  }
}

row.names(ziRef) <- names(ziFreq)

# check
for (i in 1:300) {
  cat(sprintf("%s %s %8s\t%s\n", 
      ziRef$S[i],
      ziRef$T[i],
      ziRef$PY[i],
      substr(ziRef$def[i], 1, 50)))
}

# save(ziRef, file = "../data/ziRef.RData")

rm(cedictFile)
rm(cedict)
rm(l)
rm(i)
rm(s)
rm(m)


#    
# ==== TESTS ===================================================================




# [END]