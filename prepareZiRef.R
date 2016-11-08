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
cedict <- cedict[- grep(" \\[[A-Z]", cedict)]  # drop surnames (capital PY)
cedict <- cedict[- grep("variant of", cedict)] # drop variants


l <- length(ziFreq)
ziRef <- data.frame(S = character(l),
                    T = character(l),
                    PY = character(l),
                    def = character(l),
                    stringsAsFactors = FALSE)


for (i in 1:length(ziFreq)) {
  s <- cedict[substr(cedict, 3, 3) == names(ziFreq)[i]]
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
}


rm(cedictFile)
rm(cedict)
rm(l)
rm(i)
rm(s)
rm(m)

# save(ziRef, file = "../data/ziRef.RData")

#    
# ==== TESTS ===================================================================




# [END]