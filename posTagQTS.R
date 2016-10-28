# posTagQTS.R
#
# Purpose:  POS tag the QTS corpus
#           
# Precondition: The poemDF object exists in
#               "../data/poemDF.RData"
#               pos-tagging python code exists in ../POS/tagQTS.py
#                
# Postcondition:  A tagged file was written to ../POS/ for each volume,
#                 one tagged poem per line.
#                
#
# Notes: 
# 
#
# V 1.0
# Date:     October 2016
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




# ==== FUNCTIONS ===============================================================


# ==== PROCESS =================================================================

load("../data/poemDF.RData")
load("../data/authorDF.RData")

# List volumes
QTSvolumes <- unique(poemDF$QTSvol)

poemFile <- "../POS/QTSpoems.txt"
last <- 10
for (v in QTSvolumes) {
  if (v > last) {
    posFile <- sprintf("../POS/QTS_POS.%03d.txt", v)
    cat(sprintf("tagging volume %03d to %s (%s)\n", v, posFile, Sys.time()))
    poems <- poemDF$body[poemDF$QTSvol == v]
    out <- character(length(poems))
    for (i in 1:length(poems)) {
      out[i] <- paste(unlist(strsplit(gsub(" ", "", poems[i]), "")),
                      collapse = " ")
    }
    writeLines(out, poemFile)
    system(sprintf("../POS/tagQTS.py %s %s", poemFile, posFile))
  }
}


#    
# ==== TESTS ===================================================================




# [END]