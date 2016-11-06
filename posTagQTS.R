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
# process each volume and write tags to output
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

# add column to poemDF
poemDF <- data.frame(poemDF[ , 1:7], 
                     POS = character(nrow(poemDF)), 
                     poemDF[ , 8:10], 
                     stringsAsFactors = FALSE)


iPoem <- 0

# process each file and add POS to poemDF
for (v in unique(poemDF$QTSvol)) {
  
  if (! v %% 10) { print(v) }
  
  posFile <- sprintf("../POS/QTS_POS.%03d.txt", v)
  posTags <- readLines(posFile)
  poems <- poemDF$body[poemDF$QTSvol == v]
  for (i in 1:length(poems)) { # process poems in volume
    iPoem <- iPoem + 1
    lines <- unlist(strsplit(poems[i], " "))
    srcPOS <- unlist(strsplit(posTags[i], " "))
    bodyPOS  <- character(length(lines))
    for (j in 1:length(lines)) { # process lines in poem
      chars <- unlist(strsplit(lines[j], ""))
      linePOS <- character(length(chars))
      for (k in 1:length(chars)) { # process characters in line
        # validate tag
        POS <- srcPOS[k]
        if (substr(POS, 1, 1) != chars[k]) {
          stop(sprintf("Character mismatch in vol:%d, poem:%d, line:%d",
                       v, i, j),
               sprintf(" (%s vs. %s)", chars[k], POS))
        }
        # add tag to linePOS
        linePOS[k] <- POS
      } # end characters
      # collapse linePos and add to bodyPos
      bodyPOS[j] <- paste(linePOS, collapse = "+")
      # remove processed characters from srcPOS
      srcPOS <- srcPOS[-(1:length(chars))]
    } # end lines
    # collapse bodyPos and add to poemDF
    poemDF$POS[iPoem] <- paste(bodyPOS, collapse = " ")
    # sanity check
    if (substr(poemDF$POS[iPoem], 1, 1) != substr(poemDF$bodyS[iPoem], 1, 1)) {
      stop(sprintf("First character mismatch in row:%d (%s vs. %s)",
                   iPoem,
                   substr(poemDF$POS[iPoem], 1, 1),
                   substr(poemDF$bodyS[iPoem], 1, 1)))
    }
  } # end poems
} # end volumes


# save(poemDF, file = "../data/poemDF.RData")




#    
# ==== TESTS ===================================================================




# [END]