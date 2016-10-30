# analyzeZiEnrichment.R
#
# Purpose:  analyze Character enrichment
#           
# Precondition: 
#                
# Postcondition: None. Prints Output
#                
#
# Notes: 
# 
#
# V 0.1
# Date:     October 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo      
#           
# V 0.1     First code experiments
#
# ==============================================================================


printZiEnrichment <- function(N, fX, fY, xlab = "", ylab = "") {
  # N - number of words
  # fX, fY - frequency tables. fX is the corpus of interest, fY is the 
  # reference table.

  fX <- sort(fX, decreasing = TRUE)
  fY <- sort(fY, decreasing = TRUE)

  # define union of N-most frequent zi in both tables
  # In this way, the output will hav more than N lines, but it will
  # contain the N-ranked zi from both tables.
  allZi <- unique(c(names(fX[1:N]), names(fY[1:N])))
  N <- length(allZi)
  
  rX <- as.table(1:length(fX)) # make list of ranks
  names(rX) <- names(fX)
  
  rY <- as.table(1:length(fY))
  names(rY) <- names(fY)
  
  enDat <- data.frame(zi = allZi,
                      Xrank = as.numeric(rX[allZi]),
                      Yrank = as.numeric(rY[allZi]),
                      fX = numeric(N),
                      fY = numeric(N),
                      relX = numeric(N),
                      relY = numeric(N),
                      logRatio = numeric(N),
                      stringsAsFactors = FALSE
  )
  enDat$fX <- as.numeric(fX[rX[allZi]])
  enDat$fY <- as.numeric(fY[rY[allZi]])
  enDat$relX <- enDat$fX / sum(fX)
  enDat$relY <- enDat$fY / sum(fY)
  enDat$logRatio <- log10(enDat$relX / enDat$relY)
  
  # print output 
  cat("\n\n")
  cat(sprintf("column \"X\" is %s\n", xlab))
  cat(sprintf("column \"Y\" is %s\n", ylab))
  cat(sprintf("fX values are out of %d total counts.\n", sum(fX)))
  cat(sprintf("fY values are out of %d total counts.\n\n", sum(fY)))
  
  cat(" rank |   rank |      |        |        |                       \n")
  cat("    X |      Y | char |     fX |    fY  | ratio                 \n")
  cat("------|--------|------|--------|--------|-----------------------\n")

  for (i in order(abs(enDat$logRatio), decreasing = TRUE)) {
    
    if (enDat$logRatio[i] > 0) {
    
    # express enrichment / depletion as 1:x resp. x:1 ratio.  
    ratio <- sprintf (" %4.2f : 1", enDat$relX[i] / enDat$relY[i])
    } else {
      ratio <- sprintf ("             1 : %4.2f", enDat$relY[i] / enDat$relX[i])
    }
    
    cat(sprintf("  %3d | %6d |  %s  | %6d | %6d | %s\n",
                enDat$Xrank[i],
                enDat$Yrank[i],
                enDat$zi[i],
                enDat$fX[i],
                enDat$fY[i],
                ratio
    ))
  }
}


# test with Wang Wei Corpus:

# P <- getQTSpoemsByAuthor("王维")
# fX <- ziTable(P$bodyS)
# 
# printZiEnrichment(50, 
#                   fX, ziFreq, 
#                   xlab = "Wang Wei - all poems", ylab = "all QTS")

# [END]