# QuanTangShiFrequencies.R
#
# Purpose:  Calculate character frequency table from Quang Tang Shi
#             and do simple analyses
#           
# Precondition: The QTS database objects poemDF and authorDF have been loaded
#                
# Postcondition:  (global) ziFreq exists
#                 (global) ziRanks exists
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
# V 1.0     Stable code
# V 0.1     First code experiments
#
# ==============================================================================

if (FALSE) { # Do not execute when source()'d
  setwd(WENJIDIR)
}

# ==== PACKAGES ================================================================


# ==== DEFINITIONS =============================================================


# ==== FUNCTIONS ===============================================================

ziTable <- function(C) {
  # compile a frequency table from zi in Corpus C.
  # Purpose: describe
  # 
  # Parameters:
  #     C: corpus
  #     b: ...
  # Value:
  #     ...
  
  x <- paste(C, collapse = "")
  x <- gsub("\\s+|[‘’1()《》¤�]", "", x)
  x <- unlist(strsplit(x, ""))
  return(sort(table(x), decreasing = TRUE))
}


# ==== PROCESS =================================================================

# == compile Table
ziFreq <- ziTable(poemDF$bodyS)


# == create named vector of ranks
ziRanks <- 1:length(ziFreq)
names(ziRanks) <- names(ziFreq)

# replace ranks for characters with the same frequency with the 
# mean rank of those characters
ziRankRLE <- rle(as.numeric(ziFreq))
start <- 1
for (l in ziRankRLE$lengths) {
  end <- start + l -1
  ziRanks[start:end] <- mean(ziRanks[start:end])
  start <- end + 1
}
rm(ziRankRLE)
rm(start)
rm(end)
rm(l)

# Done: ziFreq and ziRanks exist


# ===  ANALYZE AND DISPLAY =====================================================


if (FALSE) { # Do not execute when source()'d
  

length(ziFreq)    # 7454
sum(ziFreq == 1)  # 969 hapax
sum(ziFreq <= 3)  # 1745

print(paste(names(ziFreq), collapse=""))  # show all characters


# == plot log(rank) vs. log(frequency)
l <- length(ziFreq)
cV <- getFcol(mode = "internals")

plot(log10(1:l), log10(as.numeric(ziFreq[1:l])),
     xlab=expression(log[10](rank)), ylab=expression(log[10](frequency)),
     xaxt = "n", yaxt = "n",
     xlim = c(0, 4), ylim = c(0, 5), 
     cex = 1.3)
axisMinorTicks(1, 9, mn=0, mx=4)
axisMinorTicks(2, 5, mn=0, mx=5)

for (i in 1:length(cV$cut)) {
  abline(v=log10(cV$cut[i]), col="#cccccc", lwd=0.5)
}

cuts <- c(1, cV$cut)
for (i in 1:length(cV$cut)) {
  points(log10(cuts[i]:cuts[i+1]),
         log10(as.numeric(ziFreq[cuts[i]:cuts[i+1]])),
         col = cV$col[i], bg = cV$col[i], pch=21)
}

# == plot (rank vs. frequency)
l <- length(ziFreq)
cV <- getFcol(mode = "internals")

plot(1:l, as.numeric(ziFreq[1:l]),
     xlab = "rank", ylab = "frequency",
     cex = 1.3)

for (i in 1:length(cV$cut)) {
  abline(v = cV$cut[i], col="#cccccc", lwd=0.5)
}

cuts <- c(1, cV$cut)
for (i in 1:length(cV$cut)) {
  points(cuts[i]:cuts[i+1],
         as.numeric(ziFreq[cuts[i]:cuts[i+1]]),
         col = cV$col[i], bg = cV$col[i], pch=21)
}

# plot absolute frequencies for top 30
plot(1:30, as.numeric(ziFreq[1:30]),
     main = "Top 30 characters in Quán Táng Shī",
     xlab="rank", ylab="Frequency",
     ylim = c(1, 27000),
     bg = "#c2ffd8", pch = 21, type = "b")

}  # END  if (FALSE)  block

#    
# ==== TESTS ===================================================================




# [END]


