# QuanTangShiFrequencies.R
#
# Purpose:  Calculate character frequency table from Quang Tang Shi
#             and do simple analyses
#           
# Precondition: The QTS database objects poemDF and authorDF have been loaded
#                
# Postcondition:  (global) ziCounts exists
#                 (global) ziRanks exists
#                
#
# Notes: 
# 
#
# V 1.2
# Date:     2016-10  -  2022-10
# Author:   Boris Steipe and Yi Chen
#
# ToDo      
#           
# V 1.2     Maintenance 
# V 1.1     Moved ranks calculation into its own function, chqnged 
#              "frequency" to "counts"
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

compileZiCounts <- function(C) {
  # Purpose: compile a counts table from zi in Corpus C.
  # 
  # Parameters:
  #     C: corpus
  # Value:
  #     Counts table
  
  x <- paste(C, collapse = "")
  x <- gsub("\\s+|[‘’1()《》¤�]", "", x)
  x <- unlist(strsplit(x, ""))
  return(sort(table(x), decreasing = TRUE))
}


compileZiRanks <- function(cZ) {
  # Purpose: compile ranks from characters in a counts table cZ.
  # 
  # Parameters:
  #     cZ: counts table
  # Value:
  #     named list of ranks
  
  ranks <- 1:length(cZ)
  names(ranks) <- names(cZ)
  
  # replace ranks for characters with the same frequency with the 
  # rounded mean rank of those characters
  rankRLE <- rle(as.numeric(cZ))
  start <- 1
  for (l in rankRLE$lengths) {
    end <- start + l -1
    ranks[start:end] <- round(mean(ranks[start:end]))
    start <- end + 1
  }
  return(ranks)
}


# ==== PROCESS =================================================================

# == compile Counts and ranks
ziCounts <- compileZiCounts(poemDF$bodyS)

ziRanks <- compileZiRanks(ziCounts)


# Done: ziCounts and ziRanks exist


# ===  ANALYZE AND DISPLAY =====================================================


if (FALSE) { # Do not execute when source()'d
  

length(ziCounts)    # 7454
sum(ziCounts == 1)  # 969 hapax
sum(ziCounts <= 3)  # 1745

print(paste(names(ziCounts), collapse=""))  # show all characters


# == plot log(rank) vs. log(frequency)
l <- length(ziCounts)
cV <- getFcol(mode = "internals")

plot(log10(1:l), log10(as.numeric(ziCounts[1:l])),
     xlab=expression(log[10](rank)), ylab=expression(log[10](counts)),
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
         log10(as.numeric(ziCounts[cuts[i]:cuts[i+1]])),
         col = cV$col[i], bg = cV$col[i], pch=21)
}

# == plot (rank vs. frequency)
l <- length(ziCounts)
cV <- getFcol(mode = "internals")

plot(1:l, as.numeric(ziCounts[1:l]),
     xlab = "rank", ylab = "counts",
     cex = 1.3)

for (i in 1:length(cV$cut)) {
  abline(v = cV$cut[i], col="#cccccc", lwd=0.5)
}

cuts <- c(1, cV$cut)
for (i in 1:length(cV$cut)) {
  points(cuts[i]:cuts[i+1],
         as.numeric(ziCounts[cuts[i]:cuts[i+1]]),
         col = cV$col[i], bg = cV$col[i], pch=21)
}

# plot counts for top 30
plot(1:30, as.numeric(ziCounts[1:30]),
     main = "Top 30 characters in Quán Táng Shī",
     xlab="rank", ylab="Counts",
     ylim = c(1, 27000),
     bg = "#c2ffd8", pch = 21, type = "b")

}  # END  if (FALSE)  block

#    
# ==== TESTS ===================================================================




# [END]


