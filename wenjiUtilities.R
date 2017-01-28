# wenjiUtilities.R
# 
# Purpose:  utility functions useful to the wenji project
# 
# Preconditions: None
# 
# Postcondition: The enclosed functios are defined
# 
# 
# Version:  1.1 
# Date:     October - November 2016 
# Author:   Boris Steipe and Yi Chen
# 
# History:
#   V 1.1  add pareto plot function
#   V 1.0  First code
# 
# ==============================================================================


# ==== FUNCTIONS ===============================================================
axisMinorTicks <- function(ax, n, t.ratio=0.5, mn, mx,...){
  # minimally modified from function by Joris Meys
  # http://stackoverflow.com/questions/6955440
  lims <- par("usr")
  if(ax %in% c(1,3)) lims <- lims[1:2] else lims[3:4]
  
  major.ticks <- pretty(lims,n=5)
  if(missing(mn)) mn <- min(major.ticks)
  if(missing(mx)) mx <- max(major.ticks)
  
  major.ticks <- major.ticks[major.ticks >= mn & major.ticks <= mx]
  
  labels <- sapply(major.ticks,function(i)
    as.expression(bquote(10^ .(i)))
  )
  axis(ax, at = major.ticks, labels = labels,...)
  
  n <- n+2
  minors <- log10(pretty(10^major.ticks[1:2], n)) - major.ticks[1]
  minors <- minors[-c(1,n)]
  
  minor.ticks = c(outer(minors,major.ticks,`+`))
  minor.ticks <- minor.ticks[minor.ticks > mn & minor.ticks < mx]
  
  axis(ax, at = minor.ticks, tcl = par("tcl") * t.ratio, labels = FALSE)
}

pareto <- function(x, col, sorted = FALSE) {
  # plot a pareto-plot of x using colours col
  if (sorted) {
    x <- sort(x, decreasing = TRUE)
  }
  oPar <- par(mar = c(3, 4, 0.75, 0))
  barplot(as.numeric(x) / sum(as.numeric(x)) * 100,
          ylim = c(0,105),
          ylab = "% of total",
          names.arg = names(x),
          cex.names = 0.5,
          col = col[names(x)])
  points((1:length(x) * 1.2) - 0.5,
         cumsum(as.numeric(x) / sum(as.numeric(x)) * 100),
         type = "b", pch = 21, bg = col[names(x)])
  par(oPar)
}


calcMeanLogRank <- function(s, ranks = ziRanks, noLog = FALSE) {
  # Calculate the mean (log) frequency rank for all characters in a poem "s"
  # using rank vector ranks.
  s <- unlist(strsplit(gsub(" ", "", s), ""))
  if (noLog) {
    x <- as.numeric(mean(ranks[s]))
  } else {
    x <- as.numeric(mean(log(ranks[s])))
  }
  return(x)
}


#    
# ==== TESTS ===================================================================
if (FALSE) { # Do not execute when source()'d
  
  x <- 10^(0:8)
  y <- 1:9
  plot(log10(x), y, xaxt="n", xlab="x", xlim=c(0,9))
  axisMinorTicks(1, 9, mn=0, mx=8)
  

} # end if (FALSE)
# [END]
