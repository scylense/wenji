# getFcol.R
#
# Purpose:  return rank-based colors for characters 
#           
# Precondition: 
#                
# Postcondition:  
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
#
# ==============================================================================

if (FALSE) { # Do not execute when source()'d
  setwd(WENJIDIR)
}

# ==== PACKAGES ================================================================


# ==== DEFINITIONS =============================================================


# ==== FUNCTIONS ===============================================================

getFcol <- function(ranks, char, mode = "man", n = 7, bias = 0.7, cols, cuts) {
  
  # Purpose: return a color value for character char based in its rank in ranks
  # 
  # Parameters:
  #     ranks: a table of ranks and names
  #     char: a character expected to be present among the names of ranks
  #     mode: man / log to return color values, internals to return the function
  #             internal parameters
  #     n:    for mode log, the number of steps for colorRampPalette
  #     bias: cf. colorRampPalette()
  #     cols: manually set color values (must be seven values)
  #     cuts: manually set cutoffs
  # Value:
  #     a single colour value, for modes man and log
  #     a list exposing the internals for mode internals 
  
  
  cDat <- data.frame(cut = c(   30, # top 30 (~15% of all char)
                               100, # ~ 30%
                               515, # ~ 67%
                              1000, # ~ 82%
                              5715, # f <= 4
                              6491, # hapax
                             7455),
                     col = c("#c2ffd8",
                             "#d3eff5",
                             "#f2f6ff",
                             "#fff5f0",
                             "#ffdadf",
                             "#f67884",
                             "#dd0000"),
                     stringsAsFactors = FALSE)
  
  # allow override by parameter
  if (! missing(cols)) {
    cDat$col <- cols
  }

  if (! missing(cuts)) {
    cDat$cut <- cuts
  }
    
  if (mode == "log") {
    # get a color representing the frequency of "char" in table
    # "tab", spaced on a "mode" (log/lin) scale with "n"
    # intervals
    spect <- colorRampPalette(c("#d8ffe8",
                                "#fbfbfb",
                                "#fcca83",
                                "#ff2d55"),
                              bias = bias) 
    # barplot(rep(1, n), col=spect(n), axes=F, main="")
    
    maxF <- log10(max(ranks))
    minF <- log10(min(ranks))
    charF <- log10(ranks[char])
    iF <- (round((n-1) * (1 - (charF - minF) / (maxF - minF)))) + 1
    
    return(spect(n)[iF])
    
  } else if (mode == "man") {
    # get manually spaced colors
    i <- sum(ranks[char] > cDat$cut) + 1
    return(cDat$col[i])
    
  }  else if (mode == "internals") {
    return(cDat)
    
  } else {
    stop(sprintf("Unknown mode \"%s\"", mode))
  }
  
}





# ==== PROCESS =================================================================

#    
# ==== DISPLAY COLOR LEVELS =================================================

if (FALSE) { # Do not execute when source()'d
  
  # plot the color scale:
  cV <- getFcol(mode = "internals")
  l <- nrow(cV)
  opar <- par()
  cex <- 1.0
  par(oma = c(0,0,0,0), mar = c(0,0,0,0))
  barplot(rep(0.25, l), col=cV$col, axes = FALSE,
          xlim = c(-1.2, l + 1.5), ylim = c(-0.5, 1),
          )
  text(-1.2, -0.1, "Ranks:", pos = 4, cex = cex)
  for (i in 1:l) {
    text( (i - 1) * 1.2, -0.1, sprintf(" < %d", cV$cut[i]), pos = 4, cex = cex)
  }

  text(6.0, -0.2, "(observed 2\nor 3 times)", pos = 4, cex = cex)
  text(7.2, -0.3, "(observed\nonly once)", pos = 4, cex = 0.67)

  text(-1.2, -0.4, "% covered:", pos = 4, cex = cex)

  for (i in 1:l) {
    text( (i - 1) * 1.2, -0.4,
          sprintf("%5.2f", sum(ziFreq[1:cV$cut[i]]) / sum(ziFreq) * 100),
          pos = 4, cex = cex)
  }
  
  par <- opar
  
}  # END  if (FALSE)  block

#    
# ==== TESTS ===================================================================

if (FALSE) { # Do not execute when source()'d
  
  
  
}  # END  if (FALSE)  block



# [END]


