# plotPoems.R
#
# Purpose:  return rank-based colors for characters of a poem
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

# showtext to plot Chinese characters
if (!require(showtext)) {
  install.packages("showtext")
  library(showtext)
}


# ==== DEFINITIONS =============================================================


# ==== FUNCTIONS ===============================================================


printPoems <- function(P) {
  # Purpose: describe
  # 
  # Parameters:
  #     a: ...
  #     b: ...
  # Value:
  #     ...
  for (i in 1:nrow(P)) {
    p <- P[i,]
    cat(sprintf("%5d: (%3d:%3d) %-5s - %-20s   %s\n",
                p$poemID,
                p$QTSvol,
                p$QTSnum,
                authorDF$nameS[authorDF$authorID == p$authorID],
                p$titleS,
                p$bodyS))
  }
}


xyHor <- function(iC, nC, iL, nL) {
  # 
  # Purpose: return an x, y pair for grid coordinates according to 
  #            index and number of line and character
  #            in horizontal mode
  # 
  # Parameters:
  #     iC: index of characters
  #     nC: total number of characters
  #     iL: index of lines
  #     nL: total number of lines
  # Value:
  #     (x, y)
       
  return(c(iC - 1 + 0.5 , nL - iL + 0.5))
}

xyVert <- function(iC, nC, iL, nL) {
  # return an x, y pair for grid coordinates according to 
  # index and number of line and character
  # in vertical mode
  # Purpose: describe
  # 
  # Parameters:
  #     a: ...
  #     b: ...
  # Value:
  #     ...
  return(c(nL - iL + 0.5, nC - iC + 0.5))
}


plotPoemCharRanks <- function(P,
                              ranks,
                              mode = "h", 
                              fName = "",
                              fNameSuffix = "",
                              subTitle = "",
                              cex,
                              colMode = "man",
                              nIntervals,
                              bias,
                              cols,
                              cuts) {
  # 
  # Purpose: plots poem P on a grid colour-coded by ranks found in
  #            table "ranks", horizontal or vertical according to "mode" h or v.
  #            If fName is "", construct a filename from author, title.
  # 
  # Preconditions:  (global) authorDF exists
  # 
  # Parameters:
  #     P:           a data frame row that is taken from poemDF or has at least
  #                    the following elements:
  #                    $authorID
  #                    $QTSvol
  #                    $QTSnum
  #                    $titleS
  #                    $bodyS
  #                    
  #     ranks:       table of ranks for each character in P
  #     mode:        "h" for horizontal lines, "v" for plotting in vertical
  #                    (traditional) order
  #     fName:       file name for the postscript output. If empty, filename 
  #                    will be computed from author name and poem title.
  #     fNameSuffix: string to be added to fName. Meant for ".QTS" or ".WY"
  #     subTitle:    text to be plotted below title. Originally meant to 
  #                    identify the source of the character ranks (QTS or WY).
  #     cex:         character expansion factor. If missing, compute a good
  #                    value based on the number of lines.
  #     colMode:     "man" or "log"
  #     nIntervals:  see colorRampPalette() for colMode log
  #     bias:        see colorRampPalette() for colMode log
  # Value:
  #     none, plots to PDF file
  
  if (fName == "") {
    fName <- sprintf("%s - %s%s.pdf",
                     authorDF$nameS[authorDF$authorID == P$authorID],
                     P$titleS,
                     fNameSuffix)
  }
  
  title <- sprintf("%s – %s (%d:%d)",
                   authorDF$nameS[authorDF$authorID == P$authorID],
                   P$titleS,
                   P$QTSvol,
                   P$QTSnum)
  
  lines <- unlist(strsplit(P$bodyS, "\\s+"))
  nLines <- length(lines)
  nChars <- max(nchar(lines))
  
  if (missing(cex)) {  # calculate a reasonable cex
    m <- max(nLines, nChars)
    if (m <= 5) {
      cex <- 3
    } else {
      cex <- 3 / (m / 5)
    }
  }
  
  # white-space between boxes
  padX <- 0.05
  padY <- 0.05
  
  # compute row and column lengths
  if (mode == "h") {
    nHor <- nChars - 1
    nVert <- nLines -1
    getXY <- xyHor
  } else if (mode == "v") {
    nHor <- nLines - 1
    nVert <- nChars - 1
    getXY <- xyVert
  } else {
    stop(sprintf("Unknown mode \"%s\".", mode))
  }
  
  font.add("zhFont", "华文仿宋.ttf")
  
  # open pdf graphics device
  pdf(fName)
  par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0)) # turn all margins off
  
  # plot empty frame
  plot(c(0, nHor + 1), c(0, nVert + 3),
       type = "n", axes = FALSE,
       xlab = "", ylab = "",
       asp = 1.0
  )
  
  # for debugging:
  # abline(v=c(0, nHor + 1)); abline(h=c(0, nVert + 3)); 
  
  showtext.begin()
  
  # adjust title / subtitle cex
  incTitleCex <- 1.2 
  incSubTitleCex <- 0.9
  
  w <- strwidth(title, cex = cex * incTitleCex)
  if (w  > nHor) {
    fCex <- nHor / w
  } else {
    fCex <- 1.0
  }

  # adjust subtitle height
  dY <- 1.5 * strheight(title, cex = cex)
  
  if (mode == "h") {
    tX <- 0 + padX
    tPos <- 4
  } else if (mode == "v") {
    tX <- nHor + 1 - padX
    tPos <- 2
  }
  
  # title
  text(tX, nVert + 2.5, title, cex = incTitleCex * cex * fCex,
       family = "zhFont", pos = tPos, offset = 0)
  #subtitle
  text(tX, nVert + 2.5 - dY, subTitle, cex = incSubTitleCex * cex * fCex,
       family = "zhFont", pos = tPos, offset = 0)
  
  # plot each character in turn
  for (i in 1:nLines) {
    for (j in 1:nChars) {
      xy <- getXY(j, nChars, i, nLines)
      zi <- substr(lines[i], j, j)
      rect(xy[1] - 0.5 + padX,
           xy[2] - 0.5 + padY,
           xy[1] + 0.5 - padX,
           xy[2] + 0.5 - padY,
           col = getFcol(ranks, zi, mode = colMode, n = nIntervals, bias = bias,
                         cols = cols, cuts = cuts),
           border = "#AAAAAA",
           lwd = 0.5)
      text(xy[1], xy[2], zi, cex = cex, family = "zhFont")
    }
  }
  
  showtext.end()
  dev.off()
}




plotPoemGrid <- function(P, ranks, nCol, nRow,
                         colMode = "man", fName = "poemGrid.pdf") {
  
  # Purpose: plots poems P color-coded by frequency ranks found in
  #            table "ranks", on a nCol by nRow grid. Use colMode
  #            as the mode for getFcol() and save result to pdf
  #            file fName.
  # 
  # Parameters:
  #     P:     ...
  #     ranks: ...
  # Value:
  #     None. Saves pdf file.
  
  nIntervals <- 7
  bias <- 0.7
  
  padX <- 0
  padY <- 0
  
  # count longest line and largest number of rows
  maxChar <- 0
  maxRow <- 0
  for (i in 1: nrow(P)) {
    lines <- unlist(strsplit(P$bodyS[i], "\\s+"))
    maxChar <- max(maxChar, max(nchar(lines)))
    maxRow <- max(maxRow, length(lines))
  }
  
  # prepare grid
  nSep <- 1  # number of separation cells
  xGrid <- (nCol * maxChar) + ((nCol - 1) * nSep)
  yGrid <- (nRow * maxRow) + ((nRow - 1) * nSep)
  
  fGrid <- matrix(rep("#FFFFFF", xGrid * yGrid), ncol = xGrid, nrow = yGrid)
  
  iPoem <- 0
  for (iRow in 1:nRow) {
    dY <- (iRow - 1) * (maxRow + nSep) # top cell
    for (iCol in 1:nCol) {
      dX <- (iCol - 1) * (maxChar + nSep) # left cell
      iPoem <- iPoem + 1 # update Poem index
      lines <- unlist(strsplit(P$bodyS[iPoem], "\\s+")) # get lines
      for (i in 1:length(lines)) {
        chars <- unlist(strsplit(lines[i], "")) # get chars
        y <- i + dY          # calculate row height
        for (j in 1:length(chars)) {
          x <- j + dX        # calculate x position
          zi <- chars[j]          
          # get color and put in grid cell
          fGrid[y, x] <- getFcol(ranks, zi,
                                 mode = colMode,
                                 n = nIntervals, bias = bias)
        }
      }
    }
  }
  
  pdf(fName)
  
  par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0)) # turn all margins off
  
  plot(c(0, xGrid + nSep), c(0, yGrid + nSep),
       type = "n", axes = FALSE,
       xlab = "", ylab = "",
       asp = xGrid/yGrid
  )
  
  
  for (i in 1:nrow(fGrid)) {
    for (j in 1:ncol(fGrid)) {
      rect(j + padX,
           yGrid - (i - 1) - padY,
           j + 1 - padX,
           yGrid - i + padY,
           col = fGrid[i, j],
           border = NA,
           lwd = 0.5)
    }
  }
  
  dev.off()
  
}




#    
# ==== EXAMPLES ================================================================

if (FALSE) { # Do not execute when source()'d

  plotPoemCharRanks(poemDF[5251, ], ziRanks,
                    fName = "test.pdf", subTitle = "(QTS ranks)")
  plotPoemCharRanks(poemDF[5251, ], ziRanks, mode = "v",
                    fName = "test.pdf", subTitle = "(QTS ranks)")
  plotPoemCharRanks(poemDF[16479, ], ziRanks,
                    fName = "test.pdf", subTitle = "(QTS ranks)")
  plotPoemCharRanks(poemDF[16479, ], ziRanks, mode = "v",
                    fName = "test.pdf", subTitle = "(QTS ranks)")

  plotPoemGrid(poemDF[poemDF$QTSvol == 128 &
                        poemDF$QTSnum >= 23 &
                        poemDF$QTSnum <= 42, ],
               ranks = ziRanks, nCol = 4, nRow = 5,
               fName = "test.pdf")
  
  
}  # END  if (FALSE)  block

#    
# ==== TESTS ===================================================================

if (FALSE) { # Do not execute when source()'d
  
  
  
}  # END  if (FALSE)  block



# [END]


