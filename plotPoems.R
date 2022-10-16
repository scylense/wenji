# plotPoems.R
#
# Purpose:  return rank-based colors for characters of a poem
#           
# Precondition: system font "华文仿宋.ttf" exists
#                
# Postcondition:  may print plots to pdf
#                
#
# Notes: 
# 
#
# V 2.0
# Date:     October 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo      
#           
# V 2.0     Use new mapCol function and switch for different types of coloring
# V 1.0     Stable code
#
# ==============================================================================

if (FALSE) { # Do not execute when source()'d
  setwd(WENJIDIR)
}

# ==== PACKAGES ================================================================

# showtext to plot Chinese characters
if (! requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext")
}
library(showtext)


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
    cat(sprintf("%20s\t  %5d: (%3d:%3d)\t%-5s -\t%s\n",
                p$bodyS,
                p$poemID,
                p$QTSvol,
                p$QTSnum,
                authorDF$nameS[authorDF$authorID == p$authorID],
                p$titleS))
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


getTagMatrix <- function(s) {
  # 
  # Purpose: return a matrix of pos tags, one column per tag, one row per line
  # 
  # Parameters:
  #     s: a string of pos tags
  # Value:
  #     m: a matrix of tags
  
  lines <- unlist(strsplit(s, "\\s+"))
  
  l <- list() # put values into a list at first because we don't know what the
              # max number of characters in a line will be
  maxC <- 0
  for (i in 1:length(lines)) {
    l[[i]] <- unlist(strsplit(lines[i], "\\+"))
    maxC <- max(maxC, length(l[[i]]))
    for (j in 1:length(l[[i]])) {
      l[[i]][j] <- unlist(strsplit(l[[i]][j], "#"))[2]
    }
  }
  
  m <- matrix(character(length(lines) * maxC), nrow = length(lines))
  
  for (i in 1:length(l)) {
    for (j in 1:length(l[[i]])) {
      m[i, j] <- l[[i]][j]
    }
  }
  return(m)
}


plotPoem <- function(P,
                     map,
                     mode = "h", 
                     fName = "",
                     fNameSuffix = "",
                     subTitle = "",
                     cex) {
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
  #                    $POS    (if POS tag coloring is used)
  #                    
  #     map:         a map of values to colors. See mapCol.R for details.
  #     mode:        "h" for horizontal lines, "v" for plotting in vertical
  #                    (traditional) order
  #     fName:       file name for the postscript output. If empty, filename 
  #                    will be computed from author name and poem title.
  #     fNameSuffix: string to be added to fName. Meant for ".QTS" or ".WY"
  #     subTitle:    text to be plotted below title. Originally meant to 
  #                    identify the source of the character ranks (QTS or WY).
  #     cex:         character expansion factor. If missing, compute a good
  #                    value based on the number of lines.
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
  
  if (map$type == "pos") { # part-of-speech map
    posMat <- getTagMatrix(P$POS)
  }
  
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
  
  # compute row and column lengths for mode h or v
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
  incTitleCex <- 1.2     # relative scale to main text
  incSubTitleCex <- 0.9
  
  w <- strwidth(title, cex = cex * incTitleCex)
  if (w  > nHor) {    # reduce scale for long titles that would print beyond
                      # the margin
    fCex <- nHor / w
  } else {
    fCex <- 1.0
  }

  # adjust subtitle vertical separtion from title
  dY <- 1.5 * strheight(title, cex = cex)
  
  if (mode == "h") {
    tX <- 0 + padX
    tPos <- 4
  } else if (mode == "v") {
    tX <- nHor + 1 - padX
    tPos <- 2
  }
  
  # plot title
  text(tX, nVert + 2.5, title, cex = incTitleCex * cex * fCex,
       family = "zhFont", pos = tPos, offset = 0)
  # plot subtitle
  text(tX, nVert + 2.5 - dY, subTitle, cex = incSubTitleCex * cex * fCex,
       family = "zhFont", pos = tPos, offset = 0)
  
  # plot each character in turn
  for (i in 1:nLines) {
    for (j in 1:nChars) {
      xy <- getXY(j, nChars, i, nLines)
      zi <- substr(lines[i], j, j)

      if (map$type == "pos") {
        char <- posMat[i, j]
      } else if (map$type == "char") {
        char <- zi
      }
      cCol <- mapCol(map, char)
      
      rect(xy[1] - 0.5 + padX,
           xy[2] - 0.5 + padY,
           xy[1] + 0.5 - padX,
           xy[2] + 0.5 - padY,
           col = cCol,
           border = "#AAAAAA",
           lwd = 0.5)
      text(xy[1], xy[2], zi, cex = cex, family = "zhFont")
    }
  }
  
  showtext.end()
  dev.off()
}




plotPoemGrid <- function(P, map, nCol, nRow,
                         fName = "poemGrid.pdf") {
  
  # Purpose: plots poems P color-coded by values found in
  #            map, on a nCol by nRow grid. Save result to pdf
  #            file fName.
  # 
  # Parameters:
  #     P:           a data frame row that is taken from poemDF or has at least
  #                    the following elements:
  #                    $bodyS  (if rank based colouring is used)
  #                    $POS    (if pos tag colouring is used)
  #                    
  #     map:         a map of values to colors. See mapCol.R for details.
  #     nCol, nRow: layout of grid on the page
  # Value:
  #     None. Saves pdf file.
  
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
  for (iRow in 1:nRow) { # for each row in grid ...
    dY <- (iRow - 1) * (maxRow + nSep) # top cell position
    
    for (iCol in 1:nCol) { # for each column in grid...
      dX <- (iCol - 1) * (maxChar + nSep) # left cell position
      iPoem <- iPoem + 1 # update Poem index
      lines <- unlist(strsplit(P$bodyS[iPoem], "\\s+")) # get lines
      if (map$type == "pos") { # switch for map type
        posMat <- getTagMatrix(P$POS[iPoem])
      }
      
      for (i in 1:length(lines)) { # for each line in poem ...
        chars <- unlist(strsplit(lines[i], "")) # get chars
        y <- i + dY          # calculate row y position
        
        for (j in 1:length(chars)) { # for each character in line ...
          x <- j + dX        # calculate character x position
          
          if (map$type == "pos") { # switch for map type
            char <- posMat[i, j]
          } else if (map$type == "char") {
            char <- chars[j]
          }
          
          # get color and put in grid cell
          fGrid[y, x] <- mapCol(map, char)
        }
      }
    }
  }
  
  pdf(fName)
  
  par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0)) # turn all margins off
  
  plot(c(0, xGrid + nSep), c(0, yGrid + nSep), # plot empty frame
       type = "n", axes = FALSE,
       xlab = "", ylab = "",
       asp = xGrid/yGrid
  )
  
  
  for (i in 1:nrow(fGrid)) {
    for (j in 1:ncol(fGrid)) {
      rect(j + padX,    # draw colored rectangle
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

  plotPoem(poemDF[5251, ], map = qtsMap,
           fName = "test1.qts.pdf", subTitle = "(QTS ranks)")
  plotPoem(poemDF[5251, ], map = wyMap,
           fName = "test1.wy.pdf", subTitle = "(WY ranks)")
  plotPoem(poemDF[5251, ], map = posMap,
           fName = "test1.pos.pdf", subTitle = "(POS tags)")
  
  plotPoem(poemDF[16479, ], map = qtsMap,
           fName = "test2.qts.pdf", subTitle = "(QTS ranks)")
  plotPoem(poemDF[16479, ], map = wyMap,
           fName = "test2.wy.pdf", subTitle = "(WY ranks)")
  plotPoem(poemDF[16479, ], map = posMap,
           fName = "test2.pos.pdf", subTitle = "(POS tags)")
  
  
  plotPoemGrid(poemDF[poemDF$QTSvol == 128 &
                        poemDF$QTSnum >= 23 &
                        poemDF$QTSnum <= 42, ],
               map = qtsMap, nCol = 4, nRow = 5,
               fName = "testGrid.qts.pdf")
  
  plotPoemGrid(poemDF[poemDF$QTSvol == 128 &
                        poemDF$QTSnum >= 23 &
                        poemDF$QTSnum <= 42, ],
               map = wyMap, nCol = 4, nRow = 5,
               fName = "testGrid.wy.pdf")
  
  plotPoemGrid(poemDF[poemDF$QTSvol == 128 &
                        poemDF$QTSnum >= 23 &
                        poemDF$QTSnum <= 42, ],
               map = posMap, nCol = 4, nRow = 5,
               fName = "testGrid.pos.pdf")
  
  
  
    
}  # END  if (FALSE)  block

#    
# ==== TESTS ===================================================================

if (FALSE) { # Do not execute when source()'d
  
  
  
}  # END  if (FALSE)  block



# [END]


