# QTSfrequencyAnalysis.R
#
# Purpose:  Perform character frequency analysis in the QTS corpus
#           
# Precondition: The poemDF object exists in
#               "../data/poemDF.RData"
#                
# Postcondition:
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

setwd(WENJIDIR)

# ==== PACKAGES ================================================================

# showtext to plot Chinese characters
if (!require(showtext)) {
  install.packages("showtext")
  library(showtext)
}


# ==== DEFINITIONS =============================================================

# frequently searched authors:
# Wang Wei    王维
# Li Bai      李白
# Li Shangyin 李商隐
# Du Fu       杜甫
# Song Zhiwen 宋之问


# ==== FUNCTIONS ===============================================================

xyHor <- function(iC, nC, iL, nL) {
  # return an x, y pair for grid coordinates according to 
  # index and number of line and character
  # in horizontal mode
  return(c(iC + 0.5 , nL + 1 - iL + 0.5))
}

xyVert <- function(iC, nC, iL, nL) {
  # return an x, y pair for grid coordinates according to 
  # index and number of line and character
  # in vertical mode
  return(c(nL + 1 - iL + 0.5, nC + 1 - iC + 0.5))
}

getFcol <- function(tab, char, mode = "log", n = 7, bias = 0.7) {
  # get a color representing the frequency of "char" in table
  # "tab", spaced on a "mode" (log/lin) scale with "n"
  # intervals
  spect <- colorRampPalette(c("#d8ffe8",
                              "#fbfbfb",
                              "#fcca83",
                              "#ff2d55"),
                            bias = bias) 
  # barplot(rep(1, n), col=spect(n), axes=F, main="")
  
  maxF <- log(max(tab))
  minF <- log(min(tab))
  charF <- log(tab[char])
  iF <- (round((n-1) * (1 - (charF - minF) / (maxF - minF)))) + 1
  
  return(spect(n)[iF])
}

plotPoemFreq <- function(P, freq, mode = "h", fName = "poemFreq.pdf",
                         nIntervals = 7, cex = 1, bias = 0.7) {
  # plots poem P on a grid color-coded by frequencies found in
  # table "freq", horizontal or vertical according to "mode" h or v.
  lines <- unlist(strsplit(P$bodyS, "\\s+"))
  nLines <- length(lines)
  nChars <- max(nchar(lines))
  padX <- 0.05
  padY <- 0.05
  
  if (mode == "h") {
    nHor <- nChars
    nVert <- nLines
    getXY <- xyHor
  } else if (mode == "v") {
    nHor <- nLines
    nVert <- nChars
    getXY <- xyVert
  } else {
    stop(sprintf("Unknown mode \"%s\".", mode))
  }
  
  font.add("zhFont", "华文仿宋.ttf")
  
  pdf(fName)
  
  plot(c(0, nHor + 1), c(0, nVert + 1),
       type = "n", axes = FALSE,
       # main = sprintf("%s (%d:%d) (author)",
       #                P$titleS,
       #                P$QTSvol,
       #                P$QTSnum),
       xlab = "", ylab = ""
  )
  
  showtext.begin()
  
  for (i in 1:nLines) {
    for (j in 1:nChars) {
      xy <- getXY(j, nChars, i, nLines)
      zi <- substr(lines[i], j, j)
      rect(xy[1] - 0.5 + padX,
           xy[2] - 0.5 + padY,
           xy[1] + 0.5 - padX,
           xy[2] + 0.5 - padY,
           col = getFcol(freq, zi, n = nIntervals, bias = bias),
           border = "#AAAAAA",
           lwd = 0.5)
      text(xy[1], xy[2], zi, cex = cex, family = "zhFont")
    }
  }
  
  showtext.end()
  dev.off()
  
}


calcMeanRank <- function(s) {
  # calculate the mean frequency rank for all characters in a poem "s"
  # rank vector ziRank must exist
  s <- unlist(strsplit(gsub(" ", "", s), ""))
  return(mean(ziRanks[s]))
}

calcMeanQ <- function(s) {
  # calculate the mean frequency rank for the lower and upper
  # quartile of characters in a poem "s"
  # rank vector ziRank must exist
  r <- ziRanks[unlist(strsplit(gsub(" ", "", s), ""))]
  q <- quantile(r)
  l <- mean(r[r <= q[2]])
  u <- mean(r[r >= q[4]])
  return(c(l, u))
}

getQTSpoemsByAuthor <- function(name) {
  ID <- which(authorDF$nameS == name)
  rows <- which(poemDF$authorID == ID)
  return(poemDF[rows, ])
}


# ==== PROCESS =================================================================

load("../data/poemDF.RData")
load("../data/authorDF.RData")


# === Compile Character frequencies


# paste poems together
x <- paste(poemDF$bodyS, collapse = "")
x <- gsub("\\s+", "", x)
x <- unlist(strsplit(x, ""))

# get frequencies
ziFreq <- sort(table(x), decreasing = TRUE)

# analyze
plot(log(1:length(ziFreq)), log(ziFreq),
     xlab="log(rank)", ylab="log(frequency)")
sum(ziFreq == 1)
sum(ziFreq == 2)
hapax <- ziFreq[ziFreq == 1]
names(hapax[1:100])

plot(1:length(ziFreq), log(ziFreq),
     xlab="rank", ylab="log(frequency)")

# =====

# plot a poem 

P <- poemDF[16479, ]   # Wang Wei, "桃源行"

plotPoemFreq(P, ziFreq, mode = "v", cex = 0.9, nIntervals = 7, bias = 0.9)

poemDF$authorID[poemDF$titleS == "将进酒"]
authorDF$nameS[authorDF$authorID == 431]   # "李白"

P <- poemDF[6899, ]  # 李白, "将进酒"
plotPoemFreq(P, ziFreq, mode = "h", cex = 0.9, nIntervals = 7, bias = 0.9)

which(poemDF$titleS == "李凭箜篌引")
P <- poemDF[18150, ]  # 李白, "将进酒"
plotPoemFreq(P, ziFreq, mode = "h", cex = 0.9, nIntervals = 7, bias = 0.9)

x <- getQTSpoemsByAuthor("孟郊")

P <- poemDF[17305, ]  # 
plotPoemFreq(P, ziFreq, mode = "h", cex = 0.9, nIntervals = 7, bias = 0.9)

# Wang Wei and Song Zhiwen

which(poemDF$QTSvol == 128 & poemDF$QTSnum == 21)
P <- poemDF[5245, ]  # 
plotPoemFreq(P, ziFreq, mode = "h", cex = 0.9, nIntervals = 7, bias = 0.9)

which(poemDF$QTSvol == 51 & poemDF$QTSnum == 1)
P <- poemDF[2589, ]  # 
plotPoemFreq(P, ziFreq, mode = "h", cex = 0.9, nIntervals = 7, bias = 0.9)

# more comparisons

p1 <- which(poemDF$QTSvol == 128 & poemDF$QTSnum == 23)
p2 <- which(poemDF$QTSvol ==  51 & poemDF$QTSnum ==  7) # 1

p3 <- which(poemDF$QTSvol == 128 & poemDF$QTSnum == 24)
p4 <- which(poemDF$QTSvol ==  51 & poemDF$QTSnum ==  7) # 2

p5 <- which(poemDF$QTSvol == 128 & poemDF$QTSnum == 25)
p6 <- which(poemDF$QTSvol ==  51 & poemDF$QTSnum ==  7) # 3

P <- poemDF[p1, ]  # 
plotPoemFreq(P, ziFreq, mode = "v", cex = 0.9, nIntervals = 7, bias = 0.9)

P$bodyS <- "药栏听蝉噪 书幌见禽过 愁至愿甘寝 其如乡梦何峨"

nchar(P$bodyS)

sum(nchar(poemDF$bodyS) == 23)


# ==== simplicity distributions ========

# create named vector of ranks
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




# add mean rank to all poems

poemDF <- data.frame(poemDF, 
                     meanRank = numeric(nrow(poemDF)), 
                     stringsAsFactors = FALSE)

for (i in 1:nrow(poemDF)) {
  poemDF$meanRank[i] <- calcMeanRank(poemDF$bodyS[i])
  if (! i %% 1000) { print(i) }
}


# === compare mean rank distributions for QTS with those of Wang Wei

WWpoems <- getQTSpoemsByAuthor("王维")

hist(poemDF$meanRank[-WWpoems$poemID],
     col = "#888888",
     freq = FALSE,
     ylim = c(0, 0.0025),
     main = "QTS vs. Wang Wei mean ranks",
     xlab = "mean rank of character frequency")
hist(WWpoems$meanRank, col = "#DD000044", freq=FALSE, add=TRUE)

plot(WWpoems$meanRank, cex = 0.7,
     xlab = "Sequence in QTS", 
     ylab = "mean rank of character frequency")
lines(lowess(WWpoems$meanRank), col = "#DD000055", lwd = 3)

# ==== compare Wang Wei with Li Shangyin

LSYpoems <- getQTSpoemsByAuthor("李商隐")
hist(LSYpoems$meanRank,
     col = "#0000DD44",
     freq = FALSE,
     breaks = seq(0, 1500, by = 100),
     xlim = c(0, 1500),
     ylim = c(0, 0.0025),
     main = "Li ShangYin vs. Wang Wei mean ranks",
     xlab = "mean rank of character frequency")
hist(WWpoems$meanRank,
     breaks = seq(0, 1500, by = 100),
     col = "#DD000044", freq=FALSE, add=TRUE)

plot(LSYpoems$meanRank, cex = 0.7,
     xlab = "Sequence in QTS", 
     ylab = "mean rank of character frequency")
lines(lowess(LSYpoems$meanRank), col = "#0000DD55", lwd = 3)

# === add mean rank top and bottom quartiles

poemDF <- data.frame(poemDF, 
                     meanLQ = numeric(nrow(poemDF)), 
                     meanUQ = numeric(nrow(poemDF)), 
                     stringsAsFactors = FALSE)

for (i in 1:nrow(poemDF)) {
  luQ <- calcMeanQ(poemDF$bodyS[i])
  poemDF$meanLQ[i] <- luQ[1]
  poemDF$meanUQ[i] <- luQ[2]
  if (! i %% 1000) { print(i) }
}


# ==== analyze LQ distributions

hist(poemDF$meanLQ[-WWpoems$poemID],
     col = "#888888",
     freq = FALSE,
     breaks = seq(0, 800, by = 20),
     xlim = c(0, 300),
     ylim = c(0, 0.03),
     main = "QTS vs. Wang Wei mean LQ ranks",
     xlab = "mean rank of LQ of character frequency")
hist(WWpoems$meanLQ,
     breaks = seq(0, 800, by = 20),
     col = "#DD000044",
     freq=FALSE, add=TRUE)

plot(WWpoems$meanLQ, cex = 0.7,
     xlab = "Sequence in QTS", 
     ylab = "mean rank of LQ of character frequency")
lines(lowess(WWpoems$meanLQ), col = "#DD000055", lwd = 3)

hist(LSYpoems$meanLQ[-WWpoems$poemID],
     col = "#0000DD44",
     freq = FALSE,
     breaks = seq(0, 800, by = 20),
     xlim = c(0, 300),
     ylim = c(0, 0.03),
     main = "LSY vs. Wang Wei mean LQ ranks",
     xlab = "mean rank of LQ of character frequency")
hist(WWpoems$meanLQ,
     breaks = seq(0, 800, by = 20),
     col = "#DD000044",
     freq=FALSE, add=TRUE)

plot(LSYpoems$meanLQ, cex = 0.7,
     xlab = "Sequence in QTS", 
     ylab = "mean rank of LQ of character frequency")
lines(lowess(LSYpoems$meanLQ), col = "#0000DD55", lwd = 3)
lines(lowess(WWpoems$meanLQ), col = "#DD000055", lwd = 3)

# ==== analyze UQ distributions

hist(poemDF$meanUQ[-WWpoems$poemID],
     col = "#888888",
     freq = FALSE,
     breaks = seq(0, 6000, by = 100),
     xlim = c(0, 4000),
     ylim = c(0, 0.001),
     main = "QTS vs. Wang Wei mean UQ ranks",
     xlab = "mean rank of UQ of character frequency")
hist(WWpoems$meanUQ,
     breaks = seq(0, 6000, by = 100),
     col = "#DD000044",
     freq=FALSE, add=TRUE)

plot(WWpoems$meanUQ, cex = 0.7,
     xlab = "Sequence in QTS", 
     ylab = "mean rank of UQ of character frequency")
lines(lowess(WWpoems$meanUQ), col = "#DD000055", lwd = 3)

hist(LSYpoems$meanUQ[-WWpoems$poemID],
     col = "#0000DD44",
     freq = FALSE,
     breaks = seq(0, 6000, by = 100),
     xlim = c(0, 4000),
     ylim = c(0, 0.001),
     main = "LSY vs. Wang Wei mean UQ ranks",
     xlab = "mean rank of UQ of character frequency")
hist(WWpoems$meanUQ,
     breaks = seq(0, 6000, by = 100),
     col = "#DD000044",
     freq=FALSE, add=TRUE)

plot(LSYpoems$meanUQ, cex = 0.7,
     xlab = "Sequence in QTS", 
     ylab = "mean rank of UQ of character frequency")
lines(lowess(LSYpoems$meanUQ), col = "#0000DD55", lwd = 3)
lines(lowess(WWpoems$meanUQ), col = "#DD000055", lwd = 3)



#    
# ==== TESTS ===================================================================




# [END]