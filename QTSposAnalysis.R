# QTSsimplicityAnalysis.R
#
# Purpose:  Perform part-of-speech analysis in the QTS corpus
#           
# Precondition: poemDF
#               authorDF
#               
#                
# Postcondition:
#                
#
# Notes: 
# 
#
# V 0.1
# Date:     November 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo      
#           
# V 0.1     First code
#
# ==============================================================================


# ==== PACKAGES ================================================================



# ==== DEFINITIONS =============================================================

# frequently searched authors:
# Du Fu        杜甫   (507)
# Li Bai       李白   (431)
# Li Shangyin  李商隐 (1069)
# Liu Zongyuan 柳宗元 (812)
# Meng Haoran  孟浩然 (430)
# Song Zhiwen  宋之问 (130)
# Wang Wei     王维   (389)
# Wei Yingwu   韦应物 (432)
# 
# getAuthorID("韦应物")

# ==== FUNCTIONS ===============================================================



# ==== PROCESS =================================================================


# plot a poem on a character POS grid

# Wei Yingwu (刘禹锡), "桃源行"
plotPoem(poemDF[16479, ], posMap,
         fName = "test.pdf", subTitle = "(POS tags)")

# Wang Wei (王维), "辋川集·孟城坳" (WRC #1)
plotPoem(poemDF[5247, ], posMap,
         fName = "test.pdf", subTitle = "(POS tags)")


# ==== select JueJu (Wu Ju) ... ===========
iJueJu <- grep("^([^ ]{5} ){3}[^ ]{5}$", poemDF$bodyS)  # 2306 poems
JJ <- poemDF[iJueJu, ]  
rm(iJueJu)


# ==== gridplot the Wang River Cycle =======
# Wang River Cycle: Vol 128, # 23 - 42
WangRiverCycle <- poemDF[poemDF$QTSvol == 128 &
                           poemDF$QTSnum >= 23 &
                           poemDF$QTSnum <= 42, ]

# plot entire Wang River Cycle
for (i in 1:nrow(WangRiverCycle)) {
  plotPoem(WangRiverCycle[i, ],
           map = posMap, subTitle = "(POS tags)",
           fNameSuffix = ".POS")
}

plotPoemGrid(WangRiverCycle, posMap,
             nCol = 4, nRow = 5,
             fName = "WW_WRC-grid.POS.pdf")

# === analyze Tag distributions ================================================

# table all tags.

allTags <- character()
x <- character()
for (i in 1:nrow(poemDF)) {
  if (! i %% 1000) {
    print(i) 
    allTags <- c(allTags, x)
    x <- character()
  }
  x <- c(x, unlist(strsplit(poemDF$POS[i], "\\+|\\s+")))
}
length(allTags) #2553898
allTagsTab <- table(allTags)
length(allTagsTab) # 21001 ... on average all characters have three different
                   #           categories.

# === bar plot of categories
allLabels <- table(substr(allTags, 3, 6))
allLabels <- allLabels[names(posMap$col)]

pareto(allLabels, posMap$col)

# === analyse relationship of categories

x <- allTagsTab[ grep("上", names(allTagsTab))]
names(x) <- gsub("[^A-Z]", "", names(x))
y <- posMap$col[names(x)]
y <- col2rgb(y)
z2 <- z
avCol <- col2rgb("#000000")
for (i in 1:ncol(z)) {
  z2[ , i] <- z2[ , i] * x[i]
}
for (i in 1:nrow(z)) {
  avCol[i , 1] <- sum(z2[i, ]) / sum(x)
}
rgb(avCol[1,1], avCol[2,1], avCol[3,1], maxColorValue=255)
rgb(t(avCol), maxColorValue=255)

avZiCol <- function(zi) {
  # calculate average color value of zi based on the POS categories
  # in allTagsTab and the colors in posMap$col

  # get all observations
  x <- allTagsTab[ grep(zi, names(allTagsTab))]
  names(x) <- gsub("[^A-Z]", "", names(x))
  
  if (length(x) == 0) { # not observed in poem bodies
    return("#FFFFFF")
    
  } else if (length(x) == 1) { # only one: no need to average
    return(posMap$col[names(x)])
    
  } else { #compute and return average colour
    # fetch colors and convert to rgb
    y <- posMap$col[names(x)]
    y <- col2rgb(y)
    
    avCol <- col2rgb("#000000")
    # compute averages for each channel
    for (i in 1:ncol(y)) {
      y[ , i] <- y[ , i] * x[i]
    }
    for (i in 1:nrow(y)) {
      avCol[i , 1] <- sum(y[i, ]) / sum(x)
    }
    
    # convert back to hex and return
    return(rgb(t(avCol), maxColorValue=255))
  }
  
}

# collect average color values for all Zi

avPosCol <- character(length(ziFreq))
names(avPosCol) <- names(ziFreq)

for (i in 1:length(avPosCol)) {
  if (! i %% 200) { print(i) }
  avPosCol[i] <- avZiCol(names(avPosCol)[i])
}


# plot QTS/WY differences and colors
n <- 100

plot(1:n, log10(as.numeric(ziFreq[1:n])) /
     log10(as.numeric(ziWYscaledFreq[1:n])),
     type="n")
points(1:n, log10(as.numeric(ziFreq[1:n])) /
         log10(as.numeric(ziWYscaledFreq[1:n])),
     cex = 0.8,
     pch = 21,
     col = avPosCol[1:n],
     bg = avPosCol[1:n])
abline(h=1, col="#FFFF66", lwd=0.5)

# === plot character category distributions

parPOS <- function(zi) {
  x <- allTagsTab[ grep(zi, names(allTagsTab))]
  names(x) <- gsub("[^A-Z]", "", names(x))
  pareto(x, col = posMap$col[names(x)], sorted = TRUE)
  cat(sprintf("%s %s %8s\t%s\n", 
              ziRef[zi,"S"],
              ziRef[zi,"T"],
              ziRef[zi, "PY"],
              substr(ziRef[zi, "def"], 1, 50)))
}

parPOS("空")
parPOS("山")
parPOS("不")

for (zi in names(ziFreq)[20:40]) {
  parPOS(zi)
  line <- readline()
}

# === cluster analysis of character categories
nC <- 2500 # approximately 100 observations
ziDim <- matrix(numeric(nC * length(posMap$cat)), nrow = 2500,
                ncol = length(posMap$cat))
colnames(ziDim) <- names(posMap$cat)
rownames(ziDim) <- names(ziFreq[1:nC])

for (i in 1:nC) {
  x <- allTagsTab[ grep(names(ziFreq)[i], names(allTagsTab))]
  names(x) <- gsub("[^A-Z]", "", names(x))
  ziDim[i, names(x)] <- x
}

nouny <- c("NN", "NR", "NT", "PN")
adjectivy <- c("VA", "DT", "CD", "OD", "DEC", "DEG", "JJ", "P", "LC", "M")
verby <- c("VC", "VE", "VV")
adverby <- c("DER", "DEV", "AD", "AS", "SB", "LB")
syntaxy <- c("BA", "CC", "CS", "SP", "MSP")

rSum <- apply(ziDim[ , ], 1, sum)
fN <- apply(ziDim[  , nouny], 1, sum) / rSum
fAdj <- apply(ziDim[  , adjectivy], 1, sum) / rSum
fV <- apply(ziDim[  , verby], 1, sum) / rSum
fAdv <- apply(ziDim[  , adverby], 1, sum) / rSum
fSyn <- apply(ziDim[  , syntaxy], 1, sum) / rSum

plot(fN, fV, pch=21, cex=0.5, col="#00BBFF22", bg="#0099FF22", )
plot(fN, fAdj, pch=21, cex=0.5, col="#00BBFF22", bg="#0099FF22", )
plot(fV, fAdv, pch=21, cex=0.5, col="#00BBFF22", bg="#0099FF22", )
plot(fAdv, fAdj, pch=21, cex=0.5, col="#00BBFF22", bg="#0099FF22", )
plot(fN, fAdv, pch=21, cex=0.5, col="#00BBFF22", bg="#0099FF22", )
plot(fV, fAdj, pch=21, cex=0.5, col="#00BBFF22", bg="#0099FF22", )

# only nouny/verby and adjectives/adverbs have a significant overlap.
# Quick-and-dirty categories

ziCats <- character(nC)
names(ziCats) <- row.names(ziDim)
for (i in 1:nC) {
  if (fN[i] > 0.9) {
    ziCats[i] <- "N"
  } else if (fV[i] > 0.85) {
    ziCats[i] <- "V"
  } else if (fAdj[i] > 0.85) {
    ziCats[i] <- "Adj"
  } else if (fAdv[i] > 0.85) {
    ziCats[i] <- "Adv"
  } else if (fN[i] + fV[i] > 0.75) {
    ziCats[i] <- "NV"
  } else if (fN[i] + fAdj[i] > 0.75) {
    ziCats[i] <- "NAdj"
  } else if (fV[i] + fAdj[i] > 0.75) {
    ziCats[i] <- "VAdj"
  } else if (fN[i] + fAdv[i] > 0.75) {
    ziCats[i] <- "NAdv"
  } else if (fV[i] + fAdv[i] > 0.75) {
    ziCats[i] <- "VAdv"
  } else if (fAdv[i] + fAdj[i] > 0.75) {
    ziCats[i] <- "AdJV"
  } else if (fSyn[i] > 0.4) {
    ziCats[i] <- "Syn"
  } else {
    ziCats[i] <- "X"
  }
}

x <- which(ziCats == "X")
parPOS(names(ziCats)[x[1]])

parPOS("山")


catOrder <- 
  c("V","VAdj","NV","NAdv","N","NAdj","Adj","AdJV","X","Syn","Adv","VAdv")

ziTab <- table(ziCats)[catOrder]

eqSpect <- colorRampPalette(
  c(
    "#f2003c",  # red
    "#F0A200",  # orange
    "#f0ea00",  # yellow
    "#62C923",  # green
    "#0A9A9B",  # blue
    "#1958C3",  # indigo
    "#8000D3",  # violet
    "#D0007F"), # red
  space="Lab",
  interpolate="linear")
n <- length(ziTab)
barplot(rep(1, n), col=eqSpect(n),
        axes=F, main="", names.arg = names(ziTab), cex.names = 0.6)

ziCatCols <- c(eqSpect(n)[1:2], eqSpect(n)[4:9], "#BBBBBB", eqSpect(n)[10:12])
names(ziCatCols) <- names(ziTab)
barplot(rep(1, n), col=ziCatCols,
        axes=F, main="", names.arg = names(ziTab), cex.names = 0.6)


# compare QTS and WY frequencies, colored by category


plotQTS_WY <- function(zC) {
  x <- which(ziCats %in% zC)
  nC <- length(x)
  plot(1:nC, 
       log10(as.numeric(ziFreq[names(ziFreq)[x]]) /
             as.numeric(ziWYscaledFreq[names(ziFreq)[x]])),
       ylab = sprintf("log_10( f(QTS)/f(WY) ) for word category %s",
                      paste(zC, collapse = ", ")),
       ylim = c(-2, 2),
       type = "n")
  points(1:nC,
         log10(as.numeric(ziFreq[names(ziFreq)[x]]) /
               as.numeric(ziWYscaledFreq[names(ziFreq)[x]])),
         cex = 0.8,
         pch = 21,
         col = ziCatCols[ziCats[x]],
         bg = ziCatCols[ziCats[x]])
  abline(h=0, col="#777777", lwd=0.5)
  
}

plotQTS_WY(c("V"))
plotQTS_WY(c("VAdj"))
plotQTS_WY(c("NV"))
plotQTS_WY(c("NAdv"))
plotQTS_WY(c("N"))
plotQTS_WY(c("NAdj"))
plotQTS_WY(c("Adj"))
plotQTS_WY(c("AdJV"))
plotQTS_WY(c("Syn"))
plotQTS_WY(c("Adv"))
plotQTS_WY(c("VAdj"))

#    
# ==== TESTS ===================================================================


 

# [END]