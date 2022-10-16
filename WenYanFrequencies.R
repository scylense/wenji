# WenYanFrequencies.R
#
# Purpose:  Read the JunDa WenHua Frequency table and do simple analyses
#           
# Precondition: The frequency list exists in
#               "../data/JunDa/JunDa_FrequencyList.tsv"
#                 (global) ziCounts exists - the character counts from QTS
#                 (global) ziRanks exists - the ranks from QTS
#                
# Postcondition:  (global) ziWYCounts exists
#                 (global) ziWYscaledCounts exists
#                 (global) ziWYRanks exists
#                
#
# Notes: ziWYscaledCounts has same order of characters as ziCounts for direct
#          comparison of frequency differences.
# 
#
# V 1.2
# Date:     2016-10  -  2022-10
# Author:   Boris Steipe and Yi Chen
#
# ToDo      Remove leftover code from analysis section
#           
# V 1.2     change ziWYFreq to ziWYCounts
# V 1.1     Add code to scale frequencies to QTS global counts; keep global
#             object ziWYscaledFreq. 
# V 1.0     Stable code
# V 0.1     First code experiments
#
# ==============================================================================

if (FALSE) { # Do not execute when source()'d
  setwd(WENJIDIR)
}

# ==== PACKAGES ================================================================


# ==== DEFINITIONS =============================================================

WYfile <- "../data/JunDa/JunDa_FrequencyList.tsv"

# ==== FUNCTIONS ===============================================================


# ==== PROCESS =================================================================

# == read WenYan frequencies from file
tmp <- read.csv(WYfile,
                comment.char = "#",
                blank.lines.skip = TRUE,
                header = FALSE,
                sep = "\t",
                stringsAsFactors = FALSE)
rm(WYfile)

ziWYCounts <- tmp$V3
names(ziWYCounts) <- tmp$V2
ziWYCounts <- as.table(ziWYCounts)
rm(tmp)

# == make rank vector
# create named vector of ranks
wyRanks <- 1:length(ziWYCounts)
names(wyRanks) <- names(ziWYCounts)

# replace ranks for characters with the same frequency with the 
# mean rank of those characters
wyRankRLE <- rle(as.numeric(ziWYCounts))
start <- 1
for (l in wyRankRLE$lengths) {
  end <- start + l -1
  wyRanks[start:end] <- mean(wyRanks[start:end])
  start <- end + 1
}
rm(wyRankRLE)
rm(start)
rm(end)
rm(l)

ziWYRanks <- ziRanks                     # copy the QTS ranks table
ziWYRanks <- wyRanks[names(ziWYRanks)]   # get the ranks from wyRanks
naWY <- which(is.na(ziWYRanks))          # collect the NA indices
names(ziWYRanks) <- names(ziRanks)       # repair the names
# rescale to QTS length, otherwise colour scale will be off
ziWYRanks <- ziWYRanks * ( length(ziWYRanks) / length(wyRanks) )
# replace NA with QTS ranks, scaled to wyRanks length
ziWYRanks[naWY] <- ziRanks[naWY] 
rm(naWY)

# == Done: ziWYCounts and ziWYRanks are ready

ziWYscaledCounts <- as.numeric(ziWYCounts[names(ziCounts)])
names(ziWYscaledCounts) <- names(ziCounts)

# scale observations to qts
x <- which(is.na(ziWYscaledCounts))
# scale is the ratio of characters present in both tables
sc <- sum(ziCounts[- x]) / sum(ziWYscaledCounts, na.rm = TRUE)
# adjust NA values by this scale
ziWYscaledCounts[x] <- ziCounts[x] / sc
# recalculate scale
sc <- sum(ziCounts) / sum(ziWYscaledCounts)
# rescale wyMap$cat
ziWYscaledCounts <- round(ziWYscaledCounts * sc)
# change all 0's to 1
ziWYscaledCounts[ziWYscaledCounts == 0] <- 1
# rescale one last time 
ziWYscaledCounts <- round(ziWYscaledCounts *
                          (sum(ziCounts) / sum(ziWYscaledCounts)))
# check
# min(ziWYscaledCounts)  # must be 1
# sum(ziCounts) / sum(ziWYscaledCounts)  #  0.9999295

# clean up
rm(x)
rm(sc)

# == Done: ziWYscaledFreq is ready




#    
# ==== ANALYZE WY DISTRIBUTION =================================================

if (FALSE) { # Do not execute when source()'d
  
# analyze
plot(log10(1:length(ziWYFreq)), log10(as.numeric(ziWYFreq)),
     main = "Wén Yán (WY) Character Frequencies (black) vs. QTS (red)",
     cex.main = 0.7,
     xaxt = "n", yaxt = "n",
     xlab=expression(log[10](rank)), ylab=expression(log[10](frequency)))
axisMinorTicks(1, 9, mn=0, mx=4)
axisMinorTicks(2, 9, mn=0, mx=6)
points(log10(1:length(ziFreq)), log10(as.numeric(ziFreq)), col="#CC0000", cex=0.6)

length(ziWYFreq)    # 11115 characters
sum(ziWYFreq == 1)  # 957 hapax
sum(ziWYFreq <= 3)  # 2036

# =====

# plot a poem 

P <- poemDF[16479, ]   # Wang Wei, "桃源行"
plotPoemCharRanks(P, ziWYRanks, fName = "test.pdf")

P <- poemDF[6899, ]  # 李白, "将进酒"
plotPoemCharRanks(P, ziWYRanks,  fName = "test.pdf")


# ==== gridplot the Wang River Cycle =======
# Vol 128, # 23 - 42

WangRiverCycle <- poemDF[5247:5266, ]
plotPoemCharRanks(WangRiverCycle[4, ],
                  ranks = ziWYRanks, 
                  mode = "h", cex = 0.9,
                  colMode = "man",
                  nIntervals = 7, bias = 0.9,
                  fName = "test.pdf")
plotPoemGrid(WangRiverCycle, ranks = ziWYRanks, nCol = 4, nRow = 5,
             fName = "test.pdf")

plotPoemCharRanks(WangRiverCycle[20, ],
                  ranks = ziRanks, 
                  fName = "WW_WRC-20.pdf")
plotPoemCharRanks(WangRiverCycle[20, ],
                  ranks = ziWYRanks, 
                  fName = "WW_WRC-20(WY).pdf")


plotPoemCharRanks(poemDF[8146,],
                  ranks = ziRanks, 
                  fName = "WYW_193.63.(QTS).pdf")
plotPoemCharRanks(poemDF[8146,],
                  ranks = ziWYRanks, 
                  fName = "WYW_193.63.(WY).pdf")





# ==== gridplot Song ZhiWen poems =======
# Vol 52, # 19 - 38
x <- poemDF[poemDF$QTSvol == 52,]

SZW <- poemDF[2655:2674, ]
SZW <- SZW[order(SZW$meanRank), ]
plotPoemGrid(SZW, ranks = ziRanks, nCol = 4, nRow = 5,
             fName = "SZW-grid.pdf")

# SZW JueJu
LiBaiJJ <- getQTSpoemsByAuthor("李白")


# ==== gridplot 李白 ===============
LiBaiJJ <- getQTSpoemsByAuthor("李白")
LiBaiJJ <- LiBaiJJ[which(nchar(LiBaiJJ$bodyS) == 23), ]

LiBaiJJ <- LiBaiJJ[order(LiBaiJJ$meanRank), ]
plotPoemGrid(LiBaiJJ, ranks = ziRanks, nCol = 6, nRow = 8,
             fName = "LiBai_JJ-grid.pdf")

# ==== gridplot 李商隐 ==============
LiShangYin <- getQTSpoemsByAuthor("李商隐")
LiShangYin <- LiShangYin[which(nchar(LiShangYin$bodyS) == 23), ]

LiShangYin <- LiShangYin[order(LiShangYin$meanRank), ]
plotPoemGrid(LiShangYin[1:25, ], ranks = ziRanks, nCol = 5, nRow = 5,
             colMode = "man", fName = "LiShangYin_JJ-grid.pdf")

# ==== gridplot 杜甫 ================
DuFuJJ <- getQTSpoemsByAuthor("杜甫")
DuFuJJ <- DuFuJJ[which(nchar(DuFuJJ$bodyS) == 23), ]

DuFuJJ <- DuFuJJ[order(DuFuJJ$meanRank), ]
plotPoemGrid(DuFuJJ, ranks = ziRanks, nCol = 3, nRow = 3,
             fName = "DuFu_JJ-grid.pdf")

# ==== gridplot 韦应物 ==============
WeiYingWuJJ <- getQTSpoemsByAuthor("韦应物")
WeiYingWuJJ <- WeiYingWuJJ[which(nchar(WeiYingWuJJ$bodyS) == 23), ]

WeiYingWuJJ <- WeiYingWuJJ[order(WeiYingWuJJ$meanRank), ]
plotPoemGrid(WeiYingWuJJ[1:56, ], ranks = ziRanks, nCol = 7, nRow = 8,
             fName = "WeiYingWu_JJ-grid.pdf")

# ==== JueJu in general ... ===========
iJueJu <- grep("^([^ ]{5} ){3}[^ ]{5}$", poemDF$bodyS)  # 2306 poems
JJ <- poemDF[iJueJu[1:(39 * 59)], ]   # take first 2301

# ==== gridplot random JueJu =======
set.seed(123)
rJJ <- JJ[sample(1:nrow(JJ), 20), ]
plotPoemGrid(rJJ, ranks = ziRanks, nCol = 4, nRow = 5,
             fName = "JueJue-grid.1.pdf")
rJJ <- JJ[sample(1:nrow(JJ), 20), ]
plotPoemGrid(rJJ, ranks = ziRanks, nCol = 4, nRow = 5,
             fName = "JueJue-grid.2.pdf")
rJJ <- JJ[sample(1:nrow(JJ), 20), ]
plotPoemGrid(rJJ, ranks = ziRanks, nCol = 4, nRow = 5,
             fName = "JueJue-grid.3.pdf")

# ==== gridplot all JueJu ==============
JJ <- JJ[order(JJ$meanRank), ] # order by mean Rank
plotPoemGrid(JJ, ranks = ziRanks, nCol = 39, nRow = 59,
             colMode = "man", fName = "All_JJ-grid.pdf")
#re-randomize
JJ <- JJ[sample(1:nrow(JJ)), ]
JJ <- JJ[order(JJ$authorID), ] # order by author
plotPoemGrid(JJ, ranks = ziRanks, nCol = 39, nRow = 59,
             colMode = "man", fName = "All_JJ-by-author.pdf")

SZW_JJ <- JJ[JJ$authorID == 130,]
SZW_JJ[, c("titleS", "bodyS")]

# ==============================================================================

# ==== frequency distributions ========

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

save(poemDF, file = "../data/poemDF.RData")



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

}  # END  if (FALSE)  block

#    
# ==== TESTS ===================================================================




# [END]


