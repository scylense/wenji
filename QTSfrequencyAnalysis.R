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
# V 0.4
# Date:     October 2016 - March 2017
# Author:   Boris Steipe and Yi Chen
#           
# V 0.4     Pronoun analyses, add Qi Ju, Wu Lü, Qi Lü
# V 0.3     Moved calcMeanLogRank() to wenjiUtilities.R
# V 0.2     Add part-of-speech code and new mapCol function, consolidate.
# V 0.1     First code experiments, until Olomouc presentation
#
# ToDo:     Add contemporary word frequencies     
#           Definition length complexity as simplicity measure
#           Statistical analysis of author or cycle simplicity measures
#             against random chocices from the corpus 
#
# ==============================================================================

setwd(WENJIDIR)


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

# ==== PREPARE ================================================================

load("../data/poemDF.RData")
load("../data/authorDF.RData")
load("../data/ziRef.RData")

source("QuanTangShiFrequencies.R") # prepare ziCounts and ziRanks objects
source("WenYanFrequencies.R") # prepare ziWYCounts and ziWYRanks objects
source("mapCol.R") # prepare mapCol maps and functions
source("plotPoems.R") # load printing and plotting functions

# ==== select JueJu (Wu Jue, Qi Jue, Wu Lü, Qi Lü) ... ===========
sel <- grep("^([^ ]{5} ){3}[^ ]{5}$", poemDF$bodyS)  # 2306 poems
WJ <- poemDF[sel, ]  

sel <- grep("^([^ ]{7} ){3}[^ ]{7}$", poemDF$bodyS)  # 7257 poems
QJ <- poemDF[sel, ]  

sel <- grep("^([^ ]{5} ){7}[^ ]{5}$", poemDF$bodyS)  # 12,476 poems
WL <- poemDF[sel, ]  

sel <- grep("^([^ ]{7} ){7}[^ ]{7}$", poemDF$bodyS)  # 7467 poems
QL <- poemDF[sel, ]  

rm(sel)



# ==== FUNCTIONS ===============================================================



# ==== PROCESS =================================================================


# plot a poem on a character frequency grid

# Wei Yingwu (刘禹锡), "桃源行"
plotPoem(poemDF[16479, ], qtsMap,
         fName = "test.pdf", subTitle = "(QTS ranks)")

# Wang Wei (王维), "辋川集·孟城坳" (WRC #1)
plotPoem(poemDF[5247, ], qtsMap,
         fName = "test.pdf", subTitle = "(QTS ranks)")



# ==== gridplot the Wang River Cycle =======
# Wang River Cycle: Vol 128, # 23 - 42
WangRiverCycle <- poemDF[poemDF$QTSvol == 128 &
                         poemDF$QTSnum >= 23 &
                         poemDF$QTSnum <= 42, ]

# plot entire Wang River Cycle
for (i in 1:nrow(WangRiverCycle)) {
  plotPoem(WangRiverCycle[i, ],
           map = qtsMap, subTitle = "(QTS ranks)",
           fNameSuffix = ".QTS")
  plotPoem(WangRiverCycle[i, ],
           map = wyMap, subTitle = "(WY ranks)",
           fNameSuffix = ".WY")
}

plotPoemGrid(WangRiverCycle, qtsMap,
             nCol = 4, nRow = 5,
             fName = "WW_WRC-grid.QTS.pdf")
plotPoemGrid(WangRiverCycle, wyMap,
             nCol = 4, nRow = 5,
             fName = "WW_WRC-grid.WY.pdf")


# ==== gridplot Song ZhiWen poems =======
# Vol 52, # 19 - 38
SZW <- poemDF[poemDF$QTSvol == 52 &
              poemDF$QTSnum >= 19 &
              poemDF$QTSnum <= 38, ]
SZW <- SZW[order(SZW$meanLR), ]
plotPoemGrid(SZW, qtsMap, nCol = 4, nRow = 5,
             fName = "SZW-grid.pdf")

SZW_WJ <- WJ[WJ$authorID == 130, ]


# ==== gridplot 李白 ===============
LiBaiWJ <- WJ[WJ$authorID == getAuthorID("李白"), ]
LiBaiWJ <- LiBaiWJ[order(LiBaiWJ$meanLR), ]

plotPoemGrid(LiBaiWJ, map = qtsMap, nCol = 6, nRow = 8,
             fName = "LiBai_WJ-grid.pdf")

# ==== gridplot 李商隐 ==============
LiShangYin <- WJ[WJ$authorID == getAuthorID("李商隐"), ]
LiShangYin <- LiShangYin[order(LiShangYin$meanLR), ]
plotPoemGrid(LiShangYin, map = qtsMap, nCol = 6, nRow = 5,
             fName = "LiShangYin_WJ-grid.pdf")

# ==== gridplot 杜甫 ================
DuFuWJ <- WJ[WJ$authorID == getAuthorID("杜甫"), ]
DuFuWJ <- DuFuWJ[order(DuFuWJ$meanLR), ]
plotPoemGrid(DuFuWJ, map = qtsMap, nCol = 3, nRow = 3,
             fName = "DuFu_WJ-grid.pdf")

# ==== gridplot 韦应物 ==============
WeiYingWuWJ <- WJ[WJ$authorID == getAuthorID("韦应物"), ]
WeiYingWuWJ <- WeiYingWuWJ[order(WeiYingWuWJ$meanLR), ]
plotPoemGrid(WeiYingWuWJ, map = qtsMap, nCol = 6, nRow = 10,
             fName = "WeiYingWu_WJ-grid.pdf")

# ==== gridplot all JueJu ==============
WJ <- WJ[order(WJ$meanLR), ] # order by mean log rank
plotPoemGrid(WJ, map = qtsMap, nCol = 43, nRow = 54,
             fName = "test.pdf")
#             fName = "All_WJ-grid.qts.pdf")

# plot highest and lowest ranked poem
plotPoem(WJ[1, ],
         map = qtsMap, subTitle = "",
         fNameSuffix = "")
plotPoem(WJ[nrow(WJ), ],
         map = qtsMap, subTitle = "",
         fNameSuffix = "")

# Locate the four sample poems on the full grid:
# (1) Mèng Hào Rán 孟浩然 (430):
#     “Chūn Xiǎo” 《春晓》 “Spring Dawn” (6874)
#     getAuthorID("孟浩然")
#     which(poemDF$titleS == "春晓" & poemDF$authorID == 430)
#     which(WJ$poemID == 6874)   ... # 37
#     ceiling(37/43); 629 %% 43  # row and column
# (2) Wáng Wéi 王維: “Lù Zhài” 《鹿柴》 “Deer Grove” 
#     grep("鹿柴", WJ$titleS)   # 101 (!), not # 307
#     ceiling(101/43); 101 %% 43  # 3/17
# (3) Wéi Yīng Wù 韋應物 (432):
#     “Chú Zhōu Xī Jiàn” 《滁州西澗》 “Western Valley of Chú Zhōu”
#     grep("滁州西涧", poemDF$titleS)
#     this is not in WJ - but a WJ of the same mean LR is #629
#     ceiling(629/ 43); 629 %% 43  # 15 / 27
# (4) Liǔ Zōng Yuán 柳宗元: “Jiāng Xuě” 《江雪》 “River Snow”
#     which(WJ$titleS == "江雪")   #1433
#     ceiling(1433/ 43); 1433 %% 43   # 34 / 14

# locate all Wang River cycle poems on the full grid.
head(WangRiverCycle)

s <- character()
for (i in 1:nrow(WangRiverCycle))  {  # for each poem in the cycle
  pID <- WangRiverCycle$poemID[i]     # fetch poem ID
  iWJ <- which(WJ$poemID == pID)      # locate the poem in WJ
  
  # Print name, row, and column.
  s[i] <- sprintf("No. %d - poem %d (%s - %s): Row: %d  Column: %d\n",
      iWJ,
      pID,
      authorDF$nameS[WangRiverCycle$authorID[i]],
      WangRiverCycle$titleS[i],
      ceiling(iWJ / 43),
      iWJ %% 43)
  names(s)[i] <- iWJ
}

s <- s[order(as.numeric(names(s)))]
cat(s)
WangRiverCycle[18,]

# print out all Wang River Cycle poems sorted by character complexity
ord <- order(WangRiverCycle$meanLR)
s <- character()
for (i in 1:nrow(WangRiverCycle))  {  # for each poem in the cycle
  # Print ID (LR), title, and body.
  idx <- ord[i]
  s[i] <- sprintf("\nPoem %d (%3.2f):\t%s\t-  %s",
                  WangRiverCycle$poemID[idx],
                  WangRiverCycle$meanLR[idx],
                  WangRiverCycle$titleS[idx],
                  WangRiverCycle$bodyS[idx])
}
cat(s)
nchar(WangRiverCycle$titleS)

# === Individual analyses =======
# === "韦应物" # 7837
plotPoem(poemDF[7837, ],
         map = qtsMap,
         fNameSuffix = ".QTS",
         subTitle = "(QTS frequency ranks)")
plotPoem(poemDF[7837, ],
         map = wyMap,
         fNameSuffix = ".WY",
         subTitle = "(WY frequency ranks)")
# === Liu Zong Yuan 江雪 # 16331
plotPoem(poemDF[16331, ],
         map = qtsMap,
         fNameSuffix = ".QTS",
         subTitle = "(QTS frequency ranks)")
plotPoem(poemDF[16331, ],
         map = wyMap,
         fNameSuffix = ".WY",
         subTitle = "(WY frequency ranks)")

# === Meng Hao Ran 春晓 # 6874
plotPoem(poemDF[6874, ],
         map = qtsMap,
         fNameSuffix = ".QTS",
         subTitle = "(QTS frequency ranks)")
plotPoem(poemDF[6874, ],
         map = wyMap,
         fNameSuffix = ".WY",
         subTitle = "(WY frequency ranks)")



# ===== FREQUENCY DISTRIBUTIONS ================================================


# === compare mean rank distributions for QTS with those of Wang Wei

WWpoems <- getQTSpoemsByAuthor("王维")

hist(poemDF$meanLR[-WWpoems$poemID],
     col = "#eeeeff",
     freq = FALSE,
     ylim = c(0, 2.5),
     main = "QTS vs. Wang Wei mean log ranks",
     xlab = "mean rank of character frequency")
hist(WWpoems$meanLR, col = "#DD000044", freq=FALSE, add=TRUE)

x <- seq(0, 1, length.out = length(WWpoems$meanLR))
plot(x,
     WWpoems$meanLR,
     cex = 0.7,
     xaxt = "n",
     xlab = "Sequence in QTS", 
     ylab = "mean log rank of character frequency")
lines(lowess(x, WWpoems$meanLR),
      col = "#DD000055", lwd = 3)

# ==== compare Wang Wei with Li Shangyin
LSYpoems <- getQTSpoemsByAuthor("李商隐")

br <- seq(1.5, 3, by = 0.125)
hist(LSYpoems$meanLR,
     col = "#0000EE44",
     freq = FALSE,
     breaks = br,
     xlim = c(1.5, 3),
     ylim = c(0, 3),
     main = "Li ShangYin vs. Wang Wei mean log ranks",
     xlab = "mean log rank of character frequency")
hist(WWpoems$meanLR,
     breaks = br,
     col = "#DD000044", freq=FALSE, add=TRUE)


x2 <- seq(0, 1, length.out = length(LSYpoems$meanLR))
plot(x2,
     LSYpoems$meanLR,
     cex = 0.7,
     xaxt = "n",
     xlab = "Sequence in QTS", 
     ylab = "mean log rank of character frequency")
lines(lowess(x2, LSYpoems$meanLR),
      col = "#0000DD55", lwd = 3)
lines(lowess(x, WWpoems$meanLR),
      col = "#DD000055", lwd = 3)


# ==== calculate mean ranks separately for JueJu lines

iJueJu <- grep("^([^ ]{5} ){3}[^ ]{5}$", poemDF$bodyS)  # 2306 poems
WJ <- poemDF[iJueJu, ]
WJ <- WJ[order(WJ$meanLR), ] # order by mean Rank

lineLRanks <- matrix(numeric(4 * nrow(WJ)), ncol = 4)
colnames(lineLRanks) <- c("line.1", "line.2", "line.3", "line.4")

for (i in 1:nrow(WJ)) {
  lines <- unlist(strsplit(WJ$bodyS[i], " "))
  for (j in 1:4){
    lineLRanks[i, j] <- calcMeanLogRank(lines[j])
  }
}

boxplot(lineLRanks,
        cex.main = 0.8,
        main = "mean log(ranks) of character frequency for JueJu lines")


# same for Wang Wei
# 
WW_WJ <- WJ[WJ$authorID == 389, ]

WW_lr <- matrix(numeric(4 * nrow(WW_WJ)), ncol = 4)
colnames(WW_lr) <- c("line.1", "line.2", "line.3", "line.4")

for (i in 1:nrow(WW_WJ)) {
  lines <- unlist(strsplit(WW_WJ$bodyS[i], " "))
  for (j in 1:4){
    WW_lr[i, j] <- calcMeanLogRank(lines[j])
  }
}

boxplot(WW_lr,
        cex.main = 0.8,
        col = "#ddeedd",
        main = "mean log(ranks) of character frequency for JueJu lines (Wang Wei only)")

# same for Wang River Cycle only
# 
WRC_lr <- matrix(numeric(4 * nrow(WangRiverCycle)), ncol = 4)
colnames(WRC_lr) <- c("line.1", "line.2", "line.3", "line.4")

for (i in 1:nrow(WangRiverCycle)) {
  lines <- unlist(strsplit(WangRiverCycle$bodyS[i], " "))
  for (j in 1:4){
    WRC_lr[i, j] <- calcMeanLogRank(lines[j])
  }
}

boxplot(WRC_lr,
        cex.main = 0.8,
        col = "#ccccee",
        main = "mean log(ranks) of character frequency for JueJu lines (Wang River Cycle only)")

#  === Li Shangyin
LSY_WJ <- WJ[WJ$authorID == 1069, ]

LSY_lr <- matrix(numeric(4 * nrow(LSY_WJ)), ncol = 4)
colnames(LSY_lr) <- c("line.1", "line.2", "line.3", "line.4")

for (i in 1:nrow(LSY_WJ)) {
  lines <- unlist(strsplit(LSY_WJ$bodyS[i], " "))
  for (j in 1:4){
    LSY_lr[i, j] <- calcMeanLogRank(lines[j])
  }
}

boxplot(LSY_lr,
        cex.main = 0.8,
        col = "#eecccc",
        main = "mean log(ranks) of character frequency for JueJu lines (Li Shangyin only)")

# combine 

allLr <- matrix(nrow = nrow(WJ), ncol=16)
allLr[1:nrow(WJ), 1:4] <- lineLRanks
allLr[1:nrow(LSY_lr), 5:8] <- LSY_lr
allLr[1:nrow(WW_lr), 9:12] <- WW_lr
allLr[1:nrow(WRC_lr), 13:16] <- WRC_lr

par(mar = c(5,4.5,0.1,0.1))
boxplot(allLr,
        col = c(rep("#eeeeee", 4), 
                rep("#eecccc", 4),
                rep("#ddeedd", 4),
                rep("#ccccee", 4)),
        xaxt = "n",
        ylab = expression(mean(log[10](rank))),
        xlab = "Lines 1 - 4 for:  all Jue Ju / Li Shangyin / Wang Wei / Wang River Cycle",
        na = "ignore" ) 
abline(v=4.5)
abline(v=8.5)
abline(v=12.5)
axis(1, at=1:16, labels = rep(1:4, 4))

# what is the outlier low value in line 1?
which(WRC_lr[,1] == min(WRC_lr[,1])) #   Ah. 空山不见人。。。

# ==== Frequencies of personal pronouns

pp1 <- c("余", "吾", "予", "我", "自")# first person
names(pp1) <- rep("1", length(pp1))
pp2 <- c("子", "若", "君", "你", "尔", "您")# second person
names(pp2) <- rep("2", length(pp2))
pp3 <- c("彼", "之", "其", "他", "她")# third person
names(pp3) <- rep("3", length(pp3))


printCFR <- function(s, zi, header) {
  # print counts, frequencies and ranks from text s for characters in zi
  myCounts <- compileZiCounts(s)
  myRanks  <- compileZiRanks(myCounts)
  myFreq   <- myCounts / sum(myCounts)  
  cat(sprintf("\n%s\n", header))
  sel <- which(names(myCounts) %in% zi)
  cat(" pp:Zi   rank   counts  (freq.)\n")
  for (i in 1:length(sel)) {
    thisZi <- names(myCounts)[sel[i]]
    cat(sprintf("  %s:%s  %5d  %7d  (%f)\n",
                names(zi)[zi == thisZi],
                thisZi,
                myRanks[thisZi], 
                myCounts[thisZi], 
                myFreq[thisZi]))
  }
  cat("\n\n")
}

printCFR(poemDF$bodyS, c(pp1, pp2, pp3), "Personal pronouns in QTS")
# === Wen Yan ====
zi <- c(pp1, pp2, pp3)
myCounts <- ziWYFreq
myRanks  <- wyRanks
myFreq   <- myCounts / sum(myCounts)  
sel <- which(names(myCounts) %in% zi)
cat("Personal pronouns in WenYan\n")
cat(" pp:Zi   rank   counts  (freq.)\n")
for (i in seq_along(sel)) {
  thisZi <- names(myCounts)[sel[i]]
  cat(sprintf("  %s:%s  %5d  %7d  (%f)\n",
              names(zi)[zi == thisZi],
              thisZi,
              round(myRanks[thisZi]), 
              myCounts[thisZi], 
              myFreq[thisZi]))
}
cat("\n\n")
# =================

printAuthorZiUsage <- function(au, zi) {
  aID <- authorDF$authorID[authorDF$nameS == au]
  printCFR(poemDF$bodyS[poemDF$authorID == aID],
           zi, 
           sprintf("Usage by %s (in complete QTS corpus)", authorDF$nameS[aID]))
  printCFR(WJ$bodyS[poemDF$authorID == aID],
           zi, 
           sprintf("Usage by %s (Wu Jue only)", authorDF$nameS[aID]))
  printCFR(QJ$bodyS[poemDF$authorID == aID],
           zi, 
           sprintf("Usage by %s (Qi Jue only)", authorDF$nameS[aID]))
  printCFR(WL$bodyS[poemDF$authorID == aID],
           zi, 
           sprintf("Usage by %s (Wu Lü only)", authorDF$nameS[aID]))
  printCFR(QL$bodyS[poemDF$authorID == aID],
           zi, 
           sprintf("Usage by %s (Qi Lü only)", authorDF$nameS[aID]))
} 

# Meng Haoran  孟浩然 (430)
printAuthorZiUsage("孟浩然", c(pp1, pp2, pp3))

# Wang Wei     王维   (389)
printAuthorZiUsage("王维", c(pp1, pp2, pp3))

printCFR(WangRiverCycle$bodyS[poemDF$authorID == 389],
         c(pp1, pp2, pp3), 
         sprintf("Usage by %s (Wang River Cycle only)", "王维"))



# Wei Yingwu   韦应物 (432)
printAuthorZiUsage("韦应物", c(pp1, pp2, pp3))

# Liu Zongyuan 柳宗元 (812)
printAuthorZiUsage("柳宗元", c(pp1, pp2, pp3))


# Counts
sum(poemDF$authorID == 430) # 268
sum(poemDF$authorID == 389) # 351
sum(poemDF$authorID == 432) # 551
sum(poemDF$authorID == 812) # 551

sum(WJ$authorID == 430) # 19
sum(WJ$authorID == 389) # 43
sum(WJ$authorID == 432) # 59
sum(WJ$authorID == 812) # 9



#    
# ==== TESTS ===================================================================




# [END]