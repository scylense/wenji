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
# V 0.2
# Date:     October 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo      
#           
# V 0.2     Add part-of-speech code and new mapCol function, consolidate.
# V 0.1     First code experiments, until Olomouc presentation
#
# ==============================================================================

setwd(WENJIDIR)

load("../data/poemDF.RData")
load("../data/authorDF.RData")

source("QuanTangShiFrequencies.R")
source("WenYanFrequencies.R")
source("mapCol.R")
source("plotPoems.R")

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

calcMeanLogRank <- function(s) {
  # calculate the mean log frequency rank for all characters in a poem "s"
  # rank vector ziRank must exist
  s <- unlist(strsplit(gsub(" ", "", s), ""))
  return(as.numeric(mean(log(ziRanks[s]))))
}


getQTSpoemsByAuthor <- function(name) {
  ID <- which(authorDF$nameS == name)
  rows <- which(poemDF$authorID == ID)
  return(poemDF[rows, ])
}

getAuthorID <- function(name) {
  ID <- which(authorDF$nameS == name)
  if (length(ID) == 0) { stop("Name not found in authorDF.")}
  if (length(ID) >  1) { stop("Name not unique in authorDF.")}
  return(ID)
}

getAuthorName <- function(ID) {
  name <- authorDF$nameS[authorDF$authorID == ID]
  if (length(ID) == 0) { stop("ID not found in authorDF.")}
  return(name)
}



# ==== PROCESS =================================================================


# plot a poem on a character frequency grid

# Wei Yingwu (刘禹锡), "桃源行"
plotPoem(poemDF[16479, ], qtsMap,
         fName = "test.pdf", subTitle = "(QTS ranks)")

# Wang Wei (王维), "辋川集·孟城坳" (WRC #1)
plotPoem(poemDF[5247, ], qtsMap,
         fName = "test.pdf", subTitle = "(QTS ranks)")


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
           map = qtsMap, subTitle = "(QTS ranks)",
           fNameSuffix = ".QTS")
  plotPoem(WangRiverCycle[i, ],
           map = wyMap, subTitle = "(WY ranks)",
           fNameSuffix = ".WY")
  plotPoem(WangRiverCycle[i, ],
           map = posMap, subTitle = "(POS tags)",
           fNameSuffix = ".POS")
}

plotPoemGrid(WangRiverCycle, qtsMap,
             nCol = 4, nRow = 5,
             fName = "WW_WRC-grid.QTS.pdf")
plotPoemGrid(WangRiverCycle, wyMap,
             nCol = 4, nRow = 5,
             fName = "WW_WRC-grid.WY.pdf")
plotPoemGrid(WangRiverCycle, posMap,
             nCol = 4, nRow = 5,
             fName = "WW_WRC-grid.POS.pdf")
 

# ==== gridplot Song ZhiWen poems =======
# Vol 52, # 19 - 38
SZW <- poemDF[poemDF$QTSvol == 52 &
              poemDF$QTSnum >= 19 &
              poemDF$QTSnum <= 38, ]
SZW <- SZW[order(SZW$meanLR), ]
plotPoemGrid(SZW, qtsMap, nCol = 4, nRow = 5,
             fName = "SZW-grid.pdf")

SZW_JJ <- JJ[JJ$authorID == 130, ]


# ==== gridplot 李白 ===============
LiBaiJJ <- JJ[JJ$authorID == getAuthorID("李白"), ]
LiBaiJJ <- LiBaiJJ[order(LiBaiJJ$meanLR), ]

plotPoemGrid(LiBaiJJ, map = qtsMap, nCol = 6, nRow = 8,
             fName = "LiBai_JJ-grid.pdf")

# ==== gridplot 李商隐 ==============
LiShangYin <- JJ[JJ$authorID == getAuthorID("李商隐"), ]
LiShangYin <- LiShangYin[order(LiShangYin$meanLR), ]
plotPoemGrid(LiShangYin, map = qtsMap, nCol = 6, nRow = 5,
             fName = "LiShangYin_JJ-grid.pdf")

# ==== gridplot 杜甫 ================
DuFuJJ <- JJ[JJ$authorID == getAuthorID("杜甫"), ]
DuFuJJ <- DuFuJJ[order(DuFuJJ$meanLR), ]
plotPoemGrid(DuFuJJ, map = qtsMap, nCol = 3, nRow = 3,
             fName = "DuFu_JJ-grid.pdf")

# ==== gridplot 韦应物 ==============
WeiYingWuJJ <- JJ[JJ$authorID == getAuthorID("韦应物"), ]
WeiYingWuJJ <- WeiYingWuJJ[order(WeiYingWuJJ$meanLR), ]
plotPoemGrid(WeiYingWuJJ, map = qtsMap, nCol = 6, nRow = 10,
             fName = "WeiYingWu_JJ-grid.pdf")

# ==== gridplot all JueJu ==============
JJ <- JJ[order(JJ$meanLR), ] # order by mean log rank
plotPoemGrid(JJ, map = qtsMap, nCol = 43, nRow = 54,
             fName = "All_JJ-grid.qts.pdf")

# Locate the four sample poems on the full grid:
# (1) Mèng Hào Rán 孟浩然 (430):
#     “Chūn Xiǎo” 《春晓》 “Spring Dawn” (6874)
#     getAuthorID("孟浩然")
#     which(poemDF$titleS == "春晓" & poemDF$authorID == 430)
#     which(JJ$poemID == 6874)   ... # 37
#     ceiling(37/43); 629 %% 43  # row and column
# (2) Wáng Wéi 王維: “Lù Zhài” 《鹿柴》 “Deer Grove” 
#     grep("鹿柴", JJ$titleS)   # 102 (!), not # 306
#     ceiling(103/43); 103 %% 43  # 3/17
# (3) Wéi Yīng Wù 韋應物 (432):
#     “Chú Zhōu Xī Jiàn” 《滁州西澗》 “Western Valley of Chú Zhōu”
#     grep("滁州西涧", poemDF$titleS)
#     this is not in JJ - but a JJ of the same mean LR is #629
#     ceiling(629/ 43); 629 %% 43  # 15 / 27
# (4) Liǔ Zōng Yuán 柳宗元: “Jiāng Xuě” 《江雪》 “River Snow”
#     which(JJ$titleS == "江雪")   #1433
#     ceiling(1433/ 43); 1433 %% 43   # 34 / 14


# === list highest/lowest rank JJ
JJ <- JJ[order(JJ$meanLR), ] # order by mean log rank
printPoems(JJ[1:10,])
printPoems(JJ[-(1:(nrow(JJ) - 10)), ])


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
JJ <- poemDF[iJueJu, ]
JJ <- JJ[order(JJ$meanLR), ] # order by mean Rank

lineLRanks <- matrix(numeric(4 * nrow(JJ)), ncol = 4)
colnames(lineLRanks) <- c("line.1", "line.2", "line.3", "line.4")

for (i in 1:nrow(JJ)) {
  lines <- unlist(strsplit(JJ$bodyS[i], " "))
  for (j in 1:4){
    lineLRanks[i, j] <- calcMeanLogRank(lines[j])
  }
}

boxplot(lineLRanks,
        cex.main = 0.8,
        main = "mean log(ranks) of character frequency for JueJu lines")


# same for Wang Wei
# 
WW_JJ <- JJ[JJ$authorID == 389, ]

WW_lr <- matrix(numeric(4 * nrow(WW_JJ)), ncol = 4)
colnames(WW_lr) <- c("line.1", "line.2", "line.3", "line.4")

for (i in 1:nrow(WW_JJ)) {
  lines <- unlist(strsplit(WW_JJ$bodyS[i], " "))
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
LSY_JJ <- JJ[JJ$authorID == 1069, ]

LSY_lr <- matrix(numeric(4 * nrow(LSY_JJ)), ncol = 4)
colnames(LSY_lr) <- c("line.1", "line.2", "line.3", "line.4")

for (i in 1:nrow(LSY_JJ)) {
  lines <- unlist(strsplit(LSY_JJ$bodyS[i], " "))
  for (j in 1:4){
    LSY_lr[i, j] <- calcMeanLogRank(lines[j])
  }
}

boxplot(LSY_lr,
        cex.main = 0.8,
        col = "#eecccc",
        main = "mean log(ranks) of character frequency for JueJu lines (Li Shangyin only)")

# combine 

allLr <- matrix(nrow = nrow(JJ), ncol=16)
allLr[1:nrow(JJ), 1:4] <- lineLRanks
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


# === MANUALLY JUDGE JUE JU ====================================================
# To manually judge Jue Ju, we present them in random order so as not to induce
# a poet-bias. The randomized Jue Ju are printed into one line, with a
# scale bar. Judgers replace one of the symbols in the scale bar with an x.

# 
# |=====-=====|  	画君年少时 如今君已老 今时新识人 知君旧时好	   5285: ...
# |=====-=====|  	北邙不种田 但种松与柏 松柏未生处 留待市朝客	  11693: ...
# |=====-=====|  	孤灯照不寐 风雨满西林 多少关心事 书灰到夜深	  27846: ...
# |=====-=====|  	玉管朝朝弄 清歌日日新 折花当驿路 寄与陇头人	   1685: ...

rJJ <- JJ[sample(1:nrow(JJ)), ]
sink(file="randomJJ.txt")

for (i in 1:nrow(rJJ)) {
  p <- rJJ[i, ]
  cat(sprintf("|=====-=====|  \t%20s\t  %5d: (%3d:%3d)\t%-5s -\t%s\n",
              p$bodyS,
              p$poemID,
              p$QTSvol,
              p$QTSnum,
              authorDF$nameS[authorDF$authorID == p$authorID],
              p$titleS))
}

sink(NULL)


#    
# ==== TESTS ===================================================================




# [END]