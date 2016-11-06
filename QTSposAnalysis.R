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




#    
# ==== TESTS ===================================================================




# [END]