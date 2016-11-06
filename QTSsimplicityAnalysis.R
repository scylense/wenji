# QTSsimplicityAnalysis.R
#
# Purpose:  Perform analysis of simplicity judgements in the QTS corpus
#           
# Precondition: poemDF
#               authorDF
#                
# Postcondition: ""
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

setwd(WENJIDIR)

# ==== PACKAGES ================================================================


# ==== DEFINITIONS =============================================================


# ==== FUNCTIONS ===============================================================

printJudgingFormat <- function(P) {
  
  # print poems in P in a format suitable for simplicity judgement.
  
  for (i in 1:nrow(P)) {
    p <- P[i, ]
    cat(sprintf("|=====-=====|  \t%20s\t  %5d: (%3d:%3d)\t%-5s -\t%s\n",
                p$bodyS,
                p$poemID,
                p$QTSvol,
                p$QTSnum,
                authorDF$nameS[authorDF$authorID == p$authorID],
                p$titleS))
  }
}


# ==== PROCESS =================================================================




# === MANUALLY JUDGE JUE JU ====================================================
# To manually judge Jue Ju, we present them in random order so as not to induce
# a poet-bias. The randomized Jue Ju are printed into one line, with a
# scale bar. Judgers replace one of the symbols in the scale bar with an x.

# 
# |=====-=====|  	画君年少时 如今君已老 今时新识人 知君旧时好	   5285: ...
# |=====-=====|  	北邙不种田 但种松与柏 松柏未生处 留待市朝客	  11693: ...
# |=====-=====|  	孤灯照不寐 风雨满西林 多少关心事 书灰到夜深	  27846: ...
# |=====-=====|  	玉管朝朝弄 清歌日日新 折花当驿路 寄与陇头人	   1685: ...

# rJJ <- JJ[sample(1:nrow(JJ)), ]

# Unfortunately, this was done without using set.seed() - therefore the object 
# is saved here, in case it needs to be reconstructed. All Judgers should work
# on the same order of poems to ensure best coverage of a selection.

# save(rJJ, file = "../data/randomJJ.RData")
load(file = "../data/randomJJ.RData")

# fName <- "randomJJ.txt"
# sink(file=fName)

printJudgingFormat(rJJ[1:10, ])

# sink(NULL)


#    
# ==== TESTS ===================================================================




# [END]