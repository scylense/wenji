# QuanTangShi.R
#
# Purpose:  Analyse the QTS corpus.
#           This is the main project files, all creation of assets
#           and individual analyses branch from here.
#                
#
# Notes: 
# 
#
# V 0.1
# 
# Date:     November 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo      
#           
# V 0.1     First code
#
# ==============================================================================

setwd(WENJIDIR)


# ==== PREPARE ================================================================

load("../data/poemDF.RData")
load("../data/authorDF.RData")


# prepare ziFreq and ziRanks objects
source("QuanTangShiFrequencies.R")

# prepare ziWYFreq and ziWYRanks objects
source("WenYanFrequencies.R")

# prepare mapCol maps and functions
source("mapCol.R")

# load printing and plotting functions
source("plotPoems.R")




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


# frequency analysis:
file.edit("QTSfrequencyAnalysis.R")


# port-of-speech analysis:
file.edit("QTSposAnalysis.R")


# simplicity analysis:
file.edit("QTSsimplicityAnalysis.R")



#    
# ==== TESTS ===================================================================




# [END]