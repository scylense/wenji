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
load("../data/ziRef.RData")


# Scripts that are commented out in this section are referenced here for
# documentation only and do not normally need to be run again. These were
# written to prepare data that was subsequently saved and is being loaded where
# required.
#
# file.edit("repairShitanSources.R")   # prepare sources
# file.edit("parseQTSPoems.R")         # parse poems
# file.edit("parseQTSAuthors.R")       # parse authors
# file.edit("posTagQTS.R")             # add POS tags



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


# frequency analysis:
file.edit("QTSfrequencyAnalysis.R")


# part-of-speech analysis:
file.edit("QTSposAnalysis.R")


# simplicity analysis:
file.edit("QTSsimplicityAnalysis.R")



#    
# ==== TESTS ===================================================================




# [END]