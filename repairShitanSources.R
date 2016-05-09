# repairShitanSources.R
#
# Purpose:  Support repair of some volumes from the QTS collection
#           downloaded from shitan.org
#           
# Preconditions: Source html documents are expected in a directory called "qts"
#                within "data" (which is a sister to the
#                wenji project directory).
#                                
#                The Working Directory should be set to the project directory
#                (WENJIDIR) prior to executing this code.
#                
# Postcondition: Not applicable
#                
#
# Notes: The source documents were downloaded from 
#     http://www.shitan.org/xlib/gd/qts/qts.zip
# The extracted .zip archive contains the 900 volumes of poems, and the 
# author biographies, as HTML documents. The HTML documents are erroneously
# annotated to be in GB2312 encoding, however they are actually encoded in 
# GB18030 (which supersedes GB2312 and supports both simplified and traditional
# characters). For encoding details see https://en.wikipedia.org/wiki/GB_18030
# 
# The shitan.org documents contain the author biographies. However some of the
# files are broken. Repair was undertaken using data from 
#    http://www.shigeku.org/shiku/gs/tangshi/
#
#    qts_0019.htm had a corrupted <font> tag
#    qts_0031.htm poems 31-36 missing
#    qts_0231.htm poems 38-45 missing
#    qts_0236.htm poems 73-81 missing
#    qts_0291.htm poems 17-20 missing
#    qts_0292.htm poems  1-16 missing
#    qts_0297.htm html corrupted after 292_41
#    qts_0367.htm file completely corrupted
#    qts_0386.htm 49-123 missing
#    qts_0447.htm - corrupted html 
#    qts_0511.htm corrupted and 73-92 missing
#    qts_0513.htm completely corrupted
#    qts_0515.htm 10-17 and 32-89 missing
#    qts_0516.htm 3-25 missing
#    qts_0517.htm 2-21 missing
#    qts_0519.htm completely empty (36 poems)
#    qts_0525.htm 2-11 missing
#    qts_0525.htm 2-11 missing
#    qts_0528.htm 17-28 partially corrupted and missing
#    qts_0532.htm 13-29 partially corrupted and missing
#    qts_0538.htm 3-60 missing
#    qts_0540.htm file missing
#    qts_0575.htm minor HTML problem after 575_17
#    qts_0751.htm completely corrupted (30 poems)
#    qts_0752.htm 6-9  partially corrupted and missing
#    qts_0795.htm annotate volume as fragments/various authors
#    qts_0796.htm annotate volume as fragments/unknown authors
#    qts_0798.htm unreadable
#    qts_0799.htm unreadable
#    qts_0800.htm unreadable
#    qts_0891.htm #19 partially corrupted, all replaced to conserve "¤"
#                     stanza marker
#
#    All remaining files were edited to replace non-CJK characters with "� "
#    
#    All in all, 248 files were edited.
#
# V 1.0
# Date:     May. 2016
# Author:   Boris Steipe and Yi Chen
#
# ==============================================================================

setwd(WENJIDIR)

# Note: source functions and load libraries from parseQTSpoems.R



# ==== PROCESS =================================================================

poemDir <- "../data/qts"
poemDocs <- list.files(poemDir, pattern="qts_.+")

nFiles <- length(poemDocs)

for (i in 601:nFiles) {
  print(sprintf("Volume: %d", i))
  fileName <- sprintf("%s/%s", poemDir, poemDocs[i])
  po <- extractPoems(fileName)
  for (j in 1: length(po)) {
    poem <- analysePoem(po[j], j)
    
    s <- sprintf("%d.%d  %04d  \"%s\" (%s) %s\n",
                 i, poem$qtsNum, as.integer(paste(poem$flags, sep="", collapse="")), poem$title,
                 authorDF$nameS[authorDF$authorID == poem$authorID],
                 substr(poem$body, 1, 20))
    if (validateCJK(poem$title)[1] != 0) {
      print(paste("Warning: Non CJK in title: ", validateCJK(poem$title), s))
    }
    if (validateCJK(poem$body)[1] != 0) {
      print(paste("Warning: Non CJK in body of No.", j, ": ", validateCJK(poem$body), s))
    }
    
    if (is.character(poem$authorID)) {
      print(sprintf("Warning: Poem %d: author not in DB. Adding author %s.", j, poem$authorID))
      iRow <- nrow(authorDF) + 1
      authorDF[iRow, "authorID"] <- max(authorDF$authorID) + 1
      authorDF[iRow, "QTSbio"]   <- "未知。"
      authorDF[iRow, "nameS"]    <- poem$authorID
    }
    
    #    if (readline(prompt = "next poem> ") == "x") { break() }
  }
#  if (readline(prompt = "next volume> ") == "x") { break() }
}

# save(...)




# development ...
# 

utf8ToInt("（")
u_char_inspect(utf8ToInt("帝䴔䴖"))
u_char_inspect(utf8ToInt(" "))
u_char_inspect(utf8ToInt("-"))

#    
# ==== TESTS ===================================================================




# [END]
