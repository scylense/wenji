# parseQTSPoems.R
#
# Purpose:  Extract a poems dataframe from the QTS collection
#           downloaded from shitan.org
#           
# Preconditions: Source html documents are expected in a directory called "qts"
#                within "data" (which is a sister to the
#                wenji project directory).
#                                
#                The Working Directory should be set to the project directory
#                (WENJIDIR) prior to executing this code.
#                
#                The authorDF object must have been saved to 
#                "../data/authorDF.RData" (as result of parseQTSAuthors.R)
#                
# Postcondition: The poemDF object has been saved to
#                "../data/poemDF.RData"
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
# For file repair documentation: see notes in repairShitanSources.R
# 
#
# V 1.1
# 
# Date:     May - November 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo      Move flagsDF and poemFlagsDF in poemAnalysis.R
#           - Flag title ellipsis
#           - unknown characters
#           Refine parsing
#           Improve documentation
#           
# V 1.1     Add Mean Log Ranks and lower and upper log rank quartile columns to
#           poemDF data frame.
# V 1.0     - use Unicode replace glyph "�" for unknown characters.
#           - most requirements implemented
# V 0.1     write functions to extract and analyse poems
#
# ==============================================================================

setwd(WENJIDIR)

# ==== PACKAGES ================================================================

# XML parses HTML files
if (!require(XML)) {
  install.packages("XML")
  library(XML)
}

# string handling functions, e.g. str_trim
if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

# various Unicode functions for validation
if (!require(Unicode)) {
  install.packages("Unicode")
  library(Unicode)
}


# ==== DEFINITIONS =============================================================

notesDF <- data.frame(notesID = numeric(),
                      name = character(),
                      definition = character(),
                      stringsAsFactors = FALSE)

i<- 1
notesDF$notesID[i] <- i 
notesDF$name[i] <- "TITLETRUNC" 
notesDF$definition[i] <- "Title truncated, contains ellipsis \"…\"." 

i<- 2
notesDF$notesID[i] <- i 
notesDF$name[i] <- "UNKCHAR" 
notesDF$definition[i] <- "Unknown or untranscribable character(s) replaced with \"�.\"" 

i<- 3
notesDF$notesID[i] <- i 
notesDF$name[i] <- "MISSCHAR" 
notesDF$definition[i] <- "Character missing in QTS source shown with \"□\"." 

i<- 4
# this flag is used for poems with "X首" notes in the title, as well as
# poems that contain "¤" as a stanza annotation
notesDF$notesID[i] <- i 
notesDF$name[i] <- "TODOMULTISECT" 
notesDF$definition[i] <- "Poem has multiple sections or stanzas that need to be annotated." 

i<- 5
notesDF$notesID[i] <- i 
notesDF$name[i] <- "MULTISECT" 
notesDF$definition[i] <- "Poem has multiple sections or stanzas that have been annotated." 


# ==== FUNCTIONS ===============================================================


extractPoems <- function(fileName) {
  # break apart a QTS volume into its poems.
  # Value: a character vector with one poem per element
  
  # Check that file exists and is accessible
  if (!file.exists(fileName)) { stop(sprintf("File %s does not exist.", fileName))}
  if (file.access(fileName, mode=4) != 0) { stop(sprintf("File %s can't be read.", fileName))}
  
  FN <- file(fileName, encoding="GB18030")
  txt <- readLines(FN)
  close(FN)
  
  if (txt[1] != "<html>" | txt[length(txt)] != "</html>") {
    stop(sprintf("Volume %s missing starting or ending html tag.", fileName))
  }
  
  txt <- paste(txt, sep="", collapse="")
  if (nchar(gsub("<", "", txt)) != nchar(gsub(">", "", txt))) {
    stop(sprintf("Volume %s contains unbalanced angle brackets.", fileName))
  }
  
  # remove homepage link at end
  txt <- gsub("<a href=\"index.htm\">回目录</a>", "", txt)
  
  # replace links to character image with "�"
  txt <- gsub("<a href=\"\\.\\./BZK/\\w+\\.jpg\"target=\"_blank\">za</a>zb", "��", txt)
  txt <- gsub("<a href=\"\\.\\./BZK/\\w+\\.jpg\"target=\"_blank\">[^<]+</a>", "�", txt)
  
  # handle shigeku AND shitan style titles
  txt <- gsub("「", "【", txt)
  txt <- gsub("」", "】", txt)
  
  # normalize "" to "�": - QTS shows these to be untranscribable, not missing
  txt <- gsub("", "�", txt)

  # normalize "xX々" to "��"
  txt <- gsub("[a-zA-Z0-9]{2}々", "��", txt)
  
  # test titles
  if (gregexpr("[【]", txt)[[1]][1] == -1) {  
    stop(sprintf("Volume in file %s: no match for \"【\".", fileName))
  }
  nTitles <- length(gregexpr("【", txt)[[1]])
  
  # test authors
  if (gregexpr("<font face=\"黑体\">", txt)[[1]][1] == -1) {
    stop(sprintf("Volume in file %s: contains no \"author font\" tags.", fileName))
  }
  nAuthors <- length(gregexpr("<font face=\"黑体\">", txt)[[1]])

  if (nTitles != nAuthors) {
    print(sprintf("Warning: Volume in file %s: 
                   mismatch between number of titles (%d) and authors(%d) .",
                  fileName, nTitles, nAuthors))
  }
  
  # add period after poem author
  txt <- gsub("<font face=\"黑体\">([^<]*)</font>", "author\\1。", txt)
  
  # replace with "unknown" if missing
  txt <- gsub("author。", "未知。", txt)
  txt <- gsub("author", "", txt)
  
  # add period after "drinking game" author
  txt <- gsub("--([^<]+)</em>", "--\\1。", txt)
  
  # remove all html tags
  txt <- gsub("<[^>]*>", "", txt) 
  
  # replace &nbsp; with plain space
  txt <- gsub("&nbsp;", " ", txt)  
  
  # remove all whitespace
  txt <- gsub("[\\s ]+", "", txt) # CAVE: the character class contains a
                                  # "special" blank " " that is NOT recognized
                                  # as \s in the regex!?
                                  # 
  
  # replace punctuation and line-breaks with a single blank
  txt <- gsub("[。，、：！？； \\s\r\n]+", " ", txt)
  
  # replace multiple blanks with a single blank
  txt <- gsub("\\s{2,}", " ", txt)
  
  # replace fullwidth parentheses with plain parentheses
  txt <- gsub("（", "(", txt)
  txt <- gsub("）", ")", txt)
  
  # do not delete comments (in parentheses)
  # txt <- gsub("\\([^)]*\\)", "", txt)
  
  # don't replace U+25A1 WHITE SQUARE □ with questionmark
  # txt <- gsub("□", "?", txt)

  # reattach "drinking game" author to preceding verse
  txt <- gsub(" --", "--", txt)

  # test for presence of volume character
  if (gregexpr("\\s*卷\\d+_", txt)[[1]][1] == -1) {
    stop(sprintf("Volume in file %s: no match for \"\\s*卷\\d+_\".", fileName))
  }
  
  # split on each poem number and drop the first element (contains the title)
  poems <- unlist(strsplit(txt, "\\s*卷\\d+_"))[-1]
  
  if (length(poems) != nTitles) {
    stop(sprintf("File %s: numbers of titles and poems don't match.", fileName))
  }
  
  for (i in 1:length(poems)) {
    poems[i] <- str_trim(poems[i])
  }

  return(poems)
}



analysePoem <- function(p, i) {
  # analyse a QTS poem.
  # parameters: p  a QTS poem as extracted by extractPoems()
  #             i  an index associated with the poem 
  # Value: a list object with QTS number, authorID, title, and body

  poem <- list()
  poem$body <- p
  poem$flags <- character(0)
  
  # extract QTS number
  regEx <- "^(\\d+)"
  m <- regexec(regEx, poem$body)
  if (m[[1]][1] == -1) {
    print(sprintf("Warning: Poem %d: no QTS number present.", i))
    poem$qtsNum <- 0
  } else {
    poem$qtsNum <- as.integer(regmatches(poem$body, m)[[1]][2])
    poem$body <- gsub(regEx, "", poem$body)
  }
  
  # extract Title
  regEx <- "^【([^】]+)】"
  m <- regexec(regEx, poem$body)
  if (m[[1]][1] == -1) {
    print(sprintf("Warning: Poem %d: no title present.", i))
    poem$title <- "???"
  } else {
    poem$title <- regmatches(poem$body, m)[[1]][2]
    poem$body <- gsub(regEx, "", poem$body)
  }
  
  # extract Author
  regEx <- "^([^ ]+) "
  m <- regexec(regEx, poem$body)
  if (m[[1]][1] == -1) {print(sprintf("Warning: Poem %d: no author found.", i))}
  aut <- regmatches(poem$body, m)[[1]][2]
  poem$authorID <- authorDF$authorID[authorDF$nameS == aut]
  if (length(poem$authorID) > 1) {
    stop(sprintf("Stop: Poem %d: author %s is not unique in DB.", i, aut))
  } else if (length(poem$authorID) == 0) {
    poem$authorID <- aut  # to be added to authorDF
  }
  poem$body <- gsub(regEx, "", poem$body)
  
  # annotate 
  poem$flags <- numeric(0)

  if (length(grep("…", poem$title)) != 0) { poem$flags <- c(poem$flags, 1)} # TITLETRUNC
  if (length(grep("�", poem$body))  != 0) { poem$flags <- c(poem$flags, 2)}  # UNKCHAR
  if (length(grep("□", poem$body)) != 0) { poem$flags <- c(poem$flags, 3)}  # MISSCHAR
  if (length(grep("[一二三四五六七八九十]首", poem$title)) != 0 |          # TODOMULTISECT
      length(grep("¤", poem$body) != 0)) { 
    poem$flags <- c(poem$flags, 4)
  }  
  
  return(poem)
}


validateCJK <- function(s) {
  # Validate that the Characters in "s" are all in the CJK unified Block.
  # Allowed exceptions are defined in vector "allowed".

  allowed <- c(" ", "-", "(", ")", "“", "”", "‘", "’", "《", "》", "…", "□", "·", "¤", "�")
  
  CJKBlocks <- c("CJK Unified Ideographs",
                 "CJK Symbols and Punctuation",
                 "CJK Unified Ideographs Extension A")

  props <- u_char_property(utf8ToInt(s), which = "Block")
  chars <- unlist(strsplit(s, ""))
  x <- which(! props %in% CJKBlocks)
  if (length(x) == 0 | all(chars[x] %in% allowed)) {
    # all validated
    return(0)
  } else {
    x <- x[! chars[x] %in% allowed]
    return(chars[x])
  }
}


calcMeanLogRankQ <- function(s) {
  # calculate the mean log_10 frequency rank for the lower and upper
  # quartile of characters in string "s"
  # (global) rank vector ziRank must exist
  r <- as.numeric(log10(ziRanks[unlist(strsplit(gsub(" ", "", s), ""))]))
  q <- quantile(r, na.rm = TRUE)
  l <- mean(r[r <= q[2]], na.rm = TRUE)
  u <- mean(r[r >= q[4]], na.rm = TRUE)
  return(c(l, mean(r, na.rm = TRUE), u))
}



# ==== PROCESS =================================================================

load("../data/authorDF.RData")
load("../data/poemDF.RData")


poemDir <- "../data/qts"
poemDocs <- list.files(poemDir, pattern="qts_.+")

nFiles <- length(poemDocs)

#system("grep authers ../data/qts/qts_*.htm | wc -l")  # 41,667
nPoems <- 45000 # estimate - see above

poemDF <- data.frame(poemID   = numeric(nPoems),
                     QTSvol   = numeric(nPoems),
                     QTSnum   = numeric(nPoems),
                     ShitanNum  = numeric(nPoems),
                     authorID = numeric(nPoems),
                     titleS   = character(nPoems),
                     bodyS    = character(nPoems),
                     stringsAsFactors = FALSE)

for (i in 1:nFiles) {
  print(sprintf("Volume: %d", i))
  po <- extractPoems(sprintf("%s/%s", poemDir, poemDocs[i]))
  for (j in 1:length(po)) {
    poemID <- max(poemDF$poemID) + 1
    rowNum <- min(which(poemDF$poemID == 0))
    
    poem <- analysePoem(po[j], j)
    
    # s <- sprintf("%d.%d \"%s\" (%s) %s\n",
    #             i, poem$qtsNum, poem$title,
    #             authorDF$nameS[authorDF$authorID == poem$authorID],
    #             substr(poem$body, 1, 20))
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
    
    # add this poem to DF:
    poemDF[rowNum, "poemID"] <- poemID
    poemDF[rowNum, "QTSvol"] <- i
    poemDF[rowNum, "QTSnum"] <- j
    poemDF[rowNum, "ShitanNum"] <- poem$qtsNum  # confusing, but correct
    poemDF[rowNum, "authorID"] <- poem$authorID
    poemDF[rowNum, "titleS"] <- poem$title
    poemDF[rowNum, "bodyS"] <- poem$body
  }
  # if (readline(prompt = "next volume> ") == "x") { break() }
}


poemDF <- poemDF[1:42867,]
save(poemDF, file="../data/poemDF.RData")




# development ...

utf8ToInt("（")
u_char_inspect(utf8ToInt("帝京篇十首"))
u_char_inspect(utf8ToInt("‘‘"))
u_char_inspect(utf8ToInt("-"))



# Add Mean Log Ranks and lower and upper log rank quartiles to data frame.

poemDF <- data.frame(poemDF, 
                     meanLR = numeric(nrow(poemDF)), 
                     meanLRLQ = numeric(nrow(poemDF)), 
                     meanLRUQ = numeric(nrow(poemDF)), 
                     stringsAsFactors = FALSE)

for (i in 1:nrow(poemDF)) {
  lmu <- calcMeanLogRankQ(poemDF$bodyS[i])
  poemDF$meanLRLQ[i] <- lmu[1]
  poemDF$meanLR[i]   <- lmu[2]
  poemDF$meanLRUQ[i] <- lmu[3]
  if (! i %% 1000) { print(i) }
}

# save(poemDF, file = "../data/poemDF.RData")







#    
# ==== TESTS ===================================================================




# [END]