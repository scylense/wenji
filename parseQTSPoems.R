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
# The following poem files were manually repaired.
#    qts_0540.htm was missing
# 
#
# V 0.1
# Date:     May. 2016
# Author:   Boris Steipe and Yi Chen
#
# V 0.1     build author dataframe
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
  
  # replace links to character image with "?"
  txt <- gsub("<a href=\"\\.\\./BZK/\\w+\\.jpg\"target=\"_blank\">[^<]+</a>", "?", txt)
  
  # test titles
  if (gregexpr("【", txt)[[1]][1] == -1) {
    stop(sprintf("Volume in file %s: no match for \"【\".", fileName))
  }
  nTitles <- length(gregexpr("【", txt)[[1]])
  
  # test authors
  if (gregexpr("<font face=\"黑体\">", txt)[[1]][1] == -1) {
    stop(sprintf("Volume in file %s: contains no \"author font\" tags.", fileName))
  }
  nAuthors <- length(gregexpr("<font face=\"黑体\">", txt)[[1]])

  if (nTitles != nAuthors) {
    stop(sprintf("Volume in file %s: mismatch between number of titles and authors.", fileName))
  }
  
  # add period after poem author
  txt <- gsub("<font face=\"黑体\">([^<]*)</font>", "author\\1。", txt)
  
  # replace with "unknown" if missing
  txt <- gsub("author。", "未名。", txt)
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
  txt <- gsub("[。， \\s\r\n]+", " ", txt)
  
  # replace fullwidth parentheses with plain parentheses
  txt <- gsub("（", "(", txt)
  txt <- gsub("）", ")", txt)
  
  # delete all comments (in parentheses)
  txt <- gsub("\\([^)]*\\)", "", txt)
  
  # replace U+25A1 WHITE SQUARE □ with questionmark
  txt <- gsub("□", "?", txt)

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
  # Value: a list object with QTS number, authorID, title, and body

  poem <- list()
  poem$body <- p
  
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
  if (m[[1]][1] == -1) {stop(sprintf("Poem %d: no author found.", i))}
  aut <- regmatches(poem$body, m)[[1]][2]
  poem$authorID <- authorDF$authorID[authorDF$nameS == aut]
  if (length(poem$authorID) != 1) {
    print(sprintf("Warning: Poem %d: author %s not in DB.", i, aut))
    poem$authorID <- authorDF$authorID[authorDF$nameS == "未名"]
  }
  poem$body <- gsub(regEx, "", poem$body)

  return(poem)
}


validateCJK <- function(s, skipUnk = TRUE) {
  # validate that the Characters in "s" are all in the CJK unified Block
  # some exceptions are passed in vector "allowed"

  if (skipUnk) {
    allowed <- c(" ", "(", ")", "-", "?")
  } else {
    allowed <- c(" ", "(", ")", "-")
  }
  
  props <- u_char_property(utf8ToInt(s), which = "Block")
  chars <- unlist(strsplit(s, ""))
  x <- which(props != "CJK Unified Ideographs")
  if (length(x) == 0 | all(chars[x] %in% allowed)) {
    # all validated
    return(0)
  } else {
    x <- x[! chars[x] %in% allowed]
    return(chars[x])
  }
}



# ==== PROCESS =================================================================

load("../data/authorDF.RData")

poemDir <- "../data/qts"
poemDocs <- list.files(poemDir, pattern="qts_.+")

nFiles <- length(poemDocs)

#system("grep authers ../data/qts/qts_*.htm | wc -l")  # 41,667
nPoems <- 45000 # estimate - see above

poemDF <- data.frame(poemID   = numeric(nPoems),
                     QTSvol   = numeric(nPoems),
                     QTSnum   = numeric(nPoems),
                     period   = character(nPoems),
                     authorID = numeric(nPoems),
                     titleS   = character(nPoems),
                     bodyS    = character(nPoems),
                     stringsAsFactors = FALSE)

for (i in 20:nFiles) {
  print(sprintf("Volume: %d", i))
  po <- extractPoems(sprintf("%s/%s", poemDir, poemDocs[i]))
  for (j in 1:length(po)) {
#    poemID <- max(poemDF$poemID) + 1
#    rowNum <- min(which(poemDF$poemID == 0))
    
    poem <- analysePoem(po[j], j)
    
    s <- sprintf("%d.%d \"%s\" (%s) %s\n",
                i, poem$qtsNum, poem$title,
                authorDF$nameS[authorDF$authorID == poem$authorID],
                substr(poem$body, 1, 20))
    if (validateCJK(poem$title)[1] != 0) {
      print(paste("Warning: Non CJK in title: ", validateCJK(poem$title), s))
    }
    if (validateCJK(poem$body)[1] != 0) {
      print(paste("Warning: Non CJK in body: ", validateCJK(poem$body), s))
    }
#    if (readline(prompt = "next poem> ") == "x") { break() }
  }
  if (readline(prompt = "next volume> ") == "x") { break() }
}

# save(...)


fileName <- "../data/qts/qts_0001.htm"
p <- extractPoems(fileName)

i <- 1


utf8ToInt("（")
u_char_inspect(utf8ToInt("帝京篇十首"))
u_char_inspect(utf8ToInt("（"))
u_char_inspect(utf8ToInt("-"))

# development ...



#    
#    
#    
# ==== TESTS ===================================================================




# [END]