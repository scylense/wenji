# parseQTSAuthors.R
#
# Purpose:  Extract an author dataframe from the QTS collection
#           downloaded from shitan.org
#
# Preconditions: Source html documents are expected in a directory called
#                "authers" [sic], within the "qts" directory
#                within "data" (which is a sister to the
#                wenji project directory).
#                
#                The Working Directory should be set to the project directory
#                (WENJIDIR) prior to executing this code.
#                
# Postcondition: The authorDF object has been saved to
#                "../data/authorDF.RData"
#                
# Notes: The source documents were downloaded from 
#     http://www.shitan.org/xlib/gd/qts/qts.zip
# The extracted .zip archive contains the 900 volumes of poems, and the 
# author biographies, as HTML documents. The HTML documents are erroneously
# annotated to be in GB2312 encoding, however they are actually encoded in 
# GB18030 (which supersedes GB2312 and supports both simplified and traditional
# characters). For encoding details see https://en.wikipedia.org/wiki/GB_18030
# 
# The following author files were manually repaired.
#    qzz_0416.htm   (bio taken from alternative source)
#    qzz_0849.htm   (bio taken from alternative source)
#    qzz_0863.htm   (incomplete HTML repaired)
#    qzz_1209.htm   (incomplete HTML repaired)
#    qzz_1639.htm   (removed a spurious blank)
#    qzz_1901.htm   (bio taken from alternative source)
#    qzz_1956.htm   (removed a spurious blank)
#    qzz_1957.htm   (removed a spurious blank)
#    qzz_1960.htm   (removed a spurious blank)
#    
# An unknown author "未知" with bio "未知。" was added to the dataframe with
# ID 1967.
# 
# ID 1478 / 1651 is a name duplicate. However, there is no reference to author
# 1651, and only one to 1478. Thus the 1651 author was "inactivated" by adding
# "未知" to the name.
# 
# During analysis of the Shitan files, an additional 663 authors were added
# to the DF - however these have unknown biographies.  
#
# V 1.0
# Date:     May 2016
# Author:   Boris Steipe and Yi Chen
#
# V 1.0     build author dataframe and save authorDF object
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



# ==== FUNCTIONS ===============================================================


extractAuthorData <- function(fileName) {
  # reads author and biography information from one shitan biography HTML file.
  # Value: a list object with $author and $bio elements.
  
  # Check that file exists and is accessible
  if (!file.exists(fileName)) { stop(sprintf("File %s does not exist.", file))}
  if (file.access(fileName, mode=4) != 0) { stop(sprintf("File %s can't be read.", file))}
  
  doc <- htmlTreeParse(fileName,
                       useInternalNodes = TRUE,
                       encoding = "GB18030")
  
  # These documents seem to be formatted as a table. The information we seek
  # should be in the 5th "tr" element 
  tr <- xpathApply(doc, "//tr", xmlValue)
  if (length(tr) != 7) { stop(sprintf("File %s has unexpected structure.", file))}

  # trim the fifth element
  # trim whitespace from ends of the fifth element and 
  # strsplit() by whitespace in the middle
  x <- unlist(strsplit(str_trim(tr[[5]]), "\\s+"))
  if (length(x) != 2) { stop(sprintf("Name/bio %s has unexpected structure.", file))}
  
  return(list(author=x[1], bio=x[2]))
}


# ==== PROCESS =================================================================

authorDir <- "../data/qts/authers"
authorDocs <- list.files(authorDir, pattern="qzz_.+")

nFiles <- length(authorDocs)

authorDF <- data.frame(authorID = 1:nFiles,
                       nameS = character(nFiles),
                       QTSbio =  character(nFiles),
                       stringsAsFactors = FALSE)

for (i in 1:nFiles) {
  print(i)
  au <- extractAuthorData(sprintf("%s/%s", authorDir, authorDocs[i]))
  authorDF[i, "authorID"] <- i
  authorDF[i, "nameS"]    <- au$author
  authorDF[i, "QTSbio"]   <- au$bio
}

i <- nrow(authorDF) + 1
authorDF[i, "authorID"] <- max(authorDF$authorID) + 1
authorDF[i, "nameS"]    <- "未名" 
authorDF[i, "QTSbio"]   <- "未知。"


save(authorDF, file="../data/authorDF.RData")

# Done.


# ==== TESTS ===================================================================

file <- "../data/qts/authers/qzz_1234.htm"
doc <- htmlTreeParse(file,
                     useInternalNodes = TRUE,
                     encoding = "GB18030")
xpathApply(doc, "//tr", xmlValue)

which(duplicated(authorDF$nameS))  # must be none ... otherwise:
grep(authorDF$nameS[which(duplicated(authorDF$nameS))], authorDF$nameS)

# [END]