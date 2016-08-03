# loadMannheimCorpus.R
#
# Purpose:  Sample code to read Mannheim Corpus text in TEI I5 format and
#           process the XML contents with the xml2 package.
#           
# Preconditions: i5.xml files exist in ../data/mk folder. The files need to be
#                converted to utf-8 prior to processing since their native
#                encoding, iso-8859-1, is not properly handled by the libraries.
#                                
#                
# Postcondition: ...
#                
## 
#
# V 0.1
# Date:     August 2016
# Author:   Boris Steipe and Yi Chen
#
# ToDo:     - ...
#
# V 0.1     first code
#
# ==============================================================================

# ==== PARAMETERS ================================================================

inFile <- "../data/mk/mk1.i5.xml"


# ==== PACKAGES ================================================================

# The new xml2 package is much easier to work with than the XML library.
# It parses XML files. Note: package XML does NOT treat the encoding correctly.
if (!require(xml2)) {
  install.packages("xml2")
  library(xml2)
}

# ==== INITIALIZATIONS =========================================================



# ==== FUNCTIONS ===============================================================


# ==== EXPLORE xml2 ===============================================================

test <- read_xml("<corpus>
                    <header type=\"corpus\">corpus header ...</header>
                 <doc type=\"text\">
                 <header type=\"document\">document header 1 ...</header>
                 <text>text 1 ...</text>
                 </doc>
                 <doc type=\"text\">
                 <header type=\"document\">document header 2 ...</header>
                 <text>text 2 ...</text>
                 </doc>
                 </corpus>")

html_structure(test)

# working with nodes at specific levels
xml_children(test)
xml_children(xml_children(test))

xml_child(xml_children(test), search = "text")

xml_contents(xml_children(test)[[1]])  

xml_contents(xml_child(xml_children(test)[[2]], search="text"))  
xml_contents(xml_child(xml_children(test)[[3]], search="text"))  

# working with nodes based on the results of xpath expressions
xml_find_all(test, ".//text")


# ==== PROCESS THE CORPUS ======================================================


sourceXML <- read_xml(inFile)

xml_name(sourceXML)   # idsCorpus
xml_length(sourceXML)  # 33

# The online documentation at 
# http://www1.ids-mannheim.de/kl/projekte/korpora/archiv/mk.html only gives us 
# information in general terms. Looking into the actual XML, we see that the 
# children of the corpus are tagged as idsHeader (there is one) and idsDoc 
# (several, containing the actual contents).
# 
# We can iterate through all children of the corpus root-node and find the
# <d.title ...> tags to get a listing of the contents.

length(xml_children(sourceXML))

for (i in 1:length(xml_children(sourceXML))) {
  print(paste(i, ": ",
              xml_contents(xml_find_all(xml_children(sourceXML)[[i]], ".//d.title")),
              sep=""))
}


# With this information, we can choose documents. Here, for example, we extract
# the text contained in all sentences (tagged <s>) in child 4 - Max Frisch's
# "Homo Faber"
x <- xml_text(xml_find_all(xml_children(sourceXML)[[4]], ".//s"))

head(x, 15)

# ... etc.


#    
# ==== TESTS ===================================================================




# [END]