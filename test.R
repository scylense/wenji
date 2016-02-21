

linkURL <- "http://www.shigeku.org/shiku/gs/tangshi/"


x <- readLines(linkURL)
head(x, 20)

# this is encoded with the gb2312 charcter encoding. inconv() will
# convert to utf-8

y <- iconv(x, "GB2312", "UTF8")
head(y, 20)
tail(y, 20)


# ToDo:
#
# Parse all lines and make a list of URLs to process
# 

m <- regexpr("(qts_\\d{4}.htm)", y)
urlList <- c(urlList, regmatches(y, m))
  

# 
# 
# 
# 
# For each URL in list:

for (i in 1:length(urlList)) {
  if nchar(urlList[i]) > 1 {
    url <- paste(linkURL, urlList[i], sep="")
    print(paste("downloading", urlList[i]))
    x <- readLines(url)
    y <- iconv(x, "GB2312", "UTF8")
    fileName <- paste("../data/", urlList[i], sep="")
    writeLines(y, fileName)
  }
}


#    fetch the link
#    Convert to utf-8
#    Make a filename
#    save as a file in the data-directory.