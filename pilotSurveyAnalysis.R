# pilotSurveyAnalysis.R
#
# Purpose:  Perform exploratory analysis in the pilot survey responses.
#           
# Precondition: The poemDF object exists in
#               "../data/poemDF.RData"
#                
# Postcondition:
#                
#
# Notes: 
# 
#
# V 1.0
# Date:     January 2017
# Author:   Boris Steipe and Yi Chen
#
# ToDo      
#           
# V 1.0     First code 
#
# ==============================================================================

setwd(WENJIDIR)


# ==== PREPARE ================================================================

load("../data/poemDF.RData")
load("../data/authorDF.RData")
load("../data/ziRef.RData")

source("QuanTangShiFrequencies.R") # prepare ziFreq and ziRanks objects
source("WenYanFrequencies.R") # prepare ziWYFreq and ziWYRanks objects
source("mapCol.R") # prepare mapCol maps and functions
source("plotPoems.R") # load printing and plotting functions


patt2score <- function(patt) {
  # transforms a (vector) of response patterns |=====-=x===| into 
  # their corresponding numerical score (1, 11): left is low, right is high.
  sco <- numeric(length(patt))
  for (i in 1:length(patt)) {
    sco[i] <- which(unlist(strsplit(toupper(patt[i]), "")) == "X")
  }
  return(sco)
}

permuteCorrelations <- function(r = rMean, f, myCol, doRanks = FALSE) {
  if (doRanks) {
    r <- rank(r)
    f <- rank(f)
  }
  N <- 100000
  thisCor <- cor(r, f)
  cors <- numeric(N)
  for (i in 1:N) {
    cors[i] <- cor(r, sample(f))
  }
  cat(sprintf("Observed correlation is at p = %7.5f \n",
              sum(cors > thisCor) / N))
  hist(cors, breaks = 50)
  abline(v = thisCor, col = myCol)
}


# ==== PROCESS =================================================================

# The indices of the 20 survey poems ...
iPoems <- c(  5285, 11693, 27846,  1685, 19525,
              13798,  6874,  8611, 28320, 32738,
              33343, 14400, 41686, 38040, 36299,
              8139, 26522, 41655, 42108, 16042)

# read survey results
surveyFiles <- list.files("../data/pilot-01_results", full.names = TRUE)
nResp <- length(surveyFiles)
nPoems <- length(iPoems)
surveyMat <- matrix(numeric(nPoems * nResp), nrow = nPoems)
row.names(surveyMat) <- iPoems
cN <- character(nResp)
for (i in 1:length(surveyFiles)) {
  fName <- surveyFiles[i]
  cN[i] <- gsub("\\.txt", "", gsub(".*/", "", fName))
  cN[i] <- gsub("\\.", "", make.names(cN[i]))
  surveyMat[ , i] <- patt2score(substr(readLines(fName), 3, 13))
}
colnames(surveyMat) <- cN
rm(list = c("nResp", "cN", "surveyFiles", "fName"))

# Reader #10 is an outlier and is removed from further analysis
surveyMat <- surveyMat[ , -10]

# Poem #13 has an outlier character and is removed from further analysis
surveyMat <- surveyMat[ -13, ]
nPoems <- nrow(surveyMat)

# Calculate reader response means:
rMean <- rowMeans(surveyMat)

# average reader scores and ranks:
for (i in 1:length(rMean)) {
  cat(sprintf("%s\t%3.1f\n", names(rMean)[i], rank(rMean)[i]))
}


# Calculate means of character frequency rank for all poems
fQTSMean    <- numeric(nPoems)
fWYMean     <- numeric(nPoems)
fQTSlogMean <- numeric(nPoems)
fWYlogMean  <- numeric(nPoems)
for (i in 1:nPoems) {
  s <-poemDF$bodyS[(iPoems[-13])[i]]
  fQTSMean[i]    <- calcMeanLogRank(s, noLog = TRUE)
  fWYMean[i]     <- calcMeanLogRank(s, ranks = ziWYRanks, noLog = TRUE)
  fQTSlogMean[i] <- calcMeanLogRank(s)
  fWYlogMean[i]  <- calcMeanLogRank(s, ranks = ziWYRanks)
}
names(fQTSMean)    <- names(rMean) 
names(fWYMean)     <- names(rMean) 
names(fQTSlogMean) <- names(rMean) 
names(fWYlogMean)  <- names(rMean) 


cor(rMean, fQTSMean)      # 0.546
cor(rMean, fWYMean)       # 0.339
cor(rMean, fQTSlogMean)   # 0.507
cor(rMean, fWYlogMean)    # 0.148


plot(rMean, fQTSMean)
abline(lm(fQTSMean ~ rMean), col = "maroon")

plot(rMean, fWYMean)
abline(lm(fWYMean ~ rMean), col = "skyblue")

plot(rMean, fQTSlogMean)
abline(lm(fQTSlogMean ~ rMean), col = "darkviolet")

plot(rMean, fWYlogMean)
abline(lm(fWYlogMean ~ rMean), col = "darkturquoise")


# How significant?
permuteCorrelations(f = fQTSMean,    myCol = "maroon")        # p = 0.00851
permuteCorrelations(f = fWYMean,     myCol = "skyblue")       # p = 0.07808
permuteCorrelations(f = fQTSlogMean, myCol = "darkviolet")    # p = 0.01298
permuteCorrelations(f = fWYlogMean,  myCol = "darkturquoise") # p = 0.27381


# QTS vs. WY mean frequency ranks ...
cor(fQTSMean, fWYMean) # 0.825
plot(fQTSMean, fWYMean, type = "n")
abline(lm(fWYMean ~ fQTSMean), col = "#BBFFCC")
text(fQTSMean, fWYMean, labels = names(fQTSMean), cex = 0.8)

cor(fQTSlogMean, fWYlogMean) # 0.799
plot(fQTSlogMean, fWYlogMean, type = "n")
abline(lm(fWYlogMean ~ fQTSlogMean), col = "#FFBBCC")
text(fQTSlogMean, fWYlogMean, labels = names(fQTSlogMean), cex = 0.8)

# 6874 is the "most poetic" poem.


# All ranks
cat("\npoem\tscore\tQTS\tWY\tQTSlog\tWYlog\n")
for (i in 1:length(fQTSMean)) {
  cat(sprintf("%s\t%d\t%d\t%d\t%d\t%d\n",
              names(fQTSMean)[i],
              rank(rMean, ties.method = "first")[i],
              rank(fQTSMean, ties.method = "first")[i],
              rank(fWYMean, ties.method = "first")[i],
              rank(fQTSlogMean, ties.method = "first")[i],
              rank(fWYlogMean, ties.method = "first")[i]))
}


# ==== non-parametric
cor(rank(rMean), rank(fQTSMean))     # 0.639
plot(rank(rMean), rank(fQTSMean))
abline(lm(rank(fQTSMean) ~ rank(rMean)), col = "maroon")

cor(rank(rMean), rank(fWYMean))      # 0.308
plot(rank(rMean), rank(fWYMean))
abline(lm(rank(fWYMean) ~ rank(rMean)), col = "skyblue")

cor(rank(rMean), rank(fQTSlogMean))  # 0.463
plot(rank(rMean), rank(fQTSlogMean))
abline(lm(rank(fQTSlogMean) ~ rank(rMean)), col = "darkviolet")

cor(rank(rMean), rank(fWYlogMean))   # 0.280
plot(rank(rMean), rank(fWYlogMean))
abline(lm(rank(fWYlogMean) ~ rank(rMean)), col = "darkturquoise")


# How significant?
permuteCorrelations(f = fQTSMean,    myCol = "maroon", doRanks = TRUE)        
permuteCorrelations(f = fWYMean,     myCol = "skyblue", doRanks = TRUE)       
permuteCorrelations(f = fQTSlogMean, myCol = "darkviolet", doRanks = TRUE)    
permuteCorrelations(f = fWYlogMean,  myCol = "darkturquoise", doRanks = TRUE) 
# p = 0.00217
# p = 0.09811
# p = 0.02366
# p = 0.11922


# ==== Heatmap ....
myCol <- colorRampPalette(c("#00DD55",
                            "#003344",
                            "#000000",
                            "#660022",
                            "#DD0033"))(10)
heatmap(surveyMat, scale = "none", cexCol = 0.8, col = myCol)



#    
# ==== TESTS ===================================================================




# [END]