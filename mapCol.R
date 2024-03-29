# tocID <- "mapCol.R"
#
# Purpose:  return a category-defined colours for a characters 
#           
# Precondition: QTS and WenYan frequency tables must exist in the
#                 global namespace: ziCounts and ziWYscaledCounts 
#                
# Postcondition:  (global) qtsMap, wyMap and posMap exist
#                
#
# Notes:    (global) map objects are defined here.
#             $type is "char" or "pos" and defines the type of value that
#                is expected
#             $cat    named vector that maps values to categories
#             $col    vector of colors, one for each category
#             $cut    vector of upper bounds used to construct rank-based
#                       categories
#             $labels vector of strings, to be used in producing a legend       
#             
# 
#
# V 2.3
# Date:     2015-10  -  2022-10
# Author:   Boris Steipe and Yi Chen
#
# V 2.3     Add a "vivid" spectrum for slides etc.
# V 2.2     Rename "..Freq" to "..Counts
# V 2.1     Move code to calculate scaled WY frequencies to WenYanFrequencies.R
# V 2.0     Refactored from retired code getFcol.R with entirely new concept
# V 1.0     Stable code
#
# ToDo:          
#           
# ==============================================================================


#TOC> ==========================================================================
#TOC> 
#TOC>   Section  Title                                                     Line
#TOC> -------------------------------------------------------------------------
#TOC>   1        qtsMap                                                      68
#TOC>   1.1        qtsMap$type (character or part-of-speech)                 71
#TOC>   1.2        qtsMap$cat (values to categories)                         75
#TOC>   1.3        qtsMap$cut (bounds for rank-based categories)             81
#TOC>   1.4        qtsMap$col (colours for character counts or POS)          92
#TOC>   1.5        qtsMap$labels (... for legend)                           122
#TOC>   1.6        qtsMap$cat: frequencies to categories                    133
#TOC>   2        wyMap                                                      142
#TOC>   2.1        wyMap$type (character or part-of-speech)                 147
#TOC>   2.2        wyMap$cat (scaled counts)                                152
#TOC>   2.3        wyMap$cut (bounds for rank-based categories)             157
#TOC>   2.4        wyMap$col (colours for character counts or POS)          168
#TOC>   2.5        wyMap$labels (... for legend)                            172
#TOC>   2.6        wyMap$cat: frequencies to categories                     183
#TOC>   3        posMap                                                     193
#TOC>   3.1        posMap$col (colours for character counts or POS)         203
#TOC>   4        FUNCTIONS                                                  482
#TOC>   5        Utilities                                                  504
#TOC>   5.1        plotMapLegend()                                          508
#TOC>   6        Tests                                                      559
#TOC> 
#TOC> ==========================================================================


if (FALSE) { # Do not execute when source()'d
  setwd(WENJIDIR)
}

# =    1  qtsMap  ==============================================================
qtsMap <- list()    # == the global map object qtsMap

# ==   1.1  qtsMap$type (character or part-of-speech)  =========================
qtsMap$type <- "char"


# ==   1.2  qtsMap$cat (values to categories)  =================================

qtsMap$cat <- as.numeric(ziCounts)
names(qtsMap$cat) <- names(ziCounts) 


# ==   1.3  qtsMap$cut (bounds for rank-based categories)  =====================

qtsMap$cut <-    c(  30, # top 30 (~15% of all char)
                     100, # ~ 30%
                     515, # ~ 67%
                     1000, # ~ 82%
                     5709, # ~ 99.9%
                     6485, # 2 or 3
                     7454) # hapax


# ==   1.4  qtsMap$col (colours for character counts or POS)  ==================

doPastel <- FALSE
if (doPastel) {
  qtsMap$col <-    c("#c2ffd8",                 # pastel spectrum
                     "#d3eff5",
                     "#f2f6ff",
                     "#fff5f0",
                     "#ffdadf",
                     "#f67884",
                     "#dd0000")
} else {
  # do vivid
  N <- 7
  qtsMap$col <- colorRampPalette(c("#37dbc8",   # vivid spectrum
                                   "#fcfa8b",
                                   "#fc7977",
                                   "#fc267f"))(N)
  barplot(rep(N, N), axes = FALSE, col=qtsMap$col)
}
if (FALSE) {  # use for spectrum development
  N <- 7
  x <- colorRampPalette(c("#37dbc8",
                          "#fcfa8b",
                          "#fc7977",
                          "#fc267f"))(N)
  barplot(rep(N, N), axes = FALSE, col=x)
}


# ==   1.5  qtsMap$labels (... for legend)  ====================================

qtsMap$labels <- c("top 30\n(~ 15%)",
                   "top 100\n(~ 30%)",
                   "top 515\n(~ 67%)",
                   "top 1000\n(~ 82%)",
                   "(~99.9 %)",
                   "two or three",
                   "hapax")


# ==   1.6  qtsMap$cat: frequencies to categories  =============================
qtsMap$cat <- qtsMap$cat[order(qtsMap$cat, decreasing = TRUE)] # indexes are now ranks
for (i in 1:length(qtsMap$cat)) {
  qtsMap$cat[i] <- sum(i > qtsMap$cut) + 1
}

rm(i)


# =    2  wyMap  ===============================================================
# == derive a global map object wyMap for WenYan character frequencies
wyMap <- list()


# ==   2.1  wyMap$type (character or part-of-speech)  ==========================

wyMap$type <- "char"


# ==   2.2  wyMap$cat (scaled counts)  =========================================

wyMap$cat <- ziWYscaledCounts


# ==   2.3  wyMap$cut (bounds for rank-based categories)  ======================

wyMap$cut <- c(    22, # top 22 (~15% of all char)
                   73, # ~ 30%
                  465, # ~ 67%
                  964, # ~ 82%
                 5799, # ~ 99.9%
                 6355, # 2 or 3
                 7454) # hapax


# ==   2.4  wyMap$col (colours for character counts or POS)  ===================
wyMap$col <-  qtsMap$col


# ==   2.5  wyMap$labels (... for legend)  =====================================

wyMap$labels <- c("top 22\n(~ 15%)",
                  "top 73\n(~ 30%)",
                  "top 465\n(~ 67%)",
                  "top 964\n(~ 82%)",
                  "(~99.9 %)",
                  "two or three",
                  "hapax")


# ==   2.6  wyMap$cat: frequencies to categories  ==============================

wyMap$cat <- wyMap$cat[order(wyMap$cat, decreasing = TRUE)] # indexes are now ranks
for (i in 1:length(wyMap$cat)) {
  wyMap$cat[i] <- sum(i > wyMap$cut) + 1
}
# clean up
rm(i)


# =    3  posMap  ==============================================================
# == create a global map object for part-of-speech Tags

posMap <- list(type   = "pos",
               cat    = numeric(),
               col    = character(),
               labels = character())
nam <- character()


# ==   3.1  posMap$col (colours for character counts or POS)  ==================

# define category colors
catCols <- c(
  "#40A6B5",   #  1 Nouns
  "#5CAFB2",   #  2 Nouns
  "#79B8B0",   #  3 Nouns
  "#96C2AE",   #  4 Nouns
  "#B3D6A1",   #  5 noun modifiers
  "#AECC9E",   #  6 noun modifiers
  "#A9C39C",   #  7 noun modifiers
  "#A4BA9A",   #  8 noun modifiers
  "#9FB198",   #  9 noun modifiers
  "#9AA796",   # 10 noun modifiers
  "#959E94",   # 11 noun modifiers
  "#909592",   # 12 noun modifiers
  "#8B8C90",   # 13 noun modifiers
  "#87838E",   # 14 noun modifiers
  "#AD6A70",   # 15 verbs
  "#BF5F62",   # 16 verbs
  "#D15454",   # 17 verbs
  "#E44946",   # 18 verbs
  "#EE8C69",   # 19 verb modifiers
  "#F1A174",   # 20 verb modifiers
  "#F5B680",   # 21 verb modifiers
  "#F8CB8B",   # 22 verb modifiers
  "#FCE097",   # 23 verb modifiers
  "#C2B4E2",   # 24 syntax
  "#BFB6DF",   # 25 syntax
  "#BDB9DD",   # 26 syntax
  "#BABCDB",   # 27 syntax
  "#B8BFD9",   # 28 syntax
  "#BCCEDF",   # 29 other
  "#C6D7E7",   # 30 other
  "#D1E0EF",   # 31 other
  "#DBE9F7",   # 32 other
  "#E6F2FF"    # 33 other
)

# for color development:
# fCol <- colorRampPalette(catCols)
# n <- 33
# oPar <- par(mar=c(0,0,0,0))
# barplot(rep(1, n), col=fCol(n), axes=F, main="", ylim=c(-0.5,1.5))
# sc <- 1.2
# abline(v = (c(4, 14, 18, 23, 28) * sc + 0.1))
# par(oPar)
# fCol(n)
# 
# fCol <- colorRampPalette(c(
#   "#c2B4e2",   
#   "#b8bFd9"    
# ))
# n <- 5
# par(mar=c(0,0,0,0))
# barplot(rep(1, n), col=fCol(n), axes=F, main="", ylim=c(-0.5,1.5))
# fCol(n)
# 
# k <- 5
# x <- fCol(k)
# for (i in 1:k){
#   cat(sprintf("%s\n", x[i]))
# }

# === nouns and pronouns =======================
i <- 1
posMap$cat[i] <- i
nam[i] <- "NN"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "NN (common noun)"

i <- 2
posMap$cat[i] <- i
nam[i] <- "NR"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "NR (proper noun)"

i <- 3
posMap$cat[i] <- i
nam[i] <- "NT"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "NT (temporal noun)"

i <- 4
posMap$cat[i] <- i
nam[i] <- "PN"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "PN (pronoun)"

# === noun modifiers ===========================
i <- 5
posMap$cat[i] <- i
nam[i] <- "DT"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "DT (determiner)"

i <- 6
posMap$cat[i] <- i
nam[i] <- "M"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "M (measure word)"

i <- 7
posMap$cat[i] <- i
nam[i] <- "CD"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "CD (cardinal number)"

i <- 8
posMap$cat[i] <- i
nam[i] <- "OD"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "OD (ordinal number)"

i <- 9
posMap$cat[i] <- i
nam[i] <- "DEC"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "DEC (的 in relative clause)"

i <- 10
posMap$cat[i] <- i
nam[i] <- "DEG"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "DEG (associative 的)"

i <- 11
posMap$cat[i] <- i
nam[i] <- "DER"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "DER (得 in V-de construction)"

i <- 12
posMap$cat[i] <- i
nam[i] <- "JJ"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "JJ (other noun modifier)"

i <- 13
posMap$cat[i] <- i
nam[i] <- "P"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "P (preposition)"

i <- 14
posMap$cat[i] <- i
nam[i] <- "LC"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "LC (localizer)"


# === verbs ====================================
i <- 15
  posMap$cat[i] <- i
nam[i] <- "VA"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "VA (predicative adjective)"

i <- 16
  posMap$cat[i] <- i
nam[i] <- "VC"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "VC (是)"

i <- 17
  posMap$cat[i] <- i
nam[i] <- "VE"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "VE (有as main verb)"

i <- 18
  posMap$cat[i] <- i
nam[i] <- "VV"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "VV (verb)"


# === verb modifiers ===========================
i <- 19
posMap$cat[i] <- i
nam[i] <- "DEV"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "DEV (地 before VP)"

i <- 20
  posMap$cat[i] <- i
nam[i] <- "AD"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "AD (adverb)"

i <- 21
  posMap$cat[i] <- i
nam[i] <- "AS"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "AS (aspect marker)"

i <- 22
  posMap$cat[i] <- i
nam[i] <- "SB"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "SB (被 in short bei-construction)"

i <- 23
  posMap$cat[i] <- i
nam[i] <- "LB"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "LB (被 in long bei-construction)"

# === syntax markers ===========================
i <- 24
  posMap$cat[i] <- i
nam[i] <- "BA"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "BA (把 in ba-construction)"

i <- 25
  posMap$cat[i] <- i
nam[i] <- "CC"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "CC (coordinating conjunction)"

i <- 26
  posMap$cat[i] <- i
nam[i] <- "CS"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "CS (subordinating conjunction)"

i <- 27
  posMap$cat[i] <- i
nam[i] <- "SP"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "SP (sentence-final particle)"

i <- 28
  posMap$cat[i] <- i
nam[i] <- "MSP"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "MSP (other particle)"


# === other ====================================
i <- 29
  posMap$cat[i] <- i
nam[i] <- "ON"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "ON (onomatopoeia)"

i <- 30
  posMap$cat[i] <- i
nam[i] <- "IJ"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "IJ (interjection)"

i <- 31
  posMap$cat[i] <- i
nam[i] <- "ETC"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "ETC (等)"

i <- 32
  posMap$cat[i] <- i
nam[i] <- "FW"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "FW (foreign word)"

i <- 33
  posMap$cat[i] <- i
nam[i] <- "PU"
posMap$col[i] <- catCols[i]
posMap$labels[i] <- "PU (punctuation)"

names(posMap$cat) <- nam
names(posMap$col) <- nam
names(posMap$labels) <- nam
rm(catCols)
rm(nam)
rm(i)


# =    4  FUNCTIONS  ===========================================================

mapCol <- function(map, char) {
  
  # Purpose: return a color value for character char based on its category
  # 
  # Parameters:
  #     map: a list containing
  #            $type   "char" or "pos", used to switch behaviour of plotting
  #                      functions
  #            $cat    A table or named vector that maps a character to a
  #                      category represented as integer
  #            $col    A vector of colors, one for each category
  #            $labels A vector of labels defining each category for
  #                    drawing a legend.
  #     char: a character expected to be present among the names of $cat
  # Value:
  #     a single colour value 
  
    return(map$col[map$cat[char]])
}

# =    5  Utilities  ===========================================================

if (FALSE) { 
  
# ==   5.1  plotMapLegend()  ===================================================

  plotMapLegend <- function(map) {
    
    font.add("zhFont", "华文仿宋.ttf")
    
    # open pdf graphics device
    pdf("legend.pdf")
    par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0)) # turn all margins off

    h <- length(map$col)
    w <- 6
    
    plot(c(0, w), c(0, h),
         type = "n", axes = FALSE,
         xlab = "", ylab = "",
         asp = 1.0
    )
    
    padX <- 0.1
    pady <- 0.1
    
    # for debugging:
    # abline(v=c(0, w)); abline(h=c(0, h)); 
    
    showtext.begin()
    
    # plot each category in turn
    for (i in 1:length(map$col)) {
        rect(padX,
             h - i + padY,
             1 - padX,
             h - i + 1 - padY,
             col = map$col[i],
             border = "#AAAAAA",
             lwd = 0.5)
        text(1.2, h - i + 0.5, map$labels[i], family = "zhFont", pos = 4)
    }
    
    showtext.end()
    dev.off()
    
  }
  
  plotMapLegend(qtsMap)
  plotMapLegend(wyMap)
  plotMapLegend(posMap)
  
}  # END  if (FALSE)  block

#    
# =    6  Tests  ===============================================================

if (FALSE) { 
  
  mapCol(qtsMap, "山")
  mapCol(wyMap, "山")
  mapCol(posMap, "AD")
  
  
}  # END  if (FALSE)  block



# [END]


