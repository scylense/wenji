#!/usr/local/bin/python
# -*- coding: utf-8 -*-

"""
SYNOPSIS

    tagQTS.py infile outfile

DESCRIPTION

    infile: QTS poems, one per line, words separated by blanks
    outfile: POS tagged poems, one per line

EXAMPLE
    python tagQTS.py QTSpoems.txt QTS_PO-tagged.txt

NOTES
   You need to   sudo pip install -U nltk
   to get the nltk dependency, if not present on your system.

   Obviously, the Stanford-postagger is also a dependency. Install as
   per instructions at http://nlp.stanford.edu/software/tagger.shtml#Download
   You need the "full" tagger, the "basic" tagger does not contain
   Chinese.

AUTHOR

    Boris Steipe <boris.steipe@utoronto.ca>

LICENSE

    None

VERSION

    1.0

HISTORY

    1.0 First Code   2016-10-28
"""

# ==== PACKAGES ================================================================

import os
os.chdir('/Users/steipe/Documents/00.3.REFERENCE_AND_SUPPORT/Empirical Aesthetics/POS')

import sys
sys.path.append("/usr/local/lib/python2.7/site-packages")

import io

import nltk
from nltk.tag import StanfordPOSTagger

# ==== INIT ====================================================================

def initialize():

    # process commandline arguments
    if len(sys.argv) < 2:
        sys.exit('tagQTS.py: missing options\n' +
                 'usage: tagQTS.py infile outfile\n')

    global INFILE
    INFILE = sys.argv[1]

    global OUTFILE
    OUTFILE = sys.argv[2]

# ==== FUNCTIONS ===============================================================



# ==== MAIN ====================================================================

def main():

    initialize()
    # create tagger
    model = '../stanford-postagger/models/chinese-distsim.tagger'
    jar = '../stanford-postagger/stanford-postagger.jar'
    zhPOS = StanfordPOSTagger(model, jar)

    # streaming model: process each line in turn
    with io.open(INFILE, 'r', encoding='utf8') as qts, io.open(OUTFILE, 'w', encoding='utf8') as pos:

        for line in qts:
            qtsPOS = zhPOS.tag(line)
            s = " ".join("%s" % tup[1] for tup in qtsPOS) + "\n"
            pos.write(s)

    return()


if __name__ == '__main__':
    main()

# [End of code]
