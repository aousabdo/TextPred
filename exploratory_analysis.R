rm(list=ls())
library(R.utils)
library(rPython)
library(ggplot2) 
library(NLP) 
library(openNLP) 
library(RWeka) 
library(tm)
library(wordcloud)
library(ITSA)


# file connections
en_US_blog    <- file("../Capstone_Project_data/en_US/en_US.blogs.txt")
en_US_twitter <- file("../Capstone_Project_data/en_US/en_US.twitter.txt")
en_US_news    <- file("../Capstone_Project_data/en_US/en_US.news.txt")

fileInfo <- file.info("../Capstone_Project_data/en_US/en_US.blogs.txt")
cat(paste("blogs file is ", fileInfo$size/1e6, " MB in size", sep = ""), "\n")

# I will be using linux commands to read number of lines since it is much faster
numlines <- system("wc -l ../Capstone_Project_data/en_US/en_US.twitter.txt", intern = TRUE)
numlines <- as.numeric(strsplit(numlines, " ")[[1]][2])
cat(paste("The twitter file has", numlines), " lines\n", sep = " ")

##########################################################################################

numlines     <- system("wc ../Capstone_Project_data/en_US/en_US.blogs.txt", intern = TRUE)
numlinesblog <- as.numeric(strsplit(numlines, " ")[[1]][3])
numwordsblog <- as.numeric(strsplit(numlines, " ")[[1]][4])

numlines     <- system("wc  ../Capstone_Project_data/en_US/en_US.twitter.txt", intern = TRUE)
numlinestwit <- as.numeric(strsplit(numlines, " ")[[1]][2])
numwordstwit <- as.numeric(strsplit(numlines, " ")[[1]][3])

numlines     <- system("wc ../Capstone_Project_data/en_US/en_US.news.txt", intern = TRUE)
numlinesnews <- as.numeric(strsplit(numlines, " ")[[1]][2])
numwordsnews <- as.numeric(strsplit(numlines, " ")[[1]][3])

cat(sprintf(" blog file has: %d lines and %d words\n",numlinesblog, numwordsblog), 
    sprintf("news file has: %d lines and %d words\n",numlinesnews, numwordsnews),
    sprintf("twitter file has: %d lines and %d words\n",numlinestwit, numwordstwit))

cat(sprintf("Total lines %d", numlinesblog+numlinesnews+numlinestwit))
cat(sprintf("Total words %d", numwordsblog+numwordsnews+numwordstwit))

python.exec("print(len(max(open('../Capstone_Project_data/en_US/en_US.blogs.txt', 'r'), key=len)))")
python.exec("print(len(max(open('../Capstone_Project_data/en_US/en_US.news.txt', 'r'), key=len)))")
python.exec("print(len(max(open('../Capstone_Project_data/en_US/en_US.twitter.txt', 'r'), key=len)))")
# 
# system("grep -c \"A computer once beat me at chess, but it was no match for me at kickboxing\" ../Capstone_Project_data/en_US/en_US.twitter.txt")
# 
# system("grep \"biostats\" ../Capstone_Project_data/en_US/en_US.twitter.txt")
# 
# love <- as.numeric(system("grep -c \"love\" ../Capstone_Project_data/en_US/en_US.twitter.txt", intern = TRUE))
# hate <- as.numeric(system("grep -c \"hate\" ../Capstone_Project_data/en_US/en_US.twitter.txt", intern = TRUE))
# love/hate

##########################################################################################
# read first 10,000 lines from each data file
n <- 100
blogtxt    <- readLines(en_US_blog, n)
twittertxt <- readLines(en_US_twitter, n)
newstxt    <- readLines(en_US_news, n)

# close file connections
close(en_US_blog)
close(en_US_twitter)
close(en_US_news)

# merge subsets in one dataset to use for the rest of the analysis
txt <- c(blogtxt, twittertxt, newstxt)

# build corpus from text source
corpus <- VCorpus(VectorSource(txt))

# clean corpus
# read list of bad words
badwords    <- VectorSource(readLines("badwords.txt"))
# remove profanity words
corpus.tmp <- tm_map(corpus, removeWords, badwords) 
# convert letters to lower case
corpous.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
# remove white spaces
corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
# remove numbers
corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
# remove puncuations
corpus.tmp <- tm_map(corpus.tmp, removePunctuation)

ITSA::generateFreqPlot(corpus.tmp, doClean = FALSE, minfreq = 50, horizontal = TRUE)

ITSA::generateWordcloud(corpus.tmp, freq = 10)



corpusDf <-data.frame(text=unlist(sapply(corpus,  `[`, "content")), stringsAsFactors=F)

findNGrams <- function(corp, grams) {
        ngram <- NGramTokenizer(corp, Weka_control(min = grams, max = grams,
                                                   delimiters = " \\r\\n\\t.,;:\"()?!"))
        ngram2 <- data.frame(table(ngram))
        #pick only top 25
        ngram3 <- ngram2[order(ngram2$Freq,decreasing = TRUE),][1:100,]
        colnames(ngram3) <- c("String","Count")
        ngram3
}

TwoGrams <- findNGrams(corpusDf, 2)
ThreeGrams <- findNGrams(corpusDf, 3)
FourGrams <- findNGrams(corpusDf, 4)



