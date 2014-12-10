rm(list=ls())
library(R.utils)
# library(rPython)
library(ggplot2) 
library(NLP) 
library(openNLP) 
library(RWeka) 
library(tm)
library(wordcloud)
library(stringi)
library(slam)
library(data.table)

source("cleanStrings.R")
source("cleanCorpus.R")

makePlots <- FALSE

# file connections
en_US_blog    <- file("../Capstone_Project_data/en_US/en_US.blogs.txt")
en_US_twitter <- file("../Capstone_Project_data/en_US/en_US.twitter.txt")
en_US_news    <- file("../Capstone_Project_data/en_US/en_US.news.txt")

# read first n lines from each data file
n <- 4000
blogtxt    <- readLines(en_US_blog, n)
twittertxt <- readLines(en_US_twitter, n)
newstxt    <- readLines(en_US_news, n)

# close file connections
close(en_US_blog)
close(en_US_twitter)
close(en_US_news)

# merge subsets in one dataset to use for the rest of the analysis
txt <- c(blogtxt, twittertxt, newstxt)

# clean text
txt <- cleanStrings(txt)

# build corpus from text source
corpus <- VCorpus(VectorSource(txt))

corpus.tmp <- cleanCorpus(corpus)

# save corpus
save(corpus.tmp, file="corpus.tmp.RData")

# build TDM
tdm <- TermDocumentMatrix(corpus.tmp)

# explore tdm
# total number of terms in tdm
totalNbrOfTerms <- nrow(tdm)

# remove sparse terms
tdms <- removeSparseTerms(tdm, sparse = 0.96) 

# play with the tdm
# # find the most frequenct term
# maxFreq      <- max(apply(tdm, 1, sum))
# mostFreqTerm <- which(apply(tdm, 1, sum)==maxFreq)
# 
# # find terms with frequency greater than 200
# which(apply(tdm, 1, sum) > 200)
# 
# # index of most frequent term
# index <- which(dimnames(tdm)$Terms == names(mostFreqTerm))
# 
# inspect(tdm[index + (0:6), 60:70])

# findFreqTerms(tdm, lowfreq = 200)

# build term frequency matrix for plotting
termFreq <- rowSums(as.matrix(tdm))

frequencies   <- sort(termFreq, decreasing=TRUE)
wf            <- data.table(word=names(frequencies), freq=frequencies, percent = frequencies/totalNbrOfTerms, key = "percent")
wf            <- setorder(wf, -percent)

if(makePlots){
        
        # make frequency bar plot
        n <- 500
        p <- ggplot(wf[wf$freq > n,], aes(x=reorder(word, -freq), y = freq)) 
        p <- p + geom_bar(stat="identity", fill="lightblue") + theme_bw() 
        p <- p + ylab("Word Frequency") + xlab("Words")
        p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
        print(p)
        
        # make frequency plot for top 100 terms
        within(wf[1:100,], 
               plot(wf$percent[1:100]~row.names(wf[1:100,]), type="b", col="blue", log="y", 
                    xlab="Term Index", ylab="Percentage", main="Single Term Frequency, Top 100 Terms") 
        )
}

wf$percent <- NULL
setnames(wf, c('w1', 'freq'))
setkeyv(wf, c('w1', 'freq'))

#---------------------------------------------------------------------------------------------------#
# n-gram stuff -------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
# corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)

# you need the following line for RWeks to work
options(mc.cores=1)

# bi-gram terms
BigramTokenizer   <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigrams           <- TermDocumentMatrix(corpus.tmp, control = list(tokenize = BigramTokenizer))
# inspect(bigrams[1:100, 1:10])

# tri-gram terms
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigrams         <- TermDocumentMatrix(corpus.tmp, control = list(tokenize = TrigramTokenizer))
# inspect(trigrams[1:100, 1:10])

# quad-gram terms
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quadgrams         <- TermDocumentMatrix(corpus.tmp, control = list(tokenize = QuadgramTokenizer))
# inspect(quadgrams[1:100, 1:10])

# total number of bigrams and trigrams

# freq                <- rowSums(as.matrix(bigrams)) # this doesn't work for large matrices
freq                <- rollup(bigrams, 2, na.rm=TRUE, FUN = sum)
totalNbrOfBigrams   <- length(freq)
totalBigramCoverage <- sum(freq)

# build df for barplot
frequencies    <- sort(rowSums(as.matrix(freq)), decreasing=TRUE)
bif            <- data.table(word=names(frequencies), freq=frequencies)
bif            <- setorder(bif, -freq)

# freq                 <- rowSums(as.matrix(trigrams)) # this doesn't work for large matrices
freq                 <- as.matrix(rollup(trigrams, 2, na.rm=TRUE, FUN = sum))
totalNbrOfTrigrams   <- length(freq)
totalTrigramCoverage <- sum(freq)

# build df for barplot
frequencies     <- sort(rowSums(as.matrix(freq)), decreasing=TRUE)
trif            <- data.table(word=names(frequencies), freq=frequencies)
trif            <- setorder(trif, -freq)

freq                 <- as.matrix(rollup(quadgrams, 2, na.rm=TRUE, FUN = sum))
totalNbrOfQuadgrams  <- length(freq)
totalQuadgramCoverage <- sum(freq)

# build df for barplot
frequencies     <- sort(rowSums(as.matrix(freq)), decreasing=TRUE)
quadf           <- data.table(word=names(frequencies), freq=frequencies)
quadf           <- setorder(quadf, -freq)

if(makePlots){
        # bigram freq plot
        # make frequency bar plot
        n <- 50
        pbi <- ggplot(bif[bif$freq > n,], aes(x=reorder(word, -freq), y = freq)) 
        pbi <- pbi + geom_bar(stat="identity", fill="orange") + theme_bw() 
        pbi <- pbi + ylab("Word Frequency") + xlab("bi-grams")
        pbi <- pbi + theme(axis.text.x=element_text(angle=45, hjust=1)) 
        print(pbi)
        
        # trigram freq plot
        # make frequency bar plot
        n <- 10
        ptri <- ggplot(trif[trif$freq > n,], aes(x=reorder(word, -freq), y = freq)) 
        ptri <- ptri + geom_bar(stat="identity", fill="lightgreen") + theme_bw() 
        ptri <- ptri + ylab("Word Frequency") + xlab("tri-grams")
        ptri <- ptri + theme(axis.text.x=element_text(angle=45, hjust=1)) 
        print(ptri)
        
        # quadgram freq plot
        # make frequency bar plot
        n <- 2
        pquad <- ggplot(quadf[quadf$freq > n,], aes(x=reorder(word, -freq), y = freq)) 
        pquad <- pquad + geom_bar(stat="identity", fill="lightgreen") + theme_bw() 
        pquad <- pquad + ylab("Word Frequency") + xlab("quad-grams")
        pquad <- pquad + theme(axis.text.x=element_text(angle=45, hjust=1)) 
        print(pquad)
}

#---------------------------------------------------------------------------------------------------#
# predict ------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#

# trif[like(word, "^i had")][1:3]
# strsplit(trif[like(word, "^i have")][1:3]$word[1], " ")
# strsplit(trif[like(word, "^i have")][1:3]$word[1], " ")[[1]][3]

# faster way to search data.tables 
bif[, word:=strsplit(word, '\\s')]
bif[, `:=`(w1=sapply(word, function(s) s[1]),
           w2=sapply(word, function(s) s[2]),
           word=NULL)]
setkeyv(bif, c('w1', 'w2','freq'))

bif[list('i')][1:3]

trif[, word:=strsplit(word, '\\s')]
trif[, `:=`(w1=sapply(word, function(s) s[1]),
            w2=sapply(word, function(s) s[2]),
            w3=sapply(word, function(s) s[3]),
            word=NULL)]
setkeyv(trif, c('w1', 'w2', 'w3', 'freq'))

trif[list('i', 'love')][1:3]


quadf[, word:=strsplit(word, '\\s')]
quadf[, `:=`(w1=sapply(word, function(s) s[1]),
             w2=sapply(word, function(s) s[2]),
             w3=sapply(word, function(s) s[3]),
             w4=sapply(word, function(s) s[4]),
             word=NULL)]
setkeyv(quadf, c('w1', 'w2', 'w3', 'w4', 'freq'))

print(setorder(trif[list('i', 'am')], -'freq'))

bif[list('the', 'man'), sum(freq)]
trif[list('the', 'man'), sum(freq)]
quadf[list('the', 'man'), sum(freq)]


wf[list('the'), sum(freq)]
bif[list('the'), sum(freq)]
trif[list('the'), sum(freq)]
quadf[list('the'), sum(freq)]

# add probabilities
wf[, prob:=freq/nrow(wf)]
bif[, prob:=freq/nrow(bif)]
trif[, prob:=freq/nrow(trif)]
quadf[, prob:=freq/nrow(quadf)]

# write files 
saveRDS(wf, 'unigrams.Rds')
saveRDS(bif, 'bigrams.Rds')
saveRDS(trif, 'trigrams.Rds')
saveRDS(quadf, 'quadgrams.Rds')

# test how to merge data tables 
quadf.subset <- quadf[sample(1:nrow(quadf), 1e5)]
setkeyv(quadf.subset, c('w1', 'w2', 'w3', 'w4', 'freq'))

quadf.all <- merge(quadf, quadf.subset, all = T, by=c('w1', 'w2', 'w3','w4'))
quadf.all[ , freq :=rowSums(.SD, na.rm = TRUE), .SDcols = c("freq.x", "freq.y")]
quadf.all[ , c('freq.x', 'freq.y', 'prob.x', 'prob.y') := NULL] 

txt      <- "the"
txt.list <- as.list(strsplit(txt, " ")[[1]])
print(setorder(quadf[txt.list], -'freq')[1:5])

