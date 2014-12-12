unigrams  <- readRDS('unigrams.Rds')
bigrams   <- readRDS('bigrams.Rds')
trigrams  <- readRDS('trigrams.Rds')
quadgrams <- readRDS('quadgrams.Rds')
fivegrams <- readRDS('fivegrams.Rds')

source("cleanStrings.R")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

predictText <- function(txt, N = 3){
        # reset max N to 3
        if(N > 3) N <- 3
        
        # clean incoming text
        txt.list   <- as.list(strsplit(trim(cleanStrings(txt)), " "))[[1]]
        
        # length of terms entered
        n <- length(txt.list)
        
        # get next words
        prediction <- fivegrams[as.list(tail(txt.list, 4))]
        prediction <- prediction[, c(1, n+1), with = FALSE]
        prediction <- setorder(prediction, -'freq')
        if(is.na(prediction$freq[1])){
                prediction <- quadgrams[as.list(tail(txt.list, 3))]
                prediction <- setorder(prediction, -'freq')
                if(is.na(prediction$freq[1])){
                        prediction <- trigrams[as.list(tail(txt.list, 2))]
                        prediction <- setorder(prediction, -'freq')             
                        if(is.na(prediction$freq[1])){
                                prediction <- bigrams[as.list(tail(txt.list, 1))]
                                prediction <- setorder(prediction, -'freq')             
                                if(is.na(prediction$freq[1])){
                                        prediction <- setorder(unigrams, -'freq')             
                                }
                        }
                }
                prediction <- unique(prediction)
        }
        return(prediction[1:N])
}
