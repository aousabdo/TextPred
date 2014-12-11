unigrams  <- readRDS('unigrams.Rds')
bigrams   <- readRDS('bigrams.Rds')
trigrams  <- readRDS('trigrams.Rds')
quadgrams <- readRDS('quadgrams.Rds')

setcolorder(bigrams,   c("w1", "w2", "freq", "prob"))
setcolorder(trigrams,  c("w1", "w2", "w3", "freq", "prob"))
setcolorder(quadgrams, c("w1", "w2", "w3", "w4", "freq", "prob"))

source("cleanStrings.R")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

predictText <- function(txt, N = 3){
        # reset max N to 3
        #         if(N > 3) N <- 3
        txt.list   <- as.list(strsplit(trim(cleanStrings(txt)), " "))[[1]]
        n <- length(txt.list)
        if(n > 4) n <- 4
        prediction <- quadgrams[as.list(tail(txt.list, 3))]
        prediction <- setorder(prediction, -'freq')
        if(is.na(prediction$freq[1])){
                prediction <- trigrams[as.list(tail(txt.list, 2))]
#                 prediction <- prediction[, c(1, 4), with = FALSE]
                prediction <- setorder(prediction, -'freq')             
                if(is.na(prediction$freq[1])){
                        prediction <- bigrams[as.list(tail(txt.list, 1))]
#                         prediction <- prediction[, c(1, 3), with = FALSE]
                        prediction <- setorder(prediction, -'freq')             
                        if(is.na(prediction$freq[1])){
                                prediction <- setorder(unigrams, -'freq')             
                        }
                }
        }
        prediction <- unique(prediction)
        return(prediction[1:N])
}

# print(predictText('obama', N = 3))
# prediction <- predictText(' the obama', N = 3)

# which(is.na(prediction$freq), arr.ind=TRUE)
