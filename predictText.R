# unigrams  <- readRDS('unigrams.Rds')
# bigrams   <- readRDS('bigrams.Rds')
# trigrams  <- readRDS('trigrams.Rds')
# quadgrams <- readRDS('quadgrams.Rds')
# fivegrams <- readRDS('fivegrams.Rds')

source("cleanStrings.R")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

predictText <- function(txt, N = 3){
        # reset max N to 3
        if(N > 3) N <- 3
        
        # clean incoming text
        txt.list   <- as.list(strsplit(trim(cleanStrings(txt)), " "))[[1]]
        
        # length of terms entered
        n <- length(txt.list)
        if(n > 4) n <- 4
        # get next words
        prediction <- fivegrams[as.list(tail(txt.list, 4))]
        prediction <- setorder(prediction, -'freq')
        prediction <- prediction[, -c( 1:n), with = FALSE]
        if(is.na(prediction$freq[1])){
                prediction <- quadgrams[as.list(tail(txt.list, 3))]
                prediction <- setorder(prediction, -'freq')
                if(n > 3) n <- 3
                prediction <- prediction[, -c( 1:n), with = FALSE]
                if(is.na(prediction$freq[1])){
                        prediction <- trigrams[as.list(tail(txt.list, 2))]
                        prediction <- setorder(prediction, -'freq')      
                        if(n > 2) n <- 2
                        prediction <- prediction[, -c( 1:n), with = FALSE]
                        if(is.na(prediction$freq[1])){
                                prediction <- bigrams[as.list(tail(txt.list, 1))]
                                prediction <- setorder(prediction, -'freq')             
                                if(n > 1) n <- 1
                                prediction <- prediction[, -c( 1:n), with = FALSE]
                                if(is.na(prediction$freq[1])){
                                        prediction <- setorder(unigrams, -'freq')             
                                }
                        }
                }
        }
        #method 2----------------------#
        if(n > 4) n <- 4
        pred5 <- fivegrams[as.list(tail(txt.list, 4))]
        pred5 <- setorder(pred5, -'freq')
        pred5 <- pred5[, -c( 1:n), with = FALSE]
        
        if(n > 3) n <- 3
        pred4 <- quadgrams[as.list(tail(txt.list, 3))]
        pred4 <- setorder(pred4, -'freq')
        pred4 <- pred4[, -c( 1:n), with = FALSE]
        
        if(n > 2) n <- 2
        pred3 <- trigrams[as.list(tail(txt.list, 2))]
        pred3 <- setorder(pred3, -'freq')
        pred3 <- pred3[, -c( 1:n), with = FALSE]
        
        if(n > 1) n <- 1
        pred2 <- bigrams[as.list(tail(txt.list, 1))]
        pred2 <- setorder(pred2, -'freq')
        pred2 <- pred2[, -c( 1:n), with = FALSE]
        
        pred1 <- setorder(unigrams, -'freq') 
        #------------------------------#
        prediction <- unique(prediction)
        #         return(list(prediction[1:N], pred5, pred4, pred3, pred2, pred1))
        return(list(pred5[1:N], pred4[1:N], pred3[1:N], pred2[1:N], pred1[1:N]))
#         return(prediction[1:N])
}

