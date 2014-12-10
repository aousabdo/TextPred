source("cleanStrings.R")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

predictText <- function(txt, N = 3){
        # reset max N to 3
        if(N > 3) N <- 3

        txt.list   <- as.list(strsplit(trim(cleanStrings(txt)), " "))[[1]]
        n <- length(txt.list)
        prediction <- quadf[as.list(txt.list)]
        prediction <- prediction[, c(1, n+2), with = FALSE]
        prediction <- setorder(prediction, -'freq')

        #         if(is.na(prediction$freq)){
        #                 prediction <- trif[txt.list]
        #                 if(is.na(prediction)){
        #                         prediction <- bif[txt.list]
        #                         if(is.na(prediction)){
        #                                 prediction <- wf[txt.list]
        #                                 if(is.na(prediction)){
        #                                         setorder(wf, -'freq')
        #                                         prediction <- wf[1:10]$w1
        #                                 }
        #                         }
        #                 }
        #         }
        return(unique(prediction))
}

print(predictText(' the obama', N = 3))
prediction <- predictText(' the obama', N = 3)

which(is.na(prediction$freq), arr.ind=TRUE)
