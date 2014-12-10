trim <- function (x) gsub("^\\s+|\\s+$", "", x)

predictText <- function(txt, N = 3){
        if(N > 3) N <- 3
        print(N)
#         ptm <- proc.time()
        txt.list   <- as.list(strsplit(trim(txt), " "))[[1]]
        n <- length(txt.list)
        prediction <- quadf[as.list(txt.list)]
        prediction <- prediction[, c(1, n+2), with = FALSE]
        prediction <- setorder(prediction, -'freq')
#         print(proc.time() - ptm)
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
        return(unique(prediction[1:N]))
}