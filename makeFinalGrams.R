unigrams  <- readRDS('unigrams10000.Rds')
bigrams   <- readRDS('bigrams10000.Rds')
trigrams  <- readRDS('trigrams10000.Rds')
quadgrams <- readRDS('quadgrams10000.Rds')

setcolorder(bigrams, c("w1", "w2", "freq", 'prob'))
setcolorder(trigrams, c("w1", "w2", "w3", "freq", 'prob'))

quadgrams2 <- fread('w4_.txt')
setnames(quadgrams2, c('freq', 'w1', 'w2', 'w3', 'w4'))
setcolorder(quadgrams2, c("w1", "w2", "w3", "w4", "freq"))
quadgrams.all <- merge(quadgrams, quadgrams2, all = T, by=c('w1', 'w2', 'w3','w4', 'freq'))
quadgrams.all[, prob:=freq/nrow(quadgrams.all)]
quadgrams <- quadgrams.all

fivegrams <- fread('w5_.txt')
setnames(fivegrams, c('freq', 'w1', 'w2', 'w3', 'w4', 'w5'))
setcolorder(fivegrams, c("w1", "w2", "w3", "w4", "w5", "freq"))
fivegrams[, prob:=freq/nrow(fivegrams)]
setkeyv(fivegrams, c('w1', 'w2', 'w3', 'w4', 'w5', 'freq'))

setcolorder(bigrams,   c("w1", "w2", "freq", "prob"))
setcolorder(trigrams,  c("w1", "w2", "w3", "freq", "prob"))
setcolorder(quadgrams, c("w1", "w2", "w3", "w4", "freq", "prob"))
setcolorder(fivegrams, c("w1", "w2", "w3", "w4", "w5", "freq", "prob"))

# write files 
saveRDS(bigrams, 'bigrams.Rds')
saveRDS(trigrams, 'trigrams.Rds')
saveRDS(quadgrams, 'quadgrams.Rds')
saveRDS(fivegrams, 'fivegrams.Rds')