# read list of bad words
badwords    <- VectorSource(readLines("badwords.txt"))

cleanCorpus <- function(corpus){
        # clean corpus        
        # transform to lower case
        corpus.tmp <- tm_map(corpus, content_transformer(tolower))
        
        # remove profanity words
        corpus.tmp <- tm_map(corpus.tmp, removeWords, badwords) 
        
        # remove white spaces
        corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
        
        # remove numbers
        corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
        
        corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
        # clean corpus from other junk
        corpus.tmp <- tm_map(corpus.tmp, function(x) gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_\u003B]+', "", x))
        
        # you need to do this since you are using tm > 0.5
        corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
        
        return(corpus.tmp)
}