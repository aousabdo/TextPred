# This function will clean unwanted basic characters from the stream.
# It converts basic punctuation characters except "-" and ' which are 
# still valid for the analysis we want to perform.
cleanStrings <- function(s) {
        # first fix the unicode ' characters to be all consistent
        s <- gsub("\xe2\x80\x99", "'", s, perl=TRUE)
        s <- gsub("\u0091|\u0092|\u0093|\u0094|\u0060|\u0027|\u2019|\u000A", "'", s, perl=TRUE)
        
        # Strip unwanted UTF-8 characters
        s <- iconv(s, "UTF-8", "ASCII", "?")
        # strip unused characters but leave ' and -
        s <- gsub("[^[:alpha:][:space:]'-.]", " ", s)
        
        # now let's get rid of single quotes that are quoted strings and not in the middle of a word
        # this will leave contractions like don't, I'm, etc.
        s <- gsub("(?<!\\w)[-'](?<!\\w)" , " ", s, perl=TRUE) # just leave - and ' in the middle of words
        
        s <- gsub("[[:space:]]+", " ", s, perl=TRUE) # consolidate spaces
        s <- gsub("^[[:space:]]+", "", s, perl=TRUE)  # strip leading spaces
        s <- tolower(s)
        
        return(s)
}