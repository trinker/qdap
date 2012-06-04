key_merge <-
function(transcript.df, key.df, common.column, 
    defualt.arrange = TRUE) {
    DF <- merge(transcript.df, key.df, by = c(common.column, 
        common.column), incomparables = NA)
    if (defualt.arrange) {
        DF[, c(1, 3:ncol(DF), 2)]
    } else {
        DF
    }
}
