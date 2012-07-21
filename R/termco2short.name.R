termco.short.name <-
function(termco.object){
    CLASS <- class(termco.object)
    short.name <- function(df){
        mn <- gsub("(.*)\\)([^\\)]*)", "\\1\\2", colnames(df))
        colnames(df) <- gsub("term(", "", mn, fixed=TRUE)
        return(df)
    }
    o <- lapply(termco.object, function(x){
        if (is.data.frame(x)){
            short.name(x)
        } else {
            x
        }
    })
    class(o) <- CLASS
    return(o)
}
