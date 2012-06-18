stem2df <-
function (dataframe, text.var, stem.name = NULL, ...) {
    if (!is.data.frame(dataframe)) {
        stop("Please supply a data.frame to stem2df")
    }
    if (is.numeric(dataframe[, text.var])) {
        stop("text.var can not be numeric")
    }
    new <- stemmer(dataframe[, text.var], ...)
    DF <- data.frame(dataframe, stem.text = new, stringsAsFactors = FALSE)
    if (!is.null(stem.name)) {
        names(DF)[ncol(DF)] <- stem.name
    } 
    return(DF)
}
