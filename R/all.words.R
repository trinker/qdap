all.words <-
function(text.var, begins.with = NULL, contains = NULL){
    if (!is.null(begins.with) & !is.null(contains)) {
        stop("Can not use both 'begins.with' & 'contains' arguments")
    }

    if(!is.null(begins.with)) begins.with <- tolower(begins.with)
    if(!is.null(contains)) contains <- tolower(contains)
    WORDS <- unlist(word.split(reducer(strip(text.var))))
    names(WORDS) <- NULL
    y <- data.frame(table(WORDS), stringsAsFactors = FALSE)
    names(y) <- c("WORD", "FREQ")
    y$WORD <- as.character(y$WORD)
    if (!is.null(begins.with)) {
        y <- y[substring(y[, 1], 1, nchar(begins.with)) %in% begins.with, ]
        if(nrow(y)==0) stop("No words match")
    }

    if (!is.null(contains)) {
        y <- y[grep(contains, y[, 1]), ]
        if(nrow(y)==0) stop("No words match")
    }
    left.just(y, "WORD")
}
