folder <-
function(folder.name = NULL) {
    FN <- if (is.null(folder.name)) {
        SS <- gsub(":", ".", substr(Sys.time(), 1, 19))
        paste(substr(SS, 1, 10), "  Time", substr(SS, 11, 19), sep = "")
    } else {
        as.character(substitute(folder.name))
    }
    x <- paste(getwd(), "/", FN, sep = "")
    dir.create(x)
    return(FN)
}
