exclude <-
function(word.list, ...) {
    mf <- match.call(expand.dots = FALSE)
    excluded <- as.character(mf[[3]])
    word.list[!word.list %in% excluded]
}
