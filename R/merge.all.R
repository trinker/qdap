merge.all <-
function(frames, by, na.replace = NA) {
    DF <- Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames)
    return(NAer(DF, replace = na.replace))
}
