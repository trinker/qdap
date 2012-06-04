truncDF <-
function(x, end=10, begin=1) {
    data.frame(lapply(x, substr, begin, end), check.names=FALSE)
}
