proximaty <-
function(x, y) {
    act <- which(x != 0)
    len <- length(act)
    comp <- which(y != 0)
    nums <- sapply(list(x, y), is.numeric)
    if (sum(nums) != 2) {
        stop("all variables must be numeric")
    }    
    if (identical(act, integer(0)) | identical(comp, integer(0))) {
        return(0)
    }
    lapply(1:len, function(i) {
            ming <- act[i] - comp
            min(abs(ming))
        }
    )
}
