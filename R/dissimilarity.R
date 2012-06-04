dissimilarity <-
function(wfm.object, method = "euclidean", diag = FALSE, 
    upper = FALSE, p = 2, digits = 3){
    if (comment(wfm.object)!= "true.matrix") {
        warning("not a matrix from word.freq.matrix function")
    }
    if (comment(wfm.object)!= "true.matrix"){
        wfm.object <- wfm.object[-c(nrow(wfm.object)), -c(1, 
            ncol(wfm.object))]
        wfm.object <- t(wfm.object)
    } else {
        wfm.object <- t(wfm.object)
    }
    x <- stats::dist(wfm.object, method = method, diag = diag, upper = upper, p = p) 
    return(round(x, digits = digits))
}
