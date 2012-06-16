adjacency_matrix <-
function(matrix.obj) {
    if(class(matrix.obj) %in% c("termco_d", "termco_c")){
        info <- matrix.obj #for later use
        if (matrix.obj[["zero_replace"]] != 0){
            matrix.obj <- replacer(matrix.obj[["raw"]], 
                matrix.obj[["zero_replace"]], 0)
        } else {
            matrix.obj <- matrix.obj[["raw"]]
        }
        matrix.obj <- termco2mat(matrix.obj)
    } else {    
        if (class(matrix.obj) =="matrix") {
            if (is.null(comment(matrix.obj))){
                warning("Not a termco or wfm object; results may not be correct.")
            } else {
                if(comment(matrix.obj)!="true.matrix"){
                    warning(paste("Not a termco.d, termco.c or wfm object;",
                    "results may not be correct."))
                }
            }
        } else {
             warning("Not a matrix object; results may not be correct.")
        }
    }
    Y <- matrix.obj >= 1
    Y <- apply(Y, 2, as, "numeric") 
    rownames(Y) <- rownames(matrix.obj)
    Z <- Z2 <- t(Y) %*% Y                    
    Z2[!lower.tri(Z2)] <- NA
    Z2 <- Z2[-1, -ncol(Z2)]
    o <- list(boolean = Y, adjacency = Z, shared = Z2, sum = colSums(Y))
    class(o) <- "adjacency.matrix"
    return(o)
}

adjmat <- adjacency_matrix

