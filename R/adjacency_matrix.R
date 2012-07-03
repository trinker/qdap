#' Takes a matrix and generates an adjacency matrix
#' 
#' Takes a matrix (wfm) or termco object (.c or .d) and generates an adjacency
#' matrix for use with igraph
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases adjacency_matrix adjmat
#' @param matrix.obj %% ~~Describe \code{matrix.obj} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (matrix.obj) 
#' {
#'     if (class(matrix.obj) %in% c("termco_d", "termco_c")) {
#'         info <- matrix.obj
#'         if (matrix.obj[["zero_replace"]] != 0) {
#'             matrix.obj <- replacer(matrix.obj[["raw"]], matrix.obj[["zero_replace"]], 
#'                 0)
#'         }
#'         else {
#'             matrix.obj <- matrix.obj[["raw"]]
#'         }
#'         matrix.obj <- termco2mat(matrix.obj)
#'     }
#'     else {
#'         if (class(matrix.obj) == "matrix") {
#'             if (is.null(comment(matrix.obj))) {
#'                 warning("Not a termco or wfm object; results may not be correct.")
#'             }
#'             else {
#'                 if (comment(matrix.obj) != "true.matrix") {
#'                   warning(paste("Not a termco.d, termco.c or wfm object;", 
#'                     "results may not be correct."))
#'                 }
#'             }
#'         }
#'         else {
#'             warning("Not a matrix object; results may not be correct.")
#'         }
#'     }
#'     Y <- matrix.obj >= 1
#'     Y <- apply(Y, 2, as, "numeric")
#'     rownames(Y) <- rownames(matrix.obj)
#'     Z <- Z2 <- t(Y) %*% Y
#'     Z2[!lower.tri(Z2)] <- NA
#'     Z2 <- Z2[-1, -ncol(Z2)]
#'     o <- list(boolean = Y, adjacency = Z, shared = Z2, sums = colSums(Y))
#'     class(o) <- "adjacency.matrix"
#'     return(o)
#'   }
#' 
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
    if (nrow(matrix.obj) < 2) {
      stop("matrix.obj must have > 1 rows")
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

