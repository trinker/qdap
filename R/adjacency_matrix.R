#' Takes a Matrix and Generates an Adjacency Matrix
#' 
#' Takes a matrix (wfm) or termco object and generates an adjacency matrix for 
#' use with \code{igraph}.
#' 
#' @rdname adjacency_matrix
#' @param matrix.obj A matrix object, preferably, of the class "termco" or 
#'  generated from \code{termco}, \code{termco.d} or \code{termco.c}.
#' @return Returns an adjacency matrix.
#' @seealso 
#' \code{\link[stats]{dist}}
#' @keywords adjacency matrix
#' @export 
#' @examples
#' \dontrun{
#' wordLIST <- c(" montague", " capulet", " court", " marry")
#' (raj.termco <- with(raj.act.1, termco(dialogue, person, 
#'     wordLIST, ignore.case = TRUE)))
#' (raj.adjmat <- adjmat(raj.termco))
#' names(raj.adjmat)  #see what's available from the adjacency_matrix object
#' library(igraph)
#' g <- graph.adjacency(raj.adjmat$adjacency, weighted=TRUE, mode ="undirected")
#' g <- simplify(g)
#' V(g)$label <- V(g)$name
#' V(g)$degree <- degree(g)
#' layout1 <- layout.auto(g)
#' plot(g, layout=layout1)
#' }
adjacency_matrix <-
function(matrix.obj) {
    if(class(matrix.obj) %in% c("termco")){
        info <- matrix.obj #for later use
        if (matrix.obj[["zero.replace"]] != 0){
            matrix.obj <- replacer(matrix.obj[["raw"]], 
                matrix.obj[["zero.replace"]], 0)
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
    class(o) <- "adjacency_matrix"
    return(o)
}

#' @rdname adjacency_matrix
#' @export
adjmat <- adjacency_matrix

#' Prints an adjacency_matrix Object
#' 
#' Prints an adjacency_matrix object.
#' 
#' @param x The adjacency_matrix object.
#' @param \ldots ignored
#' @method print adjacency_matrix
#' @S3method print adjacency_matrix
print.adjacency_matrix <-
  function(x, ...) {
    cat("Adjacency Matrix:\n\n")
    print(x$shared, na.print="", quote=FALSE)
    cat("\n\n")
    cat("Summed occurrences:\n\n")
    print(x$sum)
}
