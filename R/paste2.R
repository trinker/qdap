#' Paste an Unspecified Number Of Text Columns
#' 
#' \code{paste2} - Paste unspecified columns or a list of vectors together.
#' 
#' @param multi.columns The multiple columns or a list of vectors to paste 
#' together.
#' @param sep The character to be used in \code{paste2} to paste the columns.
#' @param handle.na logical.  If \code{TRUE} returns \code{NA} if any 
#' column/vector contains a missing value.
#' @param trim logical.  If \code{TRUE} leading/trailing white space is removed.
#' @return \code{paste2} - Returns a vector with row-wise elements pasted together.
#' @note \code{\link[base]{paste}} differs from \code{\link[qdap]{paste2}} 
#' because \code{paste} does not allowed an unspecified number of columns to be 
#' pasted.  This behavior can be convenient for inside of functions when the 
#' number of columns being pasted is unknown.
#' @seealso \code{\link[base]{paste}},
#' \code{\link{colsplit2df}}
#' @rdname paste2
#' @keywords paste
#' @export
#' @examples
#' \dontrun{
#' ## paste2 examples
#' v <- rep(list(state.abb[1:8],  month.abb[1:8]) , 5)
#' n <- sample(5:10, 1)
#' paste(v[1:n]) #odd looking return
#' paste2(v[1:n]) 
#' paste2(v[1:n], sep="|") 
#' paste2(mtcars[1:10,], sep="|") 
#' paste(mtcars[1:10,], sep="|") #odd looking return
#' paste2(CO2[1:10,], sep="|-|") 
#' 
#' ## colpaste2df examples
#' A <- list(
#'     a = c(1, 2, 3),
#'     b = qcv(mpg, hp),
#'     c = c("disp", "am")
#' )
#' B <- list(
#'     c(1, 2, 3),
#'     new.col = qcv(mpg, hp),
#'     c("disp", "am")
#' )
#' E <- list(
#'     c(1, 2, 3, 4, 5),
#'     qcv(mpg, hp),
#'     c("disp", "am")
#' )
#' 
#' colpaste2df(head(mtcars), A)
#' colpaste2df(head(mtcars), B)
#' colpaste2df(head(mtcars), E)
#' colpaste2df(head(mtcars), qcv(am, disp, drat), sep ="_", name.sep = "|")
#' colpaste2df(head(CO2), list(c(1, 2, 3, 4, 5), qcv("conc", "uptake")))
#' }
paste2 <-
function(multi.columns, sep=".", handle.na=TRUE, trim=TRUE){
    if (is.matrix(multi.columns)) {
        multi.columns <- data.frame(multi.columns, stringsAsFactors = FALSE)
    }
    if (trim) multi.columns <- lapply(multi.columns, function(x) {
            gsub("^\\s+|\\s+$", "", x)
        }
    )
    if (!is.data.frame(multi.columns) & is.list(multi.columns)) {
        multi.columns <- do.call('cbind', multi.columns)
    } 
    m <- if (handle.na){
                 apply(multi.columns, 1, function(x){
                     if (any(is.na(x))){
                         NA
                     } else {
                         paste(x, collapse = sep)
                     }
                 }
             )   
         } else {
             apply(multi.columns, 1, paste, collapse = sep)
    }
    names(m) <- NULL
    return(m)
}

#' Wrapper for paste2 that Returns Dataframe(s)
#' 
#' \code{colpaste2df} - Wrapper for \code{\link[qdap]{paste2}} that returns a 
#' dataframe with columns pasted together.
#' 
#' @param mat A matrix or dataframe.
#' @param combined.columns A list of named vectors of the colnames/indexes 
#' of the numeric columns to be pasted.  If a vector is unnamed a 
#' name will be assigned. 
#' @param name.sep The character to be used to paste the column names.
#' @param keep.orig logical.  If \code{TRUE} the original columns 
#' (i.e., \code{combined.columns}) will be retained as well.
#' @param \ldots Other arguments passed to \code{\link[qdap]{paste2}}.
#' @return \code{colpaste2df} - Returns a dataframe with pasted columns.
#' @rdname paste2
#' @export 
colpaste2df <- function(mat, combined.columns, sep = ".", name.sep = "&", 
    keep.orig = TRUE, ...){

    ## convert vector combined.column to a list and name
    if (!is.list(combined.columns) && is.vector(combined.columns)) {
        if (is.numeric(combined.columns)) {
            combined.columns <- names(mat)[combined.columns]
        } 
        x <- combined.columns
        combined.columns <- list(combined.columns)
        names(combined.columns)[1] <- paste(x, collapse = name.sep)
    }

    ## convert numeric columns to named
    combined.columns <- lapply(combined.columns, function(x) {
        if (is.numeric(x)) {
            x <- colnames(mat)[x]
        }
        x
    })

    ## paste columns together in a list
    L1 <- lapply(combined.columns, function(x) {  
        if(all(!x %in% colnames(mat))){
            return(unlist(rep(NA, nrow(mat))))
            warning(sprintf("%s not found in %s", 
                paste(colnames(mat)[!x %in% colnames(mat)], collapse = ", "), 
                as.character(substitute(mat))))
        }
        if(sum(x %in% colnames(mat)) == 1){
            return(unlist(mat[colnames(mat) %in% x]))
        } 
        if(all(x %in% colnames(mat))){
            return(paste2(mat[, x], sep = sep, ...))
        }       
    })

    ## if all names of combined list were null
    if (is.null(names(L1))){
        names(L1) <- rep("", length(L1))
    }

    ## name any nameless combined names by pasting old 
    ## column names together
    locs <- names(L1) == ""
    if (sum(locs) > 0) {
        names(L1)[locs] <- lapply(combined.columns[locs], function(x) {
            paste(x, collapse = name.sep)
        })
    }

    ## cbind the pasted columns into a new dataframe
    DF <- data.frame(do.call(cbind, L1), check.names = FALSE, 
        stringsAsFactors = FALSE)
    DF <- DF[!sapply(DF, function(x) all(is.na(x)))]

    ## remove columns if eliminating old
    if(!keep.orig) {
        mat <- mat[, !colnames(mat) %in% unique(unlist(combined.columns)), 
            drop=FALSE]
    }

    ## merge it back together
    data.frame(mat, DF, check.names = FALSE, stringsAsFactors = FALSE)
}

