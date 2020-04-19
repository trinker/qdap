#' Separate a Column Pasted by paste2
#' 
#' Separates a \code{\link[qdap]{paste2}} column into separate columns.
#' 
#' @param column The pasted vector.
#' @param col.sep The column separator used in \code{paste2}.
#' @param name.sep Name separator used in the column (generally for internal use 
#' with \code{\link[qdap]{colsplit2df}}).
#' @return Returns a dataframe of split columns.
#' @seealso \code{\link[qdap]{colsplit2df}}, 
#' \code{\link[qdap]{paste2}}
#' @keywords column-split
#' @export
#' @examples 
#' \dontrun{
#' foo1 <- paste2(CO2[, 1:3])
#' head(foo1, 12)
#' bar1 <- colSplit(foo1)
#' head(bar1, 10)
#' 
#' foo2  <- paste2(mtcars[, 1:3], sep="|")
#' head(foo2, 12)
#' bar2 <- colSplit(foo2, col.sep = "|")
#' head(bar2, 10)
#' }
colSplit <-
function(column, col.sep = ".", name.sep = "&"){
    column <- as.data.frame(column)
    svar <- strsplit(as.character(column[, 1]), col.sep, fixed = TRUE)
    svar <- data.frame(do.call('rbind', svar), stringsAsFactors = FALSE)
    if (!is.null(name.sep) & length(unlist(strsplit(names(column), 
        name.sep, fixed = TRUE))) > 1){
        
        cn <- strsplit(names(column)[1], name.sep, fixed = TRUE)[[1]]

        if (length(cn) == ncol(svar)) {
            names(svar) <- cn
        } else {
            colnames(svar) <- make.names(1:ncol(svar), unique = TRUE)
        }

    } 
    svar
}

