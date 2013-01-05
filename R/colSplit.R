#' Separate a Column Pasted by paste2
#' 
#' Separates a \code{paste2} column into separate columns.
#' 
#' @param column The pasted vector.
#' @param col.sep The column separator used in \code{paste2}.
#' @param name.sep Name separator used in the column (internal use within 
#' \code{colsplit2df}).
#' @return Returns a dataframe of split columns.
#' @seealso \code{\link{colsplit2df}}, 
#' \code{\link{paste2}}
#' @keywords column-split
#' @export
#' @examples 
#' dontrun{
#' (foo <- paste2(CO2[, 1:3]))
#' colSplit(foo)
#' (bar <- paste2(mtcars[, 1:3], sep="|"))
#' colSplit(bar, col.sep = "|")
#' }
colSplit <-
function(column, col.sep = ".", name.sep = "&"){
    column <- as.data.frame(column)
    svar <- strsplit(as.character(column[, 1]), col.sep, fixed = TRUE)
    svar <- data.frame(do.call('rbind', svar))
    if (!is.null(name.sep) & length(unlist(strsplit(names(column), 
        name.sep, fixed = TRUE))) > 1){
            cn <- strsplit(names(column)[1], name.sep, fixed = TRUE)[[1]]
            names(svar) <- cn
    } 
    return(svar)
}
