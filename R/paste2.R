#' Paste an Unspecified Number Of Text Columns
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param multi.columns %% ~~Describe \code{multi.columns} here~~
#' @param sep %% ~~Describe \code{sep} here~~
#' @param handle.na %% ~~Describe \code{handle.na} here~~
#' @param trim %% ~~Describe \code{trim} here~~
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
#' function (multi.columns, sep = ".", handle.na = TRUE, trim = TRUE) 
#' {
#'     if (trim) 
#'         multi.columns <- lapply(multi.columns, function(x) {
#'             gsub("^\s+|\s+$", "", x)
#'         })
#'     if (!is.data.frame(multi.columns) & is.list(multi.columns)) {
#'         multi.columns <- do.call("cbind", multi.columns)
#'     }
#'     m <- if (handle.na) {
#'         apply(multi.columns, 1, function(x) {
#'             if (any(is.na(x))) {
#'                 NA
#'             }
#'             else {
#'                 paste(x, collapse = sep)
#'             }
#'         })
#'     }
#'     else {
#'         apply(multi.columns, 1, paste, collapse = sep)
#'     }
#'     names(m) <- NULL
#'     return(m)
#'   }
#' 
paste2 <-
function(multi.columns, sep=".", handle.na=TRUE, trim=TRUE){
    if (is.matrix(multi.columns)) {
        multi.columns <- data.frame(multi.columns)
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
