#' Generate SPSS Style Frequency Tables
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param dataframe %% ~~Describe \code{dataframe} here~~
#' @param cuts %% ~~Describe \code{cuts} here~~
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
#' function (dataframe, cuts = NULL) 
#' {
#'     DF <- as.data.frame(dataframe)
#'     freq.dist <- function(var, breaks = cuts) {
#'         x1 <- substitute(var)
#'         VAR <- if (is.null(cuts)) {
#'             var
#'         }
#'         else {
#'             if (is.numeric(var) & length(table(var)) > cuts) {
#'                 cut(var, breaks, labels = NULL, include.lowest = FALSE, 
#'                   right = TRUE, dig.lab = 3, ordered_result = FALSE)
#'             }
#'             else {
#'                 var
#'             }
#'         }
#'         x <- data.frame(table(VAR))
#'         names(x)[1] <- as.character(x1)
#'         percent <- x[, 2]/sum(x[, 2]) * 100
#'         data.frame(interval = x[, 1], Freq = x[, 2], cum.Freq = cumsum(x[, 
#'             2]), percent, cum.percent = cumsum(percent))
#'     }
#'     suppressWarnings(lapply(DF, FUN = freq.dist))
#'   }
#' 
distTab <-
function(dataframe, cuts=NULL){
    DF <- as.data.frame(dataframe)
    freq.dist <- function (var, breaks=cuts){
        x1 <- substitute(var)
        VAR <- if (is.null(cuts)){
                   var
               } else {
                   if (is.numeric(var)&length(table(var))>cuts){
                       cut(var, breaks, labels = NULL,  
                           include.lowest = FALSE, right = TRUE, 
                           dig.lab = 3, ordered_result = FALSE)
                   } else {
                       var
                   }
               }
        x <- data.frame(table(VAR))
        names(x)[1] <- as.character(x1)
        percent <- x[, 2]/sum(x[, 2])*100
            data.frame("interval"=x[, 1], "Freq"=x[, 2],
            "cum.Freq"=cumsum(x[, 2]), percent, 
            "cum.percent"=cumsum(percent))
        }
    suppressWarnings(lapply(DF, FUN=freq.dist))
}
