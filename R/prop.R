#' Convert Raw Numeric Matrix or Data Frame to Proportions
#' 
#' Convert a raw matrix or dataframe to proportions/percents.  Divides each 
#' element of a column by the column sum.
#' 
#' @param mat A numeric matrix or dataframe.
#' @param digits Integer; number of decimal places to round.
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.
#' @param by.column logical.  If \code{TRUE} applies to the column.  If 
#' \code{FALSE} applies by row.
#' @param round logical.  If \code{TRUE} rounds the returned values (controlled 
#' by digits).
#' @return Returns a matrix with proportionally scaled values.
#' @export
#' @examples
#' \dontrun{
#' y <- wfdf(DATA$state, DATA$person, stopwords = c("your", "yours"), 
#'     margins = TRUE)
#' prop(wfm(y), 4)[1:10, ]        #as a proportion
#' prop(wfm(y), 4, TRUE)[1:10, ]  #as a percentage
#' heatmap(prop(wfm(y), 4))
#' wdstraj <- word_stats(rajSPLIT$dialogue, rajSPLIT$person)
#' prop(wdstraj$gts[, -1], 5)[1:15, 1:6]
#' }
prop <- function(mat, digits = 2, percent = FALSE, by.column = TRUE, 
    round = FALSE) {
    by.column <- by.column + 1
    constant <- ifelse(percent, 100, 1)
    out <- apply(mat, by.column, function(x) constant * (x/sum(x)))
    if (round) {
        out <- round(out, digits = digits)
    }
    if (by.column == 1) {
        out <- t(out)
    }
    out
}
