#' Convert Raw Numeric Matrix or Data Frame to Proportions
#' 
#' Convert a raw matrix or dataframe to proprtions/percents.  Divides each 
#' element of a column by the column sum.
#' 
#' @param mat A numeric matrix or dataframe.
#' @param digits Integer; number of decimal places to round.
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proption.
#' @param by.column logical.  If TRUE applies to the column.  If FALSE 
#' applies by row.
#' @return Returns a matrix with proportionaly scaled values.
#' @keywords proportion, percent, percentage
#' @export
#' @examples
#' \dontrun{
#' y <- wfdf(DATA$state, DATA$person, stopwords = c("your", "yours"), 
#'     margins = TRUE)
#' prop(wfm(wfdf = y), 4)       #as a proportion
#' prop(wfm(wfdf = y), 4, TRUE) #as a percentage
#' heatmap(prop(wfm(wfdf = y), 4))
#' wdstraj <- word_stats(rajSPLIT$dialogue, rajSPLIT$person)
#' prop(wdstraj$gts[, -1], 5)
#' }
prop <- function(mat, digits = 2, percent = FALSE, by.column = TRUE) {
    by.column <- by.column + 1
    constant <- ifelse(percent, 100, 1)
    out <- round(apply(mat, by.column, function(x) constant * (x/sum(x))), 
        digits = digits)
    if (by.column == 1) {
        out <- t(out)
    }
    out
}