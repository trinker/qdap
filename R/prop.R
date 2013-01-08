#' Convert Raw Numeric Matrix or Data Frame to Proportions
#' 
#' Convert a raw matrix or dataframe to proprtions/percents.  Divides each 
#' element of a column by the column sum.
#' 
#' @param mat A numeric matrix or dataframe.
#' @param digits Integer; number of decimal places to round.
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proption.
#' @return Returns a matrix with proportionaly scaled values.
#' @keywords proportion, percent, percentage
#' @export
#' @examples
#' \dontrun{
#' y <- wfdf(DATA$state, DATA$person, stopwords = c("your", "yours"), margins = TRUE)
#' prop(wfm(wfdf = y), 4)       #as a proportion
#' prop(wfm(wfdf = y), 4, TRUE) #as a percentage
#' heatmap(prop(wfm(wfdf = y), 4))
#' wdstraj <- word_stats(rajSPLIT$dialogue, rajSPLIT$person)
#' prop(wdstraj$gts[, -1], 5)
#' }
prop <-
function(mat, digits = 2, percent = FALSE) {
    per <- if (percent) {
        100
    } else {
        1
    }
    daf <- data.frame(mat)
    daf <- sapply(seq_along(daf), function(x) {
        round(per*(mat[, x]/sum(mat)), digits = digits)
    })
    mat2 <- as.matrix(daf)
    colnames(mat2) <- colnames(mat)
    return(mat2)
}