#' Search for Potential Missing Values
#' 
#' Search for potential missing values (i.e. sentences that are merely a puntuation 
#' mark) and optionally replace with missing value (\codeNA}).  Useful in the intial 
#' cleaning process.
#' 
#' @param text.var  The text variable.
#' @param n Number of characters to consider for missing (default is 3).
#' @return Returns a dataframe of potential missing values row numbers and text.
#' @export
#' @examples
#' \dontrun{
#' DATA$state[c(3, 7)] <- "."
#' potential_NA(DATA$state, 20)
#' potential_NA(DATA$state)
#' # USE TO SELCTIVELY REPLACE CELLS WITH MISSING VALUES
#' DATA$state[potential_NA(DATA$state, 20)$row[-c(3)]] <- NA
#' DATA
#' DATA <- qdap::DATA
#' }
potential_NA <- function(text.var, n = 3) {
    txt <- nchar(text.var) < n & !is.na(text.var)
    data.frame(row = which(txt), text = text.var[txt])
}