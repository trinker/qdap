#' Names to Gender Prediction
#' 
#' Predict gender from U.S. names (based on 1990 U.S. census data).
#' 
#' @param names.list Character vector containing first names.
#' @param pred.sex logical.  If \code{TRUE} overlapping M/F names will be 
#' predicted based on highest cummulative frequency.  If \code{FALSE} the 
#' overlapping names will be denoted with a \code{"B"}.
#' @return Returns a vector of predicted gender (M/F) based on first name.
#' @keywords name gender
#' @references
#' \url{http://www.census.gov/genealogy/www/data/1990surnames/names_files.html}
#' 
#' \url{http://stackoverflow.com/a/818231/1000343}
#' @export
#' @examples
#' \dontrun{
#' names2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, 
#'     tyler, jamie, JAMES, tyrone))
#'     
#' names2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, 
#'     tyler, jamie, JAMES, tyrone, cheryl, drew), FALSE)
#' }
names2sex <- function(names.list, pred.sex = TRUE) {
    if(pred.sex) {
        dat <- NAMES_SEX[, -2]
    } else {
        dat <- NAMES_SEX[, -3]
    }
    factor(lookup(toupper(names.list), dat))
}
