#' Names to Gender Prediction
#' 
#' Predict gender from U.S. names (based on 1990 U.S. census data).
#' 
#' @param names.list Character vector containing first names.
#' @param pred.sex logical.  If \code{TRUE} overlapping M/F names will be 
#' predicted based on highest cummulative frequency.  If \code{FALSE} the 
#' overlapping names will be denoted with a \code{"B"}.
#' @param fuzzy.match ligical.  If \code{TRUE} uses Levenshtein edit distance 
#' from \code{\link[base]{agrep}} to predict gender from the closest name match 
#' starting with the same letter.  This is computationally intensive and should 
#' not be used on larger vectors.  Defaults to \code{pred.sex}.
#' @param USE.NAMES logical.  If \code{TRUE} names.list is used to name the 
#' gender vector.
#' @return Returns a vector of predicted gender (M/F) based on first name.
#' @author Dason Kurkiewicz and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @keywords name gender
#' @references
#' \url{http://www.census.gov/genealogy/www/data/1990surnames/names_files.html}
#' 
#' \url{http://stackoverflow.com/a/818231/1000343}
#' 
#' \url{http://www.talkstats.com/showthread.php/31660}
#' @export
#' @seealso \code{\link[base]{agrep}}
#' @examples
#' \dontrun{
#' name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, 
#'     tyler, jamie, JAMES, tyrone, cheryl, drew))
#' 
#' name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, 
#'     tyler, jamie, JAMES, tyrone, cheryl, drew), FALSE)
#' 
#' name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, 
#'     tyler, jamie, JAMES, tyrone, cheryl, drew), FALSE, TRUE)
#' 
#' name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, 
#'     tyler, jamie, JAMES, tyrone, cheryl, drew), TRUE, FALSE)
#' }
name2sex <- 
function(names.list, pred.sex = TRUE, fuzzy.match = pred.sex, USE.NAMES = TRUE) {
    if(pred.sex) {
        dat <- NAMES_SEX[, -2]
    } else {
        dat <- NAMES_SEX[, -3]
    }
    nms <- toupper(names.list)
    out <- lookup(nms, dat)
    if (fuzzy.match) {
        FUN <- function(pattern, pred.sex2 = ifelse(pred.sex, 3, 2)) {
            sector <- NAMES_LIST[names(NAMES_LIST) %in% substring(pattern, 1, 1)][[1]]
            sector[which.min(Ldist(pattern, sector[, 1]))[1], pred.sex2]
        }
        if (length(out) == 1 && is.na(out)) {
            out <- FUN(nms)
        } else {
            out[is.na(out)] <- as.character(sapply(nms[is.na(out)], FUN))
        }
    }
    out <- factor(out)
    if (USE.NAMES) {
        names(out) <- names.list
    }
    out
}
