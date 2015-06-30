#' Names to Gender
#' 
#' A wrapper for the \code{\link[gender]{gender}} function used to predict 
#' gender based on first name.
#' 
#' @param names.list Character vector containing first names.
#' @param USE.NAMES logical.  If \code{TRUE} names.list is used to name the 
#' gender vector.
#' @param \ldots Other arguments passed to \code{\link[gender]{gender}}.
#' @return Returns a vector of predicted gender (M/F) based on first name.
#' @keywords name gender
#' @export
#' @importFrom qdapTools lookup
#' @importFrom gender gender
#' @seealso \code{\link[gender]{gender}}
#' @examples
#' \dontrun{
#' name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, 
#'     tyler, jamie, JAMES, tyrone, cheryl, drew))
#' }
name2sex <- function (names.list, USE.NAMES = FALSE, ...) {
    if (USE.NAMES) {
        nms <- names.list
    } else {
        nms <- NULL
    }
    `%lc_qdap%` <- qdapTools::`%lc%`

    key <- gender::gender(names.list, ...) %>%
        dplyr::select_("name", "gender") %>%
        as.data.frame() %>% 
        dplyr::mutate(gender = toupper(substring(gender, 1, 1)))

    names.list %lc_qdap% 
        key %>% 
        as.factor() %>% 
        stats::setNames(nm = nms)
}

## name2sex <- 
## function(names.list, pred.sex = TRUE, fuzzy.match = pred.sex, USE.NAMES = FALSE,
##     database = qdapDictionaries::NAMES_SEX,  ...) {
##     
##     if(pred.sex) {
##         dat <- database[, -2]
##         lvls <- c("F", "M")
##     } else {
##         dat <- database[, -3]
##         lvls <- c("F", "M", "B")
##     }
##     dat <- dat[order(dat[[1]]), ]
##     nms <- toupper(names.list)
##     out <- factor(lookup(nms, dat), levels=lvls)
## 
##     if (sum(is.na(out)) > 0 && fuzzy.match) {
##         
##         ## find closest match with `check_spelling`
##         mtchs <- toupper(check_spelling(tolower(nms[is.na(out)]), 
##             dictionary=tolower(dat[[1]]), ...)[[4]])
## 
##         ## lookup and assign gender
##         out[is.na(out)] <- lookup(mtchs, dat)
##     }
## 
##     if (USE.NAMES) {
##         names(out) <- names.list
##     }
##     out
## }

##  @param pred.sex logical.  If \code{TRUE} overlapping M/F names will be 
##  predicted based on highest cumulative frequency.  If \code{FALSE} the 
##  overlapping names will be denoted with a \code{"B"}.
##  @param fuzzy.match logical.  If \code{TRUE} uses Levenshtein edit distance 
##  from \code{\link[base]{agrep}} to predict gender from the closest name match 
##  starting with the same letter.  This is computationally intensive and should 
##  not be used on larger vectors.  Defaults to \code{pred.sex}.
