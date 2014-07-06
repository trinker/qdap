#' Names to Gender Prediction
#' 
#' Predict gender from U.S. names (based on 1990 U.S. census data).
#' 
#' @param names.list Character vector containing first names.
#' @param pred.sex logical.  If \code{TRUE} overlapping M/F names will be 
#' predicted based on highest cumulative frequency.  If \code{FALSE} the 
#' overlapping names will be denoted with a \code{"B"}.
#' @param fuzzy.match logical.  If \code{TRUE} uses Levenshtein edit distance 
#' from \code{\link[base]{agrep}} to predict gender from the closest name match 
#' starting with the same letter.  This is computationally intensive and should 
#' not be used on larger vectors.  Defaults to \code{pred.sex}.
#' @param USE.NAMES logical.  If \code{TRUE} names.list is used to name the 
#' gender vector.
#' @param database A database of names (mostly for internal purposes).
#' @param \ldots Other aguments passed to \code{\link[qdap]{check_spelling}}.
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
#' @importFrom qdapTools lookup
#' @seealso \code{\link[stringdist]{stringdist}}
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
#'
#' ## Get rank percent frequency ratio of being a gender
#' library(qdapDictionaries)
#' 
#' orig_nms <- qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA,
#'     tyler, jamie, JAMES, tyrone, cheryl, drew)
#' 
#' sex <- name2sex(orig_nms, FALSE, TRUE)
#' 
#' names(sex) <- rep("", length(sex))
#' names(sex)[sex == "B"] <- sapply(toupper(orig_nms[sex == "B"]), function(x) {
#'         y <- NAMES[NAMES[, 1] %in% x, ]
#'         round(log(Reduce("/", y[ order(y[, "gender"]), "per.freq"])), 2)
#'     })
#' 
#' ## The log ratio of being a female name
#' sex
#' orig_nms 
#' data.frame(name = orig_nms, sex = sex, `ratio_F:M` = names(sex), 
#'     check.names=FALSE)
#' }
name2sex <- 
function(names.list, pred.sex = TRUE, fuzzy.match = pred.sex, USE.NAMES = FALSE,
    database = qdapDictionaries::NAMES_SEX,  ...) {
    
    if(pred.sex) {
        dat <- database[, -2]
        lvls <- c("F", "M")
    } else {
        dat <- database[, -3]
        lvls <- c("F", "M", "B")
    }
    nms <- toupper(names.list)
    out <- factor(lookup(nms, dat), levels=lvls)

    if (sum(is.na(out)) > 0 && fuzzy.match) {
        
        ## find closest match with `check_spelling`
        mtchs <- toupper(check_spelling(tolower(nms[is.na(out)]), 
            dictionary=tolower(dat[[1]]), ...)[[4]])

        ## lookup and assign gender
        out[is.na(out)] <- lookup(mtchs, dat)
    }

    if (USE.NAMES) {
        names(out) <- names.list
    }
    out
}
