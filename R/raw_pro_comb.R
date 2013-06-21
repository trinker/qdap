##Currently not exported
# Combine Raw and Proportional Matrices
# 
# Combines raw matrices with proportional for better interpretation of 
# the data.
# 
# @param raw A matrix/data frame of raw numeric counts.
# @param pro An optional matrix/dataframe of proportional information 
# that corresponds to the raw argument.  If \code{NULL} the proportions will be 
# calculated using \code\link[qdap](prop}}
# @param digits Integer; number of decimal places to round.
# @param percent logical.  If \code{TRUE} output given as percent.  If 
# \code{FALSE} the output is proption.
# @param zero.replace Value to replace 0 values with.
# @param \ldots Other arguments passed to \code{prop}.
# \code{strip} should keep.  The default is to strip every symbol except 
# apostophes.
# @param digit.remove logical.  If \code{TRUE} strips digits from the text.
# @param apostrophe.remove logical.  If \code{TRUE} removes apostrophes from the 
# output.
# @return Returns a dataframe of combined raw and proportional information.
# @export
# @examples
# \dontrun{
# raw_pro_comb(mtcars)
# raw_pro_comb(mtcars, percent = FALSE, digits = 3)
# raw_pro_comb(mtcars, percent = FALSE)
# raw_pro_comb(mtcars, percent = FALSE, zero.replace = "-")
# }
raw_pro_comb <- function(raw, pro = NULL, digits = 2, percent = TRUE, 
    zero.replace = 0, override = FALSE, ...) {
    if (is.null(pro)) {
        pro <- prop(raw, digits = digits, by.column = FALSE, percent = percent, 
            round = TRUE, ...)
    }
    s.ymb <- ifelse(percent, "%", "")
    charp <- apply(pro, 2, paster, digits = digits, symbol = s.ymb, 
        override = override)
    if (nrow(pro) == 1) {
        out <- paste0(raw, charp)
        rnp <- data.frame(matrix(out, nrow = 1), stringsAsFactors = FALSE, 
            check.names = FALSE)
    } else {
        out <- lapply(1:ncol(raw), function(i) paste0(raw[, i], charp[, i])) 
        rnp <- data.frame(do.call(cbind, out), stringsAsFactors = FALSE,
            check.names = FALSE)
    }
    colnames(rnp) <- colnames(raw)
    if (percent) {
        replacer(rnp, "0(0%)", zero.replace)
    } else {
        replacer(rnp, "0(0)", zero.replace)
    }
}



