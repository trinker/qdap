#' Find Unique Words by Grouping Variable
#' 
#' Find unique words used by grouping variable.
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @return Returns a list of unique words by grouping variable.
#' @keywords unique
#' @export
#' @examples
#' \dontrun{
#' dat <- pres_debates2012[pres_debates2012$time == "time 3", ]
#' with(dat, unique_by(dialogue, person))
#' with(pres_debates2012, unique_by(dialogue, list(time, person)))
#' 
#' with(DATA, unique_by(state, person))
#' }
unique_by <- function(text.var, grouping.var) {

    if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                x[length(x)]
            }
        )
        G <- paste(m, collapse="&")
    } else {
        G <- as.character(substitute(grouping.var))
        G <- G[length(G)]
    }

    if (is.list(grouping.var) & length(grouping.var)>1) {
        grouping <- paste2(grouping.var)
    } else {
        grouping <- unlist(grouping.var)
    } 

    DF <- stats::na.omit(data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE))
    DF[, "grouping"] <- factor(DF[, "grouping"])

    LIST <- split(DF[, "text.var"], DF[, "grouping"])
    LIST <- lapply(LIST, function(x) unique(bag_o_words(x)))

    stats::setNames(lapply(seq_along(LIST), function(i) {
        inds <- seq_along(LIST)[!seq_along(LIST) %in% i]
        sort(LIST[[i]][!LIST[[i]] %in% unique(unlist(LIST[inds]))])
    }), names(LIST))

}