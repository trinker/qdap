#' Combine Columns to Class
#' 
#' Combine columns from qdap classes or a \code{data.frame}.
#' 
#' @param dataframe A dataframe or qdap class (e.g., 
#' \code{termco}, \code{question_type}, \code{pos_by}, \code{character_table}).
#' @param combined.columns A list of named vectors of the colnames/indexes 
#' of the numeric columns to be combined (summed).  If a vector is unnamed a 
#' name will be assigned. 
#' @param class The class to assign to the output.
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.
#' @param digits Integer; number of decimal places to round when printing.   
#' @param elim.old logical.  If \code{TRUE} eliminates the columns that are 
#' combined together by the named match.list. \code{TRUE} outputs the table 
#' proportionally (see \code{\link[qdap]{prop}}).
#' @param zero.replace Value to replace 0 values with.
#' @param override logical.  If \code{TRUE} the printing options (e.g., 
#' percent, digits, etc.) of the dataframe argument are overrode.
#' @return Returns a list with raw counts, percents and combined raw 
#' and percents.
#' @export
#' @examples
#' \dontrun{
#' ## `termco` example
#' ml <- list(
#'     cat1 = c(" the ", " a ", " an "),
#'     cat2 = c(" I'" ),
#'     "good",
#'     the = c("the", " the ", " the", "the")
#' )
#' dat1 <- with(raj.act.1,  termco(dialogue, person, ml))
#' colcomb2class(dat1, list(cats = c("cat1", "cat2")))
#' 
#' ## `question_type` example
#' dat2 <- question_type(DATA.SPLIT$state, DATA.SPLIT$person)
#' combs <- list(
#'     `wh/how` = c("what", "how"), 
#'     oth = c("shall", "implied_do/does/did")
#' )
#' colcomb2class(dat2, combs)
#' 
#' ## `pos_by` example
#' dat3 <- with(DATA, pos_by(state, list(adult, sex)))
#' colcomb2class(dat3, qcv(DT, EX, FW))
#' 
#' 
#' ## data.frame example
#' dat4 <- data.frame(X=LETTERS[1:5], matrix(sample(0:5, 20, TRUE), ncol = 4))
#' colcomb2class(dat4, list(new = c("X1", "X4")))
#' }
colcomb2class <- function(dataframe, combined.columns, class = "list", 
    percent = TRUE, digits = 2, elim.old = TRUE, zero.replace = 0, 
    override = FALSE) {

    ordat <- dataframe
    cls <- class
    attrib <- FALSE

    ## If question type or lit pull raw or count in as dataframe
    if (is.list(ordat) && !is.data.frame(ordat)) {
        sel <- ifelse(any(class(dataframe) %in% "question_type"), "count", 
            ifelse(any(class(dataframe) %in% "pos_by"), "pos.by.freq","raw"))
        dataframe <- dataframe[[sel]]

        ## overide defualt transfering calss print options
        if (!override) {
            cls <- class(ordat)

            ## check for use of class or attributes to store info
            attrib <- ifelse(is.null(ordat[["percent"]]),
                TRUE, FALSE)
            percent <- ifelse(is.null(ordat[["percent"]]),
                ifelse(is.null(attributes(ordat)[["percent"]]), 
                    TRUE, is.null(attributes(ordat)[["percent"]])), 
                    ordat[["percent"]])
            digits <- ifelse(is.null(ordat[["digits"]]),
                ifelse(is.null(attributes(ordat)[["digits"]]),
                    2, attributes(ordat)[["digits"]]), 
                    ordat[["digits"]])

        }

    }

    ## Convert numeric columns to named
    nums <- sapply(combined.columns, is.numeric)
    combined.columns[nums] <- lapply(combined.columns[nums], function(x){
       colnames(dataframe)[x]
    })
   
    ## Error checking
    mtch <- unique(unlist(combined.columns)) %in% colnames(dataframe)
    if (!all(mtch)){
        missings <- unique(unlist(combined.columns))[!mtch]
        stop(sprintf("The following columns do not exist:\n%s", 
            paste(missings, collapse = ", ")))
    }

    ## Rename the combined.columns list as necessary
    if (!is.list(combined.columns) && is.vector(combined.columns)) {
        combined.columns <- list(X1=combined.columns)
    }
    lsnms <- names(combined.columns)
    len <- length(combined.columns)
    if (len == 1 && is.null(lsnms)) {
        names(combined.columns) <- "X1"
    }
    if (len > 1 && any(lsnms == "")) {
        names(combined.columns)[lsnms == ""] <- paste0("X", seq_len(sum(lsnms == "")))
    }

    ## combine columns together
    combs <- qcombine(dataframe, combined.columns = combined.columns, 
        elim.old = elim.old)

    ## Create percents
    avoids <- c("n.words", "word.count", "tot.quest", "wrd.cnt", "wc")
    counts <- colnames(combs) %in% avoids
    notinclude <- !sapply(combs, is.numeric) | counts
    combs2 <- combs[, !notinclude]
    cons <- ifelse(percent, 100, 1)
    if (sum(counts) == 0) {
        divs <- rowSums(combs2)
    } else {
        divs <- combs[, counts]
    }
    props <- combs2/divs
    FUN <- function(x, repl = 0) {
        x[is.nan(x)] <- repl
        x
    }

    props <- apply(props, 2, FUN)*cons

    ## combine raw and proportion
    rnp <- raw_pro_comb(combs2, props, digits = digits, percent = percent, 
        zero.replace = zero.replace)

    ## Create lsit with three dataframes
    o <- lapply(list(combs2, props, rnp), function(x) {
        data.frame(cbind(dataframe[, which(notinclude), drop = FALSE], x), 
            check.names = FALSE)
    })

    ## Rename dataframes in list
    if (any(class(ordat) %in% "pos_by")) {
        names(o)[1:3] <- c("pos.by.freq", "pos.by.prop", "pos.by.rnp")
    } else {
        names(o)[1] <- ifelse(any(colnames(combs) %in% "tot.quest"), "count", "raw")
        names(o)[2:3] <- c("prop", "rnp")
    }

    ## Add percents and digits
    if (attrib) {
        attributes(o) <- attributes(ordat)
        return(o)
    } else {
        o[["digits"]] <- digits  
        o[["percent"]] <-  percent    
    }

    ## add any items from the original class not recreated
    if (is.list(ordat) && !is.data.frame(ordat)) {
        adds <- names(ordat)[!names(ordat) %in% names(o)]
        o[adds] <- ordat[adds]
    }
    
    ## add the class
    class(o) <- cls
    o
}
