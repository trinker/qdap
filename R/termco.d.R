#' Search for Terms
#' 
#' Search a transcript by any number of grouping variables for root terms.
#' 
#' @aliases termco.d print.termco_d
#' @param text.var text.var The text variable
#' @param grouping.var The grouping variables.  Default NULL generates one word list for all text.  Also takes a single grouping variable or a list of 1 or more grouping variables.
#' @param match.string a vector of terms to search for.
#' @param short.term logical.  If TRUE column names are trimmed versions of the match list, other wise the terms are wrapped with 'term(phrase)'
#' @param ignore.case logical.  If TRUE case is ignored.
#' @param zero.replace value to replace 0 values with
#' @param output Type of proportion output; either "proportion" (decimal format) or "percent".  Default is percent.
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed
#' @param lazy.term legical.  If TRUE finds terms with the same root.
#' @param apostrophe.remove logical.  If TRUE removes apostrophes from the text before examining.
#' @param \ldots Other argument supplied to strip.
#' @param zero.replace Value to replace 0 values with.
#' @param output Type of proportion output; either "proportion" (decimal format) or "percent".  Default is percent.
#' @param digits  Integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed.
#' @param apostrophe.remove logical.  If TRUE removes apostrophes from the text before examining.
#' @param char.keep A character vector of symbol character (i.e. punctioation) that strip should keep.  The default is to strip everything except apostophes.
#' @param digit.remove logical.  If TRUE strips digits from the text.
#' @param \ldots other argument supplied to strip
#' @return Returns a list, of class "termco.d", of data frames and information regarding word counts.
#' \item{raw}{raw word counts by grouping variable} 
#' \item{prop}{proportional word counts by grouping variable; proportional to each individual's word use} 
#' \item{rnp}{a character combination data frame of raw and proportional}     
#' \item{zero_replace}{value to replace zeros with; mostly internal use}   
#' \item{output}{character value for outpur type (either" "proportion" or "percent"; mostly internal use}  
#' \item{digits}{integer value od number of digits to display; mostly internal use}    
#' @note The match.list is (optionally) case and character sensitive.  Spacing is an important way to grab specific words and requires careful thought.  Using "read"will find the words "bread", "read" "reading", and "ready".  If you want to search fo just the word "read" you'd supply a vector of c(" read ", " reads", " reading", " reader").  
#' @seealso See Also as \code{\link[qdap]{termco.d}}
#' See Also as \code{\link[qdap]{termco.c}}
#' See Also as \code{\link[qdap]{termco.rnp}}
#' See Also as \code{\link[qdap]{termco}}
#' See Also as \code{\link[qdap]{termcount}}
#' See Also as \code{\link[qdap]{termco2matrix}}
#' @keywords word search
#' @examples
#' term.match(DATA$state, qcv(i, the))
#' termco.d(DATA$state, DATA$person, c(" the", " i'"))
#' termco.d(DATA$state, DATA$person, c(" the", " i'"), ignore.case=FALSE)
#' termco.d(DATA$state, DATA$person, c(" the ", " i'"))
termco.d <-
  function (text.var, grouping.var=NULL, match.string, short.term = FALSE,
    ignore.case = TRUE, zero.replace = 0, output = "percent", digits = 2, 
    apostrophe.remove = FALSE, char.keep = NULL, digit.remove = NULL, ...){
    lazy.term <- TRUE
    x <- unlist(match.string)
    a <- grepl("[^a-zA-Z[:space:]]", x)
    if (any(a)) {
        b <- grepl("[0-9]", x)
        if (any(b) & is.null(digit.remove)) {   
            digit.remove <- FALSE  
        } 
        if (any(a + b == 1) & is.null(char.keep)) {  
            char.keep = unlist(strsplit(paste(gsub("[a-zA-Z0-9[:space:]]", 
                "", x), collapse=""), NULL)) 
        }
    }
    NAME <- if (is.null(grouping.var)) {
        "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) {
                x[length(x)]
            })
            paste(m, collapse = "&")
        } else {
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
    }
    x <- termco(text.var = strip(text.var, lower.case = FALSE, 
       char.keep = char.keep, digit.remove = digit.remove,
        apostrophe.remove = apostrophe.remove, ...), 
        match.string = match.string, grouping.var = grouping.var, 
        ignore.case = ignore.case)
    names(x)[1] <- NAME
    y <- termco.p(tco = x, output = output, digits = digits)
    if (is.null(grouping.var) & y[1, 1] != "all"){
        z <- termco.rnp(x, y, output = output)
        znull <- as.character(z$DF)
        names(znull) <- rownames(z)
        z <- t(as.data.frame(znull))
        z <- replacer(z, "0(0)", with = zero.replace)
        z <- replacer(z, "0(0.00)", with = zero.replace)
        z <- noquote(z)
        rownames(z) <- "all"
        if (zero.replace != 0) {
            x[, -c(1:2)] <- replacer(x[, -c(1:2)], 0, zero.replace)
            y[, -c(1:2)] <- replacer(y[, -c(1:2)], 0, zero.replace)
        }
    } else {
        if (zero.replace != 0) {
            x[, -c(1:2)] <- replacer(x[, -c(1:2), drop = FALSE], 
                0, zero.replace)
            y[, -c(1:2)] <- replacer(y[, -c(1:2), drop = FALSE], 
                0, zero.replace)
        }
        z <- termco.rnp(x, y, output = output)
        h <- paste(zero.replace, "(", zero.replace, ")", sep = "")
        z[, -c(1:2)] <- lapply(z[, -c(1:2), drop = FALSE], function(x) {
            replacer(x, h, zero.replace)
        })
    }
    o <- list(raw = x, prop = y, rnp = z, zero_replace = zero.replace,
        output = output, digits = digits)
    class(o) <- "termco_d"
    if (short.term) {
        o <- termco2short.term(o)
    }
    return(o) 
}