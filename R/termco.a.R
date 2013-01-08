#' Search For and Count Terms
#' 
#' \code{termco.a} - Search a transcript by any number of 
#' grouping variables for categories (themes) of grouped root terms.  While 
#' there are other termco functions in the termco family (i.e. \code{termco.d}) 
#' \code{termco.a} is a wrapper for general use.
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default NULL generates one word 
#' list for all text.  Also takes a single grouping variable or a list of 1 or 
#' more grouping variables.
#' @param match.list a list of named character vectors
#' @param short.term logical.  If TRUE column names are trimmed versions of the 
#' match list, other wise the terms are wrapped with 'term(phrase)'
#' @param ignore.case logical.  If TRUE case is ignored.
#' @param elim.old logical.  If TRUE eliminates the columns that are combined 
#' together by the named match.list.
#' @param output Type of proportion output; either \code{"proportion"} (decimal 
#' format) or \code{"percent"}.  Default is \code{"percent"}.
#' @param digits integer indicating the number of decimal places (round) or 
#' significant digits (signif) to be used. Negative values are allowed.
#' @param apostrophe.remove logical.  If TRUE removes apostrophes from the text 
#' before examining.
#' @param char.keep A character vector of symbol character (i.e. punctioation) 
#' that strip should keep.  The default is to strip everything except apostophes.
#' @param digit.remove logical.  If TRUE strips digits from the text.
#' @param \ldots Other argument supplied to strip.
#' @return \code{termco.a} & \code{termco.d}} - both return a list, of class 
#' "termco.d", of data frames and information regarding word counts:
#' \item{raw}{raw word counts by grouping variable} 
#' \item{prop}{proportional word counts by grouping variable; proportional to 
#' each individual's word use} 
#' \item{rnp}{a character combination data frame of raw and proportional}     
#' \item{zero_replace}{value to replace zeros with; mostly internal use}   
#' \item{output}{character value for outpur type (either" "proportion" or 
#' "percent"; mostly internal use}  
#' \item{digits}{integer value od number of digits to display; mostly internal 
#' use}  
#' @rdname termco.a  
#' @note The match.list/match.string is (optionally) case and character sensitive.  Spacing 
#' is an important way to grab specific words and requires careful thought.  
#' Using "read"will find the words "bread", "read" "reading", and "ready".  If 
#' you want to search fo just the word "read" you'd supply a vector of 
#' c(" read ", " reads", " reading", " reader").  To search for non character 
#' arguments (i.e. numbers and symbols) additional arguments from strip must be 
#' passed.
#' @seealso \code{\link[qdap]{termco_c}}
#' @keywords word-search
#' @export
#' @examples
#' \dontrun{
#' #termco.a examples:
#' 
#' # General form for match.list
#' #
#' # ml <- list(
#' #     cat1 = c(),
#' #     cat2 = c(),
#' #     catn = c()
#' # )
#' 
#' ml <- list(
#'     cat1 = c(" the ", " a ", " an "),
#'     cat2 = c(" I'" ),
#'     "good",
#'     the = c("the", " the ", " the", "the")
#' )
#' 
#' (dat <- with(raj.act.1,  termco.a(dialogue, person, ml)))
#' names(dat)
#' dat$rnp  #useful for presenting in tables
#' dat$raw  #prop and raw are useful for performing calculations
#' dat$prop
#' dat <- with(raj.act.1,  termco.a(dialogue, person, ml, 
#'     short.term = FALSE, elim.old=FALSE))
#'     
#' dat2 <- data.frame(dialogue=c("@@bryan is bryan good @@br", 
#'     "indeed", "@@ brian"), person=qcv(A, B, A))
#' 
#' ml <- list(wrds=c("bryan", "indeed"), bryan=c("bryan", "@@ br", "@@br"))
#' 
#' with(dat2, termco.a(dialogue, person, match.list=ml, char.keep="@@"))
#' 
#' with(dat2, termco.a(dialogue, person, match.list=ml, 
#'     char.keep="@@", output="proportion"))
#' 
#' DATA$state[1] <- "12 4 rgfr  r0ffrg0"
#' termco.a(DATA$state, DATA$person, '0', digit.remove=FALSE)
#' 
#' #Using with term.match and exclude    
#' exclude(term.match(DATA$state, qcv(th), FALSE), "truth")
#' termco.a(DATA$state, DATA$person, exclude(term.match(DATA$state, qcv(th), 
#'     FALSE), "truth"))
#' MTCH.LST <- exclude(term.match(DATA$state, qcv(th, i)), qcv(truth, stinks))
#' termco.a(DATA$state, DATA$person, MTCH.LST)
#' 
#' #termco.d examples:
#' term.match(DATA$state, qcv(i, the))
#' termco.d(DATA$state, DATA$person, c(" the", " i'"))
#' termco.d(DATA$state, DATA$person, c(" the", " i'"), ignore.case=FALSE)
#' termco.d(DATA$state, DATA$person, c(" the ", " i'"))
#' 
#' # termco2mat example:
#' MTCH.LST <- exclude(term.match(DATA$state, qcv(a, i)), qcv(is, it, am, shall))
#' termco_obj <- termco.a(DATA$state, DATA$person, MTCH.LST)
#' termco2mat(termco_obj)
#' 
#' # as a visual
#' dat <- termco2mat(termco_obj)
#' library(gplots)
#' heatmap.2(dat, trace="none")
#' }
termco.a <-
  function (text.var, grouping.var = NULL, match.list, short.term = TRUE,
    ignore.case = TRUE, elim.old = TRUE, output = "percent", digits = 2, 
    apostrophe.remove = FALSE, char.keep = NULL, digit.remove = NULL, ...) {
    lazy.term <- TRUE
    x <- unlist(match.list)
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
    if(any(duplicated(unblanker(names(match.list))))) {
        stop("Repeated vector name in match.list")
    }
    if (is.list(match.list) & length(match.list) == 1 & is.null(names(match.list))) {
        match.list <- unlist(match.list)
    }
    mprot <- names(match.list) != "" & sapply(match.list, length) == 1
    NAME <- if (is.null(grouping.var)) {
      "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed = TRUE), 
                function(x) x[length(x)])
            paste(m, collapse = "&")
        } else {
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
    }
    preIND <- match.list
    IND <- unlist(lapply(preIND, length))
    new.names <- paste0("term(", names(IND)[IND != 1], ")")
    CC <- match.list[sapply(match.list, length) > 1]
    ML <- unlist(match.list) 
    TD <- termco.d(text.var = text.var, grouping.var = grouping.var, 
        match.string = ML, ignore.case = ignore.case, output = output, 
        apostrophe.remove = apostrophe.remove, char.keep = NULL, 
        digit.remove = FALSE, digits = digits, ...)
    if (is.list(preIND)) {
      if(length(IND) == sum(IND)){
        o <- TD
      } else {
        o <- termco.c(TD, combined.columns = CC, new.name = new.names, 
                      zero.replace = NULL, lazy.term = lazy.term, elim.old = elim.old,
                      output = output)
        if (elim.old) {
          names(match.list)[names(match.list) == ""] <- unlist(match.list[names(match.list) == ""])
          tailend <- paste0("term(", names(match.list)[names(match.list) != ""], ")")
          subdf <- function(df, ii) {
            do.call("data.frame", c(as.list(df)[ii, drop=FALSE], check.names=FALSE))
          }
          INDS <- lapply(tailend, function(x) {
            inds <- which(colnames(o[["raw"]]) == x)
            if(identical(inds, integer(0))){
              inds <- which(names(match.list) == bracketXtract(x)) + 1
            }
            inds
          })
          keeps <- c(1:2, sapply(INDS, max))
          lapply(1:3, function(i) {
            o[[i]] <<- subdf(o[[i]], keeps)
            pv <- match.list[mprot]
            pv2 <- colnames(o[[i]]) %in% paste0("term(", pv, ")")
            colnames(o[[i]])[pv2] <<- names(match.list)[pv2[-c(1:2)]]
          }
          )
        }
      }
    } else {
      o <- TD
    }
    o[1:3] <- lapply(o[1:3], function(x) {
      colnames(x)[1] <- NAME
      rownames(x) <- NULL
      return(x)
    })
    if (short.term) {
      o <- termco2short.term(o)
    }
    o
}


#' Search for Terms
#' 
#' \code{termco.d} - Search a transcript by any number of grouping variables for 
#' root terms.
#' 
#' @param match.string A vector of terms to search for.  When using inside of 
#' \code{term.match} the term(s) must be words or partial words but do not have 
#' to be when using \code{termco.d} (i.e. they can be phrases, symbols etc.).
#' @param zero.replace Value to replace 0 values with.
#' @rdname termco.a
#' @export
termco.d <-
  function (text.var, grouping.var=NULL, match.string, short.term = FALSE,
    ignore.case = TRUE, zero.replace = 0, output = "percent", digits = 2, 
    apostrophe.remove = FALSE, char.keep = NULL, digit.remove = TRUE, ...){
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
    o 
}


#' Search a Transcript for Terms
#' 
#' \code{term.match} - Search a transcript for words that exactly match term(s).
#' 
#' @param \code{term.match} - return.list logical.  If TRUE returns the output for multiple terms as 
#' a list by term rather than a vector.
#' @return \code{term.match} -  returns a list or vector of possible words that 
#' match term(s).
#' @rdname termco.a
#' @export
term.match <-
function(text.var, terms, return.list=TRUE, apostrophe.remove=FALSE) {
    y <- stopwords(text.var, stopwords = NULL, 
        unlist=TRUE, strip=TRUE, unique=TRUE, apostrophe.remove=apostrophe.remove)
    x <- lapply(unlist(terms), function(z) {
        v <- term.find(y, mat = z, logic=TRUE)
        y[v]
    })
    names(x) <- unlist(terms)
    if (!return.list){
        x <- sort(unique(unlist(x)))
    }
    x
}


#' Convert a termco dataframe to a matrix
#' 
#' \code{termco2mat} - Convert a termco dataframe to a matrix for use with 
#' visualization functions (e.g. heatmap2 of the gplots package).
#' 
#' @param dataframe A termco.a (or termco.d) dataframe or object.
#' @param drop.wc logical.  If TRUE the word count column will be dropped.
#' @param short.colnames logical.  If TRUE the ``term()'' portion of column
#' names will be dropped.
#' @param no.quote logical.  If TRUE the matrix will be printed without quotes
#' if it's character.
#' @param transform logical.  If TRUE the matrix will be transformed.
#' @return \code{termco2mat} - returns a matrix of term counts.
#' @rdname termco.a
#' @export
termco2mat <-function (dataframe, drop.wc = TRUE, short.terms = TRUE, 
  rm.zerocol = FALSE, no.quote = TRUE, transform = TRUE, trim.terms = TRUE) {
  if (class(dataframe) %in% c("termco_d", "termco_c")) {
    dataframe <- dataframe[["raw"]]
  }
  if (!is.data.frame(dataframe)) {
    stop("Please supply a data.frame to termco2mat")
  }
  ind <- if (drop.wc) {
    1:2
  } else {
    1
  }
  MAT <- as.matrix(dataframe[, -c(ind), drop = FALSE])
  rownames(MAT) <- dataframe[, 1]
  if (short.terms) {
    mn <- gsub("(.*)\\)([^\\)]*)", "\\1\\2", colnames(MAT))
    colnames(MAT) <- gsub("term(", "", mn, fixed=TRUE)
  }
  if (rm.zerocol) {
    fun <- function(x) all(ifelse(x == 0, T, F))
    MAT <- MAT[, !apply(MAT, 2, fun)]
  }
  
  OC <- length(grep("(", as.vector(unlist(MAT)), fixed = TRUE)) == 0
  if (OC) {
    z <- rownames(MAT)
    MAT <- apply(MAT, 2, as.numeric)
    rownames(MAT) <- z
  }
  if (no.quote & !OC){ 
    MAT <- noquote(MAT)
  }
  if (transform){
    MAT <- t(MAT)
  }
  if (trim.terms) {
    rownames(MAT) <- Trim(rownames(MAT))
  }
  MAT
}

#' Prints a termco_d object.
#' 
#' Prints a termco_d object.
#' 
#' @param x The termco_d object
#' @param \ldots ignored
#' @method print termco_d
#' @S3method print termco_d
print.termco_d <-
function(x, ...) {
    print(x$rnp)
}
