#' Search for Terms
#' 
#' Search a transcript by any numbe rof grouping variables for categories (themes) of grouped terms.  While there are other termco functions int he termco family termco.a is the function that will generally be used.
#' 
#' @aliases termco.d print.termco_d
#' @param text.var text.var The text variable
#' @param grouping.var The grouping variables.  Default NULL generates one word list for all text.  Also takes a single grouping variable or a list of 1 or more grouping variables.
#' @param match.list a list of named character vectors
#' @param short.term logical.  If TRUE column names are trimmed versions of the match list, other wise the terms are wrapped with 'term(phrase)'
#' @param ignore.case logical.  If TRUE case is ignored.
#' @param elim.old logical.  If TRUE eliminates the columns that are combined together by the named match.list.
#' @param output Type of proportion output; either "proportion" (decimal format) or "percent".  Default is percent.
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed
#' @param \ldots other argument supplied to strip
#' @return Returns a list of data frames and information regarding word counts.
#' \item{raw}{raw word counts by grouping variable} 
#' \item{prop}{proportional word counts by grouping variable; proportional to each individual's word use} 
#' \item{rnp}{a character combination data frame of raw and proportional}     
#' \item{zero_replace}{value to replace zeros with; mostly internal use}   
#' \item{output}{character value for outpur type (either" "proportion" or "percent"; mostly internal use}  
#' \item{digits}{integer value od number of digits to display; mostly internal use}    
#' @note The match.list is case and character sensitive.  Spacing is an important way to grab specific words and requires careful thought.  Using "read"will find the words "bread", "read" "reading", and "ready".  If you want to search fo just the word "read" you'd supply a vector of c(" read ", " reads", " reading", " reader").  
#' @seealso See Also as \code{\link[qdap]{termco.d}}
#' See Also as \code{\link[qdap]{termco.c}}
#' See Also as \code{\link[qdap]{termco.rnp}}
#' See Also as \code{\link[qdap]{termco}}
#' See Also as \code{\link[qdap]{termcount}}
#' See Also as \code{\link[qdap]{termco2matrix}}
#' @keywords word search
#' @examples
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
#' dat2 <- data.frame(dialogue=c("@bryan is bryan good @br", 
#'     "indeed", "@ brian"), person=qcv(A, B, A))
#' 
#' ml <- list(wrds=c("bryan", "indeed"), bryan=c("bryan", "@ br", "@br"))
#' 
#' with(dat2, termco.a(dialogue, person, match.list=ml, char.keep="@"))
#' 
#' with(dat2, termco.a(dialogue, person, match.list=ml, 
#'     char.keep="@", output="proportion"))
termco.a <-
  function (text.var, grouping.var=NULL, match.list, short.term = TRUE,
            ignore.case = TRUE, elim.old = TRUE, output = "percent", digits = 2, ...) {
    lazy.term <- TRUE
    if(any(duplicated(unblanker(names(match.list))))) stop("Repeated vector name in match.list")
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
                   match.string = ML, ignore.case = ignore.case,   output = output, digits = digits, ...)
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
    return(o)
}