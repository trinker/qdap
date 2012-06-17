#' Transcript Apply Raw Word Lists and Frequency Counts
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases qda print.qda
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param group.vars %% ~~Describe \code{group.vars} here~~
#' @param stopwords %% ~~Describe \code{stopwords} here~~
#' @param cut.n %% ~~Describe \code{cut.n} here~~
#' @param cap %% ~~Describe \code{cap} here~~
#' @param cap.list %% ~~Describe \code{cap.list} here~~
#' @param cap.I %% ~~Describe \code{cap.I} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (text.var, group.vars, stopwords = NULL, cut.n = 20, 
#'     cap = TRUE, cap.list = NULL, cap.I = TRUE) 
#' {
#'     upper <- function(x) paste(substring(x, 1, 1), substring(x, 
#'         2, nchar(x)), sep = "")
#'     Sw1 <- stopwords[!substring(stopwords, 1, 1) %in% LETTERS]
#'     Sw2 <- stopwords[substring(stopwords, 1, 1) %in% LETTERS]
#'     Sw3 <- paste(Sw2, "'s", sep = "")
#'     stopwords <- c(Sw1, upper(Sw2), Sw2, tolower(Sw2), Sw3, tolower(Sw3))
#'     group.var <- if (is.list(group.vars) & length(group.vars) > 
#'         1) {
#'         apply(data.frame(group.vars), 1, function(x) {
#'             if (any(is.na(x))) {
#'                 NA
#'             }
#'             else {
#'                 paste(x, collapse = ".")
#'             }
#'         })
#'     }
#'     else {
#'         group.vars
#'     }
#'     NAME <- if (is.list(group.vars)) {
#'         m <- unlist(as.character(substitute(group.vars))[-1])
#'         m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)], 
#'             USE.NAMES = FALSE)
#'         paste(m, collapse = "&")
#'     }
#'     else {
#'         G <- as.character(substitute(group.vars))
#'         G[length(G)]
#'     }
#'     word.lists1 <- textLISTER(text.var = text.var, group.vars = group.var)
#'     words.UNLISTED <- lapply(word.lists1, function(x) {
#'         y <- unlist(x)
#'         names(y) <- NULL
#'         return(y)
#'     })
#'     if (cap) {
#'         word.lists2 <- lapply(word.lists1, function(x) {
#'             y <- capitalizer(x, caps.list = cap.list)
#'             names(y) <- NULL
#'             return(y)
#'         })
#'     }
#'     else {
#'         word.lists2 <- lapply(word.lists1, function(x) {
#'             y <- unlist(x)
#'             names(y) <- NULL
#'             return(y)
#'         })
#'     }
#'     naomit <- function(x) x[!is.na(x)]
#'     word.lists2 <- lapply(word.lists2, naomit)
#'     stopped.word.list <- lapply(words.UNLISTED, function(x) {
#'         x[!x %in% stopwords]
#'     })
#'     stopped.word.list <- lapply(stopped.word.list, naomit)
#'     stopped.word.list <- lapply(stopped.word.list, function(x) {
#'         capitalizer(x, caps.list = cap.list)
#'     })
#'     COUNT <- function(x) {
#'         DF <- data.frame(table(x))
#'         names(DF) <- c("WORD", "FREQ")
#'         DF$WORD <- as.character(DF$WORD)
#'         DF$FREQ <- as.numeric(DF$FREQ)
#'         DF <- DF[rev(order(DF$FREQ)), ]
#'         rownames(DF) <- NULL
#'         return(DF)
#'     }
#'     freq.word.list <- lapply(word.lists2, COUNT)
#'     freq.stop.word.list <- lapply(stopped.word.list, COUNT)
#'     red.freq.stop.word.list <- ncutWORDS(freq.stop.word.list, 
#'         cut.n = cut.n)
#'     word.lists2 <- lapply(word.lists2, function(x) {
#'         comment(x) <- "bagOwords"
#'         return(x)
#'     })
#'     stopped.word.list <- lapply(stopped.word.list, function(x) {
#'         comment(x) <- "bagOwords"
#'         return(x)
#'     })
#'     freq.word.list <- lapply(freq.word.list, function(x) {
#'         comment(x) <- "freqList"
#'         return(x)
#'     })
#'     freq.stop.word.list <- lapply(freq.stop.word.list, function(x) {
#'         comment(x) <- "freqList"
#'         return(x)
#'     })
#'     red.freq.stop.word.list <- lapply(red.freq.stop.word.list, 
#'         function(x) {
#'             comment(x) <- "freqList"
#'             return(x)
#'         })
#'     comment(word.lists2) <- "cwl"
#'     comment(stopped.word.list) <- "swl"
#'     comment(freq.word.list) <- "fwl"
#'     comment(freq.stop.word.list) <- "fswl"
#'     comment(red.freq.stop.word.list) <- "rfswl"
#'     o <- list(cwl = word.lists2, swl = stopped.word.list, fwl = freq.word.list, 
#'         fswl = freq.stop.word.list, rfswl = red.freq.stop.word.list)
#'     class(o) <- "qda"
#'     return(o)
#'   }
#' 
qda <- 
function(text.var, grouping.var = NULL, stopwords = NULL, 
         cut.n = 20, cap = TRUE, cap.list=NULL, cap.I=TRUE) {
  browser()    
  upper <- function(x) paste(substring(x, 1, 1), 
                             substring(x, 2, nchar(x)), sep="")
  Sw1 <- stopwords[!substring(stopwords, 1, 1) %in% LETTERS]
  Sw2 <- stopwords[substring(stopwords, 1, 1) %in% LETTERS]
  Sw3 <- if (!is.null(Sw2)) {
    paste0(Sw2, "'s")
  } else {
    NULL
  }
  stopwords <- c(Sw1, upper(Sw2), Sw2, tolower(Sw2), Sw3, tolower(Sw3))
  if(is.null(grouping.var)){
    dat <- as.data.frame(rep("all", length(text.var)), 
                         drop = FALSE, stringsAsFactors = FALSE)
    names(dat) <- "all"
    grouping.var <- dat
  }
  group.var <- if (is.list(grouping.var) & length(grouping.var)>1) {
    apply(data.frame(grouping.var), 1, function(x){
      if (any(is.na(x))){
        NA
      } else {
        paste(x, collapse = ".")
      }
    }
    )
  } else {
    grouping.var
  }   
  NAME <- if (is.list(grouping.var)) {
    m <- unlist(as.character(substitute(grouping.var))[-1])
    m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)], USE.NAMES = FALSE)
    paste(m, collapse="&")
  } else {
    G <- as.character(substitute(grouping.var))
    G[length(G)]
  }
  word.lists1 <- textLISTER(text.var = text.var, group.vars = group.var)
  words.UNLISTED <- lapply(word.lists1, function(x) {
    y <- unlist(x)
    names(y) <- NULL
    return(y)
  }
  ) 
  if (cap){ 
    word.lists2 <- lapply(word.lists1, function(x) {
      y <- capitalizer(x, caps.list=cap.list)
      names(y) <- NULL
      return(y)
    }
    )    
  } else {
    word.lists2 <- lapply(word.lists1, function(x) {
      y <- unlist(x)
      names(y) <- NULL
      return(y)
    }
    ) 
  }
  naomit <- function(x) x[!is.na(x)]
  word.lists2 <- lapply(word.lists2, naomit)
  stopped.word.list <- lapply(words.UNLISTED, function(x) {
    x[!x %in% stopwords]
  }
  )
  stopped.word.list <- lapply(stopped.word.list, naomit)
  stopped.word.list <- lapply(stopped.word.list, function(x){ 
    capitalizer(x, caps.list = cap.list)
  }
  )
  COUNT <- function(x) {
    if (is.null(x) | identical(x, character(0)) | 
      identical(x, logical(0))) {
      DF <- data.frame(WORD=NA, FREQ=NA)
    } else {
      DF <- data.frame(table(x))
      names(DF) <- c("WORD", "FREQ")
      DF$WORD <- as.character(DF$WORD)
      DF$FREQ <- as.numeric(DF$FREQ)
      DF <- DF[rev(order(DF$FREQ)), ]
      rownames(DF) <- NULL
    }
    return(DF)
  }
  freq.word.list <- lapply(word.lists2, COUNT)
  freq.stop.word.list <- lapply(stopped.word.list, COUNT)
  red.freq.stop.word.list <- ncutWORDS(freq.stop.word.list, cut.n = cut.n) 
  word.lists2 <- lapply(word.lists2, function(x) {
    if (is.null(x)){
      return(x)
    } else { 
      comment(x) <- "bagOwords"
      return(x)
    }
  }
  )
  stopped.word.list <- lapply(stopped.word.list, function(x) {
    if (is.null(x)){
      return(x)
    } else { 
      comment(x) <- "bagOwords"
      return(x)
    }
  }
  )
  freq.word.list <- lapply(freq.word.list, function(x) {
    if (is.null(x)){
      return(x)
    } else { 
      comment(x) <- "freqList"
      return(x)
    }
  }
  )
  freq.stop.word.list <- lapply(freq.stop.word.list, function(x) {
    if (is.null(x)){
      return(x)
    } else { 
      comment(x) <- "freqList"
      return(x)
    }
  }
  )
  red.freq.stop.word.list <- lapply(red.freq.stop.word.list, function(x) {
    if (is.null(x)){
      return(x)
    } else { 
      comment(x) <- "freqList"
      return(x)
    }
  }
  )
  comment(word.lists2) <- "cwl"    
  comment(stopped.word.list) <- "swl"
  comment(freq.word.list) <- "fwl" 
  comment(freq.stop.word.list) <- "fswl"
  comment(red.freq.stop.word.list) <- "rfswl"
  o <- list(cwl = word.lists2, swl = stopped.word.list, 
            fwl = freq.word.list, fswl = freq.stop.word.list, 
            rfswl = red.freq.stop.word.list)
  class(o) <- "qda"
  return(o)
}

word.list <- qda