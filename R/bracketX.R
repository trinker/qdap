#' Bracket Parsing
#' 
#' \code{bracketX} - Apply bracket removal to character vectors.
#' 
#' @param text.var The text variable
#' @param bracket The type of bracket (and encased text) to remove.  This is one 
#' or more of the strings \code{"curly"}, \code{"square"}, \code{"round"}, 
#' \code{"angle"} and \code{"all"}.  These strings correspond 
#' to: \{, [, (, < or all four types.
#' @param missing Value to assign to empty cells.
#' @param names logical.  If \code{TRUE} the sentences are given as the names of 
#' the counts.
#' @param fix.space logical.  If \code{TRUE} extra spaces left behind from an 
#' extraction will be eliminated.  Additionally, non-space (e.g., 
#' \strong{"text(no space between text and parenthesis)"}) is replaced with a 
#' single space (e.g., \strong{"text (space between text and parenthesis)"}).
#' @param scrub logical.  If \code{TRUE} \code{\link[qdap]{scrubber}} will clean 
#' the text.
#' @return \code{bracketX} -  returns a vector of text with brackets removed.
#' @rdname bracketX
#' @references https://stackoverflow.com/q/8621066/1000343
#' @export
#' @seealso 
#' \code{\link[base]{regex}}
#' @examples
#' \dontrun{
#' examp <- structure(list(person = structure(c(1L, 2L, 1L, 3L), 
#'     .Label = c("bob", "greg", "sue"), class = "factor"), text = 
#'     c("I love chicken [unintelligible]!", 
#'     "Me too! (laughter) It's so good.[interrupting]", 
#'     "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names = 
#'     c("person", "text"), row.names = c(NA, -4L), class = "data.frame")    
#' 
#' examp                                                              
#' bracketX(examp$text, "square")  
#' bracketX(examp$text, "curly") 
#' bracketX(examp$text, c("square", "round")) 
#' bracketX(examp$text)  
#'                                               
#'                                               
#' bracketXtract(examp$text, "square")  
#' bracketXtract(examp$text, "curly")  
#' bracketXtract(examp$text, c("square", "round")) 
#' bracketXtract(examp$text, c("square", "round"), merge = FALSE)  
#' bracketXtract(examp$text)  
#' bracketXtract(examp$text, with = TRUE)
#' 
#' paste2(bracketXtract(examp$text, "curly"), " ")
#' 
#' x <- c("Where is the /big dog#?", 
#'     "I think he's @@arunning@@b with /little cat#.")
#' genXtract(x, c("/", "@@a"), c("#", "@@b"))
#' 
#' x <- c("Where is the L1big dogL2?", 
#'     "I think he's 98running99 with L1little catL2.")
#' genXtract(x, c("L1", 98), c("L2", 99))
#' 
#' DATA$state  #notice number 1 and 10
#' genX(DATA$state, c("is", "we"), c("too", "on"))
#' }
bracketX <-
function (text.var, bracket = "all", missing = NULL, names = FALSE, 
    fix.space = TRUE, scrub = fix.space) {
    lside <- rside <- ""
    if (fix.space) {
        lside <- rside <- "[ ]*"
        text.var <- mgsub(c("(", ")","[", "]", "{", "}", "<", ">"), 
            c(" (", ") "," [", "] ", " {", "} ", " <", "> "), text.var)
    }
    FUN <- function(bracket, text.var, missing, names) {
        X <- switch(bracket, 
            html = sapply(text.var, function(x) gsub(paste0(lside, "<.*?>", rside), "", x)),
            angle = sapply(text.var, function(x) gsub(paste0(lside, "<.*?>", rside), "", x)),
            square = sapply(text.var, function(x) gsub(paste0(lside, "\\[.*?\\]", rside), "", x)), 
            round = sapply(text.var, function(x) gsub(paste0(lside, "\\(.*?\\)", rside), "", x)), 
            curly = sapply(text.var, function(x) gsub(paste0(lside, "\\{.*?\\}", rside), "", x)), 
            all = {
                P1 <- sapply(text.var, function(x) gsub(paste0(lside, "\\[.*?\\]", rside), "", x))
                P1 <- sapply(P1, function(x) gsub(paste0(lside, "\\(.*?\\)", rside), "", x))
                P1 <- sapply(P1, function(x) gsub(paste0(lside, "<.*?>", rside), "", x))
                sapply(P1, function(x) gsub(paste0(lside, "\\{.*?\\}", rside), "", x))
            }
        )
        if (scrub) {
            X <- scrubber(gsub(" +", " ", X), fix.space = FALSE)
        }
        if (!is.null(missing)) {
            X[X == ""] <- missing
        }
        if (!names) names(X) <- NULL
        X
    }

    invisible(lapply(bracket, function(x) {
        text.var <<- FUN(x, text.var = text.var, 
            missing = missing, names = names)
    }))
    text.var
}

#' bracketXtract
#' 
#' \code{bracketXtract} - Apply bracket extraction to character vectors.
#' 
#' @rdname bracketX
#' @param with logical.  If \code{TRUE} returns the brackets and the bracketed 
#' text.
#' @param merge logical.  If \code{TRUE} the results of each bracket type will 
#' be merged by sentence.  \code{FALSE} returns a named list of lists of vectors 
#' of bracketed text per bracket type.  
#' @return \code{bracketXtract} -  returns a list of vectors of bracketed text.
#' @author  Martin Morgan and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @export
bracketXtract <-
function(text.var, bracket = "all", with = FALSE, merge = TRUE){   
    FUN <- function(text.var, bracket, with){   
        br <- bracket
        br <- ifelse(br=="round", "(", 
            ifelse(br=="square", "[", 
            ifelse(br=="curly", "{",
            ifelse(br=="html", "<",
            ifelse(br=="angle", "<", br)))))
        left <- if ("all" == br) {
            "\\(|\\{|<|\\["
        } else {
            sprintf("\\%s", br)
        }
        map <- c(`\\(`="\\)", `\\[`="\\]", `\\{`="\\}",
                 `\\<`="\\>", `\\(|\\{|<|\\[`="\\)|\\}|\\>|\\]")
        fmt <- if (with) {
            "(%s).*?(%s)"
        } else {
            "(?<=%s).*?(?=%s)"
        }
        re <- sprintf(fmt, left, map[left])
        if(length(text.var)==1){
            unlist(regmatches(text.var, gregexpr(re, text.var, perl=TRUE)))
        }else{  
            regmatches(text.var, gregexpr(re, text.var, perl=TRUE)) 
        }
    }
    out <- invisible(lapply(bracket, function(x) {
        FUN(x, text.var = text.var, with = with)
    }))
    names(out) <- bracket
    if (length(bracket) == 1) {
        return(unlist(out, recursive = FALSE))
    } else {
        if (merge) {
            out <- invisible(lapply(seq_along(text.var), function(i) {
                unlist(invisible(lapply(seq_along(out), function(j) {
                    out[[j]][[i]]
                })))
            }))            
        }
    }
    out
}


#' genX
#' 
#' \code{genX} - Apply general chunk removal to character vectors.  A 
#' generalized version of \code{bracketX}.
#' 
#' @param left A vector of character or numeric symbols as the left edge to 
#' extract.
#' @param right A vector of character or numeric symbols as the right edge to 
#' extract.
#' @rdname bracketX
#' @return \code{genXtract} - returns a vector of text with chunks removed.
#' @export
genX <- 
function (text.var, left, right, missing = NULL, names = FALSE, fix.space = TRUE, 
    scrub = TRUE) {
    if (length(left) != length(right)) {
        stop("left and right must be equal length") 
    }
    lside <- rside <- ""
    if (fix.space) {
        lside <- rside <- "[ ]*"
    }
    specchar <- c(".", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?")
    left <- mgsub(specchar, paste0("\\", specchar), left, fixed = TRUE)
    right <- mgsub(specchar, paste0("\\", specchar), right, fixed = TRUE)
    FUN <- function(left, right, text.var, missing, names) {
        X <- sapply(text.var, function(x) gsub(paste0(lside, left, ".+?", right, rside), "", x))
        if (scrub) {
            X <- scrubber(gsub(" +", " ", X))
        }
        if (!is.null(missing)) {
            X[X == ""] <- missing
        }
        if (!names) names(X) <- NULL
        X
    }
    invisible(lapply(seq_along(left), function(i) {
        text.var <<- FUN(left[i], right[i], text.var = text.var, 
            missing = missing, names = names)
    }))
    text.var
}


#' genXtract
#' 
#' \code{genXtract} - Apply general chunk extraction to character vectors.   A 
#' generalized version of \code{bracketXtract}.
#' 
#' @rdname bracketX
#' @return \code{genX} - returns a list of vectors of removed text.
#' @export
genXtract <-
function(text.var, left, right, with = FALSE, merge = TRUE){
    if (length(left) != length(right)) {
        stop("left and right must be equal length") 
    }
    LN <- left
    RN <- right
    specchar <- c(".", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?")
    left <- mgsub(specchar, paste0("\\", specchar), left, fixed = TRUE)
    right <- mgsub(specchar, paste0("\\", specchar), right, fixed = TRUE)
    FUN <- function(left, right, text.var, with){   
        fmt <- if (with) {
            "(%s).*?(%s)"
        } else {
            "(?<=%s).*?(?=%s)"
        }
        re <- sprintf(fmt, as.character(left), as.character(right))
        if(length(text.var)==1){
            unlist(regmatches(text.var, gregexpr(re, text.var, perl=TRUE)))
        }else{  
            regmatches(text.var, gregexpr(re, text.var, perl=TRUE)) 
        }
    }
    out <- invisible(lapply(seq_along(left), function(i) {
        FUN(left[i], right[i], text.var = text.var, with = with)
    }))
    names(out) <- paste(LN, " : ", RN)
    if (length(left) == 1) {
        return(unlist(out, recursive = FALSE))
    } else {
        if (merge) {
            out <- invisible(lapply(seq_along(text.var), function(i) {
                unlist(invisible(lapply(seq_along(out), function(j) {
                    out[[j]][[i]]
                })))
            }))            
        }
    }
    out
}
