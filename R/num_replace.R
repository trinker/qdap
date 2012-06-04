#' Replace Numerbers With Text Representation
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param num.paste %% ~~Describe \code{num.paste} here~~
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
#' function (text.var, num.paste = "separate") 
#' {
#'     numb2word <- function(x, billion = c("US", "UK"), and = if (billion == 
#'         "US") 
#'         ""
#'     else "and") {
#'         billion <- match.arg(billion)
#'         trim <- function(text) {
#'             gsub("(^ *)|(( *|-|, zero|-zero)$)", "", text)
#'         }
#'         makeNumber <- function(x) as.numeric(paste(x, collapse = ""))
#'         makeDigits <- function(x) strsplit(as.character(x), "")[[1]]
#'         helper <- function(x) {
#'             negative <- x < 0
#'             x <- abs(x)
#'             digits <- makeDigits(x)
#'             nDigits <- length(digits)
#'             result <- if (nDigits == 1) 
#'                 as.vector(ones[digits])
#'             else if (nDigits == 2) 
#'                 if (x <= 19) 
#'                   as.vector(teens[digits[2]])
#'                 else trim(paste(tens[digits[1]], " ", ones[digits[2]], 
#'                   sep = ""))
#'             else if (nDigits == 3) {
#'                 tail <- makeNumber(digits[2:3])
#'                 if (tail == 0) 
#'                   paste(ones[digits[1]], "hundred")
#'                 else trim(paste(ones[digits[1]], trim(paste("hundred", 
#'                   and)), helper(tail)))
#'             }
#'             else {
#'                 nSuffix <- ((nDigits + 2)%/%3) - 1
#'                 if (nSuffix > length(suffixes) || nDigits > 15) 
#'                   stop(paste(x, "is too large!"))
#'                 pick <- 1:(nDigits - 3 * nSuffix)
#'                 trim(paste(helper(makeNumber(digits[pick])), 
#'                   suffixes[nSuffix], helper(makeNumber(digits[-pick]))))
#'             }
#'             if (billion == "UK") {
#'                 words <- strsplit(result, " ")[[1]]
#'                 if (length(grep("million,", words)) > 1) 
#'                   result <- sub(" million, ", ", ", result)
#'             }
#'             if (negative) 
#'                 paste("negative", result)
#'             else result
#'         }
#'         opts <- options(scipen = 100)
#'         on.exit(options(opts))
#'         ones <- c("zero", "one", "two", "three", "four", "five", 
#'             "six", "seven", "eight", "nine")
#'         teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", 
#'             "fifteen", "sixteen", " seventeen", "eighteen", "nineteen")
#'         names(ones) <- names(teens) <- 0:9
#'         tens <- c("twenty", "thirty", "forty", "fifty", "sixty", 
#'             "seventy", "eighty", "ninety")
#'         names(tens) <- 2:9
#'         suffixes <- if (billion == "US") {
#'             c("thousand", "million", "billion", "trillion")
#'         }
#'         else {
#'             c("thousand", "million", "thousand million", "billion")
#'         }
#'         x <- round(x)
#'         if (length(x) > 1) 
#'             sapply(x, helper)
#'         else helper(x)
#'     }
#'     num_sub <- function(x) {
#'         len <- attributes(gregexpr("[[:digit:]]+", x)[[1]])$match.length
#'         pos <- c(gregexpr("[[:digit:]]+", x)[[1]])
#'         values <- substring(x, pos, pos + len - 1)
#'         pos.end <- pos + len - 1
#'         replacements <- sapply(values, function(x) numb2word(as.numeric(x)))
#'         replacements <- switch(num.paste, separate = replacements, 
#'             combine = sapply(replacements, function(x) gsub(" ", 
#'                 "", x)), stop("Invalid num.paste argument"))
#'         numDF <- unique(data.frame(symbol = names(replacements), 
#'             text = replacements))
#'         rownames(numDF) <- 1:nrow(numDF)
#'         pat <- paste(numDF[, "symbol"], collapse = "|")
#'         repeat {
#'             m <- regexpr(pat, x)
#'             if (m == -1) 
#'                 break
#'             sym <- regmatches(x, m)
#'             regmatches(x, m) <- numDF[match(sym, numDF[, "symbol"]), 
#'                 "text"]
#'         }
#'         return(x)
#'     }
#'     unlist(lapply(text.var, num_sub))
#'   }
#' 
num_replace <-
function(text.var, num.paste = "separate") {
    numb2word <- function(x, billion = c("US", "UK"), and = if (billion == 
        "US") 
        "" else "and") {
        billion <- match.arg(billion)
        trim <- function(text) {
            gsub("(^ *)|(( *|-|, zero|-zero)$)", "", text)
        }
        makeNumber <- function(x) as.numeric(paste(x, collapse = ""))
        makeDigits <- function(x) strsplit(as.character(x), "")[[1]]
        helper <- function(x) {
            negative <- x < 0
            x <- abs(x)
            digits <- makeDigits(x)
            nDigits <- length(digits)
            result <- if (nDigits == 1) 
                as.vector(ones[digits]) else if (nDigits == 2) 
                if (x <= 19) 
                  as.vector(teens[digits[2]]) else trim(paste(tens[digits[1]], 
                  " ", ones[digits[2]], sep = "")) else if (nDigits == 3) {
                tail <- makeNumber(digits[2:3])
                if (tail == 0) 
                  paste(ones[digits[1]], "hundred") else trim(paste(
                  ones[digits[1]], trim(paste("hundred", and)), helper(tail)))
            } else {
                nSuffix <- ((nDigits + 2)%/%3) - 1
                if (nSuffix > length(suffixes) || nDigits > 15) 
                  stop(paste(x, "is too large!"))
                pick <- 1:(nDigits - 3 * nSuffix)
                trim(paste(helper(makeNumber(digits[pick])), suffixes[nSuffix], 
                  helper(makeNumber(digits[-pick]))))
            }
            if (billion == "UK") {
                words <- strsplit(result, " ")[[1]]
                if (length(grep("million,", words)) > 1) 
                  result <- sub(" million, ", ", ", result)
            }
            if (negative) 
                paste("negative", result) else result
        }
        opts <- options(scipen = 100)
        on.exit(options(opts))
        ones <- c("zero", "one", "two", "three", "four", "five", "six", 
            "seven", "eight", "nine")
        teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", 
            "fifteen", "sixteen", " seventeen", "eighteen", "nineteen")
        names(ones) <- names(teens) <- 0:9
        tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", 
            "eighty", "ninety")
        names(tens) <- 2:9
        suffixes <- if (billion == "US") {
            c("thousand", "million", "billion", "trillion") 
        } else {
            c("thousand", "million", "thousand million", "billion")
        }
        x <- round(x)
        if (length(x) > 1) 
            sapply(x, helper) else helper(x)
    }    
    num_sub <- function(x) {
        len <- attributes(gregexpr("[[:digit:]]+", x)[[1]])$match.length
        pos <- c(gregexpr("[[:digit:]]+", x)[[1]])
        values <- substring(x, pos, pos + len - 1)
        pos.end <- pos + len - 1
        replacements <- sapply(values, function(x) numb2word(as.numeric(x)))      
        replacements <- switch(num.paste,
            separate = replacements,
            combine =  sapply(replacements, function(x)gsub(" ", "", x)),
            stop("Invalid num.paste argument"))
        numDF <- unique(data.frame(symbol = names(replacements), 
            text = replacements))
        rownames(numDF) <- 1:nrow(numDF)       
        pat <- paste(numDF[, "symbol"], collapse = "|")
        repeat {
            m <- regexpr(pat, x)
            if (m == -1) 
                break
            sym <- regmatches(x, m)
            regmatches(x, m) <- numDF[match(sym, numDF[, "symbol"]), 
                "text"]
        }
        return(x)
    }
    unlist(lapply(text.var, num_sub))
}
