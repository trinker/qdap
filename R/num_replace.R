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
num_replace <-
function(text.var, num.paste = "separate") {
    numb2word <- function(x){ 
        helper <- function(x){ 
            digits <- rev(strsplit(as.character(x), "")[[1]]) 
            nDigits <- length(digits) 
            if (nDigits == 1) as.vector(ones[digits]) 
            else if (nDigits == 2) 
                if (x <= 19) as.vector(teens[digits[1]]) 
                    else trim(paste(tens[digits[2]], 
        Recall(as.numeric(digits[1])))) 
            else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred", 
                Recall(makeNumber(digits[2:1])))) 
            else { 
                nSuffix <- ((nDigits + 2) %/% 3) - 1 
                if (nSuffix > length(suffixes)) stop(paste(x, "is too large!")) 
                trim(paste(Recall(makeNumber(digits[ 
                    nDigits:(3*nSuffix + 1)])), 
                    suffixes[nSuffix], 
                    Recall(makeNumber(digits[(3*nSuffix):1])))) 
                } 
            } 
        trim <- function(text){ 
            gsub("^\ ", "", gsub("\ *$", "", text)) 
            } 
        makeNumber <- function(...) as.numeric(paste(..., collapse="")) 
        opts <- options(scipen=100) 
        on.exit(options(opts)) 
        ones <- c("", "one", "two", "three", "four", "five", "six", "seven", 
            "eight", "nine") 
        names(ones) <- 0:9 
        teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
            "sixteen", " seventeen", "eighteen", "nineteen") 
        names(teens) <- 0:9 
        tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", 
            "ninety") 
        names(tens) <- 2:9 
        x <- round(x) 
        suffixes <- c("thousand", "million", "billion", "trillion") 
        if (length(x) > 1) return(sapply(x, helper)) 
        helper(x) 
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
