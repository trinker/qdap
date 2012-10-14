#' Concatenation Merge of a termco and termco.p Objects
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param termco1 %% ~~Describe \code{termco1} here~~
#' @param termco2 %% ~~Describe \code{termco2} here~~
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
termco.rnp <-
    function(termco1, termco2, output = NULL){
    mypaste <- function(x,y) paste(x, "(", y, ")", sep="")  
    subdf <- function(df, ii) {
        do.call("data.frame", c(as.list(df)[ii, drop=FALSE], check.names=FALSE))
    }
    DF <- mapply(mypaste, subdf(termco1, -c(1:2)), 
        subdf(termco2, -c(1:2)))
    dims <- dim(DF)
    NMS <- colnames(DF)
    formatter <- function(x, output) {
        numformat <- function(val){
            sub("^(-?)0.", "\\1.", sprintf("%.2f", as.numeric(val)))
        }
        z <- unlist(strsplit(gsub("\\)", "", x), "\\("))
        z[2] <- numformat(z[2])
        paste0(z[1], "(", z[2], output, ")")
    }
    formatter2 <- function(string, output) {
        ifelse(unlist(strsplit(string, "\\("))[1] == "0", string, formatter(string))
    }
    if (output == "percent") {
        output <- "%"
    } else {
        output <- "" 
    }
    DF <- matrix(sapply(DF, formatter2, output = output), dims)
    colnames(DF) <- NMS
    DF <- data.frame(termco1[, 1:2], DF, check.names = FALSE)
    return(DF)
}

