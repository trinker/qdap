#helper function for termco_d and termco_a (not exported)
termco.rnp <-
    function(termco1, termco2, output = "proportion", digits = 2){
    mypaste <- function(x,y) paste(x, "(", y, ")", sep="")  
    subdf <- function(df, ii) {
        do.call("data.frame", c(as.list(df)[ii, drop=FALSE], check.names=FALSE))
    }
    DF <- mapply(mypaste, subdf(termco1, -c(1:2)), 
        subdf(termco2, -c(1:2)))
    dims <- dim(DF)
    NMS <- colnames(DF)
    if (is.null(dims)){
        dims <- c(1, length(DF))
        NMS <- names(DF)
    }
    formatter <- function(x, output, digits) {
        numformat <- function(val, digits){
            sub("^(-?)0.", "\\1.", sprintf(paste0("%.", digits, "f"), 
                as.numeric(val)))
        }
        z <- unlist(strsplit(gsub("\\)", "", x), "\\("))
        z[2] <- numformat(z[2], digits)
        paste0(z[1], "(", z[2], output, ")")
    }
    formatter2 <- function(string, output, digits) {
        ifelse(unlist(strsplit(string, "\\("))[1] == "0", string, 
            formatter(string, output, digits))
    }
    if (output == "percent") {
        output <- "%"
    } else {
        output <- "" 
    }
    DF <- matrix(sapply(DF, formatter2, output = output, digits = digits), dims)
    colnames(DF) <- NMS
    DF <- data.frame(termco1[, 1:2], DF, check.names = FALSE)
    return(DF)
}