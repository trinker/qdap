termco.rnp <-
function(termco1, termco2){
    mypaste <- function(x,y) paste(x, "(", y, ")", sep="")  
    DF <- mapply(mypaste, termco1[, -c(1:2)], termco2[, -c(1:2)])
    DF <- data.frame(termco1[, 1:2], DF, check.names = FALSE)
    return(DF)
}
