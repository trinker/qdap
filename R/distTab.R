distTab <-
function(dataframe, cuts=NULL){
    DF <- as.data.frame(dataframe)
    freq.dist <- function (var, breaks=cuts){
        x1 <- substitute(var)
        VAR <- if (is.null(cuts)){
                   var
               } else {
                   if (is.numeric(var)&length(table(var))>cuts){
                       cut(var, breaks, labels = NULL,  
                           include.lowest = FALSE, right = TRUE, 
                           dig.lab = 3, ordered_result = FALSE)
                   } else {
                       var
                   }
               }
        x <- data.frame(table(VAR))
        names(x)[1] <- as.character(x1)
        percent <- x[, 2]/sum(x[, 2])*100
            data.frame("interval"=x[, 1], "Freq"=x[, 2],
            "cum.Freq"=cumsum(x[, 2]), percent, 
            "cum.percent"=cumsum(percent))
        }
    suppressWarnings(lapply(DF, FUN=freq.dist))
}
