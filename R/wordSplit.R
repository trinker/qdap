wordSplit <- function(dataframe, text.var, codes = NULL, csv = FALSE, 
    file.name = NULL, transpose = FALSE, strip =FALSE){
    tv <- as.character(dataframe[, text.var])
    if (strip) {
        tv <- strip(tv)
    }
    wrds <- lapply(tv, function(x) Trim(unlist(strsplit(x, " "))))
    lens <- sapply(wrds, length) 
    leftover <- dataframe[, !colnames(dataframe) %in% text.var, drop =FALSE]
    if (!is.null(codes)) {
        lcodes <- length(codes)
        lwrds <- length(unlist(wrds))
        MAT <- matrix(rep(0, lcodes*lwrds), lwrds, lcodes)
        colnames(MAT) <- codes      
        DF <- data.frame(leftover[rep(1:nrow(leftover), lens), , 
            drop = FALSE], text=unlist(wrds), MAT)  
    } else {
        DF <- data.frame(leftover[rep(1:nrow(leftover), lens), , 
            drop = FALSE], text=unlist(wrds)) 
    }
    if (transpose) {
        DF <- t(DF)
        DF <- data.frame(vars = rownames(DF), DF, check.names = FALSE)
        rownames(DF) <- NULL
    }
    if(csv) {
        if (is.null(file.name)) {
            file.name <- as.character(substitute(dataframe))
        } 
        write.table(DF, file = paste0(file.name, ".csv"),  sep = ",", 
            col.names = T, row.names=F, qmethod = "double") 
    }
    return(DF)
} 
