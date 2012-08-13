cm2long <- function(dataframe, code.vars, repeat.vars = NULL, 
    rev.code = TRUE){
    if (is.numeric(code.vars)) {
        code.vars <- colnames(dataframe)[code.vars]
    }
    A <- paste2(dataframe[, code.vars])
    B <- lapply(strsplit(A, "\\."), function(x) as.logical(as.numeric(x)))
    D <- lapply(B, function(x) code.vars[x])
    if (is.null(repeat.vars)){
        repeat.vars <- colnames(dataframe)[!c(text.var, codes) %in% 
            colnames(dataframe)]
    } else {
        if (is.numeric(repeat.vars)) {
            repeat.vars <- colnames(dataframe)[repeat.vars]
        }
    }
    E <- dat[, repeat.vars, drop = FALSE]
    lens <- sapply(D, length)
    NEW <- data.frame(code = unlist(D), E[rep(1:nrow(E), lens), ])
    rownames(NEW) <- NULL
    if (rev.code) {
        NEW[, "code"] <- factor(NEW[, "code"], levels = rev(code.vars))
    } else {
        NEW[, "code"] <- factor(NEW[, "code"], levels = code.vars)
    }
    return(NEW)
}