#A helper function used for cm_df2long.R (not for export)
cm_d2l <- function(dataframe, code.vars, no.code = NA, 
    add.start.end = TRUE, repeat.vars = NULL, rev.code = FALSE){
    if (is.numeric(code.vars)) {
        code.vars <- colnames(dataframe)[code.vars]
    }
    if (add.start.end) {
        end <- seq_len(nrow(dataframe)) 
        dataframe$start <- end - 1
        dataframe$end <- end
    }
    A <- paste2(dataframe[, code.vars])
    B <- lapply(strsplit(A, "\\."), function(x) as.logical(as.numeric(x)))
    D <- lapply(B, function(x) code.vars[x])
    D <- lapply(D, function(x) {
            if (identical(x, character(0))) {
                x <- no.code
            }
            return(x)
        }
    )
    if (is.null(repeat.vars)){
        repeat.vars <- colnames(dataframe)[!colnames(dataframe) %in% 
            code.vars]
    } else {
        if (is.numeric(repeat.vars)) {
            repeat.vars <- colnames(dataframe)[repeat.vars]
        }
    }
    E <- dataframe[, repeat.vars, drop = FALSE]
    lens <- sapply(D, length)
    NEW <- data.frame(code = unlist(D), E[rep(1:nrow(E), lens), ], stringsAsFactors = FALSE)
    rownames(NEW) <- NULL
    if (rev.code) {
        NEW[, "code"] <- factor(NEW[, "code"], levels = rev(c(code.vars, no.code)))
    } else {
        NEW[, "code"] <- factor(NEW[, "code"], levels = c(code.vars, no.code))
    }
    return(NEW)
}
