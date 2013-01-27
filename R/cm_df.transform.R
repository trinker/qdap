#Used in cm_df.fill  not currently exported
cm_df.transform <-
function(dataframe, text.var, code.vars = NULL) {
    DF <- data.frame(t(dataframe), stringsAsFactors = FALSE)
    if (!is.numeric(text.var)) {
        text.var <- which(colnames(DF) == text.var)
    }
    if (is.null(code.vars)) {
        code.vars <- text.var + 2
    } 
    if (!is.numeric(code.vars)) {
        code.vars <- which(colnames(DF) %in% c(code.vars))
    }
    left.overs <- which(!1:ncol(DF) %in% c(code.vars, text.var))
    DF[, text.var] <- as.character(DF[, text.var])
    lapply(code.vars, function(i) {
            DF[, i] <<- as.numeric(DF[, i])
        return(DF)
        }
    )
    lapply(left.overs, function(i) {
            DF[, i] <<- as.factor(DF[, i])
        return(DF)
        }
    )
    colnames(DF) <- as.character(unlist(DF[1, ]))
    rownames(DF) <- NULL
    return(DF[-1, ])
}
