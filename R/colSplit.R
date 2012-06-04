colSplit <-
function(column, name.sep = "&", col.sep = "."){
    column <- as.data.frame(column)
    svar <- strsplit(as.character(column[, 1]), col.sep, fixed = TRUE)
    svar <- data.frame(do.call('rbind', svar))
    if (length(unlist(strsplit(names(column), 
        name.sep, fixed = TRUE))) > 1){
        cn <- strsplit(names(column)[1], name.sep, fixed = TRUE)[[1]]
        names(svar) <- cn
    } 
    return(svar)
}
