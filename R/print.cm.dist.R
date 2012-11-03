print.cm.dist <-
function(cm.dist){
    x <- unlist(cm.dist, recursive=F)
    y <- unlist(strsplit(names(x), "\\."))[c(FALSE, TRUE)]
    z <- x[y == "standardized"]
    invisible(lapply(seq_along(z), function(i) {
        a <- strsplit(names(z)[i], "\\.")
        if(length(unlist(a)) > 1) {
            cat(paste0(a[[1]][1], "\n"))
        } 
        cat(paste0(a[[1]][length(a[[1]])], ":\n"))
        print(z[[i]])
        cat("\n")
    }))
}
