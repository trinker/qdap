print.word_associate <-
function(word_associate) {
    elem <- unlist(word_associate, recursive=FALSE)
    wid <- options()$width
    options(width = 10000)
    print(left.just(elem$DF.any, 4))
    options(width = wid)
}

#print.word_associate2 <-
#function(word_associate) {
#    y <- which(substring(suppressWarnings(do.call("rbind", 
#        strsplit(names(word_associate), ".", fixed=TRUE)))[, 3], 1, 4)=="list")
#    word.associate2 <- lapply(y, function(x) {
#        rownames(word_associate[[x]]) <- NULL
#        return(word_associate[[x]])
#    })
#    nms <- names(word_associate)[y]
#    wid <- options()$width
#    options(width = 10000)
#    lapply(seq_along(word.associate2), 
#        function(x){ 
#            cat("\n")
#            print(noquote(nms[x]))
#            if (nrow(word.associate2[[x]]) > 1){
#                print(left.just(word.associate2[[x]], 2))
#            } else {
#                print(word.associate2[[x]])
#            }
#        }
#    )
#    cat("\nuse $search.terms to view the words that were searched\n\n")
#    options(width = wid)
#}
