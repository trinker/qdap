TOT <-
function(tot){
    sapply(tot, function(x) 
           as.numeric(as.character(unlist(strsplit(as.character(x), ".", 
           fixed=TRUE))[[1]])))
}
