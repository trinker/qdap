bracketX <-
function(text, bracket='all', missing=NULL){
    X <- switch(bracket,
        square=sapply(text, function(x)gsub("\\[.+?\\]", "", x)),
        round=sapply(text, function(x)gsub("\\(.+?\\)", "", x)),
        curly=sapply(text, function(x)gsub("\\{.+?\\}", "", x)),
        all={P1<-sapply(text, function(x)gsub("\\[.+?\\]", "", x))
             P1<-sapply(P1, function(x)gsub("\\(.+?\\)", "", x))
             sapply(P1, function(x)gsub("\\{.+?\\}", "", x))
             }
    ) 
    X <- Trim(gsub(" +", " ", X))
    if (!is.null(missing)) {X[X==""] <- missing}
    return(X)                                                                                                                                                         
}
