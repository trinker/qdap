#used in word_list (not exported)
reduceWORDS <-
function(rfwlDF_LIST){
    RFL <- function(rfwlDF){
    BB <- rfwlDF
        unlist(as.character(BB[ rep( seq(dim(BB)[1]), BB$FREQ), 1]))
    }
    lapply(rfwlDF_LIST, RFL)
}
