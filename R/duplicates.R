duplicates <-
function(string, threshhold=1){
    x<-sort(unlist(strsplit(string, " ")))
    names(table(x))[table(x) >= threshhold]
}
