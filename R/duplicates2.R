duplicates2 <-
function(string) {
    x<-sort(unlist(strsplit(string, " ")))
    unique(x[duplicated(x)])
}
